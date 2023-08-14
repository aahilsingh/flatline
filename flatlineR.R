flatline = function(survey_data_frame, interview_Number_var, enumerator_ID_var, list_of_modules, qn_texts, baseline, output_file) {

  # Load required libraries
  library(haven)
  library(dplyr)
  library(writexl)
  require(openxlsx)
  library(ggplot2)
  library(pdftools)
  
  # Read the .sav file and retrieve data
  data <- survey_data_frame
  Survey.df <- survey_data_frame
  
  # Create a DataFrame from the data
  df <- as.data.frame(data)
  
  module_list <- list_of_modules
  
  qn_text <- qn_texts
  
  #setting int values and enum values
  
  int_ids <- interview_Number_var
  enum_ids <- enumerator_ID_var
  
  
  module_count <- 1
  

  # Storing flagged modules for ints
  int_flagged_modules <- setNames(replicate(length(int_ids), list(character())), unique(int_ids))
  
  
  # Initialize an empty list to store the module response dictionaries
  module_df_list <- list()
  
  # Function to calculate the number of unique values in each row and add a new column "Unique" to the DataFrame
  calculate_unique <- function(df_module) {
    # Use rowwise() and mutate() from dplyr to calculate the number of unique values in each row
    df_module <- df_module %>%
      rowwise() %>%
      mutate(Unique = ifelse(all(is.na(c_across())), 999, n_distinct(c_across())))
    
    # Return the modified data frame
    return(df_module)
  }
  
  
  summary <- function(df_module, module_response_list) {
    # Create a data frame from the module_response_list
    labels_df <- as.data.frame(module_response_list)
    
    # Set row names for labels_df to match the original row names of module_response_list
    rownames(labels_df) <- NULL
    
    colnames(labels_df) <- c("Label", "Code")
    
    # Count of unique values for each label
    count <- c()
    count_percent <- c()
    
    for (i in 1:nrow(labels_df)) {  # Corrected the loop to iterate from 1 to nrow(labels_df)
      current <- sum(df_module$Unique == 1 & df_module[[2]] == labels_df[i, 2])
      count <- append(count, current)  # Corrected the append function to store the 'current' variable
      count_percent <- append(count_percent, round((current / nrow(df_module)) * 100, 1))  # Corrected the append function here as well
    }
    
    avg <- round(mean(df_module$Unique[df_module$Unique != 999]), digits = 2)
    med <- median(df_module$Unique)
    
    mean_values <<- c(mean_values, avg)
    median_values <<- c(median_values, med)
    
    # Add the count and percentage columns to labels_df
    labels_df$Count <- count
    labels_df$Percentage <- count_percent
    
    
    return(labels_df)
    
  }
  
  #creating a function that creates a dataframe for enumerator data
  int_mod <- function(df_module) {
    #getting the enum IDs
    ids_enum <- enum_ids
    
    #creating a new df from the df_module data
    enum_df <- df_module
    
    #joining to df_module
    enum_df$ENUM_ID <- ids_enum
    
    # Create an empty data frame to store enum stats
    enum_stats <- data.frame(Enum_ID = numeric(),
                             Enum_NonFlat = numeric(),
                             Enum_Flat = numeric(),
                             Enum_Total = numeric(),
                             stringsAsFactors = FALSE)
    
    # Iterating through each unique enum
    for (i in unique(enum_df$ENUM_ID)) {
      enum_flat <- 0
      enum_nonflat <- 0
      enum_total <- 0
      
      # Iterating through df to check for flat response rates
      for (j in 1:nrow(enum_df)) {
        if (enum_df$Unique[j] == 1 && i == enum_df$ENUM_ID[j]) {
          enum_flat <- enum_flat + 1
          enum_total <- enum_total + 1
        }
        if (enum_df$Unique[j] != 1 && i == enum_df$ENUM_ID[j]) {
          enum_nonflat <- enum_nonflat + 1
          enum_total <- enum_total + 1
        }
      }
      
      # Adding the stats for each enum to the data frame
      enum_stats <- rbind(enum_stats, data.frame(Enum_ID = i,
                                                 Enum_NonFlat = enum_nonflat,
                                                 Enum_Flat = enum_flat,
                                                 Enum_Total = enum_total))
    }
    
    # Reset row names of the data frame
    rownames(enum_stats) <- NULL
    
    result <- sum(enum_stats$Enum_Total)
    
    #calculating % flat
    percent_flat <- c()
    for (k in 1:nrow(enum_stats)) {
      flat <- round((enum_stats[k, "Enum_Flat"] / enum_stats[k, "Enum_Total"]) * 100, 1)
      percent_flat <- c(percent_flat, flat)
    }
    
    enum_stats$`Flat %` <- percent_flat
    
    avg <- round(mean(enum_stats$'Flat %'), digits = 2)
    med <- median(enum_stats$'Flat %')
    
    enum_mean_values <<- c(enum_mean_values, avg)
    enum_median_values <<- c(enum_median_values, med)
    
    
    return(enum_stats)
  }
  
  
  enum_data <- function(df_enum, iqr, q3, enum_flagged_modules, module_count) {
    base_ints <- baseline
    
    # Setting outlier threshold based on IQR
    outlier <- q3 + (1.5 * iqr)
    enum_outliers <<- c(enum_outliers, outlier)
    
    for (i in 1:nrow(df_enum)) {
      if (df_enum$Enum_Total[i] >= base_ints && df_enum$`Flat %`[i] >= outlier) {
        # If 'if' condition is true, append the 'module_count' value to the corresponding enum_id's list in the enum_flagged_modules dictionary
        enum_flagged_id <- as.character(df_enum$Enum_ID[i])
        enum_flagged_modules[[enum_flagged_id]] <<- append(enum_flagged_modules[[enum_flagged_id]], module_count, after = length(enum_flagged_modules[[enum_flagged_id]]))
      }
    }
  }
  
  
  int_data <- function(df_module, module_count) {
    
    for (i in 1:nrow(df_module)) {
      if (df_module$Unique[i] == 1) {
        int_flagged_id <- as.character(df_module$INTNR[i])
        int_flagged_modules[[int_flagged_id]] <<- append(int_flagged_modules[[int_flagged_id]], (as.numeric(module_count)), after = length(int_flagged_modules[[int_flagged_id]]))
      }
    }
  }
  
  
  module_count = 0
  module_response_list <- list()
  label_summary_df <- list()  # Create a list to store the summary data frames for each module
  df_enum_list <- list()
  
  
  unique_enum_ids <- unique(enum_ids)
  enum_flagged_modules <- list()
  
  # Initialize each list as an empty list
  for (enum_id in unique_enum_ids) {
    enum_flagged_modules[[as.character(enum_id)]] <- list()
  }
  
  
  mean_values <- c()
  median_values <- c()
  
  enum_mean_values <- c()
  enum_median_values <- c()
  enum_outliers <- c()
  
  
  
  # Loop through each module in the module_list
  for (module in module_list) {
    module_count = module_count + 1
    # Create an empty list to store responses for each variable in the module
    var_responses <- list()
    
    # Loop through each var_name in the current module
    for (var_name in module) {
      # Check if var_name is in names(df)
      if (var_name %in% names(df)) {
        module_response_list <- (list(cbind(as.vector(names(attributes(Survey.df[,module_list[[module_count]][1]])$labels)),
                                            as.vector(attributes(Survey.df[,module_list[[module_count]][1]])$labels))))
        response <- as.numeric(df[[var_name]])
        var_responses[[var_name]] <- response
      } else {
        cat(paste("No value labels found for variable '", var_name, "'\n", sep = ""))
      }
    }
    
    
    # Create a data frame directly from var_responses
    df_module <- do.call(data.frame, var_responses)
    
    
    # Call the function to calculate the "Unique" column for the current dataframe
    df_module <- calculate_unique(df_module)
    
    
    #creating a list of dataframes for the module data (w/ unique columns in each df)
    module_df_list <- append(module_df_list, list(df_module), after = length(module_df_list))
    
    
    # Call the summary function and store the result in the summary_df data frame
    summary_df <- summary(df_module, module_response_list)
    
    
    #creating a list of dataframes for the label summaries 
    label_summary_df <- append(label_summary_df, list(summary_df), after = length(label_summary_df))
    
    #calling the int mod function to create a table for enums
    df_enum <- int_mod(df_module)
    
    #calculating IQR 
    flat_percentage <- df_enum$`Flat %`
    iqr <- IQR(flat_percentage)
    q3 <- as.numeric(quantile(df_enum$`Flat %`, probs = 0.75))
    
    #creating a list of enum data dfs
    df_enum_list <- append(df_enum_list, list(df_enum), after = length(df_enum_list))
    
    # Calling enum_data that flags enums and counts flagged modules
    enum_mod_flags <- enum_data(df_enum, iqr, q3, enum_flagged_modules, module_count)
    
    # Add the INTNR column to df_module
    df_module$INTNR <- int_ids
    
    #calling int_data that flags based on int nr and couts flagged modules
    int_mod_flags <- int_data(df_module, module_count)
  }
  
  #Converting enum_flagged_modules into a df
  enum_flagged_df <- data.frame(Enum_ID = character(), Flagged_Modules = character(), stringsAsFactors = FALSE)
  
  for (i in seq_along(enum_flagged_modules)) {
    enum_id <- names(enum_flagged_modules[i])
    flagged_modules <- paste(enum_flagged_modules[[i]], collapse = ", ")
    enum_flagged_df <- rbind(enum_flagged_df, data.frame(Enum_ID = enum_id, Flagged_Modules = flagged_modules))
  }
  
  # Convert int_flagged_modules to a data frame
  int_flagged_df <- data.frame(INTNR = character(), Flagged_Modules = character(), stringsAsFactors = FALSE)
  
  for (i in seq_along(int_flagged_modules)) {
    intnr <- names(int_flagged_modules[i])
    flagged_modules <- paste(int_flagged_modules[[i]], collapse = ", ")
    int_flagged_df <- rbind(int_flagged_df, data.frame(INTNR = intnr, Flagged_Modules = flagged_modules))
  }
  
  #adding a 'count' column to enum_flagged_modules
  enum_flagged_df$Flag_Count <- NA
  
  for (i in seq_along(enum_flagged_df$Flagged_Modules)) {
    # Split the comma-separated values and count the number of elements
    len_count <- length(strsplit(enum_flagged_df$Flagged_Modules[i], ",")[[1]])
    
    # Assign the count to the 'Flag_Count' column for the current row
    enum_flagged_df$Flag_Count[i] <- len_count
  }
  
  #adding a 'count' and 'enum_id' column to int_flagged_modules
  int_flagged_df$Flag_Count <- NA
  int_flagged_df$enum_id <- NA
  
  for (i in seq_along(int_flagged_df$INTNR)) {
    # Split the comma-separated values and count the number of elements
    len <- length(strsplit(int_flagged_df$Flagged_Modules[i], ",")[[1]])
    
    # Assign the count to the 'Flag_Count' column for the current row
    int_flagged_df$Flag_Count[i] <- len
    
    enum <- enum_ids[i]
    
    int_flagged_df$enum_id[i] <- enum
    
  }
  
  
  
  #Excel output --------------------------
  
  
  # Create a new Excel workbook
  output_excel_name <- output_file
  
  # Initialize the Excel workbook
  wb <- createWorkbook()
  
  module_num <- 1
  mean_values <- as.numeric(mean_values)
  median_values <- as.numeric(median_values)
  
  # Write each data frame in label_summary_df to a separate sheet
  for (i in seq_along(label_summary_df)) {
    sheet_name <- paste("Module", i, sep = "_")
    addWorksheet(wb, sheetName = sheet_name)
    
    table_len <- as.numeric(nrow(label_summary_df[[i]]))
    writeDataTable(wb, sheet_name, label_summary_df[[i]], startRow = 2, startCol = 1, colNames = TRUE, tableStyle = "TableStyleMedium18")
    
    writeData(wb, sheet_name, "Average", startCol = 3, startRow = table_len + 3)
    writeData(wb, sheet_name, mean_values[i], startCol = 4, startRow = table_len + 3)
    
    writeData(wb, sheet_name, "Median", startCol = 3, startRow = table_len + 4)
    writeData(wb, sheet_name, median_values[i], startCol = 4, startRow = table_len + 4)
    
    
    # Merge the first 4 cells in the first row
    mergeCells(wb, sheet = sheet_name, cols = 1:4, rows = 1)
    
    # Write the corresponding text from 'qn_text' to the merged cell
    writeData(wb, sheet_name, qn_text[i], startCol = 1, startRow = 1)
    
    module_num <- module_num + 1
  }
  
  
  
  # Write each data frame in df_enum_list to the corresponding sheet
  for (i in seq_along(df_enum_list)) {
    sheet_name <- paste("Module", i, sep = "_")  # Match the sheet name with the one used in the first loop
    writeDataTable(wb, sheet_name, df_enum_list[[i]], startRow = 1, startCol = 6, colNames = TRUE, tableStyle = "TableStyleMedium18")
    
    table_len <- as.numeric(nrow(df_enum_list[[i]]))
    if (i <= as.numeric(length(df_enum_list))) {
      writeData(wb, sheet_name, "Average", startCol = 9, startRow = table_len + 3)
      writeData(wb, sheet_name, enum_mean_values[i], startCol = 10, startRow = table_len + 3)
      
      writeData(wb, sheet_name, "Median", startCol = 9, startRow = table_len + 4)
      writeData(wb, sheet_name, enum_median_values[i], startCol = 10, startRow = table_len + 4)
      
      writeData(wb, sheet_name, "Outlier Threshold", startCol = 9, startRow = table_len + 5)
      writeData(wb, sheet_name, enum_outliers[i], startCol = 10, startRow = table_len + 5)
      
      
    }
  }
  
  
  #Creating the summary sheet
  sheet_name <- paste("Summary")
  addWorksheet(wb, sheetName = sheet_name)
  
  #adding the enum summary table
  writeDataTable(wb, sheet_name, enum_flagged_df, startRow = 1, startCol = 1, colNames = TRUE, tableStyle = "TableStyleMedium18")
  
  #adding the int summary table
  writeDataTable(wb, sheet_name, int_flagged_df, startRow = 1, startCol = 5, colNames = TRUE, tableStyle = "TableStyleMedium18")
  
  
  # Save the plot as a PNG file
  pdf_file_path <- "enum_stats.pdf"
  png_file_path <- "enum_stats.png"
  
  pdf(pdf_file_path, width = 12, height = 10)
  num_rows <- 4
  num_cols <- 3
  par(mfrow = c(num_rows, num_cols))
  
  for (i in seq_along(df_enum_list)) {
    df_enum <- df_enum_list[[i]]
    boxplot(df_enum$`Flat %`,
            main = paste("Flat % Rates - Module", i),
            ylab = "Flat %",
            col = "orange",
            border = "black",
            horizontal = TRUE)
  }
  
  par(mfrow = c(1, 1))
  dev.off()
  
  # Convert the PDF plot to PNG
  png_file_path <- pdf_convert(pdf_file_path, format = "png", dpi = 300)
  
  
  # Add the PDF to the "Summary" sheet
  insertImage(wb, sheet_name, png_file_path, width = 12, height = 10, startRow = 1, startCol = 10)
  
  # Save the workbook to the specified Excel file
  saveWorkbook(wb, output_excel_name)
  
  message(paste("Done! Check your current working directory for ", output_excel_name))
}
