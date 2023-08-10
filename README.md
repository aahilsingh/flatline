# flatline

My program detects and analyzes cases of flatlining / straight lining in survey data. [Flatlining](https://www.qualtrics.com/blog/straightlining-what-is-it-how-can-it-hurt-you-and-how-to-protect-against-it/) occurs when respondents provide the same response to a series of questions in a module (with one overarching question).
As part of this analysis, my code loops through each module inputted, detects instances of flatlining for each label and for each enumerator, and flags enumerators with extreme rates of flatlining. Enumerators are flagged if
the proportion of flat interviews that they conduct is greater than or equal to 1.5 * IQR (interquartile range) + Q3 (3rd quartile value), provided their total interviews are greater than the baseline interviews value. This 
flagging process is repeated for each module. The program also creates a summary tab that displays rates of flatlining across all modules for both enumerators and respondents. Respondents are flagged if they had a flat
response in a particular module.

The code is written in R and outputs its analysis in an Excel workbook.

# RStudio
Prerequisite libraries: haven, dplyr, writexl, opnxlsx, ggplot2, pdftools

## Inputs:
- survey_data_frame: .sav file (of survey data) in a dataframe format. Recommend using the ‘loadsurvey.df’ function from the source code to convert the .sav file into a dataframe.
- interview_Number_var: variable name referencing the interview number variable from the dataframe. If there is no variable that tracks the interview number, create a column in the dataframe and populate it with non-duplicate values to track keep track of interviews.
- enumerator_ID_var: variable name referencing the enumerator ID variable from the dataframe.
- list_of_modules: list of character vectors, with each vector being a module.
- qn_texts: single character vector of all question texts (questions for each module).
- baseline: an integer value for baseline interviews
- output_file: string in the format “[name].xlsx” for excel file output.

## Output
Excel workbook with module-specific analysis in each tab, summary tab with enumerator and respondent summaries with boxplot for flat % rates (for enumerators) across all modules.
