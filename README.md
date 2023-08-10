# flatline

My program detects and analyzes cases of flatlining / straight lining in survey data. [Flatlining](https://www.qualtrics.com/blog/straightlining-what-is-it-how-can-it-hurt-you-and-how-to-protect-against-it/) occurs when respondents provide the same response to a series of questions in a module (with one overarching question).
As part of this analysis, my code loops through each module inputted, detects instances of flatlining for each label and for each enumerator, and flags enumerators with extreme rates of flatlining. Enumerators are flagged if
the proportion of flat interviews that they conduct is greater than or equal to 1.5 * IQR (interquartile range) + Q3 (3rd quartile value), provided their total interviews are greater than the baseline interviews value. This 
flagging process is repeated for each module. The program also creates a summary tab that displays rates of flatlining across all modules for both enumerators and respondents. Respondents are flagged if they had a flat
response in a particular module.

Prerequistes
