Homework: Simple Data Processing in Python
-----------------------------------------

Please download student skill survey results [here](https://raw.githubusercontent.com/jattenberg/PDS-Spring-2014/master/data/survey_anon.txt). Use the unix wget or curl utility. The file is in a tab-separated data format, the rows of this data correspond to: 

student_id, command line experience, relational database experience, programming experience

Use python for all questions

1. How many students took the survey?
2. Write a script that prints out a specific column of the input data: If the user asks for 1, you would print out the command line experience for each response.
Hint: use the split() method on the strings read in from the file
Hint 2: it's fine to specify the desired column in the program. If you wish to challenge yourself, use [`sys.argv`](http://www.tutorialspoint.com/python/python_command_line_arguments.htm) to read in the desired column as a command line argument.
3. How many students have no experience working with the terminal? How many don't even know what terminal experience means? How many students have more than zero experience?
4. How many students have never programmed before? 
5. What are the unique responses to the question on database skills? 


