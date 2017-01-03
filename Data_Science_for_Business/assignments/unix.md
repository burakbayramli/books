Unix Assignment: PDS Spring 2014
--------------------------------

Please download student skill survey results [here](https://raw.githubusercontent.com/jattenberg/PDS-Spring-2014/master/data/survey_anon.txt). Use the unix wget or curl utility. The file is in a tab-separated data format, the rows of this data correspond to: 

student_id, command line experience, relational database experience, programming experience

For the following questions, collaborate with your group to find the right approaches, but please do the final work yourself. Use unix command line utilities to find the answers. Use NYU classes to submit your work. Please provide both the commands used and the answers found.

1. How many students took the survey?
2. How many students have no experience working with the terminal? How many don't even know what terminal experience means (hint: use grep)
3. How many students have never programmed before? 
4. What are the unique responses to the question on database skills? 
5. Repeat question 4, this time also include the number of students responding with each particular answer.
6. Write a small script that finds the 10 largest files on your computer (hint: use the `du` utility)
7. Bonus: Automate the script from part 6. to run daily. Have the script email the results to you (hint: use the `mail` utility)
