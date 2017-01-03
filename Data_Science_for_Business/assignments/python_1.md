Python Assignment 1: PDS Spring 2014
------------------------------------

For the following questions, collaborate with your group to find the right approaches, but please do the final work yourself. Use python to find the answers. Post both your code and responses / examples of use to NYU classes (PDF format).

Please download student skill survey results [here](https://raw.githubusercontent.com/jattenberg/PDS-Spring-2014/master/data/survey_anon.txt). Use the unix wget or curl utility. The file is in a tab-separated data format, the rows of this data correspond to: 

student_id, command line experience, relational database experience, programming experience

1. Which discipline (unix, database, or programming) would you say has the highest overall skill level amongst the students responding to the survey? Which discipline has the lowest?
2. Using matplotlib, make a plot of the distribution of skill levels, starting from the lowest and going to the highest for each of the three disciplines. You may wish to substitute the strings describing the skill level with a numeric value.
3. Combine these three plots, overlaying them on a single graph. Make sure each line is a different color for each line, and make a legend to tell the different colors apart. Hint: use google to figure out how!
4. Repeat question 3 but with a bar plot.

DIY Data Utilities:

5. There are many times where a data scientist gets some columnar data and wants to simply plot it. Make a python script (a .py file) that can a take single column of numerical data from the command line via a unix pipe display that data in a plot. Hint: use the stdin function in the sys module. Hint 2: you can run a python file from the unix terminal by typing: `python [your_py_file].py`
6. Make a script similar that used in the previous question that makes a histogram instead of a line plot.
7. (bonus) Extend the first plotting question so that the script can read multiple columns of input data instead of just 1. Plot them together in the same plot window.

Income Prediction:
Please download [`marketing.data`](https://raw.githubusercontent.com/jattenberg/PDS-Spring-2014/master/data/marketing.data). This data provides household income information according to demographic factors. Please see the [description here](https://github.com/jattenberg/PDS-Spring-2014/blob/master/data/marketing.info). Using this data, answer:



8. Consider the following simple model of income level using only education level.
Let 4 be the nominal income level, with the following adjustments in income level being made according to education:


| education level | income modifier |
|----------------:|-----------------|
|        1        |        -3       |
|        2        |        -1       |
|        3        |        0        |
|        4        |        +1       |
|        5        |        +3       |
|        6        |        +4       |


What is the total difference between actual and predicted income level using the above model? What about the average difference per user? (Hint: again use a dict data structure, this time to store the modifiers of the model)

9. Consider the following modification to the model presented in question 1 that additionally incorporates the following information about a person’s occupation:


| occupation level | income modifier |
|-----------------:|-----------------|
|        1         |        +2.5     |
|        2         |        -.6      |
|        3         |        0        |
|        4         |        +.2      |
|        5         |        -.5      |
|        6         |        -1.5     |
|        7         |        +.3      |
|        8         |        +.8      |
|        9         |        -2.5     |


In this setting, we are using a two parameter model to estimate an individual’s income, according to both occupation and education level. What is the total difference between actual and predicted income level using the above model? What about the average difference per user?  Is this better or worse than the model presented in question 1? Is this model more likely to overestimate or underestimate an individual’s income level?
