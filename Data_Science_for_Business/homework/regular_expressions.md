Regular Expressions: Practical Data Science 2014
------------------------------------------------

##### Dealing with Data: Parsing Text and Extracting Information

Consider the [html document](https://raw.githubusercontent.com/jattenberg/PDS-Spring-2014/master/data/pds_2012_roster.html) representing the roster web page taken from a previous year's Practical Data Science course. Because this is the same html that is rendered by your favorite web browser, you can load it up and see what it looks like. Once you've downloaded the file, this will be something like, `File -> open file`. You will be considering this raw, semi structured data for the following tasks. These tasks mimic what I needed to do to get a list of student emails to address this class, and are typical for the data scientist processing data in the wild. 

Work with your groups to find the best answers for the following questions using python and the `re` library, however, please type in your own responses and hand in your own work. I believe that you'll learn more by typing the code yourself, even if you reached the appropriate answer with others. 

You may post your code and answers on github, posting the url to classes, or you may just copy your code and answers into a document, and host that to NYU classes in PDF format. 


1. Within the document, there are several student IDs (the column actually is titled E-mail). Extract these IDs from the html and print them to a file, one per line.

2. Constrain your search to print only those students with four letters in their last names or less. How many students were removed?

3. For every student in the class, in addition to extracting their student ID, extract their name. Present the results by printing out, one student per line:
`first (and middle) name [tab] last name [tab] student id`
Also, write the results to a text file.

4. (Optional) The E-mail column seems misnamed. Create a new html document that replaces all student ids in this field with student email addresses.
