Group Project: Practical Data Science 2014
------------------------------------------

This project simulates some of the components of building an actual "data science product", imagine you are contributing a new feature to the social e-commerce website, [Etsy](http://www.etsy.com). Work together with your group to perform the following tasks. There are some parts of this project that are left to your discretion, please answer in whatever way you feel is most interesting, or would be most useful for an actual product experience. Let us know what choices you make, and why, and, if applicable, discuss any unexplored alternatives. 

Turn in one assignment per team, recording the names of all participating members. Along with your responses, please include all code and scripts used (in appendices), in addition to some sample output.  Remember to appropriately comment your code so it can be followed by others.

Use google, course notes, and whatever resources you can muster in order to accomplish this assignment. If you remain stuck for very long with a particular detail, please reach out to the instructors for help. 


Each numbered item is worth 25 points.

1. Write a script which finds 5000 different shops on Etsy using the API (info below), and returns a sample of those shops listings. Store this shop/listing info in file in sensible structure using json format (info below).

2. Using the output from the previous script, write an algorithm that matches shops according to some similarity measure on listings. This algorithm
should run on the command line (for instance, by calling `python your_similarity_script.py`), and, for each shop, return the five shops that are most similar. Output should be in the following format:
`shop_name: similar_shop_1, similar_shop_2, ..., similar_shop_5`
Hint: Chapter 6 in Professor Provost's Data Science book is all about similarity measures. [Cosine similarity with TF-IDF weights](http://stackoverflow.com/a/12128777) is a good possiblity, but there are many others.

3. Explain why you think the algorithm you chose leads to useful similarities amongst the shops, and how its results might be integrated into Etsy as a product. What are other distance measures you might have considered? Why is your distance measure more interesting or better suited to the task of identifying similar shops? The explanation should be at the depth and length that you might use when emailing your team; less than a paper or blog post, more than a commit message. Remember that communication is critical for the data scientist, particularly, the summarization of complex ideas.  Feel free to include tables and graphs to support your explanation.

4. Is there any other shop-related information that could be incorporated in order to make more useful similarity measures? Write a script that gathers this additional data, and incorporates it into your similarity measures. Explain why this is beneficial (or not).

Project Requirements:
a.  Final report must be in PDF format.
b.  Cover page should include Team Number and Member Names, and Class Name
c.  List of all scripts with descriptions of each script should be included in an appendix.
d.  Python scripts and Unix code should be included in an appendix.
e.  List outlining each team member's contribution should be included in an appendix.

When gathering data from Etsy, use the [Etsy API](http://developer.etsy.com/). I recommend using python's [`urllib2`](https://docs.python.org/2/howto/urllib2.html) library, but any resource will do, including other python libraries or unix utilities like `wget` or `curl`.  Making requests requires an API key, this will be provided in NYU classes as a course resource. Don't share this!

Data returned from the API will be in JSON format. This is a common format for representing data structures, and will be encountered frequently by the data scientist. Python has a library for processing  JSON data, appropriately called [`json`](https://docs.python.org/2/library/json.html). JSON represents lists, dictionaries, and primitive types. For more info on JSON and data representation, [read here](https://docs.google.com/document/d/1QG7r_dx9BOi3geuZyaLLpqm51gCutIYrVoD6NGWMEX0/pub). 

To see an example of what data returned from the API looks like, put the following url into your browser: https://openapi.etsy.com/v2/private/shops?api_key=[api key given in NYU classes]. For the various components of the the project, you will need to modify the url to get the appropriate data back.

