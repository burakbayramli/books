Example: Plotting and Processing in Python
------------------------------------------

Please download [`marketing.data`](https://raw.githubusercontent.com/jattenberg/PDS-Spring-2014/master/data/marketing.data). This data provides household income information according to demographic factors. Please see the [description here](https://github.com/jattenberg/PDS-Spring-2014/blob/master/data/marketing.info). Using this data, answer:

1. How many respondants have information in this file? 
2. Notice that many lines have some fields unavailable (NA). Remove any lines without complete data. How many lines remain? Can you do this with grep? With python? Use this filtered data for the remaining questions.
3. The fifth column corresponds to education level. What is the most common education level?
4. What is the income distribution for households with some graduate school? Hint: use a histogram.
5. Plot the income distribution for all respondants

6. It is often interesting considering the behavior of random walks. Use numpy's random number generator to generate steps for a "random walker". Plot the position of this random walker over time for 10,000 steps. 
