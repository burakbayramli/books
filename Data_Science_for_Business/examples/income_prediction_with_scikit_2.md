Example: More on Income Prediction with Scikit Learn
----------------------------------------------------

Please download [`marketing.data`](https://raw.githubusercontent.com/jattenberg/PDS-Spring-2014/master/data/marketing.data). This data provides household income information according to demographic factors. Please see the [description here](https://github.com/jattenberg/PDS-Spring-2014/blob/master/data/marketing.info). Using this data, answer:

This work extends from the [previous example on income prediction](https://github.com/jattenberg/PDS-Spring-2014/blob/master/examples/income_prediction_with_scikit.md) with scikit learn.

1. Perform 10-fold cross-validation with a DecisionTreeClassifier on the marketing data to measure accuracy. What is the mean accuracy value? 
2. Try the same task with two or three other classifier types in [scikit learn](http://scikit-learn.org/stable/supervised_learning.html#supervised-learning). What model does the best? What does the worst? 
3. Take the best model from example two, write it to a file using `pickle`, then load this model back in.