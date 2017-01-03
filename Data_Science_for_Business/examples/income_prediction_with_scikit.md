Example: Income Prediction with Scikit Learn
--------------------------------------------

Please download [`marketing.data`](https://raw.githubusercontent.com/jattenberg/PDS-Spring-2014/master/data/marketing.data). This data provides household income information according to demographic factors. Please see the [description here](https://github.com/jattenberg/PDS-Spring-2014/blob/master/data/marketing.info). Using this data, answer:

This work extends from the [previous example](https://github.com/jattenberg/PDS-Spring-2014/blob/master/examples/simple_plotting_with_python.md) on data manipulation and plotting.

1. Read in the input data using `urllib2`. Filter out any lines that have unavailable fields (with NA). Convert numbers to numeric types using `int(x)`.
2. Split the data into a list of lists of feature values and a list of target values. 
3. Train a [decision tree classifier](http://scikit-learn.org/stable/modules/generated/sklearn.tree.DecisionTreeClassifier.html#sklearn.tree.DecisionTreeClassifier) on the data.
4. Train a [decision tree regressor](http://scikit-learn.org/stable/modules/generated/sklearn.tree.DecisionTreeRegressor.html#sklearn.tree.DecisionTreeRegressor) on the data.
5. Train a [support vector classifier](http://scikit-learn.org/stable/modules/generated/sklearn.svm.SVC.html) on the data. 
6. Instead of training a model on all available input data, partition the input data into "training" and "test" sets. Fit the above models to the training sets, and get predictions for the examples in the test set.
