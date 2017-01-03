'''Introductions into using "Pandas"
Pandas is a Python framework for statistical analysis. It allows you to label
and group data, etc.
The example here shows you
- how to group data, and analyze each group
- how to combine Pandas with statsmodels for elegant modeling

'''

'''
Author : Thomas Haslwanter
Date : June 2013
Ver : 2.0
'''
import numpy as np
import pandas as pd
import statsmodels.formula.api as sm

def labelled_data():
    ''' Analyzed labelled data '''
    # To get reproducable values, I provide a seed value
    np.random.seed(987654321)   

    # Generate three groups of normally distributed data, with different means
    data = np.random.randn(100,3)+np.array([2.5, 3.5, 2])

    # ... and the corresponding labels
    labels = np.tile([1,2,3], (100,1))

    # Flatten them, so you have them just as if you read them in from a file
    dataList = data.flatten()
    typeList = labels.flatten()

    # For simple data handling, put them into a Pandas "DataFrame"
    df = pd.DataFrame({'values': dataList, 'type': typeList})

    # Calculate mean, standard deviation, and size of each group
    grouped = df.groupby('type')

    grouped.mean()
    grouped.std()
    grouped.size()

    # Then, generate a boxplot
    df.boxplot(by='type')

    return df

def simple_fit(df):
    ''' Example 2: Linear regression fit '''
    # Generate a noisy line
    x = np.arange(100)
    y = 0.5*x - 20 + np.random.randn(len(x))

    # Fit a linear model ...
    model = sm.ols('y~x', data=df).fit()

    # ... and print the summary
    print model.summary()

    return model.params

if __name__ == '__main__':
    df = labelled_data()
    simple_fit(df)
