'''Introductions into using "statsmodels"

'''

# author: Thomas Haslwanter, date: April-2013

import numpy as np
import pandas as pd
from scipy import stats
import statsmodels.formula.api as sm
import sys
import matplotlib.pyplot as plt
if sys.version_info[0] == 3:
    from urllib.request import urlopen
else:
    from urllib import urlopen

def simple_fit():
    ''' Example: Linear regression fit '''
    
    # To get reproducable values, I provide a seed value
    np.random.seed(987654321)   
    
    # Generate a noisy line
    x = np.arange(100)
    y = 0.5*x - 20 + np.random.randn(len(x))
    df = pd.DataFrame({'x':x, 'y':y})

    # Fit a linear model ...
    model = sm.ols('y~x', data=df).fit()

    # ... and print the summary
    print((model.summary()))

    return model.params

def pandas_boxplot():
    '''Example from Altman "Practical statistics for medical research'''

    # Get the data
    inFile = 'altman_94.txt'
    url_base = 'https://raw.github.com/thomas-haslwanter/statsintro/master/Data/data_altman/'
    url = url_base + inFile
    data = np.genfromtxt(urlopen(url), delimiter=',')

    lean = pd.Series(data[data[:,1]==1,0])
    obese = pd.Series(data[data[:,1]==0,0])

    df = pd.DataFrame({'lean':lean, 'obese':obese})

    print(df.mean())

    df.boxplot()
    plt.show()
    
    stats.ttest_ind(lean, obese)
    
    print(df.mean())
if __name__ == '__main__':
    simple_fit()
    pandas_boxplot()
