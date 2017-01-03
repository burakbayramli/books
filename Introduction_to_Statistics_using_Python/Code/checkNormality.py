''' Graphical and quantitative check, if a given distribution is normal.

'''

'''
Author:  Thomas Haslwanter
Date:    May-2013
Version: 1.1
'''

import numpy as np
import scipy.stats as stats
import matplotlib.pyplot as plt

myMean = 0
mySD = 3
x = np.arange(-5,15,0.1)

def check_normality():
    '''Check if the distribution is normal.'''
    # Generate and show a distribution
    numData = 100
    
    # To get reproducable values, I provide a seed value
    np.random.seed(987654321)   
    
    data = stats.norm.rvs(myMean, mySD, size=numData)
    plt.hist(data)
    plt.show()

    # Graphical test: if the data lie on a line, they are pretty much
    # normally distributed
    _ = stats.probplot(data, plot=plt)
    plt.show()

    # The scipy normaltest is based on D-Agostino and Pearsons test that
    # combines skew and kurtosis to produce an omnibus test of normality.
    stats.normaltest(data)

    # Or you can check for normality with Kolmogorov-Smirnov test
    _,pVal = stats.kstest((data-np.mean(data))/np.std(data,ddof=1), 'norm')
    if pVal > 0.05:
        print('Data are normally distributed')
    
    return pVal

if __name__ == '__main__':
    p = check_normality()    
    # raw_input('Done')

