'''Analysis of one sample of data

This script shows how to
- Use a t-test for a single mean
- Use a non-parametric test (Wilcoxon signed rank) to check a single mean 
- Compare the values from the t-distribution with those of a normal distribution

'''

'''
Author: Thomas Haslwanter
Date:   May-2013
Version: 1.1
'''

import numpy as np
import scipy.stats as stats
from getdata import getData

def check_mean():        
    '''Data from Altman, check for significance of mean value.
    Compare average daily energy intake (kJ) over 10 days of 11 healthy women, and
    compare it to the recommended level of 7725 kJ.
    '''
    # Get data from Altman

    data = getData(r'data_altman/altman_91.txt')

    # Watch out: by default the SD is calculated with 1/N!
    myMean = np.mean(data)
    mySD = np.std(data, ddof=1)
    print('Mean and SD: {0:4.2f} and {1:4.2f}'.format(myMean, mySD))

    # Confidence intervals
    tf = stats.t(len(data)-1)
    ci = np.mean(data) + stats.sem(data)*np.array([-1,1])*tf.isf(0.025)
    print('The confidence intervals are {0:4.2f} to {1:4.2f}.'.format(ci[0], ci[1]))

    # Check for significance
    checkValue = 7725
    t, prob = stats.ttest_1samp(data, checkValue)
    if prob < 0.05:
        print('{0:4.2f} is significantly different from the mean (p={1:5.3f}).'.format(checkValue, prob))

    # For not normally distributed data, use the Wilcoxon signed rank test
    (rank, pVal) = stats.wilcoxon(data-checkValue)
    if pVal < 0.05:
      issignificant = 'unlikely'
    else:
      issignificant = 'likely'
      
    print('It is ' + issignificant + ' that the value is {0:d}'.format(checkValue))
    
    return prob # should be 0.018137235176105802
 
def compareWithNormal():
    # generate the data
    np.random.seed(12345)
    normDist = stats.norm(loc=7, scale=3)
    data = normDist.rvs(100)
    checkVal = 6.5

    # t-tes
    t, tProb = stats.ttest_1samp(data, checkVal)

    # Comparison with corresponding normal distribution
    mmean = np.mean(data)
    mstd = np.std(data, ddof=1)
    normProb = stats.norm.cdf(6.5, loc=mmean,
            scale=mstd/np.sqrt(len(data)))*2

    # compare
    print('The probability from the t-test is ' + '{0:4.3f}, and from the normal distribution {1:4.3f}'.format(tProb, normProb))
    
    return normProb # should be 0.054201154690070759
           
if __name__ == '__main__':
    check_mean()
    compareWithNormal()
