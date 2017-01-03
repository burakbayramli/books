''' Analysis of categorical data
- Analysis of one proportion
- Chi-square test
- Fisher exact test

'''

'''
Author:  Thomas Haslwanter
Date:    June-2013
Version: 1.2
'''

import numpy as np
import scipy.stats as stats


def oneProportion():
    '''Calculate the confidence intervals of the population, based on a
    given data sample.
    The data are taken from Altman, chapter 10.2.1.'''

    # Get the data
    numTotal = 215
    numPositive = 39

    # Calculate the confidence intervals
    p = float(numPositive)/numTotal
    se = np.sqrt(p*(1-p)/numTotal)
    td = stats.t(numTotal-1)
    ci = p + np.array([-1,1])*td.isf(0.025)*se

    # Print them
    print('ONE PROPORTION ----------------------------------------')
    print('The confidence interval for the given sample is {0:5.3f} to {1:5.3f}'.format(
        ci[0], ci[1]))
    
    return ci

def chiSquare():
    ''' Application of a chi square test to a 2x2 table.
    The calculations are done with and without Yate's continuity
    correction.
    Data are taken from Altman, Table 10.10:
    Comparison of number of hours' swimming by swimmers with or without erosion of dental enamel.
    >= 6h: 32 yes, 118 no
    <  6h: 17 yes, 127 no'''

    # Enter the data
    obs = np.array([[32, 118], [17, 127]])

    # Calculate the chi-square test
    chi2_corrected = stats.chi2_contingency(obs, correction=True)
    chi2_uncorrected = stats.chi2_contingency(obs, correction=False)

    # Print the result
    print('\nCHI SQUARE --------------------------------------------------')
    print('The corrected chi2 value is {0:5.3f}, with p={1:5.3f}'.format(
        chi2_corrected[0], chi2_corrected[1]))
    print('The uncorrected chi2 value is {0:5.3f}, with p={1:5.3f}'.format(
        chi2_uncorrected[0], chi2_uncorrected[1]))
    
    return chi2_corrected

def fisherExact():
    '''Fisher's Exact Test:
    Data are taken from Altman, Table 10.14
    Spectacle wearing among juvenile delinquensts and non-delinquents who failed a vision test
    Spectecle wearers: 1 delinquent, 5 non-delinquents
    non-spectacle wearers: 8 delinquents, 2 non-delinquents'''

    # Enter the data
    obs = np.array([[1,5], [8,2]])

    # Calculate the Fisher Exact Test
    # Note that by default, the option "alternative='two-sided'" is set;
    # other options are 'less' or 'greater'.
    fisher_result = stats.fisher_exact(obs)

    # Print the result
    print('\nFISHER --------------------------------------------------------')
    print('The probability of obtaining a distribution at least as extreme '
    + 'as the one that was actually observed, assuming that the null ' +
    'hypothesis is true, is: {0:5.3f}.'.format(fisher_result[1]))
    
    return fisher_result

if __name__ == '__main__':
    oneProportion()
    chiSquare()
    fisherExact()

