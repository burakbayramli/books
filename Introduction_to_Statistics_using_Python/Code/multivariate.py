''' Analysis of multivariate data
- Regression line
- Correlation (Pearson-rho, Spearman-rho, and Kendall-tau)

'''

'''
Author:  Thomas Haslwanter
Date:    June-2013
Version: 2.0
'''

import pandas as pd
from getdata import getData
from scipy import stats
from numpy import testing

def regression_line():
    '''Fit a line, using the powerful "ordinary least square" method of pandas'''
    
    # Get the data
    data = getData('altman_11_6.txt', subDir=r'..\Data\data_altman')
    
    df = pd.DataFrame(data, columns=['glucose', 'Vcf'])
    model = pd.ols(y=df['Vcf'], x=df['glucose'])
    print(model.summary)
    
    return model.f_stat['f-stat'] # should be 4.4140184331462571
    
def correlation():
    '''Pearson correlation, and two types of rank correlation (Spearman, Kendall)
    Data from 24 type 1 diabetic patients, relating Fasting blood glucose (mmol/l) to
    mean circumferential shortening velocity (%/sec).'''
    
    # Get the data
    data = getData('altman_11_1.txt', subDir='..\Data\data_altman')
    x = data[:,0]
    y = data[:,1]
    
    # Calculate correlations
    corr = {}
    corr['pearson'], _ = stats.pearsonr(x,y)
    corr['spearman'], _ = stats.spearmanr(x,y)
    corr['kendall'], _ = stats.kendalltau(x,y)
    
    print(corr)    
    
    # Assert that Spearman's rho is just the correlation of the ranksorted data
    testing.assert_almost_equal(corr['spearman'], stats.pearsonr(stats.rankdata(x), stats.rankdata(y))[0])
    
    return corr['pearson']  # should be 0.79208623217849117
    
if __name__ == '__main__':
    regression_line()    
    correlation()
