''' Two-way Analysis of Variance (ANOVA)
The model is formulated using the "patsy" formula description. This is very similar to the way
models are expressed in R.

'''

'''
Author:  Thomas Haslwanter
Date:    May-2013
Version: 1.3
'''

import pandas as pd
from getdata import getData
from statsmodels.formula.api import ols
from statsmodels.stats.anova import anova_lm

def anova_interaction():
    '''ANOVA with interaction: Measurement of fetal head circumference,
    by four observers in three fetuses.'''
    
    # Get the data
    data = getData(r'data_altman/altman_12_6.txt')
    
    # Bring them in dataframe-format
    df = pd.DataFrame(data, columns=['hs', 'fetus', 'observer'])
    
    # Determine the ANOVA with interaction
    formula = 'hs ~ C(fetus) + C(observer) + C(fetus):C(observer)'
    lm = ols(formula, df).fit()
    anovaResults = anova_lm(lm)
    print(anovaResults)

    return  anovaResults['F'][0]
                              
if __name__ == '__main__':
    anova_interaction()
