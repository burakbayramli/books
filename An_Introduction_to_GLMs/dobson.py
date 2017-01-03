'''This module provides Python/statsmodel solutions to all code examples in

Dobson AJ & Barnett AG: "An Introduction to Generalized Linear Models"
3rd ed
CRC Press(2008)

Points that still need to be done are marked with "tbd" below:

- [Unclear] Clarify the exact definition of "loglog" and "cloglog" in the differnent
  languages.

- [Unclear] in "senility_and_WAIS" I don't understand what the "grouped response"
  is supposed to mean

- [Missing 1] "ordinal_logistic_regression" is not yet implemented in statsmodels

- [Missing 2] Cox proportional hazards are not yet implemented in statsmodels

- [Missing 3] Repeated measures models are not yet implemented in statsmodels

author: thomas haslwanter
date:   may 2013
ver:    0.21

'''

# Standard libraries
import numpy as np
import pandas as pd
import statsmodels.api as sm
import patsy

from statsmodels.stats.api import anova_lm
from statsmodels.formula.api import glm, ols
from statsmodels.genmod.families import Poisson, Binomial, Gaussian, links

# for data import
import urllib2
import zipfile
from StringIO import StringIO

def get_data(inFile):
    '''Get data from original Excel-file'''    
    xls = pd.ExcelFile(inFile)
    df = xls.parse('Sheet1', skiprows=2)        
    return df

def regression():
    '''Poisson regression example
    chapter 4.4, p.69'''
    
    # get the data from the web
    inFile = r'GLM_data/Table 4.3 Poisson regression.xls'
    df = get_data(inFile)
    
    # do the fit
    p = glm('y~x', family=Poisson(links.identity), data=df)
    print p.fit().summary()    

def multiple_linear_regression():
    '''Multiple linear regression
    chapter 6.3, p. 98'''
    
    # get the data from the web
    inFile = r'GLM_data/Table 6.3 Carbohydrate diet.xls'
    df = get_data(inFile)
    
    # do the fit, for the original model ...
    model = ols('carbohydrate ~ age + weight + protein', data=df).fit()
    print model.summary()
    print anova_lm(model)

    # as GLM
    p = glm('carbohydrate ~ age + weight + protein',
            family=Gaussian(), data=df).fit()
    print 'Same model, calculated with GLM'
    ''' The confidence intervals are different than those from OLS.
    The reason (from Nathaniel Smith):
    OLS uses a method that gives exact results, but only works in the special
    case where all the usual OLS criteria apply - iid Gaussian noise etc. GLM
    instead uses an approximate method which is correct asymptotically but may
    be off for small samples; the tradeoff you get in return is that this method
    works the same way for all GLM models, including those with non-Gaussian
    error terms and non-trivial link functions. So that's why they're different.
    '''

    print p.summary()
    
    # ... and for model 1
    model1 = ols('carbohydrate ~ weight + protein', data=df).fit()
    print model1.summary()
    print anova_lm(model1)    

def anova():
    '''ANOVA
    chapter 6.4, p. 108, and p. 113
    GLM does not work with anova_lm.
    '''
    
    # get the data from the web
    inFile = r'GLM_data/Table 6.6 Plant experiment.xls'
    df = get_data(inFile)
    
    # fit the model (p 109)
    p = glm('weight~group', family=Gaussian(), data=df)
    print p.fit().summary()        
    
    print '-'*65
    print 'OLS'
    model = ols('weight~group', data=df)
    print model.fit().summary()
    print anova_lm(model.fit())            
    
    # The model corresponding to the null hypothesis of no treatment effect is
    model0 = ols('weight~1', data=df)
    
    # Get the data for the two-factor ANOVA (p 113)
    inFile = r'GLM_data/Table 6.9 Two-factor data.xls' 
    df = get_data(inFile)
    
    # adjust the header names from the Excel-file
    df.columns = ['A','B', 'data']
    
    # two-factor anova, with interactions
    ols_int = ols('data~A*B', data=df)
    anova_lm(ols_int.fit())
    
    # The python commands for the other four models are
    ols_add = ols('data~A+B', data=df)
    ols_A = ols('data~A', data=df)    
    ols_B = ols('data~B', data=df)    
    ols_mean = ols('data~1', data=df)    

def ancova():
    ''' ANCOVA
    chapter 6.5, p 117 '''
    
    # get the data from the web
    inFile = r'GLM_data/Table 6.12 Achievement scores.xls'
    df = get_data(inFile)
    
    # fit the model
    model = ols('y~x+method', data=df).fit()
    print anova_lm(model)
    print model.summary()    

def logistic_regression():
    '''Logistic regression example
    chapter 7.3, p 130
    [tbd]: the cloglog values are inconsistent with those mentioned in the book.
    This is probably due to the specific definitions of "loglog" and "cloglog"
    in the respective languages.
    '''
    
    inFile = r'GLM_data/Table 7.2 Beetle mortality.xls'
    df = get_data(inFile)
    
    # adjust the unusual column names in the Excel file
    colNames = [name.split(',')[1].lstrip() for name in df.columns.values]
    df.columns = colNames
    
    # fit the model
    df['tested'] = df['n']
    df['killed'] = df['y']
    df['survived'] = df['tested'] - df['killed']
    model = glm('survived + killed ~ x', data=df, family=Binomial()).fit()
    print model.summary()
    
    print '-'*65
    print 'Equivalent solution:'
    
    model = glm('I(n - y) + y ~ x', data=df, family=Binomial()).fit()
    print model.summary()    
    
    # The fitted number of survivors can be obtained by
    fits = df['n']*(1-model.fittedvalues)
    print 'Fits Logit:'
    print fits
    
    # The fits for other link functions are:
    model_probit = glm('I(n - y) + y ~ x', data=df, family=Binomial(links.probit)).fit()
    print model_probit.summary()
    
    fits_probit = df['n']*(1-model_probit.fittedvalues)
    print 'Fits Probit:'
    print fits_probit
    
    model_cll = glm('I(n - y) + y ~ x', data=df, family=Binomial(links.cloglog)).fit()
    print model_cll.summary()
    fits_cll = df['n']*(1-model_cll.fittedvalues)
    print 'Fits Extreme Value:'
    print fits_cll

def general_logistic_regression():
    '''Example General Logistic Recression,
    Example 7.4.1, p. 135'''
    
    # Get the data
    inFile = r'GLM_data/Table 7.5 Embryogenic anthers.xls'
    df = get_data(inFile)
    
    # Define the variables so that they match Dobson
    df['n_y'] = df['n'] - df['y']
    df['newstor'] = df['storage']-1
    df['x'] = np.log(df['centrifuge'])
    
    # Model 1
    model1 = glm('n_y + y ~ newstor*x', data=df, family=Binomial()).fit()
    print model1.summary()
    
    # Model 2
    model2 = glm('n_y + y ~ newstor+x', data=df, family=Binomial()).fit()
    print model2.summary()
    
    # Model 3
    model3 = glm('n_y + y ~ x', data=df, family=Binomial()).fit()
    print model3 .summary()    

def senility_and_WAIS():
    '''Another example of logistic regression.
    chapter 7.8, p 143
    [tbd]: I don't understand how the "Binomial model" (grouped response)
    is supposed to work, in either language'''

    inFile = r'GLM_data/Table 7.8 Senility and WAIS.xls'
    df = get_data(inFile)
    
    # ungrouped
    model = glm('s ~ x', data=df, family=Binomial()).fit()
    print model.summary()    
    
    # Hosmer-Lemeshow
    # grouped: Here I don't get how the grouping is supposed to be achieved, either in R or in Python
    # [tbd]

def nominal_logistic_regression():
    '''Nominal Logistic Regression
    chapter 8.3,  p. 155 
    
    At this point, nominal logistic regression cannot be done with the formula approach.
    
    Regarding the output, note that R produces log(pi2/pi1) and log(pi3/pi1), while
    statsmodels produces log(pi2/pi1) and log(pi3/pi2) 
    '''
    
    # Get the data
    inFile = r'GLM_data/Table 8.1 Car preferences.xls'
    df = get_data(inFile)    

    # to make sure that "women" and "no/little" are the reference,
    # adjust them such that they come first alphabetically
    df['response'][df['response'] == 'no/little'] = '_no/little'
    df['sex'][df['sex'] == 'women'] = '_women'
    print df
    
    
    # Generate the design matrices using patsy
    pm = patsy.dmatrices('response~sex+age', data=df)
    
    # Generate the endog and exog matrices
    endog = np.repeat(np.array(df['response']), df['frequency'].values.astype(int), axis=0)
    exog = np.array(np.repeat(pm[1], df['frequency'].values.astype(int), axis=0))
    exog = pd.DataFrame(exog, columns=pm[1].design_info.column_names) 

    # Fit the model, and print the summary
    model = sm.MNLogit(endog, exog, method='nm').fit()
    print  model.summary()

def ordinal_logistic_regression_tbd():
    
    '''Ordinal Logistic Regression
    chapter  8.4, p161
    This function is not implemented in statsmodels yet. One solution can be found at
    http://fabianp.net/blog/2013/logistic-ordinal-regression/
    '''
    
    inFile = r'GLM_data/Table 8.1 Car preferences.xls'
    df = get_data(inFile)    

def poisson_regression():
    '''Poisson Regression
    chapter 9.2, p.170 & 171 '''
    
    inFile = r"GLM_data/Table 9.1 British doctors' smoking and coronary death.xls"
    df = get_data(inFile)    
    print df

    # Generate the required variables
    df['smoke'] = np.zeros(len(df))
    df['smoke'][df['smoking']=='smoker']=1

    df['agecat'] = np.array([1,2,3,4,5,1,2,3,4,5])
    df['agesq'] = df['agecat']**2

    df['smkage'] = df['agecat']
    df['smkage'][df['smoking']=='non-smoker']=0

    model = glm('deaths~agecat+agesq+smoke+smkage',
            family=Poisson(), data=df,
            exposure=df["person-years"]).fit()
    print model.summary()

def log_linear_models():
    '''Log-linear models
    chapter 9.7, p 180 & 182 '''

    # Malignant melanoma, p 180 --------------------------------
    inFile = r'GLM_data/Table 9.4 Malignant melanoma.xls'
    df = get_data(inFile)    

    # Minimal model
    model_min = glm('frequency~1', family = Poisson(), data=df).fit()
    print 'Malignant melanoma'
    print model_min.fittedvalues[0]

    # Additive model
    model_add = glm('frequency~site+type', family = Poisson(), data=df).fit()
    print model_add.fittedvalues[0]

    # Saturated model
    # model_sat = glm('frequency~site*type', family = Poisson(), data=df).fit()
    #
    # The saturated model gives a perfect fit, and the fitted data are equal to
    # the original data. Statsmodels indicates a "PerfectSeparationError"

    # Ulcer and aspirin, p. 182 ------------------------------------- 
    inFile = r'GLM_data/Table 9.7 Ulcer and aspirin use.xls'
    df = get_data(inFile)
    df.columns = ['GD', 'CC', 'AP', 'freq']

    model1 = glm('freq~GD+CC+GD*CC', family = Poisson(), data=df).fit()
    model2 = glm('freq~GD+CC+GD*CC + AP', family = Poisson(), data=df).fit()
    model3 = glm('freq~GD+CC+GD*CC + AP + AP*CC', family = Poisson(), data=df).fit()
    model4 = glm('freq~GD+CC+GD*CC + AP + AP*CC + AP*GD', family = Poisson(), data=df).fit()
    
    print 'Ulcer and aspirin'
    print model4.fittedvalues


def remission_times_tbd():
    '''Survival analysis / Remission times
    chapter 10.7, p. 201
    These models, also known as "Cox proportional hazards model",
    are currently under development but not yet available in statsmodels.'''

    inFile = r'GLM_data/Table 10.1 Remission times.xls'
    df = get_data(inFile)    
    print df

def longitudinal_data_tbd():
    '''Stroke example
    chapter 11.6, p. 222
    Clustered and Longitudinal Data are described by repeated measures models.
    These are under development, but not yet available in statsmodels.'''

    inFile = r'GLM_data/Table 11.1 Recovery from stroke.xls'
    df = get_data(inFile)    
    print df

if __name__ == '__main__':
    regression()
    multiple_linear_regression()
    anova()
    ancova()
    logistic_regression()
    general_logistic_regression()
    senility_and_WAIS()
    nominal_logistic_regression()
    ordinal_logistic_regression_tbd()
    poisson_regression()
    log_linear_models()
    remission_times_tbd()
    multiple_linear_regression()
    anova()
    ancova()
    logistic_regression()
    general_logistic_regression()
    nominal_logistic_regression()
    ordinal_logistic_regression_tbd()
    log_linear_models()
    remission_times_tbd()
    longitudinal_data_tbd()
