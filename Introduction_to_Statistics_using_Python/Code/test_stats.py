''' Test routine for Statsintro

Author: Thomas Haslwanter
Date:   July-2013
Version: 1.1

'''

import pandas as pd

import unittest
import numpy as np
import anovaOneway
import anovaTwoway
import binomialTest
import bootstrap
import checkNormality
import compGroups
import distribution_normal
import dist_continuous
import dist_discrete
import fig_roc
import fitLine
from getdata import getData
import gettingStarted
import gettingStarted_ipy
import KruskalWallis
import modeling
import multipleTesting
import mult_regress
import multivariate

import oneSample
import pandas_intro
import residuals
import sampleSize
import showStats
import survival
import twoSample

class TestSequenceFunctions(unittest.TestCase):
    def setUp(self):
        t = np.arange(0,10,0.1)
        x = np.sin(t)
        self.data = x
        
    def test_anovaOneway(self):
        (F,p) = anovaOneway.anova_oneway()
        self.assertAlmostEqual(F, 3.711335988266943)
        self.assertAlmostEqual(p, 0.043589334959179327)
        
        (F,p) = anovaOneway.anova_byHand()
        self.assertAlmostEqual(F, 3.711335988266943)
        self.assertAlmostEqual(p, 0.043589334959179327)
        
        F = anovaOneway.show_teqf()
        self.assertAlmostEqual(F, 2083.481, places=2)
        
        F = anovaOneway.anova_statsmodels()         
        self.assertAlmostEqual(F, 933.18460306573411)

    def test_anovaTwoway(self):
        F = anovaTwoway.anova_interaction()
        self.assertAlmostEqual(F, 2113.101449275357)
        
    def test_binomialTest(self):
        p1,p2 = binomialTest.binomial_test(51)
        self.assertAlmostEqual(p1, 0.0265442457117)
        self.assertAlmostEqual(p2, 0.0437479701824)
        
    def test_bootstrap(self):
        data = bootstrap.generate_data()
        CI = bootstrap.calc_bootstrap(data)        
        self.assertAlmostEqual(CI[0], 1.884, places=2)
        
    def test_checkNormality(self):
        p = checkNormality.check_normality()
        self.assertAlmostEqual(p, 0.987205618534)
        
    def test_compGroups(self):
        ci = compGroups.oneProportion()
        self.assertAlmostEqual(ci[0], 0.130, places=2)
        
        chi2 = compGroups.chiSquare()
        self.assertAlmostEqual(chi2[0], 4.141, places=2)
        
        fisher = compGroups.fisherExact()
        self.assertAlmostEqual(fisher[1], 0.035, places=2)
        
    def test_distribution_normal(self):
        distribution_normal.simple_normal()
        distribution_normal.shifted_normal()
        distribution_normal.many_normals()
        
    def test_dist_continuous(self):
        dist_continuous.show_continuous()
        
    def test_dist_discrete(self):
        dist_discrete.show_binomial()        
        dist_discrete.show_poisson()
        
    def test_figROC(self):
        fig_roc.main()
        
    def test_fitLine(self):
        
        # example data
        x = np.array([15.3, 10.8, 8.1, 19.5, 7.2, 5.3, 9.3, 11.1, 7.5, 12.2,
                      6.7, 5.2, 19.0, 15.1, 6.7, 8.6, 4.2, 10.3, 12.5, 16.1, 
                      13.3, 4.9, 8.8, 9.5])
        y = np.array([1.76, 1.34, 1.27, 1.47, 1.27, 1.49, 1.31, 1.09, 1.18, 
                      1.22, 1.25, 1.19, 1.95, 1.28, 1.52, np.nan, 1.12, 1.37, 
                      1.19, 1.05, 1.32, 1.03, 1.12, 1.70])
                      
        goodIndex = np.invert(np.logical_or(np.isnan(x), np.isnan(y)))
        (a,b,(ci_a, ci_b), ri,newy) = fitLine.fitLine(x[goodIndex],y[goodIndex], alpha=0.01,newx=np.array([1,4.5]))        
        
        self.assertAlmostEqual(a,1.09781487777)
        self.assertAlmostEqual(b,0.02196252226)
        
    def test_getdata(self):
        data = getData('altman_93.txt', subDir='../Data/data_altman')
        self.assertEqual(data[0][0], 5260)
        
    def test_gettingStarted(self):
        gettingStarted.main()
        
    def test_gettingStarted_ipy(self):
        gettingStarted_ipy.main()
        
    def test_KruskalWallis(self):
        h = KruskalWallis.main()
        self.assertAlmostEqual(h, 16.028783253379856)
        
    def test_modeling(self):
        F = modeling.model_formulas()
        self.assertAlmostEqual(F, 156.1407931415788)
        
        params = modeling.polynomial_regression()
        self.assertAlmostEqual(params[0], 4.74244177)
        
    def test_multipleTesting(self):
        var = multipleTesting.main()
        self.assertAlmostEqual(var, 1.1296296296296295)
        
    def test_multivariate(self):
        F = multivariate.regression_line()    
        self.assertAlmostEqual(F, 4.4140184331462571)
        
        pearson = multivariate.correlation()
        self.assertAlmostEqual(pearson, 0.79208623217849117)
        
    def test_multregress(self):
        (X,Y,Z) = mult_regress.generatedata()    
        bestfit1 = mult_regress.regressionmodel(X,Y,Z)    
        self.assertAlmostEqual(bestfit1[0], -4.99754526)
        
        bestfit2 = mult_regress.linearmodel(X,Y,Z)        
        self.assertAlmostEqual(bestfit2[0][0], -4.99754526)
        
    def test_oneSample(self):
        p = oneSample.check_mean()
        self.assertAlmostEqual(p, 0.018137235176105802)
        
        p2 = oneSample.compareWithNormal()
        self.assertAlmostEqual(p2, 0.054201154690070759)
        
    def test_pandas_intro(self):
        df = pandas_intro.labelled_data()
        self.assertAlmostEqual(df['values'][0], 4.7465508100784524)
        
        parameters = pandas_intro.simple_fit(df)
        self.assertAlmostEqual(parameters['x'], 0.50516249093121246)
        
    def test_residuals(self):
        execfile('residuals.py', {})
        
    def test_sampleSize(self):
        n1 = sampleSize.sampleSize_oneGroup(0.5)
        self.assertEqual(n1, 31)
        
        n2 = sampleSize.sampleSize_twoGroups(0.4, sigma1=0.6, sigma2=0.6)
        self.assertEqual(n2, 35)
        
    def test_showStats(self):
        showStats.main()
        
    def test_survival(self):
        p = survival.main()
        self.assertAlmostEqual(p, 0.073326322306832212)
        
    def test_twoSample(self):
        p1 = twoSample.paired_data()
        self.assertAlmostEqual(p1, 0.0033300139117459797) 
        
        p2 = twoSample.unpaired_data()
        self.assertAlmostEqual(p2, 0.0010608066929400244)
        
if __name__ == '__main__':
    unittest.main()
    raw_input('Thanks for using programs from Thomas!')
    '''
    # should raise an exception 
    self.assertRaises(TypeError, savgol, np.arange(3), window_size=5)
    self.assertTrue(np.abs(1-smoothed[round(np.pi/2*10)]<0.001))
    self.assertEqual(firstDeriv[14], fD[14])
    '''
