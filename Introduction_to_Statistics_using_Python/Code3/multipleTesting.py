'''Multiple testing

This script provides an example, where three treatments are compared. It
first performs a one-way ANOVA, to see if there is a difference between the
groups. Then it performs multiple comparisons, to check which of the groups
are different.

This dataset is taken from an R-tutorial, and contains a hypothetical sample
of 30 participants who are divided into three stress reduction treatment
groups (mental, physical, and medical). The values are represented on a
scale that ranges from 1 to 5. This dataset can be conceptualized as a
comparison between three stress treatment programs, one using mental
methods, one using physical training, and one using medication. The values
represent how effective the treatment programs were at reducing
participant's stress levels, with higher numbers indicating higher
effectiveness.

Taken from an example by Josef Perktold (http://jpktd.blogspot.co.at/)

'''

# author: Thomas Haslwanter, date: June-2014

import numpy as np
from scipy import stats
import pandas as pd

def main():
    # Note: the statsmodels module is required here.
    from statsmodels.stats.multicomp import (pairwise_tukeyhsd,
                                             MultiComparison)
    from statsmodels.formula.api import ols
    from statsmodels.stats.anova import anova_lm
    
    # Set up the data, as a structured array.
    # The first and last field are 32-bit intergers; the second field is an
    # 8-byte string. Note that here we can also give names to the individual
    # fields!
    dta2 = np.rec.array([
    (  1,   'mental',  2 ),
    (  2,   'mental',  2 ),
    (  3,   'mental',  3 ),
    (  4,   'mental',  4 ),
    (  5,   'mental',  4 ),
    (  6,   'mental',  5 ),
    (  7,   'mental',  3 ),
    (  8,   'mental',  4 ),
    (  9,   'mental',  4 ),
    ( 10,   'mental',  4 ),
    ( 11, 'physical',  4 ),
    ( 12, 'physical',  4 ),
    ( 13, 'physical',  3 ),
    ( 14, 'physical',  5 ),
    ( 15, 'physical',  4 ),
    ( 16, 'physical',  1 ),
    ( 17, 'physical',  1 ),
    ( 18, 'physical',  2 ),
    ( 19, 'physical',  3 ),
    ( 20, 'physical',  3 ),
    ( 21,  'medical',  1 ),
    ( 22,  'medical',  2 ),
    ( 23,  'medical',  2 ),
    ( 24,  'medical',  2 ),
    ( 25,  'medical',  3 ),
    ( 26,  'medical',  2 ),
    ( 27,  'medical',  3 ),
    ( 28,  'medical',  1 ),
    ( 29,  'medical',  3 ),
    ( 30,  'medical',  1 )], dtype=[('idx', '<i4'),
                                    ('Treatment', '|S8'),
                                    ('StressReduction', '<i4')])
    
    # First, do an one-way ANOVA
    df = pd.DataFrame(dta2)
    model = ols('StressReduction ~ C(Treatment)',df).fit()
    
    anovaResults =  anova_lm(model)
    print(anovaResults)
    if anovaResults['PR(>F)'][0] < 0.05:
        print('One of the groups is different.')
    
    #Then, do the multiple testing
    mod = MultiComparison(dta2['StressReduction'], dta2['Treatment'])
    print((mod.tukeyhsd().summary()))
    
    # The following code produces the same printout
    res2 = pairwise_tukeyhsd(dta2['StressReduction'], dta2['Treatment'])
    #print res2[0]
    
    # Show the group names
    print((mod.groupsunique))
    
    # Generate a print
    import matplotlib.pyplot as plt
    xvals = np.arange(3)
    plt.plot(xvals, res2.meandiffs, 'o')
    #plt.errorbar(xvals, res2.meandiffs, yerr=np.abs(res2[1][4].T-res2[1][2]), ls='o')
    errors = np.ravel(np.diff(res2.confint)/2)
    plt.errorbar(xvals, res2.meandiffs, yerr=errors, ls='o')
    xlim = -0.5, 2.5
    plt.hlines(0, *xlim)
    plt.xlim(*xlim)
    pair_labels = mod.groupsunique[np.column_stack(res2._multicomp.pairindices)]
    plt.xticks(xvals, pair_labels)
    plt.title('Multiple Comparison of Means - Tukey HSD, FWER=0.05' +
              '\n Pairwise Mean Differences')          
    
    # Save to outfile
    outFile = 'MultComp.png'
    plt.savefig('MultComp.png', dpi=200)
    print(('Figure written to {0}'.format(outFile)))
    
    plt.show()
    
    # Instead of the Tukey's test, we can do pairwise t-test
    # First, with the "Holm" correction
    rtp = mod.allpairtest(stats.ttest_rel, method='Holm')
    print((rtp[0]))
    
    # and then with the Bonferroni correction
    print((mod.allpairtest(stats.ttest_rel, method='b')[0]))
    
    # Done this way, the variance is calculated at each comparison.
    # If you want the joint variance across all samples, you have to 
    # use a few tricks:(http://jpktd.blogspot.co.at/2013/03/multiple-comparison-and-tukey-hsd-or_25.html)
    res2 = pairwise_tukeyhsd(dta2['StressReduction'], dta2['Treatment'])
    studentized_mean = res2.meandiffs
    studentized_variance = res2.variance
    
    t_stat = (studentized_mean / studentized_variance) / np.sqrt(2)
    dof = len(dta2) - len(mod.groupsunique)
    my_pvalues = stats.t.sf(np.abs(t_stat), dof) * 2  # two-sided
    
    # Now with the Bonferroni correction
    from statsmodels.stats.multitest import multipletests
    res_b = multipletests(my_pvalues, method='b')
    
    return res2.variance

if __name__ == '__main__':
    main()
