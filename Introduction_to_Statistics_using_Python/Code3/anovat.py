''' Explicit demonstration of properties of ANOVA
- For the comparison of two groups, a one-way ANOVA is equivalent to
  a T-test

.. math::
    t^2 = F

- Show how the ANOVA can be done by hand.
- Example of a Kruskal-Wallis test (for not normally distributed data)

'''

# author: Thomas Haslwanter, date: Jan-2014

import os
import scipy as sp
import scipy.stats as stats
from numpy import array
import pandas
from statsmodels.formula.api import ols
from statsmodels.stats.anova import anova_lm

# Get the data
data = pandas.read_csv(r'..\Data\data_kaplan\galton.csv')

# First, calculate the F- and the T-values, ...
F_statistic, pVal = stats.f_oneway(data['father'], data['mother'])
t_val, pVal_t = stats.ttest_ind(data['father'], data['mother'])

# ... and show that t**2 = F
print(('From the t-test we get t^2={0:5.3f}, and from the F-test F={1:5.3f}'.format(t_val**2, F_statistic)))

# ---------------------------------------------------------------
# Second, do the ANOVA with a function ...
anova_results = anova_lm(ols('height ~ 1 + sex', data).fit())
print(anova_results)

# ... and then by hand, using the formulas fom Altman, p. 218
grouped = data.groupby('sex')
mdf = pandas.DataFrame({'male': grouped.get_group('M')['height'],
                        'female': grouped.get_group('F')['height']})
M = mdf.mean()
n = mdf.count()
S = sum((mdf**2).sum())
T = sum(mdf.sum())
B = sum(n*M**2)-T**2/sum(n)
W = S - sum(n*M**2)
N = sum(n)
Total = B+W


meanSq_group = B / (mdf.ndim-1)
meanSq_res = W / (N-2)
F = meanSq_group/meanSq_res
print(('The hand-calculated F-value is: {0:5.3f}'.format(F)))

# -----------------------------------------------------------------
# And finally, give an example of the Kruskal-Wallis test
# Taken from http://www.brightstat.com/index.php?option=com_content&task=view&id=41&Itemid=1&limit=1&limitstart=2

# Get the data
city1 = array([68, 93, 123, 83, 108, 122])
city2 = array([119, 116, 101, 103, 113, 84])
city3 = array([70, 68, 54, 73, 81, 68])
city4 = array([61, 54, 59, 67, 59, 70])

# Perform the Kruskal-Wallis test
h, p = stats.mstats.kruskalwallis(city1, city2, city3, city4)

# Print the results
if p<0.05:
    print('There is a significant difference between the cities.')
else:
    print('No significant difference between the cities.')
