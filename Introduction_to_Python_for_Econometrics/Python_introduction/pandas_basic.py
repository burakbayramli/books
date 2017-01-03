from __future__ import division
from __future__ import print_function
from __future__ import print_function, division
from numpy import array
from numpy import sqrt
from pandas import DataFrame
from pandas import Series
from pandas import date_range 
from pandas import read_csv
from pandas import read_excel
from pandas.tools.plotting import scatter_matrix 
from scipy.stats import norm
import StringIO 
import datetime
import gzip
import itertools
import pandas
import scipy.stats as stats
import sys
from pylab import *
from numpy import *
# End Imports


a = array([0.1, 1.2, 2.3, 3.4, 4.5])
a
s = Series([0.1, 1.2, 2.3, 3.4, 4.5])
s
s = Series(a)

s = Series([0.1, 1.2, 2.3, 3.4, 4.5], index = ['a','b','c','d','e'])
s

s = Series([0.1, 1.2, 2.3, 3.4, 4.5], index = ['a','b','c','d','e'])
s['a']
s[0]
s[['a','c']]
s[[0,2]]
s[:2]
s[s>2]

s = Series([0.1, 1.2, 2.3, 3.4, 4.5], index = ['a','b','c','a','b'])
s
s['a']

s = Series({'a': 0.1, 'b': 1.2, 'c': 2.3})
s

s = Series({'a': 0.1, 'b': 1.2, 'c': 2.3})
s * 2.0
s - 1.0

s1 = Series({'a': 0.1, 'b': 1.2, 'c': 2.3})
s2 = Series({'a': 1.0, 'b': 2.0, 'c': 3.0})
s3 = Series({'c': 0.1, 'd': 1.2, 'e': 2.3})
s1 + s2
s1 * s2
s1 + s3

s1 = Series([1.0,2,3],index=['a']*3)
s2 = Series([4.0,5],index=['a']*2)
s1 + s2

s1 = Series([1.0,2,3])
s1.values
s1.index
s1.index = ['cat','dog','elephant']
s1.index

s1 = Series(arange(10.0,20.0))
s1.describe()
summ = s1.describe()
summ['mean']

s1 = Series(arange(1.0,6),index=['a','a','b','c','d'])
s1
s1.drop('a')

s1 = Series(arange(1.0,4.0),index=['a','b','c'])
s2 = Series(arange(1.0,4.0),index=['c','d','e'])
s3 = s1 + s2
s3
s3.dropna()

s1 = Series(arange(1.0,4.0),index=['a','b','c'])
s2 = Series(arange(1.0,4.0),index=['c','d','e'])
s3 = s1 + s2
s3.fillna(-1.0)

df = DataFrame(array([[1,2],[3,4]]))
df

df = DataFrame(array([[1,2],[3,4]]),columns=['a','b'])
df
df = DataFrame(array([[1,2],[3,4]]))
df.columns = ['dogs','cats']
df

df = DataFrame(array([[1,2],[3,4]]), columns=['dogs','cats'], index=['Alice','Bob'])
df

t = dtype([('datetime', 'O8'), ('value', 'f4')])
x = zeros(1,dtype=t)
x[0][0] = datetime.datetime(2013,01,01)
x[0][1] = -99.99
x
df = DataFrame(x)
df

s1 = Series(arange(0,5.0))
s2 = Series(arange(1.0,6.0))
DataFrame({'one': s1, 'two': s2})
s3 = Series(arange(0,3.0))
DataFrame({'one': s1, 'two': s2, 'three': s3})

state_gdp = pandas.read_excel('US_state_GDP.xls','Sheet1')
state_gdp.head()

state_gdp[['state_code','state']].head()
index = state_gdp.index
state_gdp[index[1:3]].head() # Elements 1 and 2 (0-based counting)

state_gdp.state_code.head()

state_gdp[1:3]

state_gdp_copy = state_gdp.copy()
state_code = state_gdp_copy.pop('state_code')
state_gdp_copy.index = state_code
state_gdp_copy.head()
state_gdp_copy.loc[['AL','CA']]

state_long_recession = state_gdp['gdp_growth_2010']<0
state_gdp[state_long_recession].head()

state_gdp.ix[state_long_recession,'state']
state_gdp.ix[state_long_recession,['state','gdp_growth_2009','gdp_growth_2010']]
state_gdp.ix[10:15,'state']

state_gdp_2012 = state_gdp[['state','gdp_2012']]
state_gdp_2012.head()
state_gdp_2012['gdp_growth_2012'] = state_gdp['gdp_growth_2012']
state_gdp_2012.head()

state_gdp_2012 = state_gdp[['state','gdp_2012']]
state_gdp_2012.insert(1,'gdp_growth_2012',state_gdp['gdp_growth_2012'])
state_gdp_2012.head()

state_gdp_2012 = state_gdp.ix[0:2,['state','gdp_2012']]
state_gdp_2012
gdp_2011 = state_gdp.ix[1:4,'gdp_2011']
state_gdp_2012['gdp_2011'] = gdp_2011

state_gdp_copy = state_gdp.copy()
state_gdp_copy.index = state_gdp['state_code']
state_gdp_copy = state_gdp_copy[['gdp_growth_2011','gdp_growth_2012']]
state_gdp_copy.head()
gdp_growth_2012 = state_gdp_copy.pop('gdp_growth_2012')
gdp_growth_2012.head()
state_gdp_copy.head()
del state_gdp_copy['gdp_growth_2011']
state_gdp_copy.head()

df = DataFrame(array([[1, nan],[nan, 2]]))
df.columns = ['one','two']
replacements = {'one':-1, 'two':-2}
df.fillna(value=replacements)

df = DataFrame(array([[1, 3],[1, 2],[3, 2],[2,1]]), columns=['one','two'])
df.sort(columns='one')
df.sort(columns=['one','two'])
df.sort(columns=['one','two'], ascending=[0,1])

prices = [101.0,102.0,103.0]
tickers = ['GOOG','AAPL']
data = [v for v in itertools.product(tickers,prices)]
dates = pandas.date_range('2013-01-03',periods=3)
df = DataFrame(data, columns=['ticker','price'])
df['dates'] = dates.append(dates)
df
df.pivot(index='dates',columns='ticker',values='price')

original = DataFrame([[1,1],[2,2],[3.0,3]],index=['a','b','c'], columns=['one','two'])
original.reindex(index=['b','c','d'])
different = DataFrame([[1,1],[2,2],[3.0,3]],index=['c','d','e'], columns=['one','two'])
original.reindex_like(different)
original.reindex_axis(['two','one'], axis = 1)

left = DataFrame([[1,2],[3,4],[5,6]],columns=['one','two'])
right = DataFrame([[1,2],[3,4],[7,8]],columns=['one','three'])
left.merge(right,on='one') # Same as how='inner'
left.merge(right,on='one', how='left')
left.merge(right,on='one', how='right')
left.merge(right,on='one', how='outer')

left = DataFrame([[1,2],[3,4],[5,6]],columns=['one','two'])
left
right = DataFrame([[nan,12],[13,nan],[nan,8]],columns=['one','two'],index=[1,2,3])
right
left.update(right) # Updates values in left
left

subset = state_gdp[['gdp_growth_2009','gdp_growth_2010','region']]
subset.head()
grouped_data = subset.groupby(by='region')
grouped_data.groups # Lists group names and index labels for group membership
grouped_data.mean()  # Same as a pivot table

subset = state_gdp[['gdp_growth_2009','gdp_growth_2010','gdp_growth_2011','gdp_growth_2012']]
subset.index = state_gdp['state_code'].values
subset.head()
subset.apply(mean) # Same as subset.mean()
subset.apply(mean, axis=1).head() # Same as subset.mean(axis=1)

subset = state_gdp[['gdp_growth_2009','gdp_growth_2010','region']]
subset.head()
subset.pivot_table(rows='region')

state_gdp.describe()

state_gdp.region.value_counts()

GDP_data = pandas.read_excel('GDP.xls','GDP',skiprows=19)
GDP_data.head()
type(GDP_data.VALUE)
gdp = GDP_data.VALUE
gdp.index = GDP_data.DATE
gdp.head()
type(gdp)

gdp['2009']
gdp['2009-04']

gdp['2009':'2010']
gdp['2009-06-01':'2010-06-01']

date_range('2013-01-03','2013-01-05')
date_range('2013-01-03', periods = 3)

date_range('2013-01-03',periods=4, freq='Q').values
date_range('2013-01-03',periods=4, freq='7D4H').values

gdp.resample('A',how=mean).tail() # Annual average
gdp.resample('A',how=max).tail() # Maximum

gdp.pct_change().tail()
gdp.pct_change(periods=4).tail() # Quarterly data, annual difference

state_gdp.to_excel('state_gdp_from_dataframe.xls')
state_gdp.to_excel('state_gdp_from_dataframe_sheetname.xls', sheet_name='State GDP')
state_gdp.to_excel('state_gdp_from_dataframe.xlsx')
state_gdp.to_csv('state_gdp_from_dataframe.csv')
sio = StringIO.StringIO()
state_gdp.to_json(sio)
sio.seek(0)
sio.buf[:50]

df = DataFrame(zeros((1000,1000)))
df.to_csv('size_test.csv')
df.to_hdf('size_test.h5','df') # h5 is the usual extension for HDF5
df.to_hdf('size_test_compressed.h5','df',complib='zlib',complevel=6)
f = gzip.open('size_test.csvz','w')
df.to_csv(f)
f.close()
df_from_csvz = read_csv('size_test.csvz',compression='gzip')

x = randn(100,100)
DataFrame(x).to_csv('numpy_array.csv',header=False,index=False)

codes = ['GDPC1','INDPRO','CPILFESL','UNRATE','GS10','GS1','BAA','AAA']
names = ['Real GDP','Industrial Production','Core CPI','Unemployment Rate',\
   '10 Year Yield','1 Year Yield','Baa Yield','Aaa Yield']
# r to disable escape
base_url = r'http://research.stlouisfed.org/fred2/data/'

data = []
for code in codes:
    print(code)
    url = base_url + code + '.csv'
    data.append(read_csv(url))

time_series = {}
for code, d in zip(codes,data):
    d.index = d.DATE
    time_series[code] = d.VALUE
merged_data = DataFrame(time_series)
# Unequal length series
print(merged_data)

term_premium = merged_data['GS10'] - merged_data['GS1']
term_premium.name = 'Term'
merged_data = merged_data.join(term_premium,how='outer')
default_premium = merged_data['BAA'] - merged_data['AAA']
default_premium.name = 'Default'
merged_data = merged_data.join(default_premium,how='outer')
merged_data = merged_data.drop(['AAA','BAA','GS10','GS1'],axis=1)
print(merged_data.tail())

quarterly = merged_data.dropna()
print(quarterly.tail())

growth_rates_selector = ['GDPC1','INDPRO','CPILFESL']
growth_rates = quarterly[growth_rates_selector].pct_change()
final = quarterly.drop(growth_rates_selector, axis=1).join(growth_rates)

new_names = {'GDPC1':'GDP_growth','INDPRO':'IP_growth','CPILFESL':'Inflation','UNRATE':'Unemp_rate'}
final = final.rename(columns = new_names ).dropna()
final.to_hdf('FRED_data.h5','FRED',complevel=6,complib='zlib')
final.to_excel('FRED_data.xlsx')

ax = final[['GDP_growth','IP_growth','Unemp_rate']].plot(subplots=True)
fig = ax[0].get_figure()
fig.savefig('FRED_data_line_plot.pdf')
ax = scatter_matrix(final[['GDP_growth','IP_growth','Unemp_rate']], diagonal='kde')
fig = ax[0,0].get_figure()
fig.savefig('FRED_data_scatter_matrix.pdf')


NSW = read_excel('NSW.xls','NSW')
print(NSW.describe())

NSW = NSW.rename(columns={'Real income After ($)':'Income_after',
                          'Real income Before ($)':'Income_before',
                          'Education (years)':'Education'})
NSW['Minority'] = NSW['Black']+NSW['Hispanic']

print(NSW.pivot_table(rows='Treated'))
print(NSW.pivot_table(rows='Minority'))
print(NSW.pivot_table(rows=['Minority','Married']))

ax = NSW[['Income_before','Income_after']].plot(kind='kde',subplots=True)
fig = ax[0].get_figure()
fig.savefig('NSW_density.pdf')

income_diff = NSW['Income_after']-NSW['Income_before']
t = income_diff[NSW['Treated']==1]
nt = income_diff[NSW['Treated']==0]
tstat = (t.mean() - nt.mean())/sqrt(t.var()/t.count() - nt.var()/nt.count())
pval = 1 - stats.norm.cdf(tstat)
print('T-stat: {0:.2f}, P-val: {1:.3f}'.format(tstat,pval))

