import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

data = pd.read_csv('PSImodel2020.csv',index_col=0)

w_0 = 1
mu_0 = 0.1
lam = 0.5 

relWage1 = data['ProdWage'] / data['GDPpc']
relWage1 = relWage1/relWage1.loc[1980]
relWage2 = data['UnskillWage'] / data['GDPpc']
relWage2 = relWage2/relWage2.loc[1980]
data['RelWage'] = (relWage1 + relWage2) / 2

data['RelWage'] = data['RelWage'].interpolate()
data['RelDebt'] = data['RelDebt'].interpolate()
data['Distrust'] = data['Distrust'].interpolate()

data = data[data.index > 1944]

data.loc[1945]['elite'] = 1

for t in range(1946,2021):
    data.loc[t]['elite'] = data.loc[t-1]['elite'] + mu_0*( w_0 - data.loc[t-1]['RelWage'] ) / \
                           data.loc[t-1]['RelWage']

data['epsilon'] = (1 - lam*data['RelWage'])/data['elite']

data['epsilon'] = data['epsilon']/data.iloc[0]['epsilon']
    
data['Urbanization'] = data['Urbanization']/100.

data['Age20_29'] = data['Age20_29']/60.

data['RelDebt'] = data['RelDebt']/100.

data['Distrust'] = data['Distrust']/100.

res = data[['RelWage','elite','epsilon']]
res.columns = ['w','e','epsilon']
res.plot()
plt.savefig('/tmp/out.jpg')
