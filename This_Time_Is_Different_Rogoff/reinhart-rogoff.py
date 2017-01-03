# -*- coding: utf-8 -*-
# <nbformat>3.0</nbformat>

# <markdowncell>

# # Reinhart-Rogoff replication
# 
# * Replication of Reinhart-Rogoff "Growth in a Time of Debt."
# * Python port of R code by Thomas Herndon | Michael Ash | Robert Pollin
# * http://www.peri.umass.edu/236/hash/31e2ff374b6377b2ddec04deaa6388b1/publication/566/
# * Author: Vincent Arel-Bundock varel@umich.edu
# * Data: https://gist.github.com/vincentarelbundock/5409893/raw/a623f2f3bae027a0e51dd01ac5b70d44d909a7b9/RR-processed.csv

# <codecell>

import statsmodels.api as sm
import patsy
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

RR = pd.read_csv('RR-processed.csv')

# <markdowncell>

# ## Number of observations per country

# <codecell>

RR.groupby('Country').size()

# <markdowncell>

# ## Bins

# <codecell>

bins = ["0-30%","30-60%","60-90%","Above 90%"]
RR['dgcat'] = np.digitize(RR.debtgdp, [0,30,60,90,np.inf]) - 1
RR.dgcat = [bins[x] for x in RR.dgcat]

bins = ["0-30%","30-60%","60-90%","90-120%","Above 120%"]
RR['dgcat2'] = np.digitize(RR.debtgdp, [0,30,60,90,120,np.inf]) - 1
RR.dgcat2 = [bins[x] for x in RR.dgcat2]

# <markdowncell>

# ## Regression analysis 

# <codecell>

y,X = patsy.dmatrices('dRGDP ~ dgcat', data=RR[['dRGDP', 'dgcat']].dropna())
print sm.OLS(y,X).fit().summary()

# <codecell>

y2,X2 = patsy.dmatrices('dRGDP ~ dgcat2', data=RR[['dRGDP', 'dgcat2']].dropna())
print sm.OLS(y2,X2).fit().summary()

# <markdowncell>

# ## Table 3 Corrected

# <codecell>

## Country-Year average by debtgdp ("correct weights")
RR.dRGDP.groupby(RR.dgcat).mean()

# <codecell>

## Averaged Country averages by debtgdp ("equal weights")
RR.dRGDP.groupby([RR.Country, RR.dgcat]).mean().unstack()

# <codecell>

## Country-Year average by debtgdp ("correct weights") expanded categories
RR.dRGDP.groupby(RR.dgcat2).mean()

# <codecell>

## Averaged Country averages by debtgdp ("equal weights")
RR.dRGDP.groupby([RR.Country, RR.dgcat2]).mean().unstack()

# <markdowncell>

# ## Selective treatment of early years

# <codecell>

idx = (RR.Country == 'New Zealand') & (RR.Year < 1950) | (RR.Country == 'Australia') & (RR.Year < 1951) | (RR.Country == 'Canada') & (RR.Year < 1951) 
RR_selective = RR[idx == False]
RR_selective.dRGDP.groupby(RR_selective.dgcat).mean()

# <markdowncell>

# ## Equal weights
# ## Table 3 Weights,Exclusion

# <codecell>

RR_selective.mean()

# <markdowncell>

# ## Correct weights
# ## Table 3 Selective years exclusion

# <codecell>

RR_selective.dRGDP.groupby([RR_selective.Country, RR_selective.dgcat]).mean().unstack()

# <markdowncell>

# ## And dropping because of spreadsheet error

# <codecell>

drop = ["Australia","Austria","Belgium","Canada","Denmark"]
idx = [False if x in drop else True for x in RR_selective.Country]
RR_selective_spreadsheet = RR_selective[idx]
RR_selective_spreadsheet.dRGDP.groupby(RR.dgcat).mean()

# <markdowncell>

# ## New Zealand transcription error

# <codecell>

RR_selective_spreadsheet_transcription = RR_selective_spreadsheet.copy()
RR_selective_spreadsheet_transcription.RGDP[RR_selective_spreadsheet_transcription.Country=='New Zealand'] = -7.9
RR_selective_spreadsheet_transcription.dRGDP.groupby(RR.dgcat).mean()

# <codecell>

a = RR_selective_spreadsheet_transcription.Country
b = RR_selective_spreadsheet_transcription.dgcat
RR_selective_spreadsheet_transcription.dRGDP.groupby(b).mean()

# <codecell>

published_means = RR_selective_spreadsheet_transcription.dRGDP.groupby([a,b]).mean().unstack()
published_means.ix['New Zealand', 'Above 90%'] = -7.9
published_means.mean()

# <markdowncell>

# ## Medians

# <codecell>

RR.dRGDP.groupby(RR.dgcat).median() # Correct, equal weight

# <codecell>

RR.dRGDP.groupby(RR.dgcat2).median() # Correct, expanded categories, equal weight

# <markdowncell>

# ## Counts of years

# <codecell>

RR.Country.groupby([RR.Country, RR.dgcat]).size().unstack().sum()

# <codecell>

RR_selective.Country.groupby([RR.Country, RR.dgcat]).size().unstack().sum()

# <codecell>

RR_selective_spreadsheet.Country.groupby([RR.Country, RR.dgcat]).size().unstack().sum()

# <markdowncell>

# ## Categorical scatterplot

# <codecell>

labels = ["0-30%","30-60%","60-90%","Above 90%"]
dat = [np.array(RR.dRGDP[RR.dgcat==x]) for x in labels]
print sm.graphics.violinplot(dat, labels=labels)

# <codecell>

labels = ["0-30%","30-60%","60-90%","90-120%","Above 120%"]
dat = [np.array(RR.dRGDP[RR.dgcat2==x]) for x in labels]
print sm.graphics.violinplot(dat, labels=labels)

# <markdowncell>

# ## Country-Year average by debtgdp for more recent samples

# <codecell>

years = range(1950, 2001, 10)
f = lambda x: (x, RR[RR.Year >= x].dRGDP.groupby(RR[RR.Year >= x].dgcat).mean())
[f(x) for x in years]

