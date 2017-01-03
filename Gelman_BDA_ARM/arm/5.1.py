import statsmodels.api as sm
import statsmodels.formula.api as smf
import pandas as pd

import pandas as pd
df = pd.read_csv('nes.dat',sep=r'\s+')
df = df[['presvote','year','income','black']]
df = df[df['presvote'] < 3] # sadece 2 partinin oylarini al
# 1,2 oylari 1,0 yap, Cumhuriyetciye verildi mi evet/hayir 
# haline getir
df['vote'] = df['presvote'].map(lambda x: x-1) 
df = df.drop('presvote',axis=1)
df = df.dropna()

df2 = df[df['year'] == 1992]
mdlm = smf.logit("vote ~ income", df2)
mdlmf = mdlm.fit()
print(mdlmf.summary())
