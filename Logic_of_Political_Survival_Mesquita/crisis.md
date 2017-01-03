
```python
import pandas as pd
df = pd.read_csv('bdm2s2_nation_year_data_may2002.csv')
df = df[df['year']>1970]
print list(df.columns)
```

```text
['Unnamed: 0', 'year', 'AGEA', 'AGEI', 'AUT', 'BRITCOL', 'CATH', 'COMEX', 'EDT', 'EDTG', 'ELF60', 'ETHNIC', 'GINI', 'GXPDEDUC', 'GXPDHLTH', 'GXPDSSEC', 'GXPKTOTL', 'INEQ', 'INST', 'LEGSELEC', 'MOSLEM', 'OIL', 'PROT', 'REGION', 'REGION2', 'RELIGION', 'RIOTS', 'STRIKES', 'UNSTABLE', 'WAR', 'ccode', 'TAXGDP', 'TAXGNP', 'TREV', 'PRIVCON', 'INCOMEY', 'INCOMET', 'DIRT', 'INDIRT', 'democ', 'autoc', 'polity', 'polity2', 'durable', 'xrreg', 'xrcomp', 'xropen', 'xconst', 'parreg', 'parcomp', 'exrec', 'exconst', 'polcomp', 'bdate', 'bprec', 'change', 'd3', 'sf', 'regtrans', 'country', 'BANKScode', 'Radios', 'Newspapers', 'Assinations', 'GenStrikes', 'GovCrises', 'Purges', 'Riots', 'Revolutions', 'AntiGovDem', 'ConflictIndex', 'RegimeType', 'Coups', 'HeadofState', 'Guerilla', 'RegisteredVoters', 'MajConstChg', 'Premier', 'Legselec', 'NatGovRevenue', 'NatGovExpend', 'defenseovertotal', 'name', 'chcode', 'longname', 'coding_differs', 'WorldBankCode', 'WorldBankName', 'fatlev1', 'hostlev1', 'legselec', 'esolow', 'bhkavg', 'exchprem', 'sh_staa', 'sh_med_b', 'sh_med_p', 'sh_h2o_s', 'immig', 'emig', 'ID', 'NAME', 'PR', 'CL', 'cap', 'milper', 'milex', 'energy', 'irst', 'majpow', 'warnoEX', 'EXdeaths', 'EXintside', 'EXinitiate', 'sysstat', 'EX_war_start', 'EXduration', 'EX_WAR', 'number_of_EXwars', 'biggestEXwar', 'warno', 'duration', 'deaths', 'outcome', 'initiate', 'is_war_start', 'IS_WAR', 'number_of_ISwars', 'biggestISwar', 'warnoCL', 'CLdeaths', 'CLintside', 'CLinitiate', 'CL_war_start', 'CLduration', 'CL_WAR', 'number_of_CLwars', 'biggestCLwar', 'ai', 'sd', 'ainew', 'sdnew', 'idgurr', 'worldbankcode', 'WB_pop', 'WB_gdppc_constUS95', 'WB_growth', 'WB_gdp_curUS', 'WB_gdp95', 'Aid_pcUS', 'FemSec', 'Doctors', 'MilPerson', 'MilEx', 'MilExGNI', 'InfMort', 'LowBirth', 'LifeExp', 'Labor_PrimEd', 'ImmuneMEASLES', 'ImmuneDPT', 'Illiteracy', 'Beds', 'HealthEXP', 'HealthExpPPP', 'GrossNatSaving', 'GrossDomSaving', 'Capital_formation', 'Saving_dom', 'FDI', 'Foreign_Finnace', 'ConsumptionSpending', 'Expenditure', 'Deathrate', 'newspaper', 'HealthExpend', 'EducExpend', 'CapitalExpend_tot', 'CurRevenue', 'GrossDomInvest', 'GovDebt', 'Birthrate', 'total_aid', 'aid_cap_constUS', 'aid_gdp', 'CapitalExpend', 'pop', 'rgdpch', 'rgdpl', 'c', 'i', 'g', 'rgdptt', 'y', 'cgdp', 'cc', 'ci', 'cg', 'p', 'pc', 'pi', 'pg', 'xr', 'rgdpea', 'rgdpw', 'kapw', 'kdur', 'knres', 'kother', 'kres', 'ktranp', 'open', 'rgnp', 'ipri', 'stliv', 'finittrm', 'yrcurnt', 'military', 'auton', 'eda', 'strikes', 'nspolpr', 'agovdem', 'govtcris', 'corrupti80_89', 'purges', 'coups', 'riots', 'constchg', 'cabchg', 'genocide', 'worldbankname', 'NatYr', 'cowreg', 'regyr', 'demaut', 'lpop', 'lpop_WB', 's', 'S', 'W', 'W_extend', 'W_mod', 'polityW', 'polityWn', 'Parl_Pres', 'w2', 'demaut2', 'WoverS', 'dbigw', 'dW', 'dw30', 'lagW', 'lagW2', 'dwos30', 'grow', 'LongGR1', 'LongGR2', 'LongGR3', 'LongGR4', 'lagGrow', 'lrgdpc', 'laglrgdpc', 'lgdpWB', 'laglgdpWB', 'VarGrow', 'VarGrow_', 'build', 'bal_bud', 'VarBud', 'Var_bud', 'Klepto', 'nexcap']
```

```python
df['Surplus'] = df['CurRevenue']-df['Expenditure']
df['Surplusl1'] = df['Surplus'].shift(1) 
df['Surplusl2'] = df['Surplus'].shift(2) 
df['Surplusl3'] = df['Surplus'].shift(3) 
df['Surplusl4'] = df['Surplus'].shift(4) 
df3 = df[['year','WB_growth','Surplus','Surplusl1','Surplusl2','Surplusl3','Surplusl4']]
df3 = df3.dropna()
print len(df3)
import statsmodels.formula.api as smf
results = smf.ols('WB_growth ~ Surplus + Surplusl2 + Surplusl3' , data=df3).fit()
print results.summary()
```

```text
1991
                            OLS Regression Results                            
==============================================================================
Dep. Variable:              WB_growth   R-squared:                       0.033
Model:                            OLS   Adj. R-squared:                  0.031
Method:                 Least Squares   F-statistic:                     22.35
Date:                Sun, 19 Jul 2015   Prob (F-statistic):           3.13e-14
Time:                        00:02:52   Log-Likelihood:                -5936.4
No. Observations:                1991   AIC:                         1.188e+04
Df Residuals:                    1987   BIC:                         1.190e+04
Df Model:                           3                                         
Covariance Type:            nonrobust                                         
==============================================================================
                 coef    std err          t      P>|t|      [95.0% Conf. Int.]
------------------------------------------------------------------------------
Intercept      3.7920      0.128     29.651      0.000         3.541     4.043
Surplus        0.1652      0.023      7.200      0.000         0.120     0.210
Surplusl2     -0.1275      0.019     -6.652      0.000        -0.165    -0.090
Surplusl3      0.0171      0.018      0.964      0.335        -0.018     0.052
==============================================================================
Omnibus:                      193.751   Durbin-Watson:                   1.416
Prob(Omnibus):                  0.000   Jarque-Bera (JB):             1342.622
Skew:                          -0.118   Prob(JB):                    2.84e-292
Kurtosis:                       7.016   Cond. No.                         16.6
==============================================================================

Warnings:
[1] Standard Errors assume that the covariance matrix of the errors is correctly specified.
```

```python
print df3
```

```text
       year  WB_growth    Surplus  Surplusl1  Surplusl2  Surplusl3
175    1975  -0.356611  -2.564596  -0.194693  -1.083765  -1.426472
176    1976   5.562603  -3.547331  -2.564596  -0.194693  -1.083765
177    1977   4.640539  -2.136003  -3.547331  -2.564596  -0.194693
178    1978   5.513810  -1.809309  -2.136003  -3.547331  -2.564596
179    1979   3.182480  -0.693287  -1.809309  -2.136003  -3.547331
180    1980  -0.227902  -1.831499  -0.693287  -1.809309  -2.136003
181    1981   2.449727  -1.540344  -1.831499  -0.693287  -1.809309
182    1982  -2.021596  -3.260855  -1.540344  -1.831499  -0.693287
183    1983   4.329519  -5.404911  -3.260855  -1.540344  -1.831499
184    1984   7.260786  -4.198532  -5.404911  -3.260855  -1.540344
185    1985   3.848768  -4.451365  -4.198532  -5.404911  -3.260855
186    1986   3.418787  -4.746815  -4.451365  -4.198532  -5.404911
187    1987   3.397920  -3.070293  -4.746815  -4.451365  -4.198532
188    1988   4.171592  -3.086586  -3.070293  -4.746815  -4.451365
189    1989   3.509137  -2.721657  -3.086586  -3.070293  -4.746815
190    1990   1.761598  -3.806427  -2.721657  -3.086586  -3.070293
191    1991  -0.470322  -5.269194  -3.806427  -2.721657  -3.086586
192    1992   3.051290  -4.686991  -5.269194  -3.806427  -2.721657
193    1993   2.654793  -3.854988  -4.686991  -5.269194  -3.806427
194    1994   4.034653  -2.911644  -3.854988  -4.686991  -5.269194
195    1995   2.668278  -2.167093  -2.911644  -3.854988  -4.686991
196    1996   3.570261  -1.446373  -2.167093  -2.911644  -3.854988
197    1997   4.434240  -0.378687  -1.446373  -2.167093  -2.911644
198    1998   4.364516   0.741217  -0.378687  -1.446373  -2.167093
199    1999   3.600000   1.330404   0.741217  -0.378687  -1.446373
363    1977   3.361815  -3.123297  -0.997124  -0.650976   1.012888
364    1978   4.046591  -3.666927  -3.123297  -0.997124  -0.650976
365    1979   4.156019  -2.649281  -3.666927  -3.123297  -0.997124
366    1980   1.338302  -2.582191  -2.649281  -3.666927  -3.123297
367    1981   3.105074  -1.087029  -2.582191  -2.649281  -3.666927
...     ...        ...        ...        ...        ...        ...
19209  1985   0.848111  -8.796961  -4.055031 -11.018259 -10.563047
19210  1986  19.801224 -15.493069  -8.796961  -4.055031 -11.018259
19211  1987   2.425016 -17.109423 -15.493069  -8.796961  -4.055031
19212  1988   3.613707 -12.611235 -17.109423 -15.493069  -8.796961
19213  1989   8.117859  -5.914122 -12.611235 -17.109423 -15.493069
19214  1990   1.779755  -8.655430  -5.914122 -12.611235 -17.109423
19215  1991   3.585242 -15.843773  -8.655430  -5.914122 -12.611235
19319  1974   2.601156  -3.900000  -4.156075  -1.909681  -3.973572
19320  1975   0.530207  -1.376245  -3.900000  -4.156075  -1.909681
19321  1976   2.736164  -3.887733  -1.376245  -3.900000  -4.156075
19322  1977   5.904011  -5.524923  -3.887733  -1.376245  -3.900000
19323  1978   1.863400  -4.719452  -5.524923  -3.887733  -1.376245
19324  1979  12.224821  -3.622389  -4.719452  -5.524923  -3.887733
19325  1980  -1.603459  -3.549862  -3.622389  -4.719452  -5.524923
19326  1981   6.329993  -3.510698  -3.549862  -3.622389  -4.719452
19327  1982  -5.978491  -6.213959  -3.510698  -3.549862  -3.622389
19328  1983  -4.297454  -4.011557  -6.213959  -3.510698  -3.549862
19329  1984   8.502238  -3.684830  -4.011557  -6.213959  -3.510698
19330  1985  -4.008784  -3.231520  -3.684830  -4.011557  -6.213959
19331  1986   7.744247  -3.609173  -3.231520  -3.684830  -4.011557
19332  1987  -6.610626  -4.976286  -3.609173  -3.231520  -3.684830
19333  1988   0.953674  -1.862093  -4.976286  -3.609173  -3.231520
19334  1989  13.544423  -1.614185  -1.862093  -4.976286  -3.609173
19335  1990   2.600752   0.404890  -1.614185  -1.862093  -4.976286
19336  1991  -0.298523  -3.597425   0.404890  -1.614185  -1.862093
19337  1992   4.895730  -3.068319  -3.597425   0.404890  -1.614185
19338  1993   2.203415  -4.655903  -3.068319  -3.597425   0.404890
19339  1994   3.900240  -3.658665  -4.655903  -3.068319  -3.597425
19340  1995   1.400616  -3.551815  -3.658665  -4.655903  -3.068319
19341  1996   3.400000  -5.092405  -3.551815  -3.658665  -4.655903

[2153 rows x 6 columns]
```





