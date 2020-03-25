""" snippets.py """ 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
import matplotlib.pyplot as plt
import pandas as pd
import statsmodels.api as sm
from statsmodels.formula .api import ols
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
myData = pd.DataFrame ({'y' : [10 ,9 ,4 ,2 ,4 ,9] ,
'x1' : [7.4 ,1.2 ,3.1 ,4.8 ,2.8 ,6.5] ,
'x2' : [1 ,1 ,2 ,2 ,3 ,3]})
mod = ols("y~x1+x2", data= myData )
mod_matrix = pd.DataFrame (mod.exog , columns =mod. exog_names )
print ( mod_matrix )
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
myData['x2'] = myData['x2'].astype('category')
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mod2 = ols("y~x1+C(x2)", data= myData )
mod2_matrix = pd.DataFrame(mod2.exog , columns =mod2.exog_names)
print (mod2_matrix)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mod3 = ols("y~x1*C(x2)", data= myData )
mod3_matrix = pd. DataFrame (mod3.exog , columns=mod3.exog_names)
print (mod3_matrix )
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
survey = pd.read_csv ('survey.csv')
plt.scatter(survey.shoe , survey.height)
plt.xlabel("Shoe size")
plt.ylabel("Height")
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
model = ols("height~shoe", data=survey ) # define the model
fit = model.fit () #fit the model defined above
b0 , b1 = fit.params
print (fit.params )
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
plt.plot(survey.shoe , b0 + b1* survey.shoe)
plt.scatter(survey.shoe ,survey.height)
plt.xlabel("Shoe size")
plt.ylabel("Height")
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
print (fit.summary ())
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dir(fit)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fit.pvalues[1]
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
model = ols("height~shoe+weight", data=survey)
fit = model.fit ()
axes = pd.plotting.scatter_matrix(
survey[['height','shoe','weight']])
plt.show()
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fit.summary()
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
table = sm.stats.anova_lm(fit)
print(table)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
model = ols("height~weight+shoe", data=survey)
fit = model.fit ()
table = sm.stats.anova_lm(fit)
print ( table )
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
x = {'shoe': [30.0] , 'weight': [75.0]} # new input ( dictionary )
pred = fit.get_prediction(x)
pred. summary_frame(alpha =0.05).unstack ()
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
plt.plot(fit.fittedvalues ,fit.resid ,'.')
plt.xlabel ("fitted values")
plt.ylabel ("residuals")
sm.qqplot (fit.resid)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
bwt = sm.datasets.get_rdataset("birthwt","MASS").data
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ftv1 = (bwt['ftv'] >=1).astype (int)
ptl1 = (bwt['ptl'] >=1).astype (int)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
