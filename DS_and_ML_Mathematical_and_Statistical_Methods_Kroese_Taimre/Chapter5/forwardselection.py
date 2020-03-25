""" forwardselection.py """
import statsmodels.api as sm
from statsmodels.formula.api import ols

bwt = sm.datasets.get_rdataset("birthwt","MASS").data
ftv1 = (bwt['ftv']>=1).astype(int)
ptl1 = (bwt['ptl']>=1).astype(int)

remaining_features = {'lwt', 'age', 'C(ui)', 'C(smoke)',
                      'C(ht)', 'ftv1', 'ptl1'}
selected_features = []
while remaining_features: 
  PF = []  #list of (P value, feature)
  for f in remaining_features:
    temp = selected_features + [f]  #temporary list of features
    formula = 'bwt~' + '+'.join(temp) 
    fit = ols(formula,data=bwt).fit()  
    pval= fit.pvalues[-1]
    if pval < 0.05:
       PF.append((pval,f))
  if PF:  #if not empty
     PF.sort(reverse=True)
     (best_pval, best_f) = PF.pop()
     remaining_features.remove(best_f)
     print('feature {} with p-value = {:.2E}'.
            format(best_f, best_pval))
     selected_features.append(best_f)
  else:
     break
 
formula = 'bwt~lwt+age+C(race)+ smoke'
bwt_model = ols(formula, data=bwt).fit()
print(bwt_model.summary())

formula = 'bwt~age*smoke'
bwt_model = ols(formula, data=bwt).fit()
print(bwt_model.summary())
bwt_model.conf_int()

bwt['nonsmoke'] = 1 - bwt['smoke'] 
formula = 'bwt~lwt+age*nonsmoke'
bwt_model = ols(formula, data=bwt).fit()
bwt_model.conf_int()