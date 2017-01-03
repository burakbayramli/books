import logging
from commonrandom import arbitrary_timeindex, skew_returns_annualised
from matplotlib.pyplot import show, hist
from common import DAYS_IN_YEAR, ROOT_DAYS_IN_YEAR, account_curve
import pandas as pd
import numpy as np
import scipy.stats as st

length_backtest_years=2
number_of_random_curves=100
annualSR=0.5
want_skew=0.0

length_bdays=length_backtest_years*DAYS_IN_YEAR
print length_bdays

random_curves=[skew_returns_annualised(annualSR=annualSR, want_skew=want_skew, size=length_bdays) 
               for NotUsed in range(number_of_random_curves)]

random_curves_npa=np.array(random_curves).transpose()

pddf_rand_data=pd.DataFrame(random_curves_npa, index=arbitrary_timeindex(length_bdays), columns=[str(i) for i in range(number_of_random_curves)])

acccurves_rand_data=[account_curve(pddf_rand_data[x]) for x in pddf_rand_data]

#results=[x.worst_drawdown() for x in acccurves_rand_data]
#results=[x.avg_drawdown() for x in acccurves_rand_data]
#print results
