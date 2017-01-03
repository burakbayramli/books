import inspect
import sys; sys.path.append('../..')
from syscore.algos import robust_vol_calc
from syscore.accounting import accountCurve
import pandas as pd, numpy as np

def carry(daily_ann_roll, vol, smooth_days=90):
    ann_stdev = vol * 256
    raw_carry = daily_ann_roll / ann_stdev
    smooth_carry = pd.ewma(raw_carry, smooth_days)
    return smooth_carry


#inst = "MXP"; carryoffset = 3 
inst = "CORN"; carryoffset = -3 # SR 0.06
#inst = "EDOLLAR"; carryoffset = -3 # SR -0.67

carry_multiplier = 20.

f = '../../sysdata/legacycsv/%s_price.csv' % inst
df = pd.read_csv(f,index_col=0,parse_dates=True)
f = '../../sysdata/legacycsv/%s_carrydata.csv' % inst
df2 = pd.read_csv(f,index_col=0,parse_dates=True)

vol = robust_vol_calc(df.PRICE.diff())

#forecast2 = np.sign(carryoffset)*(df2.CARRY-df2.PRICE)
forecast2 = df2.PRICE-df2.CARRY/(carryoffset/12.)
forecast2 = carry(forecast2, vol) * carry_multiplier
forecast2.loc[forecast2 > 20] = 20
forecast2.loc[forecast2 < -20] = -20

account = accountCurve(df.PRICE, forecast=forecast2)
print (account.sharpe())
