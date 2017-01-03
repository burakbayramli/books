import inspect
import logging
import pandas as pd
import sys; sys.path.append('../..')
from matplotlib.pyplot import show, title

from systems.provided.futures_chapter15.estimatedsystem import futures_system
system=futures_system()
forecast=system.rules.get_raw_forecast("MXP", "carry")
#forecast=system.rules.get_raw_forecast("CRUDE_W", "ewmac64_256")    
#f = '../../sysdata/legacycsv/CRUDE_W_price.csv'
#f = '../../sysdata/legacycsv/EDOLLAR_price.csv'
f = '../../sysdata/legacycsv/MXP_price.csv'
df = pd.read_csv(f,index_col=0,parse_dates=True)
from syscore.accounting import accountCurve
account = accountCurve(df.PRICE, forecast=forecast)
print (account.sharpe())
