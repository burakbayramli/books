import zipfile, pandas as pd, util, sys
import matplotlib.pyplot as plt
import numpy as np, random, datetime
from scipy.optimize import minimize
import boot

def calc_ewmac_forecast(price,slow,fast):
    vol = util.robust_vol_calc(price.diff())
    fast_ewma = pd.ewma(price, span=slow)
    slow_ewma = pd.ewma(price, span=fast)
    raw_ewmac = fast_ewma - slow_ewma
    return raw_ewmac /  vol 

if __name__ == "__main__": 
 
    random.seed(0)
    np.random.seed(0)
    
    base = "../pysystemtrade/sysdata/legacycsv"
    df = pd.read_csv('%s/SP500_price.csv' % base, index_col=0,parse_dates=True )
    df['x'] = pd.read_csv('%s/US20_price.csv' % base, index_col=0,parse_dates=True )
    df.columns = ['SP500','US20']
    
    df = df.sort_index()
    forecast = df.copy()

    ewmac8_32_scalar = 10.6 # pg. 321
    ewmac32_128_scalar = 2.65

    df['US20_ewmac8_32'] = calc_ewmac_forecast(df['US20'], 8, 32) * ewmac8_32_scalar /10. 
    df['US20_ewmac32_128'] = calc_ewmac_forecast(df['US20'], 32, 128) * ewmac32_128_scalar /10. 
    df['SP500_ewmac8_32'] = calc_ewmac_forecast(df['SP500'], 8, 32) * ewmac8_32_scalar/10.
    df['SP500_ewmac32_128'] = calc_ewmac_forecast(df['SP500'], 32, 128) * ewmac32_128_scalar/10.

    forecast['US20'] = (df['US20_ewmac8_32'] + df['US20_ewmac32_128']) / 2
    forecast['SP500'] = (df['SP500_ewmac8_32'] + df['SP500_ewmac32_128']) / 2
    
    forecast.loc[forecast.US20 > 20, 'US20'] = 20.
    forecast.loc[forecast.SP500 > 20, 'SP500'] = 20.
    forecast.loc[forecast.US20 < -20, 'US20'] = -20.
    forecast.loc[forecast.SP500 < -20, 'SP500'] = -20.

    df['US20'] = df['US20'].pct_change() * forecast.shift(1).US20 / 10.
    df['SP500'] = df['SP500'].pct_change() * forecast.shift(1).SP500 / 10.
    df = df[['US20','SP500']]
    #df.to_csv('US20_SP500_returns.csv') # 0.38276063  0.61723937
    
    weights=boot.optimise_over_periods(df,rollyears=20, monte_carlo=20,monte_length=250)

    print np.array(weights.tail(1))
    
    weights.plot()
    plt.show()
