from matplotlib.finance import quotes_historical_yahoo
import numpy as np
ticker='IBM'
begdate=(2009,1,1)
enddate=(2013,12,31)
p = quotes_historical_yahoo(ticker, begdate, enddate, asobject=True, adjusted=True)
ret = (p.close[1:] - p.aclose[:-1])/p.aclose[1:]
std_annual=np.std(ret)*np.sqrt(252)
                            
