from matplotlib.finance import quotes_historical_yahoo
import numpy as npticker='^GSPC'
begdate=(1987,11,1)
Chapter 12
[ 363 ]
enddate=(2006,12,31)
p = quotes_historical_yahoo(ticker, begdate, enddate,asobject=True,
adjusted=True)
ret = (p.aclose[1:] - p.aclose[:-1])/p.aclose[1:]
title('Illustration of volatility clustering (S&P500)')
ylabel('Daily returns')
xlabel('Date')
x=p.date[1:]
plot(x,ret)
