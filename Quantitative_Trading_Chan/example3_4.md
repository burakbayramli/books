
## Sharpe

http://edge-fund.com/Lo02.pdf

http://www.rinfinance.com/agenda/2012/talk/StevenPav.pdf

```python
import pandas as pd
import pandas.io.excel as xl
ige = xl.read_excel('IGE.xls')
ige = ige.sort(columns='Date')
ige['Returns'] = ige['Adj Close'].pct_change()
print len(ige)
print ige.head()
```

```text
1504
           Date   Open   High    Low  Close  Volume  Adj Close   Returns
1503 2001-11-26  91.01  91.01  91.01  91.01       0      42.09       NaN
1502 2001-11-27  91.01  91.01  91.01  91.01       0      42.09  0.000000
1501 2001-11-28  91.01  91.01  91.01  91.01       0      42.09  0.000000
1500 2001-11-29  91.01  91.01  91.01  91.01       0      42.09  0.000000
1499 2001-11-30  91.32  91.32  91.32  91.32     200      42.23  0.003326
```

```python
ige['excessRet'] = ige['Returns'] - 0.04/252.
sharpeRatio = np.sqrt(252.)*ige['excessRet'].mean() / ige['excessRet'].std()
print sharpeRatio
```

```text
0.789317538345
```

## Max Drawdown

http://stackoverflow.com/questions/21058333/compute-rolling-maximum-drawdown-of-pandas-series




























