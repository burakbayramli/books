import pandas as pd, numpy as np, datetime
ticker='AAPL'
path='http://www.google.com/finance/getprices?q=ttt&i=60&p=5&f=d,o,h,l,c,v'
x=np.array(pd.read_csv(path.replace('ttt',ticker),skiprows=7,header=None))
date=[]
