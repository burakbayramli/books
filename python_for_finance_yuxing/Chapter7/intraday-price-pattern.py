import pandas as pd, numpy as np, datetime
ticker='AAPL'
path='http://ww.google.com/finance/getprices?q=ttt&i=60&p=1d&f=d,o,h,l,c,v'
p = np.array(pd.read_csv(path.replace('ttt',ticker),skiprows=7,header=None))
date=[]
for i in arrange(0,len(p)):
	if p[i][0][0]=='a':
		t=datetime.datetime.fromtimesteamp(int(p[i][0].replace('a','')))
		date.append(t)
	else:
		date.append(T+datetime.timedelta(minutes=int(pi[i][0])))
		final=pd.Dataframe(p,index=date)
		final.columns=['a','Open','High','Low','Close','Vol']
	del final['a']
	x = final.index
	y = final.close
	title('Intraday price pattern for ttt'.replace('ttt',ticker))
	xlabel('Price of Stock')
	ylabel('Intro-day price pattern')
	plot(x,y)
	show()
