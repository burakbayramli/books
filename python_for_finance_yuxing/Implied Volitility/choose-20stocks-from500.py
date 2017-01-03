import scipy as sp
from numpy import unique
n_stocks_available=500
n_stocks=20
x=sp.random.uniform(low=1,high=n_stocks_available,size=n_stocks)
y=[]
for i in range(n_stocks):
    y.append(int(x[i]))
#print y
final=unique(y)
print final
print len(final)
