import scipy as sp
from pylab import *
stock_price_today = 9.15    #Stock price at time zero
T =1.
n_steps=100.
mu=0.15
sigma = 0.2
sp.random.seed(12345)
n_simulation =5
dt=T/n_steps
S=sp.zeros([n_steps],dtype=float)
x=range(0, int(n_steps), 1)
for j in range(0, n_simulation):
    S[0]=stock_price_today
for i in x[:-1]:
    e=sp.random.normal()
    S[i+1]=S[i]+S[i]*(mu-0.5*pow(sigma,2))*dt+sigma*S[i]*sp.sqrt(dt)*e;
sqrt(dt)*e;
plot(x, S)
#figtext(0.2,0.8,'SO='+str(S0)+',mu='+str(mu)+',sigma='+str(sigma))
figtext(0.2,0.76,'T='+str(T)+', steps='+str(int(n_steps)))
title('Stock price (number of simulations = %d ' % n_simulation +')')
xlabel('Total number of steps ='+str(int(n_steps)))
ylabel('stock price')
show()
