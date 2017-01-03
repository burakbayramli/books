from scipy import zeros, sqrt, shape, exp, mean
import scipy as sp
s0=40.  #Stock Price at time zero
X= 40.
T=0.5
r=0.05
sigma=0.2
n_steps=100.
sp.random.seed(12345)
n_simulation = 5000
dt=T/n_steps
call = zeros([n_simulation], dtype=float)
x = range(0,int(n_steps), 1)
for j in range(0,n_simulation):
    sT=s0
for i in x[:-1]:
        e=sp.random.normal()
        sT*=exp((r-0.5*sigma*sigma)*dt+sigma*e*sqrt(dt))
        call[j]=max(sT-X,0)
call_price=mean(call)*exp(-r*T)
print 'call price = ',round(call_price,3)
