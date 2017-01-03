from scipy import zeros, sqrt, shape
import scipy as sp
import matplotlib.pyplot as pl
S0 = 9.15
T= 1.
n_steps=100.
mu = 0.15
sigma=0.2
sp.random.seed(12345)
n_simulation = 1000
dt =T/n_steps
S = zeros([n_simulation], dtype=float)
x = range(0, int(n_steps), 1)
for j in range(0, n_simulation):
    tt=S0
for i in x[:-1]:
    e=sp.random.normal()
    tt+=tt*(mu-0.5*pow(sigma,2))*dt+sigma*tt*sqrt(dt)*e;
    S[j]=tt
    pl.title('Histogram of terminal price')
    pl.ylabel('Number of frequencies')
    pl.xlabel('Terminal price')
    pl.figtext(0.5,0.8,'S0='+str(S0)+',mu='+str(mu)+',sigma='+str(sigma))
    pl.figtext(0.5,0.76,'T='+str(T)+',steps='+str(int(n_steps)))
    pl.figtext(0.5,0.72,'Number of terminal prices='+str(int(n_simulation)))
    pl.hist(S)
pl.show()
          
