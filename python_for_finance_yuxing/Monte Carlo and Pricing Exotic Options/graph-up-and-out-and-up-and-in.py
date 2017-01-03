import scipy as sp
import p4f as p4f
from math import exp
import matplotlib.pyplot as pl
S0=9.15
x=9.15
barrier=10.15
T=0.5
n_steps=30.
r = 0.05
sigma=0.2
sp.random.seed(125)
n_simulation =5
dt =T/n_steps
S = sp.zeros([n_steps],dtype=float)
time_ = range(0,int(n_steps), 1)
c=p4f.bs_call(S0,x,T,r,sigma)
sp.random.seed(124)
outTotal, inTotal=0.,0.
n_out,n_in=0,0
for j in range(0, n_simulation):
    S[0] = S0
    inStatus=False
    outStatus=True
for i in time_[:-1]:
        e=sp.random.normal()
        S[i+1]=S[i]*exp((r-0.5*pow(sigma,2))*dt+sigma*sp.sqrt(dt)*e)
        if S[i+1]>barrier:
            outStatus=False
            inStatus=True
        pl.plot(time_,S)
        if outStatus==True:
                outTotal+=c;n_out+=1
        else:
                inTotal+=c;n_in+=1
        S=sp.zeros(int(n_steps))+barrier
        pl.plot(time_,S,'.-')
        upOutCall=round(outTotal/n_simulation,3)
        upInCall=round(inTotal/n_simulation,3)
        pl.figtext(0.15,0.8,'S='+str(S0)+',X='+str(x))
        pl.figtext(0.15,0.76,'T='+str(T)+',r='+str(r)+',sigma=='+str(sigma))
        pl.figtext(0.15,0.6,'barrier='+str(barrier))
        pl.figtext(0.40,0.86, 'call price = '+str(round(c,3)))
        pl.figtext(0.40,0.83,'up_and_out_call='+str(upOutCall)+' (='+str(n_out)+'/'+str(n_simulation)+'*'+str(round(c,3))+')')
        pl.figtext(0.40,0.80,'up_and_in_call ='+str(upInCall)+' (='+str(n_in)+'/'+str(n_simulation +')'))
        pl.xlabel('Total number of steps ='+str(int(n_steps)))
        pl.ylabel('stock price')
        pl.show()
        
