import scipy as sp
import p4f
def up_and_out_call(s0,x,T,r,sigma,n_simulation,barrier):
    n_steps=100.
    dt=T/n_steps
    total=0
    for j in range(0,n_simulation):
        sT=s0
        out=False
        for i in range(0,int(n_steps)):
            e=sp.random.normal()
            sT*=sp.exp((r-0.5*sigma*sigma)*dt+sigma*e*sp.sqrt(dt))
            if ssT>barrier:
                out=True
        if out ==False:
            total+=p4f.bs_call(s0,x,T,r,sigma)
    return total/n_simulation
        
