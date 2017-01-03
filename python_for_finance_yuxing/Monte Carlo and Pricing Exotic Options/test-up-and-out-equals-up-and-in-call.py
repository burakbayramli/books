import p4f as p4f
def up_call(s0,x,T,r,sigma,n_simulation,barrier):
    import scipy as sp
    import p4f
    n_steps=100.
    dt=T/n_steps
    inTotal=0
    outTotal=0
    for j in range(0, n_simulation):
        sT=s0
        inStatus=False
        outStatus=True
        for i in range(0,int(n_steps)):
            e=sp.random.normal()
            sT*=sp.exp((r-0.5*sigma*sigma)*dt+sigma*e*sp.sqrt(dt))
            if (sT>barrier) : 
                outStatus=False
                inStatus=True
                #print 'sT=',sT
            #print 'j=', j, 'out=',out
            if outStatus==True:
                       outTotal+=p4f.bs_call(s0,x,T,r,sigma)
            else:
                       inTotal+=p4f.bs_call(s0,x,T,r,sigma)
    return outTotal/n_simulation, inTotal/n_simulation


s0=40.
x=40.
barrier=42
T=0.5
r=0.05
sigma=0.2
n_simulation=100
upoutCall,upInCall=up_call(s0,x,T,r,sigma,n_simulation,barrier)
print 'upOutCall=', round(upoutCall,2),'upinCall=',round(upInCall,2)
print 'Black-Scholes call', round(p4f.bs_call(s0,x,T,r,sigma),2)

