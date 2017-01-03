def looback_min_price_as_strike(s,T,r,sigma,n_simulation):
    n_steps=100.
    dt=T/n_steps
    total=0
    for j in range(n_simulation):
        min_price=100000.
        sT=s
        for i in range(int(n_steps)):
            e=sp.random.normal()
            sT*=sp.exp((r-0.5*sigma*sigma)*dt+sigma*e*sp.sqrt(dt))
    if sT<min_price:
                min_price=sT
            #print 'j=',j,'i=',i,'total=',total
            total+=p4f.bs_call(s,min_price,T,r,sigma)
        return total/n_simulation
