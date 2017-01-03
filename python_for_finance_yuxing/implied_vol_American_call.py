from math import exp,sqrt
def binomialCallAmerican(s,x,T,r,sigma,n=100):
    deltaT=T/n
    u=exp(sigma * sqrt(deltaT))
    d=1.0 / u
    a = exp(r*deltaT)
    p=(a-d) / (u-d)
    v = [[0.0 for j in xrange(i + 1)] for i in xrange (n+1)]
    for j in xrange(i+1):
        v[n][j] = max(s* u**j * d**(n-j)-x,0.0)
    for i in xrange(n-1,-1,-1):
        for j in xrange(i + 1):
            v1=exp(-r*deltaT)*(p*v[i+1][j+1]+(1.0-p)*v[i+1][j])
            v2=max(x-s,0)
            v[i][j]=max(v1,v2)
    return v[0][0]
   
def implied_vol_American_call(s,x,T,r,c):
    implied_vol=1.0
    min_value=1000
    for i in range(1000):
        sigma=0.001*(i+1)
        c2 = binomialCallAmerican(s,x,T,r,sigma)
        abs_diff=abs(c2-c)
        if abs_diff<min_value:
            min_value=abs_diff
            implied_vol=sigma
            k=1
    return implied_vol

