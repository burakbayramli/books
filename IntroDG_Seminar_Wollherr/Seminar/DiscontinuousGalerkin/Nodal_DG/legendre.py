def legendre(N,x):

# returns the value of Legendre Polynomial P_N (x) at position x [-1, 1]

    import numpy as np
    
    P = np.zeros(2*N, dtype = float)
    
    
    if N==0:
        P[0]=1
        out=P[0]
    elif N==1:
        P[1]=x;
        out=P[1];
    else:
        P[0]=1
        P[1]=x
    for i in range(2,N+1): 
        P[i] = (float(1)/float(i))  * ( (2*i - 1)*x*P[i-1] - (i-1)*P[i-2] )
    
    out=P[N]

    return(out)
    
 