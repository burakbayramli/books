## module downhill
''' x = downhill(F,xStart,side,tol=1.0e-6)
    Downhill simplex method for minimizing the user-supplied
    scalar function F(x) with respect to the vector x.
    xStart = starting vector x.
    side   = side length of the starting simplex (default is 0.1)
'''
from numpy import zeros,dot,argmax,argmin,sum
from math import sqrt

def downhill(F,xStart,side=0.1,tol=1.0e-6):
    n = len(xStart)                 # Number of variables
    x = zeros((n+1,n)) 
    f = zeros(n+1)
    
  # Generate starting simplex
    x[0] = xStart
    for i in range(1,n+1):
        x[i] = xStart
        x[i,i-1] = xStart[i-1] + side        
  # Compute values of F at the vertices of the simplex     
    for i in range(n+1): f[i] = F(x[i])
    
  # Main loop
    for k in range(500):
      # Find highest and lowest vertices
        iLo = argmin(f)
        iHi = argmax(f)       
      # Compute the move vector d
        d = (-(n+1)*x[iHi] + sum(x,axis=0))/n
      # Check for convergence
        if sqrt(dot(d,d)/n) < tol: return x[iLo]
        
      # Try reflection
        xNew = x[iHi] + 2.0*d              
        fNew = F(xNew)        
        if fNew <= f[iLo]:        # Accept reflection 
            x[iHi] = xNew
            f[iHi] = fNew
          # Try expanding the reflection
            xNew = x[iHi] + d               
            fNew = F(xNew)
            if fNew <= f[iLo]:    # Accept expansion
                x[iHi] = xNew
                f[iHi] = fNew
        else:
          # Try reflection again
            if fNew <= f[iHi]:    # Accept reflection
                x[iHi] = xNew
                f[iHi] = fNew
            else:
              # Try contraction
                xNew = x[iHi] + 0.5*d
                fNew = F(xNew)
                if fNew <= f[iHi]: # Accept contraction
                    x[iHi] = xNew
                    f[iHi] = fNew
                else:
                  # Use shrinkage
                    for i in range(len(x)):
                        if i != iLo:
                            x[i] = (x[i] - x[iLo])*0.5
                            f[i] = F(x[i])
    print "Too many iterations in downhill"
    print "Last values of x were"
    return x[iLo]

            
