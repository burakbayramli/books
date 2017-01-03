## module conjGrad
''' x, numIter = conjGrad(Av,x,b,tol=1.0e-9)
    Conjugate gradient method for solving [A]{x} = {b}.
    The matrix [A] should be sparse. User must supply
    the function Av(v) that returns the vector [A]{v}.
'''    
from numarray import dot
from math import sqrt

def conjGrad(Av,x,b,tol=1.0e-9):
    n = len(b)
    r = b - Av(x)
    s = r.copy()
    for i in range(n):
        u = Av(s)
        alpha = dot(s,r)/dot(s,u)
        x = x + alpha*s
        r = b - Av(x)
        if(sqrt(dot(r,r))) < tol:
            break
        else:
            beta = -dot(r,u)/dot(s,u)
            s = r + beta*s
    return x,i


    
