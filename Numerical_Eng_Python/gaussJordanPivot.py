## module gaussJordanPivot
''' a_inverse = gaussJordanPivot(a,tol=1.0e-9).
    Inverts matrix 'a' with Gauss-Jordan elimination using
    scaled row pivoying.
'''    
from numarray import array,zeros,Float64,argmax,abs
import swap
import error

def gaussJordanPivot(a,tol=1.0e-9):
    n = len(a)
    seq = array(range(n))
    
  # Set up scale factors
    s = zeros((n),type=Float64)
    for i in range(n):
        s[i] = max(abs(a[i,:]))      
    
    for i in range(n):

      # Row interchange, if needed
        p = int(argmax(abs(a[i:n,i])/s[i:n])) + i
        if abs(a[p,i]) <  tol: error.err('Matrix is singular')
        if p != i:
            swap.swapRows(s,i,p)
            swap.swapRows(a,i,p)
            swap.swapRows(seq,i,p)
            
      # Elimination phase
        temp = a[i,i]
        a[i,i] = 1.0
        a[i,0:n] = a[i,0:n]/temp
        for k in range(n):
            if k != i: 
                temp = a[k,i]
                a[k,i] = 0.0
                a[k,0:n] = a[k,0:n] - a[i,0:n]*temp

  # Rearrange columns in the right sequence
    for i in range(n):
        swap.swapCols(a,i,seq[i])
        swap.swapRows(seq,i,seq[i])
    return a

