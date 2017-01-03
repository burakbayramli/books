## module swap
''' swapRows(v,i,j).
    Swaps rows i and j of vector or matrix [v].

    swapCols(v,i,j).
    Swaps columns i and j of matrix [v].
'''    
def swapRows(v,i,j):
    if len(v.getshape()) == 1: v[i],v[j] = v[j],v[i]
    else:
        temp = v[i].copy()
        v[i] = v[j]
        v[j] = temp
   
def swapCols(v,i,j):
    temp = v[:,j].copy()
    v[:,j] = v[:,i]
    v[:,i] = temp
