## Module gaussJordan
''' a_inverse = gaussJordan(a).
    Inverts matrix 'a' by Gauss-Jordan elimination.
'''    
def gaussJordan(a):
    n = len(a)
    for i in range(n):
        temp = a[i,i]
        a[i,i] = 1.0
        a[i,0:n] = a[i,0:n]/temp
        for k in range(n):
            if k != i: 
                temp = a[k,i]
                a[k,i] = 0.0
                a[k,0:n] = a[k,0:n] - a[i,0:n]*temp
    return a


