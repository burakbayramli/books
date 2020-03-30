""" genham.py """
import numpy as np

def nbe(a,b):  
    numd = len(a) 
    na = a.copy()
    carry= True
    for i in reversed(range(numd)):
        if carry:
            if na[i] == b-1:
                na[i] = 0
            else:
                na[i] = na[i] + 1
                carry = False            
    if carry:
       na.insert(0,1)        
    return(na)

def vdc(b,N):
    out = np.zeros((N,1))
    numd = np.ceil(np.log(N)/np.log(b))
    bb = 1/b**np.arange(1,numd+1)
    a = []
    out[0] = 0
    for i in range(1,N):
        a = nbe(a,b)
        #print(a)
        ar = a[::-1]
        #print(ar, bb[0:len(ar)])
        out[i] = np.sum(ar*bb[0:len(ar)])
    return(out)

def halton(b,N):
   dim = len(b);
   out = np.zeros((N,dim))
   for i in range(0,dim):
      out[:,i] = vdc(b[i],N).reshape((N,))
   return(out)

def hammersley(b,N):
    dim = len(b);
    out = np.zeros((N,dim))
    h = halton(b[0:dim-1],N-1)
    h = h.reshape((N-1,dim-1))
    out[1:N,1:dim] = h
    out[:,0] = np.arange(N)/N
    return(out)

b = [2,3,5]
N = 20
#print(hammersley(b,N))
print (hammersley([2,3],20))
