
# Code from Chapter 10 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

# The Factor Analysis algorithm
from pylab import *
from numpy import *
 
def factoranalysis(y,nRedDim):
    Ndata = shape(y)[0]
    N = shape(y)[1]
 
    y = y-y.mean(axis=0)
    C = cov(transpose(y))    
    Cd = C.diagonal()
    Psi = Cd
    scaling = linalg.det(C)**(1./N)
    
    W = random.normal(0,sqrt(scaling/nRedDim),(N,nRedDim))

    nits = 1000
    oldL = -inf

    for i in range(nits):    
    
        # E-step
        A = dot(W,transpose(W)) + diag(Psi)
        logA = log(abs(linalg.det(A)))
        A = linalg.inv(A)
        
        WA = dot(transpose(W),A)
        WAC = dot(WA,C)
        Exx = eye(nRedDim) - dot(WA,W) + dot(WAC,transpose(WA)) 

        # M-step
        W = dot(transpose(WAC),linalg.inv(Exx))
        Psi = Cd - (dot(W,WAC)).diagonal()
        #Sigma1 = (dot(transpose(y),y) - dot(W,WAC)).diagonal()/Ndata

        tAC = (A*transpose(C)).sum()
        
        L = -N/2*log(2.*pi) -0.5*logA - 0.5*tAC
        if (L-oldL)<(1e-4):
            print "Stop",i
            break
        print L
        oldL = L
    A = linalg.inv(dot(W,transpose(W))+diag(Psi))
    Ex = dot(transpose(A),W)
    
    return dot(y,Ex)

data = array([[0.1,0.1],[0.2,0.2],[0.3,0.3],[0.35,0.3],[0.4,0.4],[0.6,0.4],[0.7,0.45],[0.75,0.4],[0.8,0.35]])
newData = factoranalysis(data,2)
plot(newData[:,0],newData[:,1],'.')
show()
