
# Code from Chapter 11 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

# The Levenberg Marquardt algorithm
from numpy import *

def function(p):
    r = array([10*(p[1]-p[0]**2),(1-p[0])])
    fp = dot(transpose(r),r) #= 100*(p[1]-p[0]**2)**2 + (1-p[0])**2
    J = (array([[-20*p[0],10],[-1,0]]))
    grad = dot(transpose(J),transpose(r))
    return fp,r,grad,J

def lm(p0,tol=10**(-5),maxits=100):
    
    nvars=shape(p0)[0]
    nu=0.01
    p = p0
    fp,r,grad,J = function(p)
    e = sum(dot(transpose(r),r))
    nits = 0
    while nits<maxits and linalg.norm(grad)>tol:
        nits += 1
        fp,r,grad,J = function(p)
        H=dot(transpose(J),J) + nu*eye(nvars)

        pnew = zeros(shape(p))
        nits2 = 0
        while (p!=pnew).all() and nits2<maxits:
            nits2 += 1
            dp,resid,rank,s = linalg.lstsq(H,grad)
            pnew = p - dp
            fpnew,rnew,gradnew,Jnew = function(pnew)
            enew = sum(dot(transpose(rnew),rnew))
            rho = linalg.norm(dot(transpose(r),r)-dot(transpose(rnew),rnew))
            rho /= linalg.norm(dot(transpose(grad),pnew-p))
            
            if rho>0:
                update = 1
                p = pnew
                e = enew
                if rho>0.25:
                    nu=nu/10
            else: 
                nu=nu*10
                update = 0
        print fp, p, e, linalg.norm(grad), nu

p0 = array([-1.92,2])
lm(p0)
