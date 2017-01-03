
# Code from Chapter 11 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

# The Levenberg Marquardt algorithm solving a least-squares problem

from pylab import *
from numpy import *

def function(p,x,ydata):
    fp = p[0]*cos(p[1]*x)+ p[1]*sin([p[0]*x])
    r = ydata - fp
    J = transpose([-cos(p[0]*x)-p[1]*cos(p[0]*x)*x, p[0] * sin(p[1]*x)*x-sin(p[0]*x)])
    grad = dot(transpose(J),transpose(r))
    return fp,r,grad,J

def lm(p0,x,f,tol=10**(-5),maxits=100):
    
    nvars=shape(p0)[0]
    nu=0.01
    p = p0
    fp,r,grad,J = function(p,x,f)
    e = sum(dot(transpose(r),r))
    nits = 0
    while nits<maxits and linalg.norm(grad)>tol:
        nits += 1
        
        # Compute current Jacobian and approximate Hessian
        fp,r,grad,J = function(p,x,f)
        H=dot(transpose(J),J) + nu*eye(nvars)
        pnew = zeros(shape(p))
        nits2 = 0
        while (p!=pnew).all() and nits2<maxits:
            nits2 += 1
            # Compute the new estimate pnew
            #dp = linalg.solve(H,grad)
            dp,resid,rank,s = linalg.lstsq(H,grad)
            #dp = -dot(linalg.inv(H),dot(transpose(J),transpose(d)))
            pnew = p - dp[:,0]
            
            # Decide whether the trust region is good
            fpnew,rnew,gradnew,Jnew = function(pnew,x,f)
            enew = sum(dot(transpose(rnew),rnew))
            
            rho = linalg.norm(dot(transpose(r),r)-dot(transpose(rnew),rnew))
            rho /= linalg.norm(dot(transpose(grad),pnew-p))
            
            if rho>0:
                # Keep new estimate
                p = pnew
                e = enew
                if rho>0.25:
                    # Make trust region larger (reduce nu)
                    nu=nu/10
            else: 
                # Make trust region smaller (increase nu)
                nu=nu*10
        print p, e, linalg.norm(grad), nu
    return p
    
p0 = array([100.5,102.5]) #[ 100.0001126   101.99969709] 1078.36915936 8.87386341319e-06 1e-10 (8 itns)
#p0 = array([101,101]) #[ 100.88860713  101.12607589] 631.488571159 9.36938417155e-06 1e-67

p = array([100,102])

x = arange(0,2*pi,0.1)
y = p[0]*cos(p[1]*x)+ p[1]*sin([p[0]*x]) + random.rand(len(x))
p = lm(p0,x,y)
y1 = p[0]*cos(p[1]*x)+ p[1]*sin([p[0]*x]) #+ random.rand(len(x))

plot(x,squeeze(y),'-')
plot(x,squeeze(y1),'r--')
legend(['Actual Data','Fitted Data'])
show()
