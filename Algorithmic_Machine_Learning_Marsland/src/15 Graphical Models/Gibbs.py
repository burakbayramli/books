
# Code from Chapter 15 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

# A Gibbs sampler for the Exam Panic dataset

from numpy import *
    
Pb = array([[0.5,0.5]])
Pr_b = array([[0.3,0.7],[0.8,0.2]])
Pa_b = array([[0.1,0.9],[0.5,0.5]])
Pp_ra = array([[0,1],[0.8,0.2],[0.6,0.4],[1,0]])

"""
P(b|rap)=P(b|ra)=P(ra|b)*P(b)/P(ra)=P(r|b)*P(a|b)*P(b)/P(ra)
r a    P(b)
T T    0.3*0.1*0.5/0.215=0.0698
T F    0.3*0.9*0.5/0.335=0.4030
F T    0.7*0.1*0.5/0.085=0.4118
F F    0.7*0.9*0.5/0.365=0.8630
"""

def pb_rap(values):
    if random.rand()<values[1]*values[2]*0.0698+values[1]*(1-values[2])*0.4030+(1-values[1])*values[2]*0.4118+(1-values[1])*(1-values[2])*0.8630:
        values[0]=1
    else:
        values[0]=0
    return values   

"""   
P(r|bap)=P(rap|b)*P(b)/P(bap)
        =(P(p|rab)*P(rab)/P(b))*P(b)/P(bap)
        =(P(p|rab)*P(rab))/P(bap)
       
P(bap)=P(b)*P(a|b)*(P(r|b)*P(p|ra)+P(~r|b)*P(p|~ra))
    b a p    P(bap)
    T T T    0.5*0.1*(0.3*0+0.7*0.6)=0.021
    T T F    0.5*0.1*(0.3*1+0.7*0.4)=0.029
    T F T    0.5*0.9*(0.3*0.8+0.7*1)=0.423
    T F F    0.5*0.9*(0.3*0.2+0.7*0)=0.027
    F T T    0.5*0.5*(0.8*0+0.2*0.6)=0.030
    F T F    0.5*0.5*(0.8*1+0.2*0.4)=0.220
    F F T    0.5*0.5*(0.8*0.8+0.2*1)=0.210
    F F F    0.5*0.5*(0.8*0.2+0.2*0)=0.040




P(r|bap) =(P(p|rab)*P(rab))/P(bap)
         =(P(p|ra)*P(r|b)*P(a|b)*P(b))/P(bap)

b a p    P(r)
T T T    0                     =0
T T F    1*0.3*0.1*0.5/0.029   =0.5172
T F T    0.8*0.3*0.9*0.5/0.423 =0.2553
T F F    0.2*0.3*0.9*0.5/0.027 =1
F T T    0                     =0
F T F    1*0.8*0.5*0.5/0.220   =0.9091
F F T    0.8*0.8*0.5*0.5/0.210 =0.7619
F F F    0.2*0.8*0.5*0.5/0.040 =1
"""


def pr_bap(values):
    y=random.rand(1)
    if random.rand()<values[0]*values[2]*(1-values[3])*0.5172+values[0]*(1-values[2])*values[3]*0.2553+values[0]*(1-values[2])*(1-values[3])+(1-values[0])*values[2]*(1-values[3])*0.9091+(1-values[0])*(1-values[2])*values[3]*0.7619+(1-values[0])*(1-values[2])*(1-values[3]):
        values[1]=1
    else:
        values[1]=0
    return values



"""   
P(a|brp)=P(rap|b)*P(b)/P(brp)
        =(P(p|rab)*P(rab)/P(b))*P(b)/P(brp)
        =(P(p|rab)*P(rab))/P(brp)
       
P(brp)=P(b)*P(r|b)*(P(a|b)*P(p|ra)+P(~a|b)*P(p|r~a))
    b r p    P(brp)
    T T T    0.5*0.3*(0.1*0+0.9*0.8)=0.108
    T T F    0.5*0.3*(0.1*1+0.9*0.2)=0.042
    T F T    0.5*0.7*(0.1*0.6+0.9*1)=0.334
    T F F    0.5*0.7*(0.1*0.4+0.9*0)=0.014
    F T T    0.5*0.8*(0.5*0+0.5*0.8)=0.160
    F T F    0.5*0.8*(0.5*1+0.5*0.2)=0.240
    F F T    0.5*0.2*(0.5*0.6+0.5*1)=0.080
    F F F    0.5*0.2*(0.5*0.4+0.5*0)=0.020





P(a|brp) =(P(p|rab)*P(rab))/P(brp)
         =(P(p|ra)*P(r|b)*P(a|b)*P(b))/P(brp)

b r p    P(a)
T T T    0                     =0
T T F    1*0.3*0.1*0.5/0.042   =0.3571
T F T    0.6*0.7*0.1*0.5/0.334 =0.0629
T F F    0.4*0.7*0.1*0.5/0.014 =1
F T T    0                     =0
F T F    1*0.8*0.5*0.5/0.240   =0.8333
F F T    0.6*0.2*0.5*0.5/0.080 =0.375
F F F    0.4*0.2*0.5*0.5/0.020 =1
"""


def pa_brp(values):
    if random.rand()<values[0]*values[1]*(1-values[3])*0.3571+values[0]*(1-values[1])*values[3]*0.0629+values[0]*(1-values[1])*(1-values[2])+(1-values[0])*values[1]*(1-values[3])*0.8333+(1-values[0])*(1-values[1])*values[3]*0.375+(1-values[0])*(1-values[1])*(1-values[3]):
        values[2]=1
    else:
        values[2]=0
    return values

def pp_bra(values):
    if random.rand()<values[1]*values[2]*0+values[1]*(1-values[2])*0.8+(1-values[1])*values[2]*0.6+(1-values[1])*(1-values[2])*1:
        values[3]=1
    else:
        values[3]=0
    return values


def gibbs():
        
    nsamples = 500
    nsteps = 10
    distribution = zeros(16,dtype=float)
    
    for i in range(nsamples):
        # values contains current samples of b, r, a, p
        values = where(random.rand(4)<0.5,0,1)       
        for j in range(nsteps):
            values=pb_rap(values)
            values=pr_bap(values)
            values=pa_brp(values)
            values=pp_bra(values)               
        distribution[values[0]+2*values[1]+4*values[2]+8*values[3]] += 1
    distribution /= nsamples
    print 'b  r  a  p: \t dist'
    for b in range(2):
        for r in range(2):
            for a in range(2):
                for p in range(2):
                    print 1-b,1-r,1-a,1-p,'\t', distribution[b+2*r+4*a+8*p]
gibbs()
