#  Solves a Cauchy problem with adaptive step size control
from math import *
from ode import *

def Func(t, y, f):                                             # RHSs of ODEs
   f[1] = y[2]
   f[2] = (1e0 if t < 5e0 else -100e0) * y[1]

# main

y0 = 0e0; dy0 = 1e0                                          # initial values
tmax = 10e0                                                       # time span
ht = 0.01e0                                                       # step size
eps = 1e-8                                      # relative solution precision

n = 2                                              # number of 1st order ODEs
y = [0]*(n+1)                                           # solution components

out = open("ode.txt","w")                                  # open output file
out.write("      t         y         h\n")

t = 0e0
y[1] = y0; y[2] = dy0                                        # initial values
out.write(("{0:10.5f}{1:10.5f}{2:10.5f}\n").format(t,y[1],ht))

ht1 = ht                                            # initial step size guess
while (t+ht <= tmax):                                      # propagation loop
   ht = ht1                             # update initial step size with guess
   (ht, ht1) = RKFehlberg(t,ht,eps,y,n,Func)
   t += ht

   out.write(("{0:10.5f}{1:10.5f}{2:10.5f}\n").format(t,y[1],ht))

out.close()
