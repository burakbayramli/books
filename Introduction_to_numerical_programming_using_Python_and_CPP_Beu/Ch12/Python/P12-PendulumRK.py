# Angular motion of a nonlinear pendulum by the Runge-Kutta method
#    u" = -g/l * sin(u) - k * u',   u(0) = u0, u'(0) = u0'
from math import *
from ode import *

g = 9.81e0                                       # gravitational acceleration

def Func(t, u, f):                                    # RHS of 1st order ODEs
   f[1] =  u[2]                                         # u[1] = u, u[2] = u'
   f[2] = -g/l * sin(u[1]) - k * u[2]

# main

l = 1e0                                                     # pendulum length
k = 0e0                                                # velocity coefficient
u0 = 0.5e0*pi                                          # initial displacement
du0 = 0e0                                                # initial derivative
tmax = 20e0                                                       # time span
ht = 0.001e0                                                 # time step size

n = 2                                              # number of 1st order ODEs
u = [0]*(n+1)                                           # solution components

out = open("pendulum.txt","w")                             # open output file
out.write("      t         u        du\n")

t = 0e0
u[1] = u0; u[2] = du0                                        # initial values
out.write(("{0:10.5f}{1:10.5f}{2:10.5f}\n").format(t,u[1],u[2]))

nT = 0                                               # number of half-periods
t1 = t2 = 0e0                                       # bounding solution zeros
us = u[1]                                                     # save solution
while (t+ht <= tmax):                                      # propagation loop
   RungeKutta(t,ht,u,n,Func)
   t += ht

   if (u[1]*us < 0e0):                 # count solution passages through zero
      if (t1 == 0): t1 = t                                     # initial zero
      else: t2 = t; nT += 1                                      # final zero
   us = u[1]                                                  # save solution

   out.write(("{0:10.5f}{1:10.5f}{2:10.5f}\n").format(t,u[1],u[2]))

T = 2e0*(t2-t1) / nT                                      # calculated period
T0 = 2e0*pi*sqrt(l/g)                                       # harmonic period
print("u0 = {0:7.5f}  T/T0 = {1:7.5f}".format(u0,T/T0))

out.close()
