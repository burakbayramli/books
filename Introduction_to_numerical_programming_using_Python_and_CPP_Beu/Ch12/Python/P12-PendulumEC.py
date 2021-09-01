# Angular motion of a nonlinear pendulum by the Euler-Cromer method
#    u" = -g/l * sin(u) - k * u',   u(0) = u0, u'(0) = u0'
from math import *
from ode import *

g = 9.81e0                                       # gravitational acceleration

def Func(t, u, v):
   return -g/l * sin(u) - k * v

# main

l = 1e0                                                     # pendulum length
k = 0e0                                                # velocity coefficient
u0 = 0.5e0*pi                                          # initial displacement
du0 = 0e0                                                # initial derivative
tmax = 20e0                                                       # time span
ht = 0.01e0                                                  # time step size

out = open("pendulum.txt","w")                             # open output file
out.write("      t         u        du\n")

t = 0e0
u = u0; du = du0
out.write(("{0:10.5f}{1:10.5f}{2:10.5f}\n").format(t,u,du))

while (t+ht <= tmax):                                      # propagation loop
   (u, du) = EulerCromer1(t,ht,u,du,Func)
   t += ht

   out.write(("{0:10.5f}{1:10.5f}{2:10.5f}\n").format(t,u,du))

out.close()
