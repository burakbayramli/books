# Solution of Kepler's equation
from math import *
from roots import *
from graphlib import *

pi2 = 2e0 * pi

def Func(E):
  global e, M                                    # eccentricity, mean anomaly
  f = E - e * sin(E) - M
  df = 1e0 - e * cos(E)
  return (f, df)

# main
n = 101
x = [0]*(n+1); y = [0]*(n+1)

GraphInit(1200,600)
                                              # Halley comet (Mottmann 1986):
e  = 0.9672671e0                                               # eccentricity
T = 75.96e0                                                          # period
t0 = 1986.1113e0                                         # time at perihelion

#----------------------------------------------------------------------------
t = 1986.2491e0
M = pi2 / T * (t - t0)                                         # mean anomaly
Emax = 1e0                          # eccentric anomaly vs. Kepler's function
h = Emax / (n-1)
for i in range(1,n+1):
  E = (i-1)*h
  x[i] = E; (y[i],df) = Func(E)

Plot(x,y,n,"blue",1,0.10,0.45,0.15,0.85,
     "E (rad)","f(E)","Kepler's function")

(E,ierr) = Newton(Func,0e0,Emax,E)                  # solve Kepler's equation
print("E = ",E," rad at t = ",t," years")

#----------------------------------------------------------------------------
h = T / (n-1)                          # time dependence of eccentric anomaly
x[1] = t0; y[1] = E = 0e0
for i in range(2,n+1):
   t = t0 + (i-1)*h
   M = pi2 / T * (t - t0)
   (E,ierr) = Newton(Func,0e0,pi2,E)                # solve Kepler's equation
   x[i] = t; y[i] = E

Plot(x,y,n,"red",1,0.60,0.95,0.15,0.85,
     "t (years)","E (rad)","Eccentric anomaly")

MainLoop()
