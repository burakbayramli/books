# Relative motion of the Earth and Moon using the velocity Verlet method
from math import *
from ode import *
from coords import *
from graphlib import *

G = 6.67384e-11                         # gravitational constant m^3 / kg s^2

#============================================================================
def Forces(m, x, y, z, fx, fy, fz, n):
#----------------------------------------------------------------------------
#  Returns gravitational forces acting on a system of n point-masses
#----------------------------------------------------------------------------
   Epot = 0e0
   for i in range(1,n+1): fx[i] = fy[i] = fz[i] = 0e0

   for i in range(1,n):                                 # loop over all pairs
      for j in range(i+1,n+1):
         dx = x[i] - x[j]                   # components of relative distance
         dy = y[i] - y[j]
         dz = z[i] - z[j]
         r2 = dx*dx + dy*dy + dz*dz                        # squared distance
         r = sqrt(r2)
         fr = G * m[i] * m[j] / r                               # |force| * r

         Epot += fr                                  # total potential energy

         fr /= r2                                               # |force| / r
         fx[i] -= fr * dx; fx[j] += fr * dx          # total force components
         fy[i] -= fr * dy; fy[j] += fr * dy
         fz[i] -= fr * dz; fz[j] += fr * dz

   return Epot

# main

mEarth = 5.97e24                                          # Earth's mass (kg)
mMoon  = 7.35e22                                           # Moon's mass (kg)
d0 = 4.06e8                               # Earth-Moon distance at apogee (m)
v0 = 969e0                                  # initial relative velocity (m/s)
km = 1e3
month = 27.32                  # sidereal month: Moon's orbital period (days)
day = 3600 * 24                                              # day length (s)
tmax = 2 * month * day                        # time span: 2 lunar months (s)
ht = 1e0                                                      # time step (s)

n = 2                                                     # number of planets
m = [0]*(n+1)                                                 # planet masses
x = [0]*(n+1); vx = [0]*(n+1); ax = [0]*(n+1)                   # coordinates
y = [0]*(n+1); vy = [0]*(n+1); ay = [0]*(n+1)                    # velocities
z = [0]*(n+1); vz = [0]*(n+1); az = [0]*(n+1)                 # accelerations

nt = int(tmax/ht + 0.5) + 1                            # number of time steps
n1 = 10000                                  # number of steps between records
np = int(float(nt-1)/n1) + 1                       # number of plotted points
nt = (np-1) * n1 + 1                         # corrected number of time steps
tp = [0]*(np+1); dp = [0]*(np+1)                            # plotting arrays
xp = [0]*(np+1); yp = [0]*(np+1)

m[1] = mEarth                                         # initial configuration
x[1] = 0e0; vx[1] = 0e0; ax[1] = 0e0
y[1] = 0e0; vy[1] = 0e0; ay[1] = 0e0
z[1] = 0e0; vz[1] = 0e0; az[1] = 0e0
m[2] = mMoon
x[2] = d0 ; vx[2] = 0e0; ax[2] = 0e0
y[2] = 0e0; vy[2] = v0 ; ay[2] = 0e0
z[2] = 0e0; vz[2] = 0e0; az[2] = 0e0

MovetoCM(m,x,y,z,n)                                       # move to CM system
MovetoCM(m,vx,vy,vz,n)                                # cancel total momentum

t =  0e0                                                     # initialization
it = ip = 1
tp[ip] = t / day
dp[ip] = sqrt(pow(x[1]-x[2],2)+pow(y[1]-y[2],2)) / km   # Earth-Moon distance
xp[ip] = x[2] / km; yp[ip] = y[2] / km                      # Moon's position

GraphInit(1200,600)

while (t+ht <= tmax):                                      # propagation loop
   t += ht; it += 1
   (Ekin,Epot) = Verlet(ht,m,x,y,z,vx,vy,vz,ax,ay,az,n,Forces)

   if (it % n1 == 0):
      ip += 1
      tp[ip] = t / day
      dp[ip] = sqrt(pow(x[1]-x[2],2)+pow(y[1]-y[2],2)) / km 
      xp[ip] = x[2] / km; yp[ip] = y[2] / km

      GraphClear()
      Plot(tp,dp,ip,"blue",1,0.12,0.47,0.15,0.85,
           "t (days)","d (km)","Earth-Moon distance")
      Plot(xp,yp,ip,"red",2,0.62,0.97,0.15,0.85,
           "x (km)","y (km)","Moon's trajectory in the CM system")
      GraphUpdate()

MainLoop()
