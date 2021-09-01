# Reflexion/transmission of quantum wave packet using Gauss-Seidel solver
from math import *
from pde import *
from graphlib import *

def Pot(x, a, V0):                                                # Potential
   return V0 if fabs(x) <= 0.5e0*a else 0e0

#============================================================================
def InitR(Psi, Chi, x, nx, x0, sig, k0):
#----------------------------------------------------------------------------
#  Initial Gaussian wave packet
#     Psi(x,0) = 1/sqrt(sqrt(2*pi)*sig) * exp[-(x-x0)^2/(4*sig^2)] * exp(ikx)
#  x0 - position of center, sig - half-width, k0 - average wave number
#----------------------------------------------------------------------------
   a = 1e0/sqrt(sqrt(2e0*pi)*sig)
   b =-1e0/(4e0*sig*sig)
   for i in range(1,nx+1):
      dx = x[i] - x0
      f = a * exp(b*dx*dx)
      if (f < 1e-10): f = 0e0
      Psi[i] = f * cos(k0*x[i]); Chi[i] = f * sin(k0*x[i])

#============================================================================
def ProbDensR(Psi, Chi, Psi2, nx, hx):
#----------------------------------------------------------------------------
#  Calculates the probability density Psi2[] of the wave function Psi[]
#----------------------------------------------------------------------------
   for i in range(1,nx+1):                 # unnormalized probability density
      Psi2[i] = Psi[i]*Psi[i] + Chi[i]*Chi[i]
      if (Psi2[i] <= 1e-10): Psi2[i] = 0e0

   PsiNorm = 0.5e0*(Psi2[1] + Psi2[nx])        # integral by trapezoidal rule
   for i in range(2,nx): PsiNorm += Psi2[i]
   PsiNorm *= hx

   for i in range(1,nx+1): Psi2[i] /= PsiNorm      # normalized prob. density
   return PsiNorm

# main

a    = 5e0                                       # width of potential barrier
V0   = -100e0                                   # height of potential barrier
x0   = -20e0                                # initial position of wave packet
sig  = 1e0                                             # half-width of packet
k0   = 10e0                                   # average wave number of packet
xmax = 100e0                                                      # maximum x
hx   = 1e-1                                               # spatial step size
tmax = 5e0                                         # maximum propagation time
ht   = 5e-3                                                       # time step
nout = 40                                           # output every nout steps

nx = 2*(int)(xmax/hx + 0.5) + 1                 # odd number of spatial nodes
nt = (int)(tmax/ht + 0.5)                              # number of time steps
nx2 = int(nx/2); nx4 = int(nx/4) + 1

Psi  = [0]*(nx+1)                                # real part of wave function
Chi  = [0]*(nx+1)                                # imag part of wave function
Psi2 = [0]*(nx+1)                                       # probability density
V = [0]*(nx+1)                                                    # potential
x = [0]*(nx+1)                                                 # spatial mesh

for i in range(1,nx+1):                 # tabulate spatial mesh and potential
   x[i] = (i-nx2-1)*hx
   V[i] = Pot(x[i],a,V0)

InitR(Psi,Chi,x,nx,x0,sig,k0)                           # initial wave packet

GraphInit(1000,700)

for it in range(1,nt+1):                                          # time loop
   t = it*ht
   PropagQGS(Psi,Chi,V,nx,hx,ht)           # propagate by Gauss-Seidel solver

   PsiNorm = ProbDensR(Psi,Chi,Psi2,nx,hx)              # probability density

   if (it % nout == 0 or it == nt):                 # output every nout steps
      fname = "scatter_{0:4.2f}.txt".format(t)
      out = open(fname,"w")
      out.write("t = {0:4.2f}\n".format(t))
      out.write("     x          V        PsiR      PsiI      Psi2\n")
      for i in range(1,nx+1):
         out.write("{0:10.5f}{1:10.5f}{2:10.5f}{3:10.5f}{4:10.5f}\n".\
                 format(x[i],V[i],Psi[i],Chi[i],Psi2[i]))
      out.close

      GraphClear()
      title = "Scattering of wave packet  t = {0:4.2f}".format(t)
      Plot(x[nx4:],Psi2[nx4:],nx2,"blue",1,0.15,0.95,0.50,0.90,
           "None","Psi2",title)
      Plot(x[nx4:],V[nx4:],nx2,"red",1,0.15,0.95,0.08,0.48,"x","V","")
      GraphUpdate()

MainLoop()
