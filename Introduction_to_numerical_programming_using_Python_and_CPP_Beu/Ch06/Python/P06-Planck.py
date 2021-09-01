# Planck's law of black body radiation
from math import *
from roots import *
from graphlib import *

hP = 6.62606957e-34                                 # Planck's constant (J s)
kB = 1.3806488e-23                                 # Boltzmann constant (J/K)
c  = 2.99792458e8                                      # speed of light (m/s)

def u(lam):                                  # spectral energy density u(lam)
   global lam0, K                    # reference wavelength, factor of u(lam)

   if (lam == 0e0): return 0e0
   x = lam / lam0
   return K / (pow(x,5) * (exp(1e0/x) - 1e0))

def du(lam):                                                        # du/dlam
   global lam0, K                    # reference wavelength, factor of u(lam)

   if (lam == 0e0): return 0e0
   x = lam / lam0
   expx = exp(1e0/x)
   exp1 = expx - 1e0
   return K * lam0 * (5e0*x + (1e0-5e0*x)*expx) / (pow(x,7) * exp1*exp1)

# main

GraphInit(1200,600)

lam_plot = 3e-6                                # maximum wavelength for plots
h = 1e-8                                           # step-size for wavelength
n = int(lam_plot/h + 0.5) + 1                       # number of points / plot
nmax = 3 * n                                         # total number of points

x = [0]*(nmax+1); y = [0]*(nmax+1); z = [0]*(nmax+1)

for j in range(0,3):
   T = 4000e0 + j * 1000e0                              # current temperature
   lam0 = hP*c / (kB*T)                                # reference wavelength
   K = 8e0 * pi * kB * T / pow(lam0,4)                     # factor of u(lam)
   for i in range(1,n+1):
      lam = (i-1)*h
      x[i+j*n] = lam * 1e6                                          # microns
      y[i+j*n] = u(lam) 
      z[i+j*n] = du(lam) 

fmax = 0e0                                               # normalize profiles
for i in range(1,nmax+1):
   if (y[i] > fmax): fmax = y[i]
for i in range(1,nmax+1): y[i] /= fmax
fmax = 0e0
for i in range(1,nmax+1):
   if (z[i] > fmax): fmax = z[i]
for i in range(1,nmax+1): z[i] /= fmax

nn = [0, n, 2*n, 3*n]
col = ["", "blue", "green", "red"]
sty = [0, 1, 1, 1]
MultiPlot(x,y,y,nn,col,sty,3,10,
          0e0,0e0,0,0e0,0e0,0,0.10,0.45,0.15,0.85,
          "lambda (micron)","u","Spectral energy density")

MultiPlot(x,z,z,nn,col,sty,3,10,
          0e0,0e0,0,0e0,0e0,0,0.60,0.95,0.15,0.85,
          "lambda (micron)","du","Derivative of energy density")

#----------------------------------------------------------------------------
T = 5778e0                                 # temperature of sun's photosphere
lam0 = hP*c / (kB*T)                                   # reference wavelength
K = 8e0 * pi * kB * T / pow(lam0,4)                        # factor of u(lam)

fmin = 1e10; fmax = -1e10            # positions of maximum and minimum of du
for i in range(1,n+1):
   lam = (i-1)*h; f = du(lam)
   if (f > fmax): fmax = f; lam_1 = lam                             # maximum
   if (f < fmin): fmin = f; lam_2 = lam                             # minimum

                                           # find zero of du in [lam_1,lam_2]
lam_max = 0.5e0 * (lam_1 + lam_2)                     # initial approximation
(lam_max,ierr) = NewtonNumDrv(du,lam_1,lam_2,lam_max)
print("Wien's law:")
print("T = {0:4.0f} K".format(T))
print("lambda_max = {0:e} m".format(lam_max))
print("lambda_max * T = {0:e} m K".format(lam_max * T))     # 2.897768e-3 m K

MainLoop()
