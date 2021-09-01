# Generation of random numbers with normal distribution
from math import *
from random1 import *
from graphlib import *

def wMet(x): return exp(-0.5*x*x)                  # distribution for randMet

# main

n = 21                                             # number of bin boundaries
nrnd = 1000000                                     # number of random numbers
x = [0]*(n+1); y = [0]*(n+1)                                # plotting points

seed()

GraphInit(1200,800)

a = -3.5e0; b = 3.5e0
                                                      # Central limit theorem
HistoBin(0e0,a,b,x,y,n,0)                              # initialize histogram
for irnd in range(1,nrnd+1):
   (w, rnd) = randNrm()
   HistoBin(rnd,a,b,x,y,n,1)
HistoBin(0e0,a,b,x,y,n,2)                               # normalize histogram
Plot(x,y,n,"blue",4,0.10,0.45,0.60,0.90,"x","n",
     "Mean value of uniform random numbers")
                                                   # 2D Gaussian distribution
HistoBin(0e0,a,b,x,y,n,0)                              # initialize histogram
for irnd in range(1,nrnd+1):
   (w,rnd,rnd) = randNrm2()
   HistoBin(rnd,a,b,x,y,n,1)
HistoBin(0e0,a,b,x,y,n,2)                               # normalize histogram
Plot(x,y,n,"blue",4,0.60,0.95,0.60,0.90,"x","n",
     "2D Gaussian distribution")
                                                          # Metropolis method
HistoBin(0e0,a,b,x,y,n,0)                              # initialize histogram
randMet(wMet,0.1e0,0)                                        # initialize RNG
for irnd in range(1,nrnd+1): HistoBin(randMet(wMet,0.1e0,1),a,b,x,y,n,1)
HistoBin(0e0,a,b,x,y,n,2)                               # normalize histogram
Plot(x,y,n,"blue",4,0.10,0.45,0.10,0.40,"x","n",
     "Metropolis  delta = 0.1")
                                                          # Metropolis method
HistoBin(0e0,a,b,x,y,n,0)                              # initialize histogram
randMet(wMet,0.1e0,0)                                        # initialize RNG
for irnd in range(1,nrnd+1): HistoBin(randMet(wMet,0.5e0,1),a,b,x,y,n,1)
HistoBin(0e0,a,b,x,y,n,2)                               # normalize histogram
Plot(x,y,n,"blue",4,0.60,0.95,0.10,0.40,"x","n",
     "Metropolis  delta = 0.5")

MainLoop()
