# Generation of random numbers with uniform distribution
from random1 import *
from graphlib import *

# main

n = 21                                             # number of bin boundaries
nrnd = 1000000                                     # number of random numbers
x = [0]*(n+1); y = [0]*(n+1)                                # plotting points

seed()

GraphInit(1200,800)

a = 0e0; b = 1e0

HistoBin(0e0,a,b,x,y,n,0)                              # initialize histogram
for irnd in range(1,nrnd+1): HistoBin(random(),a,b,x,y,n,1)
HistoBin(0e0,a,b,x,y,n,2)                               # normalize histogram
Plot(x,y,n,"blue",4,0.10,0.45,0.60,0.90,"x","n",
     "Built-in RNG")
                                            # Linear Congruential Generator 1
HistoBin(0e0,a,b,x,y,n,0)                              # initialize histogram
randLCG1(0)                                                  # initialize RNG
for irnd in range(1,nrnd+1): HistoBin(randLCG1(1),a,b,x,y,n,1)
HistoBin(0e0,a,b,x,y,n,2)                               # normalize histogram
Plot(x,y,n,"blue",4,0.60,0.95,0.60,0.90,"x","n",
     "Linear Congruential Generator 1")
                                            # Linear Congruential Generator 2
HistoBin(0e0,a,b,x,y,n,0)                              # initialize histogram
randLCG2(0)                                                  # initialize RNG
for irnd in range(1,nrnd+1): HistoBin(randLCG2(1),a,b,x,y,n,1)
HistoBin(0e0,a,b,x,y,n,2)                               # normalize histogram
Plot(x,y,n,"blue",4,0.10,0.45,0.10,0.40,"x","n",
     "Linear Congruential Generator 2")
                                              # Multiply-with-Carry Generator
HistoBin(0e0,a,b,x,y,n,0)                              # initialize histogram
randMCG(0)                                                   # initialize RNG
for irnd in range(1,nrnd+1): HistoBin(randMCG(),a,b,x,y,n,1)
HistoBin(0e0,a,b,x,y,n,2)                               # normalize histogram
Plot(x,y,n,"blue",4,0.60,0.95,0.10,0.40,"x","n",
     "Multiply-with-Carry Generator")

MainLoop()
