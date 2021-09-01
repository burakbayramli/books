# Histogram of random numbers
from random import *
from graphlib import *

n = 21                                             # number of bin boundaries
nrnd = 10000                                       # number of random numbers
x = [0]*(n+1); y = [0]*(n+1)                                # plotting points

a = 0e0; b = 1e0                                              # domain limits

HistoBin(0e0,a,b,x,y,n,0)                              # initialize histogram

for irnd in range(1,nrnd+1): HistoBin(random(),a,b,x,y,n,1)  # bin new values

HistoBin(0e0,a,b,x,y,n,2)                               # normalize histogram

GraphInit(800,600)                                            # create canvas
                                                           # create histogram
Plot(x,y,n,"blue",4,0.15,0.95,0.15,0.85,"x","f x","Random numbers")

MainLoop()                                         # enter Tkinter event loop
