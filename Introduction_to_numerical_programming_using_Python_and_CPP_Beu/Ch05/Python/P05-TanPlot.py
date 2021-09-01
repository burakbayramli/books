# Plots the tangent function using the continued fraction representation
from math import *
from elemfunc import *
from graphlib import *

# main

xmin = -1.5e0; xmax = 1.5e0                                 # plotting domain
h = 0.01e0                                                 # argument spacing
n = int((xmax-xmin)/h) + 1                                 # number of points

x = [0]*(n+1); y = [0]*(n+1)                               # arrays for plots

for i in range(1,n+1):
  x[i] = xmin + (i-1)*h
  y[i] = Tan(x[i])

GraphInit(600,600)

Plot(x,y,n,"blue",1,0.2,0.9,0.1,0.9,"x","Tan(x)","")

MainLoop()
