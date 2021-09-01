# Plot a function of one variable
from math import *
from graphlib import *

def Func(x):                                         # function to be plotted
   return pow(x,3) * exp(-x)

# main

xmin = -0.8; xmax = 7.8                     # limits of the plotting interval
n = 50                                                     # number of points

x = [0]*(n+1)                                         # coordinates of points
y = [0]*(n+1)

h = (xmax-xmin)/(n-1)                                      # argument spacing
for i in range(1,n+1):
   x[i] = xmin + (i-1)*h                                          # arguments
   y[i] = Func(x[i])                                        # function values

GraphInit(800,600)                                            # create canvas
                                                                # create plot
Plot(x,y,n,"blue",1,0.15,0.95,0.15,0.85,"x","y","x^3 * exp(-x)")

MainLoop()                                         # enter Tkinter event loop
