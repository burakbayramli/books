# Plot a function of one variable with different styles
from math import *
from graphlib import *

def Func(x):
   return pow(x,3) * exp(-x)

# main

xmin = 0.2; xmax = 9.8                      # limits of the plotting interval
n = 30;                                                    # number of points

x = [0]*(n+1)                                         # coordinates of points
y = [0]*(n+1)

h = (xmax-xmin)/(n-1)                                      # argument spacing
for i in range(1,n+1):
   x[i] = xmin + (i-1)*h                                          # arguments
   y[i] = Func(x[i])                                        # function values

GraphInit(1200,800)                                           # create canvas
                                                               # create plots
Plot(x,y,n,"blue",0,0.08,0.48,0.56,0.92,"None","y","Scatter plot sty = 0")
Plot(x,y,n,"red" ,2,0.56,0.96,0.56,0.92,"x","y","Polar plot sty = 2")
Plot(x,y,n,"red" ,3,0.08,0.48,0.08,0.44,"x","y","Drop lines sty = 3")
Plot(x,y,n,"blue",4,0.56,0.96,0.08,0.44,"x","None","Histogram sty = 4")

MainLoop()                                         # enter Tkinter event loop
