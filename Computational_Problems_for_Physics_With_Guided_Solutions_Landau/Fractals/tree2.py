""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# Another tree.py

from visual import *
from visual.graph import *
import random
imax = 20000                                                # pts to plot
x = 0.5;   y = 0.0                                # initial:  .x, y coord
 
graph1 = display(width=500, height=500,title='Tree')
pts = points(color=color.green, size=0.01)

for i in range(1,imax):
    r = random.random();                                  # random number
    if ( r <= 0.2):                                     # 20% probability
           xn = 0.01*x 
           yn = 0.45*y
    elif ( r > 0.2 and r <= 0.3):                       # 10% probability
            xn = -0.01 * x
            yn =  -0.45* y +0.4
    elif ( r > 0.3 and r <= 0.7):                      # 40 % probability
            xn =  0.42 * x - 0.42 * y 
            yn = 0.42* x + 0.42 * y + 0.4
    else:
            xn = 0.42 * x +0.42 *y                      # 30% probability
            yn = -0.42 * x + 0.42 *y + 0.4

    x = xn
    y = yn          
    xc = 2.0*x-0.1                                    # to plot in screen
    yc = 2.0*y-1.0                 # linear transformation of coordinates
    '''dibuja una rayita horizontal que imita un punto'''
    # curve(pos=[(xc,yc),(xc+0.01,yc)],color=color.green)     # tiny line
    pts.append(pos=(xc,yc))
