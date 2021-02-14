""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

#  Fern.py

from visual import*
from visual.graph import *
import random

imax = 12000                                            #  points to draw
i = 0
x = 0.5                                                #  initial x coord
y = 0.0                                                #  initial y coord
r = 0.0
xn = 0.0
yn = 0.0

random.seed(899432)                 #  Initialize random number generator
graph1 = display(width = 500, height = 500,          #  display 2D graphs
                 title = 'Fractal Fern', range = 1.0)    # -1x<1,  -1<y<1
pts = points(color=color.green, size=1)
for i in range(1, imax):
    r = random.random();                                 #  random number
    if ( r <=  0.02):                                    # 2% probability
        xn = 0.5
        yn = 0.27*y
    else:
        if ( r > 0.02 and r <=  0.17):                  # 15% probability
            xn =  - 0.139 * x  +  0.263 * y  +  0.57
            yn =  0.246   * x  +  0.224 * y  -  0.036
        else:
            if ( r > 0.17 and r <=  0.3):               # 13% probability
                xn =  0.17 * x  -  0.215 * y  +  0.408
                yn = 0.222 * x  +  0.176 * y  +  0.0893
            else:
                xn = 0.781    * x  + 0.034 * y  + 0.1075     # 70% probab
                yn =  - 0.032 * x  + 0.739 * y  +  0.27
    
    x = xn
    y = yn          
    xc = 2.0 * x - 1.0             #  linear transform 0<x<1  - > - 1<x<1
    yc = 1.8 * y - 1.0             #  linear transform 0<y<1  - > - 1<y<1
    pts.append(pos=(xc,yc))
    
