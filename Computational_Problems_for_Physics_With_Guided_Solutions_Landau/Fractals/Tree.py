""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# Tree.py: Creates fractal tree from simple rule

from visual import *
from visual.graph import *
import random

imax = 13000;  x = 0.5;   y = 0.0;   r = 0.0;   xn = 0.0;   yn = 0.0
i = 0
random.seed(899432)                                     # Seed generator

graph1 = display(width = 500, height = 500,            
                 title = 'Tree fractal', range = 2)    
pts = points(color=color.green, size=0.01)
for i in range(1, imax):
    r = random.random();
    if( r <= 0.1 ):
        xn = 0.05 * x
        yn = 0.6 * y
    else:
        if( r > 0.1 and r <= 0.2 ):
            xn =  0.05 * x
            yn = -0.5 * y + 1.
        else:
            if( r > 0.1 and r <= 0.2 ):
                xn =  0.05 * x
                yn = -0.5  * y  + 1.0
            else:
                if( r > 0.2 and r <= 0.4 ):
                    xn = 0.46 * x - 0.32 * y
                    yn = 0.39 * x + 0.38 * y + 0.6
                else:
                    if (r > 0.4 and r <= 0.6 ):
                        xn = 0.47 * x - 0.15 * y
                        yn = 0.17 * x + 0.42 * y + 1.1
                    else:
                        xn =  0.42 * x + 0.26 * y
                        yn = -0.35 * x + 0.31 * y + 0.7
    x = xn
    y = yn
    xc = 2.0 * x               
    yc = 2.0 * y - 2.0             
    pts.append(pos=(xc,yc))
    curve(pos = [( -1.8,  -1.85), (1.8,  -1.85)], color = color.yellow)
