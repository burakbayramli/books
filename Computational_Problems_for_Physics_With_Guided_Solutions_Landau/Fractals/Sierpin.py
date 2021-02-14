""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# Sierpin.py:            Sierpinski gasket
 
from visual import *
from visual.graph import *
import random

imax = 15000
i = 0
a1 = - 0.5             #                 (a3, b3)
b1 = - 0.433           #                   /\
a2 = 0.5               #                  /  \
b2 = - 0.433           #                 /    \
a3 = 0.0               #                /      \
b3 = 0.433             #               /________\
x = 0.0                #       (a1, b1)           (a2, b2)
y = 0.3                # 

random.seed(899432)                  # Initialize random number generator

graph1 = display(width = 500, height = 500, title = 'Sierpinski Gasket', 
                 range = 0.5, background = (0.87, 0.93, 0.87) )
# range = 0.5 means:  - 0.5<x<0.5,  - 0.5<y<0.5

for i in range(1, imax):
    r = random.random();
    if (r <= 1.0/3.0):
         x = 0.5 * (x + a1)
         y = 0.5 * (y + b1)
    else:
        if( r > 1.0/3.0 and r <= 2.0/3.0):
             x = 0.5*(x + a2)
             y = 0.5*(y + b2)
        else:
            x = 0.5 * (x + a3)
            y = 0.5 * (y + b3)
    xc = x
    yc = y
    curve(pos = [(xc, yc), (xc + 0.002, yc)], color = color.red)   
