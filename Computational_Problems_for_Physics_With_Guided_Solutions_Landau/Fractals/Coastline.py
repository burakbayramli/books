""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

#  Coastline.py: Deposition to construct fractal coastline

from visual import *
from visual.graph import *
import random

Minx = 0;    Maxx = 200;  Miny = 0;  Maxy = 60
g = gdisplay(width=500, height=500, title="Coastline Depositon",\
     xtitle='x', ytitle='coast',xmin=Minx, xmax=Maxx, ymin=Miny, ymax=Maxy)

seacoast = gcurve(color=color.yellow)
coast = zeros((200))    

for i in range(0, 5000):                           
    spot = int(200*random.random())               
    if (spot == 0):                          # Hit edge fills hole
        if (coast[spot] < coast[spot + 1]):
            coast[spot] = coast[spot + 1]
        else:
            coast[spot]= coast[spot] + 1
    else:
        if (spot == 199):
            if (coast[spot] < coast[spot - 1]):
                coast[spot] = coast[spot - 1]
            else:
                coast[spot]=coast[spot] + 1
        else:
            if ((coast[spot]<coast[spot - 1]) and
                (coast[spot]<coast[spot + 1])):
                if (coast[spot-1] > coast[spot + 1]):
                     coast[spot] = coast[spot - 1]
                else:
                     coast[spot] = coast[spot + 1]
            else:
                coast[spot] = coast[spot] + 1

for i in range(0,200):
   seacoast.plot(pos=(i,coast[i]))                       # Plot coastline
     
