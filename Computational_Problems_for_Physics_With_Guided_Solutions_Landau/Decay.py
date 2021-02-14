""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# Decay.py spontaneous decay simulation
from visual import *	 
from visual.graph import *
import random

lambda1 = 0.01                                           # Decay constant
max = 50.;  time_max = 500;   seed = 68111                       # Params
number = nloop = max                                      # Initial value
# random.seed(seed)                               # Seed number generator

graph1 = gdisplay(width=500, height=500, title ='Spontaneous Decay',xtitle='Time',
                  ytitle = 'Number left',xmax=500,xmin=0,ymax=100,ymin=0)
decayfunc = gcurve(color = color.green)

for time in arange(0, time_max + 1):                          # Time loop
    for atom in arange(1, number + 1 ):                      # Decay loop
        decay = random.random()   
        if (decay  <  lambda1):
            nloop = nloop  -  1                                 # A decay
    number = nloop 
    decayfunc.plot( pos = (time, number) )
    rate(30)
