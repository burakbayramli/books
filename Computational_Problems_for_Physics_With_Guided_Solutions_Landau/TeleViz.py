""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""
   
# TelegraphViz.py:  Lossless transmission line animation, Visual

from visual import *

g = display(width = 600, height = 300, title='Telegrapher`s Eqnt')
vibst = curve(x=list(range(0,101)), color=color.yellow,radius=0.5)
L = 0.1; C = 2.5; c = 1/sqrt(L*C); dt = 0.025; dx = 0.05 
R = (c*dt/dx)**2                       # R  = 1 for stabiity
V = zeros( (101, 3), float)                  # Declare array
xx = 0

for i in arange (0,100):
    V[i,0] =  10*exp(-(xx**2)/0.1)   
    xx = xx+dx
    vibst.x[i] = 2.0*i - 100.0   # i=0-> x=-100;  i =100, x=100
    vibst.y[i] = 0.0                        # Eliminate a curve
    
for i in range(1, 100): 
   V[i, 2] = V[i,0] + R*(V[i+1,1] + V[i-1,1] - 2*V[i,1])
   vibst.x[i] = 2.0*i - 100.0                  #  x scale again
   vibst.y[i] = V[i, 2]
V[: ,0] = V[: ,1];  V[: ,1] = V[: ,2]            # Recycle array

while 1:                                 
    rate(20)                        # Delay plot, large = slow
    for i in range(1, 100):   
        V[i, 2] = 2.*V[i,1]-V[i,0]+R*(V[i+1,1]+V[i-1,1]-2*V[i,1])
        vibst.x[i] = 2.*i - 100.0                     # Scale x
        vibst.y[i] = V[i, 2]
    V[: ,0] = V[: ,1]; V[: ,1] = V[: ,2]          # Recycle array
