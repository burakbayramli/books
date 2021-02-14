""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# rk4Call.py 4th O Runge Kutta calling rk4Algor
#           for ODE y" = -100y-2y'+ 100 sin(3t)
		 
from visual.graph import *
from rk4Algor import rk4Algor

#   Initialization
Tstart = 0.
Tend = 10.
N = 100    # Number of steps                                      
ydumb = zeros((2), float);    y = zeros((2), float)
fvector = zeros((2), float);  k1 = zeros((2), float)
k2 = zeros((2), float);       k3 = zeros((2), float) 
k4 = zeros((2), float)
graph1 = gdisplay(x=0,y=0,width=400,height=400,title='RK4',xtitle='t',\
	ytitle='y[0]=Position',xmin=0,xmax=10,ymin=-2,ymax=3)
funct1 = gcurve(color = color.yellow)
graph2 = gdisplay(x=400,y=0,width=400,height=400,title='RK4',xtitle='t',\
	ytitle='y[1]=Velocity',xmin=0,xmax=10,ymin=-25,ymax=18)
funct2 = gcurve(color = color.red)
y[0] = 3.;   y[1] = -5. # Initial position and velocity
t = Tstart;       h = (Tend-Tstart)/N;

# Force function 
def f(t, y):                       
    fvector[0] = y[1]                                            
    fvector[1] = -100.*y[0]-2.*y[1] + 10.*sin(3.*t)             
    return fvector

while (t < Tend):                         # Time loop
    if ((t + h) > Tend):
        h = Tend - t                      # Last step
    y = rk4Algor(t, h, 2, y, f)
    t = t + h
    rate(30)
    funct1.plot(pos = (t, y[0]) )
    funct2.plot(pos = (t, y[1]) )                                    
