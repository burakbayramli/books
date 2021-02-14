""" From  "COMPUTER PROBLEMS in PHYSICS"  by RH Landau & MJ Paez
    Copyright R Landau,   MJ Paez,    2017. 
    Please respect copyright & acknowledge our work."""



# K_0decay.py:  Probability of K^0 decay into two pions

from visual.graph import *

scene = gdisplay(width = 800,height = 700,background = color.white,\
    foreground = color.black, logy=True, xtitle = "Time", ytitle = "log Probability",title = "K_S -> 2 pi")
lin = gcurve(color = color.red)

tauS = 0.9e-10;  tauL = 5.2e-8; dm = 0.5333e10; eps = 2.269e-3  

for t in arange (0, 3e-9,0.1e-11):  
    a1 = exp(-t/tauS)                 
    a2 = eps**2*exp(-t/tauL)
    a3 = 2*eps*exp(-0.5*(1/tauS + 1/tauL)*t) * cos(dm*t)
    y =    a3 + a1 + a2                
    lin.plot(pos = (t*1e10,y))
    
    
