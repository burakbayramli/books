""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# SqBillardCM.py: Animated classical billiards on square table

from visual import *

dt = 0.01;   Xo = 0.;  Yo =  -56;  v = vector(13.,13.1)
r0 = r= vector(Xo,Yo); eps = 0.1;  Tmax = 5000; tp = 0

scene = display(width=500, height=500, range=120,\
                background=color.white, foreground=color.black)
table = curve(pos=([(-100,-100,0),(100,-100,0),\
                (100,100,0), (-100,100,0), (-100,-100,0)]))
ball = sphere(pos=(Xo,Yo,0),color=color.red, radius=3,make_trail=True)

for t in arange(0,Tmax,dt):
    rate(5000) 
    tp = tp + dt   
    r = r0 + v*tp
    if(r.x >= 100 or r.x<=-100):                # Right & left walls
       v = vector(-v.x,v.y,0)
       r0 = vector(r.x,r.y,0)
       tp = 0
    if(r.y >= 100 or r.y<=-100):                # Top & bottom walls
       v = vector(v.x,-v.y,0)
       r0 = vector(r.x,r.y,0)
       tp = 0
    ball.pos = r
            
    

