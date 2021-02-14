""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# TwoForces.py Forces on two moving strings

from visual.graph import *

posy=100;  Lcord=250  # basic height, cord length
Hweight=50;  W = 10   # cylinder height, weight 

scene=display(heigth=600,width=600,range=380)
alt=curve(pos=[(-300,posy,0),(300,posy,0)])
divi=curve(pos=[(0,-150,0),(0,posy,0)])
kilogr=cylinder(pos=(0,posy-Lcord,0),radius=20,axis=(0,-Hweight,0),
                color=color.red) # kg as a cylinder
cord1=cylinder(pos=(0,posy,0),axis=(0,-Lcord,0),color=color.yellow,
               radius=2)
cord2=cylinder(pos=(0,posy,0),axis=(0,-Lcord,0),color=color.yellow,
               radius=2)
arrow1=arrow(pos=(0,posy,0), color=color.orange) # Tension cord 1
arrow2=arrow(pos=(0,posy,0), color=color.orange) # Tension cord 2
magF=W/2.0          # initial force of each student
v=2.0               # (m/s) velocity of each student
x1=0.0              # initial position student 1
anglabel=label(pos=(0,240,0), text='angle (deg)',box=0)
angultext=label(pos=(20,210,0),box=0)
Flabel1=label(pos=(200,240,0), text='Force',box=0)
Ftext1=label(pos=(200,210,0),box=0)
Flabel2=label(pos=(-200,240,0), text='Force',box=0)
Ftext2=label(pos=(-200,210,0),box=0)
local_light(pos=(-10,0,20), color=color.yellow)   # light

for t in arange(0.,100.0,0.2):  
    rate(50)                  # slow motion
    x1=v*t                    # 1 to right, 2 to left
    theta=asin(x1/Lcord)      # angle cord
    poscil=posy-Lcord*cos(theta)  # cylinder height
    kilogr.pos=(0,poscil,0)     # y-position kilogram
    magF=W/(2.*cos(theta))      # Cord tension
    angle=180.*theta/pi     
    cord1.pos=(x1,posy,0)       # position cord end 
    cord1.axis=(-Lcord*sin(theta),-Lcord*cos(theta),0)
    cord2.pos=(-x1,posy,0)            # position end cord
    cord2.axis=(Lcord*sin(theta),-Lcord*cos(theta),0)
    arrow1.pos=cord1.pos            # axis arrow 
    arrow1.axis=(8*magF*sin(theta),8*magF*cos(theta),0) 
    arrow2.pos=cord2.pos
    arrow2.axis=(-8*magF*sin(theta),8*magF*cos(theta),0) 
    angultext.text='%4.2f'%angle
    force=magF
    Ftext1.text='%8.2f'%force     # Tension
    Ftext2.text='%8.2f'%force
