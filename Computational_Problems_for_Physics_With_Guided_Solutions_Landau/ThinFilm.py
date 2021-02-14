""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# ThinFilm.py: Thin film interference by reflection (AJP 72,1248-1253)

from visual.graph import *

escene = display(width=500, height=500, range=400, background=color.white,\
                 foreground=color.black, title= 'Thin Film Interference')
Rcurve = curve(color=color.red)                           # Red intensities 
Gcurve = curve(color=color.green)                       # Green intensities
Bcurve = curve(color=color.blue)                      # Blue intensities
title  = label(pos=(-20,350,0), text='Intensity vs Thickness nA in nm',box=0)
waves  = label(pos=(-30,320,0), text='Red, Green, and Blue Intensities',box=0)
trans  = label(pos=(-280,300,0),text='Transmission',box=0)
refl   = label(pos=(210,300,0),text='Reflection',box=0)
lamR   = 572;   lamB = 430; lamG = 540; i = 0          # R,B, G wavelengths
film = curve(pos=[(-150,-250),(150,-250),(150,250),(-150,250),(-150,-250)])
Rc = [];  Gc = []; Bc = []                         # R,G,B intensity arrays
nA = arange(0,1250,10)       
delR =  2*pi*nA/lamR+pi; delG = 2*pi*nA/lamG+ pi;  delB =  2*pi*nA/lamB+pi 
intR = (cos(delR/2))**2; intG = (cos(delG/2))**2; intB = (cos(delB/2))**2 
xrp =  300*intR-150; xbp =  300*intB-150;  xgp =  300*intG-150  # Linear TFs  
ap =  -500*nA/1240 +250                                        # Film height
Rcurve.x = xrp;  Rcurve.y = ap;   Gcurve.x = xgp
Gcurve.y = ap;   Bcurve.x = xbp;  Bcurve.y = ap
Rc = Rc+[intR];  Gc = Gc+[intG];  Bc = Bc+[intB]                  # Fill I's
Rt = [];  Gt = [];      Bt = []
DelRt =     4*pi*nA/lamR;     DelGt =  4*pi*nA/lamG;   DelBt =  4*pi*nA/lamB
IntRt =     cos(DelRt/2)**2;  IntGt = cos(DelGt/2)**2; IntBt = cos(DelBt/2)**2     
xRpt =      300*intR-150;     xBpt =  300*intB-150;    xGpt =  300*intG-150 
Rt =        Rt + [intR];      Gt = Gt + [intG];        Bt = Bt + [intB]
ap =       -500*nA/1240 +250                                  #  Film height

for nA in range (0,125):    
    col = (intR[nA],intG[nA],intB[nA])                      # RGB reflection
    reflesc = -500*nA/125+250                      
    box(pos=(205,reflesc,0),width=0.1,height=10,length=50,color=col)
    colt = (IntRt[nA],IntGt[nA],IntBt[nA])          # Colors by transmission
    box(pos=(-270,reflesc,0),width=0.1,height=10, length=50, color=colt) 
    if (nA%20==0):                                # Labels for vertical axis
        prof = nA*10
        escal = -500*nA/125+250
        print (escal)
        depth = label(pos=(-200,escal,0),text='%4d'%prof,box=0)
