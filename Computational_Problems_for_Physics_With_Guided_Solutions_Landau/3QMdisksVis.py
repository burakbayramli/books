""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# 3QMdisksVis.py: Wavepacket scattering from 3 disks wi Visual 

from visual import *

R = 24;  N = 101;    dx = 0.1;    k0  = 20.;  x1 = 51 #(90.*sqrt3/2-30)
k1 = 0.; dt = 0.002; dx2 = dx*dx; fc = dt/dx2
Xo = -50;   Yo=  24                            
V = zeros((N,N),float); RePsi = zeros((N,N),float); ImPsi = zeros((N,N),float)

def Pot3Disk():                        # Potential three disk
    Pot1Disk(-30,45);  Pot1Disk(-30,-45);  Pot1Disk(x1,0)
    
def Pot1Disk(xa,ya):                   # Potential single disk
     for y in range (ya-R, ya+R+1):    
        for x in range(xa-R, xa+R+1):  
            if sqrt((x-xa)**2+(y-ya)**2) <= R:   
                i = int(50./100.*y+50)
                j = int(50./100.*x+50) 
                V[i,j] = 5.                  # A very high pot

def Psi_0(Xo, Yo):                         # Psi_0 wave packet     
    for i in arange(0,N):
        y = 200./100.*i - 100          
        for j in arange(0, N):
             x = 200./100.*j - 100      
             Gaussian = exp(-0.01*(x-Xo)**2- 0.01*(y-Yo)**2)
             RePsi[i,j] = Gaussian*cos(k0*x+k1*y)
             ImPsi[i,j] = Gaussian*sin(k0*x+k1*y)
             
def PlotPsi_0():
    for i in range(1,N-1):
        yp = 200.*i/N-100
        for j in range(1,N-1): 
            if V[i,j] !=0:  RePsi[i,j] = ImPsi[i,j] = 0
            Rho = 40*(RePsi[i,j ]**2 +ImPsi[i, j ]**2)
            if Rho>.01:                  # To avoid long lines
                xx = 200.*j/N-100.    
                xm1 = 200.*(j-1)/N-100.
                Rhom1 = 40* (RePsi[i,j-1]**2 +ImPsi[i, j-1 ]**2)
                yy = yp                 # Plot segment of 40*Psi
                curve(pos = [(xm1,Rhom1,yy),(xx,Rho,yy)],color=color.red)
                
scene = display(width=500, height=500,range=120, background=color.white,\
                 foreground=color.black)
table = curve(pos=([(-100,0,-100),(100,0,-100),(100,0,100),(-100,0,100),\
                    (-100,0,-100)]))
circ1 = ring(pos=(-30,0,45), radius=R,axis=(0,1,0),color=color.blue)
circ2 = ring(pos=(-30,0,-45),radius=R,axis=(0,1,0),color=color.blue)
circ3 = ring(pos=(x1,0,0),radius=R,axis=(0,1,0),color=color.blue)
scene.forward = (0,-1,-1)               # Scene's angle of vision
Pot3Disk()                       
Psi_0(Xo,Yo)

PlotPsi_0()
for t in range(0,150):                         # Plot every 10 t's
    if t%10==0: print('time =',t)  
    ImPsi[1:-1,1:-1] =  ImPsi[1:-1,1:-1] + fc*(RePsi[2: ,1:-1 ] + RePsi[:-2 ,1:-1]\
                            -4*RePsi[1:-1,1:-1] + RePsi[1:-1,2: ] + RePsi[1:-1, :-2])\
                            + V[1:-1,1:-1]*dt*RePsi[1:-1,1:-1]
    RePsi[1:-1, 1:-1] = RePsi[1:-1,1:-1] - fc*(ImPsi[2: ,1:-1] + ImPsi[ :-2,1:-1]\
                        -4*ImPsi[1:-1,1:-1] + ImPsi[1:-1,2: ] + ImPsi[1:-1, :-2])\
                        + V[1:-1,1:-1]*dt*ImPsi[1:-1,1:-1]
    for i in range(1, N-1):
        yp = 200.*i/N-100
        for j in range(1,N-1): 
             if V[i,j] !=0:   RePsi[i,j] = ImPsi[i,j] = 0
             Rho = 40*(RePsi[i,j ]**2 + ImPsi[i, j]**2)
             xx = 200.*j/N-100.   
             xm1 = 200.*(j-1)/N-100.
             Rhom1 = 40*(RePsi[i,j-1 ]**2 + ImPsi[i, j-1 ]**2)
             yy = yp
             if  Rho > 0.1:
                 if t%10==0:    
                  curve(pos=[(xm1,Rhom1,yy), (xx,Rho,yy)], color=color.red)
print("finito") 
    

