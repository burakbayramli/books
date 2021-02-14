""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# QuarterPlate.py  FDTD solution of Maxwell's equations in 1-D

from visual import *

xmax = 401; ymax = 100;  zmax = 100; ts = 2; beta = 0.01

Ex = zeros((xmax,ts),float); Ey = zeros((xmax,ts),float)
Hx = zeros((xmax,ts),float); Hxx = zeros((xmax,ts),float)
Hy = zeros((xmax,ts),float); Hyy = zeros((xmax,ts),float)
Exx = zeros((xmax,ts),float);  Eyy = zeros((xmax,ts),float)     

scene = display(x=0,y=0,width= 800, height= 500, title= 'Ey: 
	cyan, Ex:yellow. periodic BC', forward=(-0.8,-0.3,-0.7))
Exfield = curve(x=list(range(0,xmax)),color= color.yellow,
	radius=1.5,display=scene)
Eyfield = curve(x=list(range(0,xmax)),color=color.cyan,
	radius=1.5,display=scene)
vplane= curve(pos=[(-xmax,ymax),(xmax,ymax),(xmax,-ymax),(-xmax,-ymax),
                   (-xmax,ymax)],color=color.cyan)
zaxis = curve(pos=[(-xmax,0),(xmax,0)],color=color.magenta)
hplane = curve(pos=[(-xmax,0,zmax),(xmax,0,zmax),(xmax,0,-zmax),
		(-xmax,0,-zmax),(-xmax,0,zmax)],color=color.magenta)
ball1 = sphere(pos=(xmax+30,0,0), color=color.black, radius = 2) 
ba2 = sphere(pos=(xmax-200,0),color=color.cyan,radius=3)
plate = box(pos=(-100,0,0),height=2*zmax,width=2*ymax,
	length=0.5*xmax, color=(1.0,0.6,0.0),opacity=0.4)
Exlabel1 = label( text = 'Ey', pos = (-xmax-10, 50), box = 0 )
Exlabel2 = label( text = 'Ey', pos = (xmax+10, 50), box = 0 )
Eylabel = label( text = 'Ex', pos = (-xmax-10, 0,50), box = 0 )
zlabel  =  label( text  =  'Z', pos  =  (xmax+10, 0), box  =  0 )
polfield = arrow(display = scene)
polfield2 = arrow(display = scene)
ti = 0      

def  InitField():
    kar = arange(xmax)
    phx = 0.5*pi
    Hyy[:xmax,0] = 0.1*cos(-2*pi*kar/100)    
    Exx[:xmax,0] = 0.1*cos(-2*pi*kar/100)    
    Eyy[:xmax,0] = 0.1*cos(-2*pi*kar/100)   
    Hxx[:xmax,0] = 0.1*cos(-2*pi*kar/100)   
    Ey[:xmax,0]  = 0.1*cos(-2*pi*kar/100)
    Hx[:xmax,0]  = 0.1*cos(-2*pi*kar/100)
    
def  InitExHy():
    k  =  arange(101) 
    Ex[:101,0] = 0.1*cos(-2*pi*k/100)
    Hy[:101,0] = 0.1*cos(-2*pi*k/100)
    kk = arange(101,202)         # Inside plate, delay lambda/4
    Ex[101:202,0]  =  0.1*cos(-2*pi*kk/100.0-0.005*pi*(kk-101))  
    Hy[101:202,0]  =  0.1*cos(-2*pi*kk/100.0-0.005*pi*(kk-101))
    kkk = arange(202,xmax)        # After plate, phase diff pi/2
    Ex[202:xmax,0]  =  0.1*cos(-2*pi*kkk/100-0.5*pi)
    Hy[202:xmax,0]  =  0.1*cos(-2*pi*kkk/100-0.5*pi)

def PlotFields(ti):                       # screen coordinates
    k  =  arange(xmax)
    Exfield.x  =  2*k-xmax             # world to screen coords.
    Exfield.y  =  800*Ey[k,ti]
    Eyfield.x  =  2*k-xmax             # world to screen coords.
    Eyfield.z  =  800*Ex[k,ti]   
        
InitField()   
InitExHy()
PlotFields(ti)                             
j = 0
end = 0
while end < 5:
    rate(150)       
    Exx[1:xmax-1,1] = Exx[1:xmax-1,0] + beta*(Hyy[0:xmax-2,0] - Hyy[2:xmax,0])
    Eyy[1:xmax-1,1] = Eyy[1:xmax-1,0] + beta*(Hxx[0:xmax-2,0] - Hxx[2:xmax,0])
    Hyy[1:xmax-1,1] = Hyy[1:xmax-1,0] + beta*(Exx[0:xmax-2,0] - Exx[2:xmax,0])
    Hxx[1:xmax-1,1] = Hxx[1:xmax-1,0] + beta*(Eyy[0:xmax-2,0] - Eyy[2:xmax,0])
    Ex[1:xmax-1,1] =  Ex[1:xmax-1,0]  + beta*(Hy[0:xmax-2,0]  - Hy[2:xmax,0])   
    Ey[1:xmax-1,1] =  Ey[1:xmax-1,0]  + beta*(Hxx[0:xmax-2,0] - Hxx[2:xmax,0])
    Hy[1:xmax-1,1] =  Hy[1:xmax-1,0]  + beta*(Ex[0:xmax-2,0]  - Ex[2:xmax,0])       
    Hx[1:xmax-1,1] =  Hx[1:xmax-1,0]  + beta*(Eyy[0:xmax-2,0] - Eyy[2:xmax,0])
    polfield.pos = (-280,0,0)
    polfield.axis = (0,700*Exx[60,1],700*Eyy[60,1])
    polfield2.pos = (380,0,0)
    polfield2.axis = (0,700*Ex[360,1],-700*Ey[360,1])   
    Exx[0,1] = Exx[0,0] + beta*(Hyy[xmax-2,0] - Hyy[1,0])  # Periodic BC
    Eyy[0,1]=  Eyy[0,0] + beta*(Hxx[xmax-2,0] - Hxx[1,0]) 
    Hyy[0,1] = Hyy[0,0] + beta*(Exx[xmax-2,0] - Exx[1,0])   # Periodic BC
    Hxx[0,1] = Hxx[0,0] + beta*(Eyy[xmax-2,0] - Eyy[1,0]) 
    Hyy[xmax-1,1] = Hyy[xmax-1,0] + beta*(Exx[xmax-2,0] - Exx[1,0])  
    Hxx[xmax-1,1] = Hxx[xmax-1,0] + beta*(Eyy[xmax-2,0] - Eyy[1,0]) 
    Exx[xmax-1,1] = Exx[xmax-1,0] + beta*(Hyy[xmax-2,0] - Hyy[1,0])  
    Eyy[xmax-1,1] = Eyy[xmax-1,0] + beta*(Hxx[xmax-2,0] - Hxx[1,0])
    Ex[0,1] = Exx[0,0] + beta*(Hyy[xmax-2,0] - Hyy[1,0]) # Periodic BC
    Ey[0,1] = Eyy[0,0] + beta*(Hxx[xmax-2,0] - Hxx[1,0]) 
    Hy[0,1] = Hyy[0,0] + beta*(Exx[xmax-2,0] - Exx[1,0]) # Periodic BC
    Hx[0,1] = Hxx[0,0] + beta*(Eyy[xmax-2,0] - Eyy[1,0])    
    Hy[xmax-1,1] = Hy[xmax-1,0] + beta*(Ex[xmax-2,0] - Ex[xmax-100,0])
    Hx[xmax-1,1] = Hx[xmax-1,0] + beta*(Ey[xmax-2,0] - Ey[1,0]) 
    Ex[xmax-1,1] = Ex[xmax-1,0] + beta*(Hy[xmax-2,0] - Hy[xmax-100,0])
    Ey[xmax-1,1] = Ey[xmax-1,0] + beta*(Hxx[xmax-2,0] - Hxx[1,0])
    PlotFields(ti)                      
    k = arange(101,202)
    Ex[101:202,1] = 0.1*cos(-2*pi*k/100-0.005*pi*(k-101)+2*pi*j/4996.004)
    Hy[101:202,1] = 0.1*cos(-2*pi*k/100-0.005*pi*(k-101)+2*pi*j/4996.004)
    Exx[:xmax,0]  = Exx[:xmax,1]   
    Eyy[:xmax,0]  = Eyy[:xmax,1]
    Hyy[:xmax,0] =  Hyy[:xmax,1]   
    Hxx[:xmax,0] =  Hxx[:xmax,1]  
    Ex[:xmax,0] =    Ex[:xmax,1]   
    Ey[:xmax,0] =    Ey[:xmax,1]
    Hx[:xmax,0] =    Hx[:xmax,1]
    Hy[:xmax,0]=     Hy[:xmax,1]
    if j%4996 == 0:
        j = 0
        end += 1
    j = j+1
