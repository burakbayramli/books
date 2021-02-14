""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# ImageSphereViz.py: E field lines for charge plus image wi Visual

from visual.graph import *
scene= display(width=500, height=500,range=100, 
	title="E of Charge in Sphere (Red Image)")
gridpts = points(size=4, color=color.cyan)

def grid3d():
    for z in range(-60,80,20):
        for y in range (-60,80,20):
            for x in range(-60,80,20):
                gridpts.append(pos = (x,y,z))                
grid3d()
xp = 60;  yp = 40;  zp = 0;  a = 30;  q = 1
xx = vector(xp, yp, zp)                        # Charge location
xxp = xx*a**2/(mag(xx))**2                      # Image location
qp = -q*a/mag(xx)                                 # Image charge
ball = sphere(pos=(0,0,0),radius=a, opacity=0.5)
poscharge = sphere(radius=5,color=color.red, pos=(xp,yp,zp))
negcharge = sphere(radius=5,color=color.blue, pos=xxp)

def electricF():
    for y in range(-60,80,20):
       for z in range(-60,80,20):
          for x in range(-60,80,20):
             r = vector(x,y,z)                        # E here
             d = vector(r-xx)                  # Vector q to r
             dm = vector(r-xxp)
             dd = mag(d)                         # Magnitude d         
             ddp = mag(dm)                      # Magnitude dm
             if xx.mag !=0:              
                 E1 =  d/dd**3                     # E due to q
                 E2 = -dm/ddp**3                   # E due to -q
                 E =  E1 + E2               
                 elecF = arrow(pos=r,color=color.orange) # E
                 elecF.axis = 10*E/mag(E)      # 10 x unit vector
electricF()
