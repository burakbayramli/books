""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# ImagePlaneViz.py: E field lines for charge plus image wi Visual

from visual.graph import *
scene =  display(width=500, height=500,range=100,
	title="E of Charge Left of Plane (Red Image)")
plane = box(pos=(0,0,0),length=2,height=130,width=130,
	color=(0.9,0.9,0.9),opacity=0.5)
gridpts = points(size=4, color=color.cyan)
PlusCharge = sphere(radius=5,color=color.red, pos=(40,0,0))
NegCharge  = sphere(radius=5,color=color.green, pos=(-40,0,0))

def grid3d():
    for z in range(-60,80,20):
        for y in range (-60,80,20):
            for x in range(-60,80,20):
                gridpts.append(pos=(x,y,z))
def electricF():
    for y in range(-60,80,20):
       for z in range(-60,80,20):
          for x in range(-60,80,20):
             r = vector(x,y,z)                    # E vector here
             xx = vector(40,0,0)                     # q location
             d = vector(r-xx)                     # Vector q to r
             dm = vector(r+xx)                   # Vector q' to r
             dd = mag(d)                           # Mag vector d         
             ddp = mag(dm)                        # Mag vector dm
             if xx.mag !=0:              
                 E1 =  d/dd**3                       # E due to q
                 E2 = -dm/ddp**3                    # E due to -q
                 E =  E1 + E2                           # Total E
                 elecF = arrow(pos=r,color=color.orange)  
                 elecF.axis = 10*E/mag(E)      # 10 x unit vector
grid3d()
electricF()
