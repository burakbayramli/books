""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# ImagePlaneMat.py: E for charge left of plane plus image
 
import numpy as np,  matplotlib.pyplot as plt
from matplotlib.patches import Circle

Nx = 50;                    Ny = 50                # x,y 50 grid
x = np.linspace(-5,5,Nx);   y = np.linspace(-5,5,Ny)
X,Y = np.meshgrid(x,y)                    # Transform coordinates
Ex = np.zeros((Nx,Ny));     Ey = np.zeros((Nx,Ny))   # Ex,Ey(x,y)

def E(xx,x,y):                        # E  due to charge q at xx
    r = np.sqrt(x**2+y**2)                             # Distance
    dm = (x-xx)                                # Position q to xx
    d1 = np.sqrt((dm**2+y**2))                  # Position q to x 
    dp = (x+xx)                                   # x component q
    d2 = np.sqrt((dp**2+y**2))              # Distance -q to (x,y)
    Ex =  dm/d1**3-dp/d2**3      
    Ey = y/d1**3 -y/d2**3        
    return Ex,Ey               
    
Ex,Ey = E(2.5,X,Y)
fig = plt.figure()
ax = fig.add_subplot(111)
circle1 = plt.Circle((2.5, 0),0.2, color='r')
circle2 = plt.Circle((-2.5, 0),0.2, color='b')
ax.add_artist(circle1)
ax.add_artist(circle2)
ax.streamplot(x,y,Ex,Ey)
ax.set_aspect('equal')
ax.set_title('E Field Due to Charge Left of Plane (Red Image)')
ax.set_xlabel('x')
ax.set_ylabel('y')
l = plt.axvline(x=0, linewidth=2, color='g')
plt.show()