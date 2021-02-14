""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""
   
# LaplaceCylinder.py:  Laplace's eqtn in cyclinder, Matplotlib 3D plot 

from scipy import special; from mpl_toolkits.mplot3d import Axes3D
import numpy as np, matplotlib.pyplot as plt; from matplotlib import cm

a = 20.;  L = 20;  z = 18                      # Cylinder size, U at z                  
rhop = 12.0; phip = 7*np.pi/4.;  zp = 15             # Charge location
Nzeros = 80; Nzeros2 = int(Nzeros/2)

def potential(rho,phi):    
   suma = 0                  
   for m in range (-Nzeros2, Nzeros2+1):   
       xmn = special.jn_zeros(m,Nzeros)                  #  Jm zeros
       xmnr = xmn*rho/a;  xmnp = xmn*rhop/a                
       jm1 = special.jn(m,xmnr);  jm2 = special.jn(m,xmnp)    # Jm's
       sh = np.sinh(xmn*L/a); sh2 = np.sinh(xmn*zp/a);        # sinh
       sh3 = np.sinh(xmn*(L-z)/a)
       ex = np.cos(m*(phi-phip))                  # Re exp[im(f-f')]
       jmp = special.jn(m+1,xmn)                             # J_m+1
       for n in range(0,Nzeros):                   # Sums over zeros 
           num = ex*jm1[n]*jm2[n]*sh2[n]*sh3[n]
           den = xmn[n]*sh[n]*jmp[n]**2
           pot = num/den 
       poten = pot                           # Potential from one m
       suma = suma + poten                              # Sum all m
   return suma
                       
fig = plt.figure(figsize=(8,8))
ax = fig.add_subplot(111, projection='3d')   
rho = np.linspace(0,a,Nzeros);  phi = np.linspace(0,2*np.pi,Nzeros)    
R, P = np.meshgrid(rho, phi)                         # Polar coords
X, Y = R*np.cos(P), R*np.sin(P)                  # Cartesian coord
Z = potential(R,P)                                     # U(z,r,phi)
ax.plot_surface(X, Y, Z, rstride=1, cstride=1, cmap=cm.coolwarm)   
ax.set_xlabel('X');  ax.set_ylabel('Y');  ax.set_zlabel('U(x,y)')
plt.show()