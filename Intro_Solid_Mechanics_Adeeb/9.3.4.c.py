import sympy as sp
from sympy import Matrix, diff, simplify, integrate, lambdify
import numpy as np
X1,X2,X3 = sp.symbols("X_1 X_2 X_3")
import matplotlib.pyplot as plt
X = Matrix([X1,X2,X3])
x1 = X1+0.001*X2+0.0002*X1**2
x2 = 1.001*X2-0.0002*X2**2
x3 = X3
x = Matrix([x1,x2,x3])
u = x-X
display("u =",u)
gradu = Matrix([[diff(i,j) for j in X] for i in u])
display("\u2207u =",gradu)
esmall = 1/2*(gradu+gradu.T)
display("\u03B5 =",esmall)
strainvector = Matrix([esmall[0,0], esmall[1,1], esmall[2,2], 
               2*esmall[0,1], 2*esmall[0,2], 2*esmall[1,2]])
display("Strain vector =",strainvector)
Ee = 210000
Nu = 0.3
G = Ee/2/(1+Nu)
Cc = Matrix([[1/Ee,-Nu/Ee,-Nu/Ee,0,0,0], 
      [-Nu/Ee,1/Ee,-Nu/Ee,0,0,0],
      [-Nu/Ee,-Nu/Ee,1/Ee,0,0,0],
      [0,0,0,1/G,0,0],
      [0,0,0,0,1/G,0],
      [0,0,0,0,0,1/G]])
Dd = Cc.inv()
stressvector = Dd*strainvector
display("Stress vector =",stressvector)
S = Matrix([[stressvector[0], stressvector[3], stressvector[4]],
            [stressvector[3], stressvector[1], stressvector[5]],
            [stressvector[4], stressvector[5], stressvector[2]]])
display("Stress Matrix =",S)
StrainEnergy = simplify(sum([S[i]*esmall[i]/2 for i in range(9)]))
display("Strain energy (U) =",StrainEnergy)
TotalEnergy = integrate(StrainEnergy,(X1,0,2),(X2,0,1),(X3,0,0.01))
display("Total Energy =",TotalEnergy)
Udeviatoric = simplify(1/12/G*((S[0,0]-S[1,1])**2 + (S[2,2]- 
                  S[1,1])**2 + (S[0,0] - S[2,2])**2 + 
                  6*(S[0,1]**2 + S[0,2]**2 + S[1,2]**2)))
display("U_deviatoric =",Udeviatoric)
Uvolumetric = simplify((1-2*Nu)/6/Ee*(S[0,0]+S[1,1]+S[2,2])**2)
display("U_volumetric =",Uvolumetric)
display("Check for energy total:",simplify(StrainEnergy-Udeviatoric-Uvolumetric))
display("Contour plots:")
def plot(f, limits, title):
    x1, xn, y1, yn = limits
    dx, dy = 10/100*(xn-x1),10/100*(yn-y1)
    xrange = np.arange(x1,xn,dx)
    yrange = np.arange(y1,yn,dy)
    X, Y = np.meshgrid(xrange, yrange)
    lx, ly = len(xrange), len(yrange)
    F = lambdify((X1,X2),f)
    Z = F(X,Y)*np.ones(lx*ly).reshape(lx, ly)
    fig = plt.figure(figsize = (5,2))
    ax = fig.add_subplot(111)
    cp = ax.contourf(X,Y,Z)
    fig.colorbar(cp)
    plt.title(title)
plot(StrainEnergy, [0,2,0,1],"StrainEnergy MN m/m^3")
plot(Udeviatoric, [0,2,0,1],"Deviatoric StrainEnergy MN m/m^3")
plot(Uvolumetric, [0,2,0,1],"Volumetric StrainEnergy MN m/m^3")
