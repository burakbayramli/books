from sympy import Matrix
import sympy as sp
from matplotlib import pyplot as plt
F = Matrix([[1.2,0.4],[0.4,1.2]])
coordinates=[[0,0],[0,1],[1,1],[1,0],[0,0]]
newcoordinates=[F*Matrix(coordinates[i]) for i in range(len(coordinates))]
bx =[Matrix(coordinates)[:,0], Matrix(coordinates)[:,1]]
bx2 = [[newcoordinates[i][0] for i in range(5)],[newcoordinates[i][1] for i in range(5)]]
Sigma = Matrix([[5,2],[2,3]])
N1 = Matrix([1,0])
N2 = Matrix([0,1])
Farea = F.det()*F.T.inv()
an1 = Farea*N1
an2 = Farea*N2
tn1 = Sigma.T*an1
tn2 = Sigma.T*an2
FirstPiola = Sigma.T*F.T.inv()*F.det()
SecondPiola = F.inv()*FirstPiola
TN1 = FirstPiola*N1
TN2 = FirstPiola*N2
STN1 = SecondPiola*N1
STN2 = SecondPiola*N2
Pt11 = F*N1+1/2*F*N2
Pt12 = Pt11+an1
Pt21 = 1/2*F*N1+F*N2
Pt22 = Pt21+an2
ar1 = Matrix([Pt11,Pt12]).reshape(2,2)
ar2 = Matrix([[Pt21],[Pt22]]).reshape(2,2)
A1dotted = Matrix([Pt11,Pt11+Matrix([1,0])]).reshape(2,2)
A2dotted = Matrix([Pt21,Pt21+Matrix([0,1])]).reshape(2,2)
A1 = Matrix([[1,0.5],[2,0.5]])
A2 = Matrix([[0.5,1],[0.5,2]])
artn1 = Matrix([Pt11,Pt11+tn1/10]).reshape(2,2)
artn2 = Matrix([Pt21,Pt21+tn2/10]).reshape(2,2)
arTN1 = Matrix([Matrix([1,0.5]),Matrix([1,0.5])+TN1/10]).reshape(2,2)
arTN2 = Matrix([Matrix([0.5,1]),Matrix([0.5,1])+TN2/10]).reshape(2,2)
arSTN1 = Matrix([Matrix([1,0.5]),Matrix([1,0.5])+STN1/10]).reshape(2,2)
arSTN2 = Matrix([Matrix([0.5,1]),Matrix([0.5,1])+STN2/10]).reshape(2,2)
fig, ax = plt.subplots(2, figsize=(6,8))
def plotArrow(p,M,t):
    x,y,dx,dy = M[0,0],M[0,1],float(M[1,0]-M[0,0]),float(M[1,1]-M[0,1])
    p.arrow(x,y,dx,dy,length_includes_head = True,head_width=.05, head_length=.05,alpha=t)
plotArrow(ax[0],A1,1)
plotArrow(ax[0],A2,1)
plotArrow(ax[0],arTN1,1)
plotArrow(ax[0],arTN2,1)
plotArrow(ax[0],arSTN1,.5)
plotArrow(ax[0],arSTN2,.5)
#rectangle coordinates
ax[0].plot(bx[0][:],bx[1][:])
ax[1].plot(bx2[0][:],bx2[1][:])
plotArrow(ax[1],ar1,1)
plotArrow(ax[1],ar2,1)
plotArrow(ax[1],artn1,1)
plotArrow(ax[1],artn2,1)
plotArrow(ax[1],A1dotted,.5)
plotArrow(ax[1],A2dotted,.5)
for i in ax:
    i.grid(True, which='both')
    i.axhline(y = 0, color = 'k',alpha = 0.5)
    i.axvline(x = 0, color = 'k',alpha = 0.5)
    i.set_xlabel("x")
    i.set_ylabel("y")
