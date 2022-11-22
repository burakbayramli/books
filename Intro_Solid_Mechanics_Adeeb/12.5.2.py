from sympy import Matrix,simplify,diff,integrate,lambdify
import sympy as sp
import matplotlib.pyplot as plt
from mpmath import quad,mp
xi,eta = sp.symbols("xi eta")
coordinates = Matrix([[0,0],[3,1],[35/10,32/10],[5/10,3]])
# coordinates = Matrix([[0,0],[1,0],[1,1],[0,1]])
# plot coordinates
a = Matrix([[0,0],[3,1],[35/10,32/10],[5/10,3],[0,0]])
# a = Matrix([[0,0],[1,0],[1,1],[0,1],[0,0]])
t = 1
Ee = 1
Nu = 3/10
Cc = Ee/(1+Nu)*Matrix([[1/(1 - Nu), Nu/(1 - Nu), 0], 
                       [Nu/(1 - Nu), 1/(1 - Nu), 0], 
                       [0, 0, 1/2]])
fig = plt.figure()
ax = fig.add_subplot(111)
cp = ax.plot(a[:,0], a[:,1])
ax.grid(True, which='both')
plt.xlabel("x")
plt.ylabel("y")
plt.title("Shape")
ax.axhline(y = 0, color = 'k')
ax.axvline(x = 0, color = 'k')
Shapefun = Matrix([[(1-xi)*(1-eta)],[(1+xi)*(1-eta)],
                   [(1+xi)*(1+eta)],[(1-xi)*(1+eta)]])/4
display("Mapping Functions")
Nn = Matrix([[0 for x in range(8)] for y in range(2)])
for i in range(4):
    Nn[0,2*i] = Nn[1,2*i+1] = Shapefun[i]
x = simplify(sum([Shapefun[i]*coordinates[i,0] for i in range(4)]))
display("x =",x)
y = simplify(sum([Shapefun[i]*coordinates[i,1] for i in range(4)]))
display("y =",y)
J = Matrix([[diff(x,xi),diff(x,eta)],[diff(y,xi),diff(y,eta)]])
display("Gradient of the Map")
display("J =",J)
JinvT = J.T.inv()
display("J^-1 =",JinvT)
mat1 = Matrix([[1,0,0,0],[0,0,0,1],[0,1,1,0]])
mat2 = Matrix([[JinvT[0,0],JinvT[0,1],0,0], 
               [JinvT[1,0],JinvT[1,1],0,0], 
               [0,0,JinvT[0,0],JinvT[0,1]], 
               [0,0,JinvT[1,0],JinvT[1,1]]])
mat3 = Matrix([[0 for x in range(8)] for y in range(4)])
for i in range(4):
    mat3[0,2*i] = mat3[2,2*i+1] = diff(Shapefun[i],xi)
    mat3[1,2*i] = mat3[3,2*i+1] = diff(Shapefun[i],eta)
B = simplify(mat1*mat2*mat3)
display("B =",B)
kbeforeintegration = simplify(B.T*Cc*B)
display("Stiffness Matrix")
K1 = Matrix([[0 for x in range(8)] for y in range(8)])
for i in range(8):
    for j in range(8):
        integrand = kbeforeintegration[i,j]*J.det()
        intlam = lambdify((eta,xi),integrand)
        mp.dps = 3
        K1[i,j] = quad(intlam,[-1,1],[-1,1])
display("K =",K1)
xiset = Matrix([[-1/sp.sqrt(3),1/sp.sqrt(3)]])
etaset = Matrix([[-1/sp.sqrt(3),1/sp.sqrt(3)]])
kfull = t*Matrix([[simplify(sum([sum([(kbeforeintegration[i,j]*J.det()).subs({xi:k,eta:l}) for k in xiset]) for l in etaset]))for i in range(8)]for j in range(8)])
display("k_{Full Integration} =",kfull)
xiset = Matrix([[0]])
etaset = Matrix([[0]])
kreduced = 4*t*Matrix([[simplify(sum([sum([(kbeforeintegration[i,j]*J.det()).subs({xi:k,eta:l}) for k in xiset]) for l in etaset]))for i in range(8)]for j in range(8)])
display("k_{Reduced Integration}=",kreduced)
display("Body Forces")
rb = Matrix([0,-1])
display("\u03C1_b =",rb)
fbeforeintegration = t*Nn.T*rb*J.det()
f1 = Matrix([integrate(fbeforeintegration[i],(xi,-1,1),(eta,-1,1))for i in range(8)])
display("Body Force Vetor")
display("f_{Exact Integration} =",f1)
xiset = Matrix([[-1/sp.sqrt(3),1/sp.sqrt(3)]])
etaset = Matrix([[-1/sp.sqrt(3),1/sp.sqrt(3)]])
ffull = Matrix([simplify(sum([sum([(fbeforeintegration[i]).subs({xi:l,eta:k}) for k in etaset]) for l in xiset]))for i in range(8)])
display("f_{Full Integration} =",ffull)
xiset = Matrix([[0]])
etaset = Matrix([[0]])
freduced = 4*Matrix([simplify(sum([sum([(fbeforeintegration[i]).subs({xi:l,eta:k}) for k in etaset]) for l in xiset]))for i in range(8)])
display("f_{Reduced Integration}=",freduced)
display("Traction Vectors")
dldeta = sp.sqrt(J[0,1]**2 + J[1,1]**2).subs({xi:1})
tn = Matrix([-1,0])
display("t_n =",tn)
display("Spatial Coordinate System =",dldeta)
fbeforeintegration = t*Nn.T*tn*dldeta
f1 = Matrix([(integrate(fbeforeintegration[i],(eta,-1,1))).subs({xi:1})for i in range(8)])
display("Nodal Forces due to Traction Forces")
display("f_{Exact Integration}",f1)
xiset = Matrix([[-1/sp.sqrt(3),1/sp.sqrt(3)]])
etaset = Matrix([[-1/sp.sqrt(3),1/sp.sqrt(3)]])
ffull = Matrix([simplify(sum([(fbeforeintegration[i]).subs({xi:1,eta:k}) for k in etaset]))for i in range(8)])
display("f_{Full Integration} =",ffull)
xiset = Matrix([[0]])
etaset = Matrix([[0]])
freduced = 2*Matrix([simplify(sum([(fbeforeintegration[i]).subs({xi:1,eta:k}) for k in etaset]))for i in range(8)])
display("f_{Reduced Integration}",freduced)
