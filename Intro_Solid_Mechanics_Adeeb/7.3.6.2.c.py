import sympy as sp
from sympy import *
l1,l2,Ee,nu = sp.symbols("lambda_1 lambda_2 epsilon nu")
from matplotlib import pyplot as plt
from scipy.optimize import root
# plot
x = [0]*100
RelC1 = [0]*100
RelP1 = [0]*100
RelC2 = [0]*100
RelP2 = [0]*100
RelC3 = [0]*100
RelP3 = [0]*100
for i in range(100):
    x[i] = 0.4 + (i+1)/50
F = Matrix([[l1,0,0],[0,l2,0],[0,0,l2]])
print("F =",F)
Fnt = F.inv().T
print("F^(-T) =",Fnt)
J = det(F)
U2 = F.T*F
U4 = U2*U2
I1 = sum(U2)
I2 = 1/2*(I1**2 - sum(U4))
def plot(Nu):
    print("Set Nu =",Nu)
    # linear elastic material
    E = F -eye(3)
    print("Linear Elastic Material")
    print("\u03B5 =", E)
    E11, E22, E33 = E[0,0], E[1,1], E[2,2]
    s11 = Ee/(1-2*nu)/(1+nu)*(E11*(1-nu)+E22*nu+E33*nu)
    s22 = Ee/(1-2*nu)/(1+nu)*(E22*(1-nu)+E11*nu+E33*nu)
    print("For \u03C3_22 = 0: ",Eq(s22,0))
    a1 = solve(s22, [l2])[0]
    print("\u03BB2 =", a1)
    sigma = Matrix([[s11,0,0],[0,s22,0],[0,0,s22]])
    sigma = simplify(sigma.subs({l2:a1}))
    P = (J*sigma*Fnt).subs({l2:a1})
    print("\u03C3 =",sigma)
    print("P =",P)
    sigma = sigma.subs({Ee:20000, nu:Nu})
    P = P.subs({Ee:20000, nu:0.2})
    for i in range(100):
        RelC1[i] = sigma[0,0].subs({l1:x[i]})
        RelP1[i] = P[0,0].subs({l1:x[i]})
    # Compressible Neo-Hookean materials
    print("Compressible Neo-Hookean materials")
    G = Ee/2/(1+nu)
    G = G.subs({nu:Nu,Ee:20000})
    Kk = Ee/3/(1-2*nu)
    Kk = Kk.subs({nu:Nu,Ee:20000})
    C10 = G/2
    print("C10 =",C10)
    Dd = 2/Kk
    print("Dd =",Dd)
    sigmaNH = simplify(C10*J**(-5/3)*(2*F*F.T-2*I1/3*eye(3))
                      +2/Dd*(J-1)*eye(3))
    PiolaNH = simplify(J*sigmaNH*Fnt)
    print("\u03C3 =",sigmaNH)
    print("P =",PiolaNH)
    SolsigmaNH = sigmaNH[1,1].subs({l1:x[0]})
    for i in range(100):
        SolsigmaNH = sigmaNH[1,1].subs({l1:x[i]})
        SolsigmaNH = lambdify(l2,SolsigmaNH)
        sol = root(SolsigmaNH,[2])
        RelC2[i] = sigmaNH[0,0].subs({l1:x[i], l2:sol.x[0]})
        RelP2[i] = PiolaNH[0,0].subs({l1:x[i], l2:sol.x[0]})
    # Compressible Mooney-Rivlin materials
    print("Compressible Mooney-Rivlin materials")
    C10 = G/4
    C01 = G/4
    print("C10 =",C10)
    print("C01 =",C01)
    sigmaMN = simplify(C10*J**(-5/3)*(2*F*F.T-2*I1/3*eye(3))
                      +C01*J**(-7/3)*(2*I1*F*F.T-2*F*F.T*F*F.T-4*I2/3*eye(3))
                      +2/Dd*(J-1)*eye(3))
    PiolaMN = simplify(J*sigmaMN*Fnt)
    print("\u03C3 =",sigmaMN)
    print("P =",PiolaMN)
    for i in range(100):
        SolsigmaNH = sigmaNH[1,1].subs({l1:x[i]})
        SolsigmaNH = lambdify(l2,SolsigmaNH)
        sol = root(SolsigmaNH,[2])
        RelC3[i] = sigmaMN[0,0].subs({l1:x[i], l2:sol.x[0]})
        RelP3[i] = PiolaMN[0,0].subs({l1:x[i], l2:sol.x[0]})
    # plots
    xL = [i -1 for i in x]
    fig, ax = plt.subplots()
    title = "\u03BC = " + str(Nu)
    plt.title(title)
    plt.xlabel("\u03BB - 1")
    plt.ylabel("P11")
    ax.scatter(xL, RelP1, label="Linear Elastic", s=1)
    ax.scatter(xL, RelP2, label="NeoHookean",s=1)
    ax.scatter(xL, RelP3, label="Mooney-Rivlin ",s=1)
    ax.legend()
    ax.grid(True, which='both')
    ax.axhline(y = 0, color = 'k')
    ax.axvline(x = 0, color = 'k')
    fig, ax = plt.subplots()
    plt.title(title)
    plt.xlabel("\u03BB - 1")
    plt.ylabel("Sigma11")
    ax.scatter(xL, RelC1, label="Linear Elastic",s=1)
    ax.scatter(xL, RelC2, label="NeoHookean",s=1)
    ax.scatter(xL, RelC3, label="Mooney-Rivlin",s=1)
    ax.legend()
    ax.grid(True, which='both')
    ax.axhline(y = 0, color = 'k')
    ax.axvline(x = 0, color = 'k')
plot(0.49)
plot(0.2)
