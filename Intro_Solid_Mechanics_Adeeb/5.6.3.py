from sympy import Matrix
import sympy as sp
sp.init_printing(use_latex="mathjax")
s1 = Matrix([[2,2,0],[2,5,0],[0,0,-5]])
s2 = Matrix([[2,2,1],[2,4,-3],[1,-3,7]])
# vonMises function
def vonMises(M):
    return sp.sqrt(1/2*((M[0,0] - M[1,1])**2+(M[1,1] - M[2,2])**2+
                       (M[2,2] - M[0,0])**2+6*(M[0,1]**2+M[0,2]**2+M[1,2]**2)))
# example of a rotation matrix
t = sp.symbols("\u03B8")
Qx = Matrix([[1, 0, 0], 
            [0, sp.cos(t), -sp.sin(t)], 
            [0, sp.sin(t), sp.cos(t)]])
Qx = Qx.subs({t:30*sp.pi/180})
# outputs
print("s_1 =",s1)
print("Von Mises =",vonMises(s1))
print(vonMises(Qx*s1*Qx.T))
print("s_2 =",s2)
print("Von Mises =",vonMises(s2))
print(vonMises(Qx*s2*Qx.T).evalf())
