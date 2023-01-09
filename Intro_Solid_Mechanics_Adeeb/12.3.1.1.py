import sympy as sp
from sympy import solve
a0,a1,a2,x1,x2 = sp.symbols("a_0 a_1 a_2 x_1 x_2")
b0,b1,b2= sp.symbols("b_0 b_1 b_2")
u1,u2,u3,v1,v2,v3 = sp.symbols("u_1 u_2 u_3 v_1 v_2 v_3")
u = a0+a1*x1+a2*x2
v = b0+b1*x1+b2*x2
print("u =",u,"v =",v)
Coordinates = [{x1:0,x2:0},{x1:1,x2:0},{x1:0,x2:1}]
print("Coordinates: ",Coordinates)
Eq1 = u.subs(Coordinates[0])
Eq2 = u.subs(Coordinates[1])
Eq3 = u.subs(Coordinates[2])
Eq4 = v.subs(Coordinates[0])
Eq5 = v.subs(Coordinates[1])
Eq6 = v.subs(Coordinates[2])
s = solve((Eq1-u1,Eq2-u2,Eq3-u3,Eq4-v1,Eq5-v2,Eq6-v3),
          (a0,a1,a2,b0,b1,b2))
print("Solve: ",s)
u = u.subs(s).expand()
v = v.subs(s).expand()
print("u =",u,"v =",v)
N1_u = u.coeff(u1)
N1_v = v.coeff(v1)
N2_u = u.coeff(u2)
N2_v = v.coeff(v2)
N3_u = u.coeff(u3)
N3_v = v.coeff(v3)
print("N1u1 =",N1_u,"N1v1 =",N1_v,"N2u2 =",N2_u,
        "N2v2 =",N2_v,"N3u3 =",N3_u,"N3v3 =",N3_v)
