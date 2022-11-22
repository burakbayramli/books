import sympy as sp
from sympy import *
a,a0,a1,a2,a3,b,b0,b1,b2,b3,X1,X2,u1,u2,u3,u4,v1,v2,v3,v4 = symbols("a a_0 a_1 a_2 a_3 b b_0 b_1 b_2 b_3 X_1 X_2 u_1 u_2 u_3 u_4 v_1 v_2 v_3 v_4")
u = a0+a1*X1+a2*X2+a3*X1*X2
v = b0+b1*X1+b2*X2+b3*X1*X2
Coordinates = [{X1:-a,X2:-b},{X1:a,X2:-b},{X1:a,X2:b}, {X1:-a, X2:b}]
display("Coordinates: ",Coordinates)
Eq1 = u.subs(Coordinates[0])
Eq2 = u.subs(Coordinates[1])
Eq3 = u.subs(Coordinates[2])
Eq4 = u.subs(Coordinates[3])
Eq5 = v.subs(Coordinates[0])
Eq6 = v.subs(Coordinates[1])
Eq7 = v.subs(Coordinates[2])
Eq8 = v.subs(Coordinates[3])
s = solve((Eq1-u1,Eq2-u2,Eq3-u3,Eq4-u4,Eq5-v1,Eq6-v2, Eq7-v3, Eq8-v4),
          (a0,a1,a2,a3,b0,b1,b2,b3))
display("Solve: ",s)
u = u.subs(s).expand()
v = v.subs(s).expand()
display("u =",u,"v =",v)
N1_u = u.coeff(u1)
N1_v = v.coeff(v1)
N2_u = u.coeff(u2)
N2_v = v.coeff(v2)
N3_u = u.coeff(u3)
N3_v = v.coeff(v3)
N4_u = u.coeff(u4)
N4_v = v.coeff(v4)
display("N1u1 =",N1_u,"N1v1 =",N1_v,"N2u2 =",N2_u,
        "N2v2 =",N2_v,"N3u3 =",N3_u,"N3v3 =",N3_v,
        "N4u4=",N4_u,"N4v4 =",N4_v)
