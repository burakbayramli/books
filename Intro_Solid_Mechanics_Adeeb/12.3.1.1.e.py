import sympy as sp
from sympy import solve
a0,a1,a2,a3,a4,a5,x1,x2 = sp.symbols("a_0 a_1 a_2 a_3 a_4 a_5 x_1 x_2")
b0,b1,b2,b3,b4,b5 = sp.symbols("b_0 b_1 b_2 b_3 b_4 b_5")
u1,u2,u3,u4,u5,u6 = sp.symbols("u_1 u_2 u_3 u_4 u_5 u_6")
v1,v2,v3,v4,v5,v6 = sp.symbols("v_1 v_2 v_3 v_4 v_5 v_6")
u = a0+a1*x1+a2*x2+a3*x1*x2+a4*x1**2+a5*x2**2
v = b0+b1*x1+b2*x2+b3*x1*x2+b4*x1**2+b5*x2**2
display("u =",u,"v =",v)
Coordinates = [{x1:0,x2:0},{x1:1,x2:0},{x1:0,x2:1},
               {x1:1/2,x2:0},{x1:1/2,x2:1/2},{x1:0,x2:1/2}]
display("Coordinates =",Coordinates)
Eq1 = u.subs(Coordinates[0])
Eq2 = u.subs(Coordinates[1])
Eq3 = u.subs(Coordinates[2])
Eq4 = v.subs(Coordinates[0])
Eq5 = v.subs(Coordinates[1])
Eq6 = v.subs(Coordinates[2])
Eq7 = u.subs(Coordinates[3])
Eq8 = u.subs(Coordinates[4])
Eq9 = u.subs(Coordinates[5])
Eq10 = v.subs(Coordinates[3])
Eq11 = v.subs(Coordinates[4])
Eq12 = v.subs(Coordinates[5])
s = solve((Eq1-u1,Eq2-u2,Eq3-u3,Eq4-v1,Eq5-v2,Eq6-v3,
          Eq7-u4,Eq8-u5,Eq9-u6,Eq10-v4,Eq11-v5,Eq12-v6),
          (a0,a1,a2,a3,a4,a5,b0,b1,b2,b3,b4,b5))
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
N5_u = u.coeff(u5)
N5_v = v.coeff(v5)
N6_u = u.coeff(u6)
N6_v = v.coeff(v6)
display("N1u1 =",N1_u,"N1v1 =",N1_v,"N2u2 =",N2_u,
        "N2v2 =",N2_v,"N3u3 =",N3_u,"N3v3 =",N3_v,
        "N4u4 =",N4_u,"N4v4 =",N4_v,"N5u5 =",N5_u,
        "N5v5 =",N5_v,"N6u6 =",N6_u,"N6v6 =",N6_v)
