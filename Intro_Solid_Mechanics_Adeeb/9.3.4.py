import sympy as sp
from sympy import Matrix
E11,E22,E33 = sp.symbols("E_{11} E_{22} E_{33}")
v12,v13,v23 = sp.symbols("\u03BD_{12} \u03BD_{13} \u03BD_{23}")
G12,G13,G23 = sp.symbols("G_{12} G_{13} G_{23}")
E = Matrix([[0.01,0.015, 0.001],
            [0.015,-0.02, 0.002],
            [0.001, 0.002, 0.001]])
Evect=Matrix([E[0,0],E[1,1],E[2,2],2*E[0,1],2*E[0,2],2*E[1,2]])
Dd=Matrix([[E11,0,0,0,0,0],
           [0,E22,0,0,0,0],
           [0,0,E33,0,0,0],
           [0,0,0,G12,0,0],
           [0,0,0,0,G13,0],
           [0,0,0,0,0,G23]])
#s = Matrix([[E[0,0]*E11,2*E[0,1]*G12,2*E[0,2]*G13],
#           [2*E[1,0]*G12,E[1,1]*E11,2*E[1,2]*G23],
#          [2*E[2,0]*G13, 2*E[2,1]*G23, E[2,2]*E33]])
svector=Dd*Evect
svector = svector.subs({E11:100,E22:100,E33:200,G12:35,G13:35,G23:35,})
s=Matrix([[svector[0],svector[3],svector[4]],
          [svector[3],svector[1],svector[5]],
          [svector[4],svector[5],svector[2]]])
display("\u03B5 =",E)
display("\u03B5_vector =",Evect)
display("\u03C3_vector =",svector)
display("\u03C3 =",s)
Energy = sum([s[i]*E[i] for i in range(9)])/2
display("Energy (U) =",Energy)
