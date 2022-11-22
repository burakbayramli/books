import sympy as sp
sp.init_printing(use_latex="mathjax")
e1, e2, e3 = -500, -100, -500
maxShearStress = max([abs(e1-e2),abs(e2-e3),abs(e2-e3)])/2
display("max shear stress (MPa) =",maxShearStress)
