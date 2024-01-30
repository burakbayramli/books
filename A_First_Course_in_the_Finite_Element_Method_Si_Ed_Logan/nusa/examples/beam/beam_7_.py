# -*- coding: utf-8 -*-
# ***********************************
#  Author: Pedro Jorge De Los Santos     
#  E-mail: delossantosmfq@gmail.com 
#  License: MIT License
# ***********************************
import numpy as np
from nusa import *
import itertools
import matplotlib.pyplot as plt

def pairwise(iterable):
    #~ "s -> (s0,s1), (s1,s2), (s2, s3), ..."
    a, b = itertools.tee(iterable)
    next(b, None)
    return zip(a, b)


# Input data 
E = 29e6 # psi
I = 10.
L = 10.
P = 10e3

nn= 20.
parts = np.linspace(0,L,nn)

nodos = []
for xc in parts:
    cn = Node((xc,0))
    nodos.append(cn)

elementos = []
for x in pairwise(nodos):
    ni,nj = x[0], x[1]
    ce = Beam((ni,nj),E,I)
    elementos.append(ce)

m = BeamModel()

for n in nodos: m.add_node(n)
for e in elementos: m.add_element(e)

m.add_constraint(nodos[0], ux=0, uy=0)
m.add_constraint(nodos[-1], ux=0, uy=0)
m.add_force(nodos[5], (-P,))
m.solve()

m.plot_disp(1)

xa = np.linspace(0, nodos[5].x)
xb = np.linspace(nodos[5].x, nodos[-1].x)
a = nodos[5].x - 0
b = nodos[-1].x - nodos[5].x
da = ((-P*b*xa)/(6*L*E*I))*(L**2-b**2-xa**2)
db = ((-P*a*(xb-L))/(6*L*E*I))*(a**2-2*L*xb+xb**2)
plt.plot(xa, da)
plt.plot(xb, db)
plt.axis("auto")
plt.xlim(-1,L+1)
plt.show()

print( nodos[4].uy )

#~ m.show()



