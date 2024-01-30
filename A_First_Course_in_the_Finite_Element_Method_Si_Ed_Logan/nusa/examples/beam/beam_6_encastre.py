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
I = 10
L = 10
P = 10e3

nelm = 10
parts = np.linspace(0, L, nelm + 1)

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

m.add_constraint(nodos[0], ux=0, uy=0, ur=0)
m.add_force(nodos[-1], (-P,))
m.solve()

m.plot_disp(1, label="Approx.")

xx = np.linspace(0,L)
d = ((-P*xx**2.0)/(6.0*E*I))*(3*L - xx)
plt.plot(xx, d, label="Classic")
plt.legend()
plt.axis("auto")
plt.xlim(0,L+1)

m.show()



