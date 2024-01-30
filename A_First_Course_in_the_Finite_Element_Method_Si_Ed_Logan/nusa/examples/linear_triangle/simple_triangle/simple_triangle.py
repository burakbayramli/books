# -*- coding: utf-8 -*-
# ***********************************
#  Author: Pedro Jorge De Los Santos     
#  E-mail: delossantosmfq@gmail.com 
#  Blog: numython.github.io
#  License: MIT License
# ***********************************
from nusa import *

# Creando el modelo
m = LinearTriangleModel()
# Creando nodos
n1 = Node((0,0))
n2 = Node((1,0.5))
n3 = Node((0,1))
# Creando elemento
e1 = LinearTriangle((n1,n2,n3),200e9,0.3,0.1)
for n in (n1,n2,n3): m.add_node(n)
m.add_element(e1)
# Agregando condiciones de frontera
m.add_constraint(n1, ux=0, uy=0)
m.add_constraint(n3, ux=0, uy=0)
m.add_force(n2, (1000,0))
m.plot_model()
m.solve()
m.plot_nsol("ux")
m.plot_nsol("sxx")
m.show()
