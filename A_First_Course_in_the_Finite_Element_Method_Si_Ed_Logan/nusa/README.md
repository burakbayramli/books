![](nusa/img/nusa-logo.png)

A Python library for structural analysis using the finite element method, designed for academic purposes.

## Versions

* **0.1.0** (16/11/2016)
* **0.2.0** (14/07/2019)
* **0.3.dev0** Development version 

## Requirements

* NumPy
* Matplotlib
* Tabulate
* [GMSH](http://gmsh.info/)


## Installation

From PyPI (0.2.0 version):

```
$ pip install nusa
```

or from this repo (development version):

```
$ pip install git+https://github.com/JorgeDeLosSantos/nusa.git
```


## Elements type supported

* Spring
* Bar
* Truss
* Beam
* Linear triangle (currently, only plane stress)


## Mini-Demos

### Linear Triangle Element

```python
from nusa import *
import nusa.mesh as nmsh

md = nmsh.Modeler()
a = md.add_rectangle((0,0),(1,1), esize=0.1)
b = md.add_circle((0.5,0.5), 0.1, esize=0.05)
md.substract_surfaces(a,b)
nc, ec = md.generate_mesh()
x,y = nc[:,0], nc[:,1]

nodos = []
elementos = []

for k,nd in enumerate(nc):
    cn = Node((x[k],y[k]))
    nodos.append(cn)
    
for k,elm in enumerate(ec):
    i,j,m = int(elm[0]),int(elm[1]),int(elm[2])
    ni,nj,nm = nodos[i],nodos[j],nodos[m]
    ce = LinearTriangle((ni,nj,nm),200e9,0.3,0.1)
    elementos.append(ce)

m = LinearTriangleModel()
for node in nodos: m.add_node(node)
for elm in elementos: m.add_element(elm)
    
# Boundary conditions and loads
minx, maxx = min(x), max(x)
miny, maxy = min(y), max(y)

for node in nodos:
    if node.x == minx:
        m.add_constraint(node, ux=0, uy=0)
    if node.x == maxx:
        m.add_force(node, (10e3,0))

m.plot_model()
m.solve()
m.plot_nsol("seqv")
```

![](docs/nusa-info/es/src/linear-triangle-element/model_plot.png)
![](docs/nusa-info/es/src/linear-triangle-element/nsol.png)


### Spring element

**Example 01**. For the spring assemblage with arbitrarily numbered nodes shown in the figure 
obtain (a) the global stiffness matrix, (b) the displacements of nodes 3 and 4, (c) the 
reaction forces at nodes 1 and 2, and (d) the forces in each spring. A force of 5000 lb
is applied at node 4 in the `x` direction. The spring constants are given in the figure.
Nodes 1 and 2 are fixed.

![](docs/nusa-info/es/src/spring-element/example_01.PNG)

```python
# -*- coding: utf-8 -*-
# NuSA Demo
from nusa import *
    
def test1():
    """
    Logan, D. (2007). A first course in the finite element analysis.
    Example 2.1, pp. 42.
    """
    P = 5000.0

    # Model
    m1 = SpringModel("2D Model")

    # Nodes
    n1 = Node((0,0))
    n2 = Node((0,0))
    n3 = Node((0,0))
    n4 = Node((0,0))

    # Elements
    e1 = Spring((n1,n3),1000.0)
    e2 = Spring((n3,n4),2000.0)
    e3 = Spring((n4,n2),3000.0)

    # Adding elements and nodes to the model
    for nd in (n1,n2,n3,n4):
        m1.add_node(nd)
    for el in (e1,e2,e3):
        m1.add_element(el)

    m1.add_force(n4, (P,))
    m1.add_constraint(n1, ux=0)
    m1.add_constraint(n2, ux=0)
    m1.solve()

if __name__ == '__main__':
    test1()
```


### Beam element

**Example 02**. For the beam and loading shown, determine the deflection at point C. 
Use E = 29 x 10<sup>6</sup> psi.

![](docs/nusa-info/es/src/beam-element/P913_beer.PNG)

```python
"""
Beer & Johnston. (2012) Mechanics of materials. 
Problem 9.13 , pp. 568.
"""
from nusa import *

# Input data 
E = 29e6
I = 291 # W14x30 
P = 35e3
L1 = 5*12 # in
L2 = 10*12 #in
# Model
m1 = BeamModel("Beam Model")
# Nodes
n1 = Node((0,0))
n2 = Node((L1,0))
n3 = Node((L1+L2,0))
# Elements
e1 = Beam((n1,n2),E,I)
e2 = Beam((n2,n3),E,I)

# Add elements 
for nd in (n1,n2,n3): m1.add_node(nd)
for el in (e1,e2): m1.add_element(el)
    
m1.add_force(n2, (-P,))
m1.add_constraint(n1, ux=0, uy=0) # fixed 
m1.add_constraint(n3, uy=0) # fixed
m1.solve() # Solve model

# Displacement at C point
print(n2.uy)
```

## GUIs based on NuSA

* [wxTruss](https://github.com/JorgeDeLosSantos/wxtruss)


## Documentation

To build documentation based on docstrings execute the `docs/toHTML.py` script. (Sphinx required)

Tutorials (Jupyter notebooks):

Spanish version (in progress):

* [Introducción a NuSA](docs/nusa-info/es/intro-nusa.ipynb)
* [Elemento Spring](docs/nusa-info/es/spring-element.ipynb)
* [Elemento Bar](docs/nusa-info/es/bar-element.ipynb)
* [Elemento Beam](docs/nusa-info/es/beam-element.ipynb)
* [Elemento Truss](docs/nusa-info/es/truss-element.ipynb)
* [Elemento LinearTriangle](docs/nusa-info/es/linear-triangle-element.ipynb)

English version (TODO): 

* [Introduction to NuSA](docs/nusa-info/en/intro-nusa.ipynb)
* [Spring element](docs/nusa-info/en/spring-element.ipynb)
* [Bar element](docs/nusa-info/en/bar-element.ipynb)
* [Beam element](docs/nusa-info/en/beam-element.ipynb)
* [Truss element](docs/nusa-info/en/truss-element.ipynb)
* [LinearTriangle element](docs/nusa-info/en/linear-triangle-element.ipynb)


## About...

```
Developer: Pedro Jorge De Los Santos
E-mail: delossantosmfq@gmail.com
```
