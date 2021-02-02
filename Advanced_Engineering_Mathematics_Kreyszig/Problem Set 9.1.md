```python
from sympy import *
from sympy.plotting import plot
import matplotlib.pyplot as plt
init_printing(use_unicode= True)
from sympy.vector import *

v,p,q = symbols("v p q")
```


```python
#1
N = CoordSys3D('N')
p = N.i + N.j
q = 6*N.i + 2*N.j
v = q-p
v
```




$$(5)\mathbf{\hat{i}_{N}} + \mathbf{\hat{j}_{N}}$$




```python
v.magnitude()
```




$$\sqrt{26}$$




```python
v.normalize()
```




$$(\frac{5 \sqrt{26}}{26})\mathbf{\hat{i}_{N}} + (\frac{\sqrt{26}}{26})\mathbf{\hat{j}_{N}}$$




```python
#2
p = N.i + N.j +N.k
q = 2*N.i + 2*N.j
v = q-p
v
```




$$\mathbf{\hat{i}_{N}} + \mathbf{\hat{j}_{N}} - \mathbf{\hat{k}_{N}}$$




```python
v.magnitude()
```




$$\sqrt{3}$$




```python
v.normalize()
```




$$(\frac{\sqrt{3}}{3})\mathbf{\hat{i}_{N}} + (\frac{\sqrt{3}}{3})\mathbf{\hat{j}_{N}} + (- \frac{\sqrt{3}}{3})\mathbf{\hat{k}_{N}}$$




```python
#3
p = -3*N.i + 4*N.j -.5*N.k
q = 5.5*N.i + 1.2*N.k
v = q-p
v
```




$$(8.5)\mathbf{\hat{i}_{N}} + (-4)\mathbf{\hat{j}_{N}} + (1.7)\mathbf{\hat{k}_{N}}$$




```python
v.magnitude()
```




$$9.5467271878901$$




```python
v.normalize()
```




$$(0.890357484058216)\mathbf{\hat{i}_{N}} + (-0.418991757203867)\mathbf{\hat{j}_{N}} + (0.178071496811643)\mathbf{\hat{k}_{N}}$$




```python
#4
p = 1*N.i + 4*N.j +2*N.k
q = -1*N.i -4*N.j -2*N.k
v = q-p
v
```




$$(-2)\mathbf{\hat{i}_{N}} + (-8)\mathbf{\hat{j}_{N}} + (-4)\mathbf{\hat{k}_{N}}$$




```python
v.magnitude()
```




$$2 \sqrt{21}$$




```python
v.normalize()
```




$$(- \frac{\sqrt{21}}{21})\mathbf{\hat{i}_{N}} + (- \frac{4 \sqrt{21}}{21})\mathbf{\hat{j}_{N}} + (- \frac{2 \sqrt{21}}{21})\mathbf{\hat{k}_{N}}$$




```python
#5
p = 0*N.i
q = 2*N.i +1*N.j -2*N.k
v = q-p
v
```




$$(2)\mathbf{\hat{i}_{N}} + \mathbf{\hat{j}_{N}} + (-2)\mathbf{\hat{k}_{N}}$$




```python
v.magnitude()
```




$$3$$




```python
v.normalize()
```




$$(\frac{2}{3})\mathbf{\hat{i}_{N}} + (\frac{1}{3})\mathbf{\hat{j}_{N}} + (- \frac{2}{3})\mathbf{\hat{k}_{N}}$$




```python
#6
v = 4*N.i
p = 2*N.j+ 13*N.k
q = p+v
q
```




$$(4)\mathbf{\hat{i}_{N}} + (2)\mathbf{\hat{j}_{N}} + (13)\mathbf{\hat{k}_{N}}$$




```python
q.magnitude()
```




$$4.03112887414927$$




```python
#7
v = (1/2)*N.i + 3*N.j -(1/4)*N.k
p = (7/2)*N.i+(-3)*N.j+ (3/4)*N.k
q = p+v
q
```




$$(4.0)\mathbf{\hat{i}_{N}} + (0.5)\mathbf{\hat{k}_{N}}$$




```python
q.magnitude()
```




$$4.03112887414927$$




```python
#8
v = 13.1*N.i + .8*N.j -2*N.k
p = 0*N.i
q = p+v
q
```




$$(13.1)\mathbf{\hat{i}_{N}} + (0.8)\mathbf{\hat{j}_{N}} + (-2)\mathbf{\hat{k}_{N}}$$




```python
q.magnitude()
```




$$13.2759180473518$$




```python
#9
v = 6*N.i + 1*N.j -4*N.k
p = -6*N.i-1*N.j-4*N.k
q = p+v
q
```




$$(-8)\mathbf{\hat{k}_{N}}$$




```python
q.magnitude()
```




$$8$$




```python
#10
v = - 3*N.j +3*N.k
p = 3*N.j- 3*N.k
q = p+v
q
```




$$\mathbf{\hat{0}}$$




```python
q.magnitude()
```




$$0$$




```python
a = 3*N.i + 2*N.j
b = -4*N.i+6*N.j
c = 5*N.i-N.j +8*N.k
d = 4*N.k
```


```python
#11
2*a
```




$$(6)\mathbf{\hat{i}_{N}} + (4)\mathbf{\hat{j}_{N}}$$




```python
a/2
```




$$(\frac{3}{2})\mathbf{\hat{i}_{N}} + \mathbf{\hat{j}_{N}}$$




```python
-a
```




$$(-3)\mathbf{\hat{i}_{N}} + (-2)\mathbf{\hat{j}_{N}}$$




```python
#12
(a+b)+c
```




$$(4)\mathbf{\hat{i}_{N}} + (7)\mathbf{\hat{j}_{N}} + (8)\mathbf{\hat{k}_{N}}$$




```python
a+(b+c)
```




$$(4)\mathbf{\hat{i}_{N}} + (7)\mathbf{\hat{j}_{N}} + (8)\mathbf{\hat{k}_{N}}$$




```python
#13
b+c
```




$$\mathbf{\hat{i}_{N}} + (5)\mathbf{\hat{j}_{N}} + (8)\mathbf{\hat{k}_{N}}$$




```python
c+b
```




$$\mathbf{\hat{i}_{N}} + (5)\mathbf{\hat{j}_{N}} + (8)\mathbf{\hat{k}_{N}}$$




```python
#14
3*c - 6*d
```




$$(15)\mathbf{\hat{i}_{N}} + (-3)\mathbf{\hat{j}_{N}}$$




```python
3*(c- 2*d)
```




$$(15)\mathbf{\hat{i}_{N}} + (-3)\mathbf{\hat{j}_{N}}$$




```python
7*(c-b)
```




$$(63)\mathbf{\hat{i}_{N}} + (-49)\mathbf{\hat{j}_{N}} + (56)\mathbf{\hat{k}_{N}}$$




```python
7*c -7*b
```




$$(63)\mathbf{\hat{i}_{N}} + (-49)\mathbf{\hat{j}_{N}} + (56)\mathbf{\hat{k}_{N}}$$




```python
4*a +3*b
```




$$(26)\mathbf{\hat{j}_{N}}$$




```python
-4*a -3*b
```




$$(-26)\mathbf{\hat{j}_{N}}$$




```python
(a+b).magnitude()
```




$$\sqrt{65}$$




```python

```
