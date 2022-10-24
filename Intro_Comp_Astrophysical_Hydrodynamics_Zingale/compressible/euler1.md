
I reran it myself


```python
import sympy as sp
x, y, z, t = sp.symbols('x y z t')
k, m, n = sp.symbols('k m n', integer=True)
f, g, h = sp.symbols('f g h', cls=sp.Function)
sp.init_printing()
```

```python
#from sympy.abc import rho
rho, u, c = sp.symbols('rho u c')

A = sp.Matrix([[u, rho, 0], [0, u, rho**-1], [0, c**2 * rho, u]])
A
```

```text
Out[1]: 
⎡u   ρ    0⎤
⎢          ⎥
⎢         1⎥
⎢0   u    ─⎥
⎢         ρ⎥
⎢          ⎥
⎢    2     ⎥
⎣0  c ⋅ρ  u⎦
```

```python
print (A.eigenvals())
R = A.eigenvects()
for (ev, _, rtmp) in R:
   print (ev)
```

```text
{-c + u: 1, u: 1, c + u: 1}
u
-c + u
c + u
```

```python
R = A.eigenvects()   
r = []
lam = []
for (ev, _, rtmp) in R:
    r.append(rtmp[0])
    lam.append(ev)
    
# we can normalize them anyway we want, so let's make the first entry 1
for n in range(len(r)):
    v = r[n]
    r[n] = v/v[0]

print (r[0])
print (lam[0])
```

```text
{-c + u: 1, u: 1, c + u: 1}
Matrix([[1], [0], [0]])
u
```

