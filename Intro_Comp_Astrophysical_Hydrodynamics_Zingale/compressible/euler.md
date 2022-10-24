```python
from sympy import init_session
init_session()
```

    IPython console for SymPy 1.0 (Python 2.7.12-64-bit) (ground types: gmpy)
    
    These commands were executed:
    >>> from __future__ import division
    >>> from sympy import *
    >>> x, y, z, t = symbols('x y z t')
    >>> k, m, n = symbols('k m n', integer=True)
    >>> f, g, h = symbols('f g h', cls=Function)
    >>> init_printing()
    
    Documentation can be found at http://docs.sympy.org/1.0/


# Euler Equations

The Euler equations in primitive variable form, $q = (\rho, u, p)^\intercal$ appear as:

$$q_t + A(q) q_x = 0$$

with the matrix $A(q)$:


$$A(q) = \left ( \begin{array}{ccc} u  & \rho     & 0 \\                          
                                  0  &  u       & 1/\rho \\                     
                                  0  & \gamma p & u \end{array} \right )  
$$

The sound speed is related to the adiabatic index, $\gamma$, as $c^2 = \gamma p /\rho$.

We can represent this matrix symbolically in SymPy and explore its eigensystem.


```python
from sympy.abc import rho
rho, u, c = symbols('rho u c')

A = Matrix([[u, rho, 0], [0, u, rho**-1], [0, c**2 * rho, u]])
A
```




$$\left[\begin{matrix}u & \rho & 0\\0 & u & \frac{1}{\rho}\\0 & c^{2} \rho & u\end{matrix}\right]$$



The eigenvalues are the speeds at which information propagates with.  SymPy returns them as a
dictionary, giving the multiplicity for each eigenvalue.


```python
A.eigenvals()
```




$$\left \{ u : 1, \quad - c + u : 1, \quad c + u : 1\right \}$$



The right eigenvectors are what SymPy gives natively.  For a given eigenvalue, $\lambda$, these 
satisfy:
    
$$A r = \lambda r$$

## Right Eigenvectors


```python
R = A.eigenvects()   # this returns a tuple for each eigenvector with multiplicity -- unpack it
r = []
lam = []
for (ev, _, rtmp) in R:
    r.append(rtmp[0])
    lam.append(ev)
    
# we can normalize them anyway we want, so let's make the first entry 1
for n in range(len(r)):
    v = r[n]
    r[n] = v/v[0]
```

### 0-th right eigenvector 


```python
r[0]
```




$$\left[\begin{matrix}1\\0\\0\end{matrix}\right]$$



this corresponds to the eigenvalue


```python
lam[0]
```




$$u$$



### 1-st right eigenvector


```python
r[1]
```




$$\left[\begin{matrix}1\\- \frac{c}{\rho}\\c^{2}\end{matrix}\right]$$



this corresponds to the eigenvalue


```python
lam[1]
```




$$- c + u$$



### 2-nd right eigenvector


```python
r[2]
```




$$\left[\begin{matrix}1\\\frac{c}{\rho}\\c^{2}\end{matrix}\right]$$



this corresponds to the eigenvalue


```python
lam[2]
```




$$c + u$$



Here they are as a matrix, $R$, in order from smallest to largest eigenvalue


```python
R = zeros(3,3)
R[:,0] = r[1]
R[:,1] = r[0]
R[:,2] = r[2]
R
```




$$\left[\begin{matrix}1 & 1 & 1\\- \frac{c}{\rho} & 0 & \frac{c}{\rho}\\c^{2} & 0 & c^{2}\end{matrix}\right]$$



## Left Eigenvectors

The left eigenvectors satisfy:

$$l A = \lambda l$$

SymPy doesn't have a method to get left eigenvectors directly, so we take the transpose of this expression:

$$(l A)^\intercal = A^\intercal l^\intercal = \lambda l^\intercal$$

Therefore, the transpose of the left eigenvectors, $l^\intercal$, are the right eigenvectors of transpose of $A$


```python
B = A.transpose()
B
```




$$\left[\begin{matrix}u & 0 & 0\\\rho & u & c^{2} \rho\\0 & \frac{1}{\rho} & u\end{matrix}\right]$$




```python
L = B.eigenvects()
l = []
laml = []
for (ev, _, ltmp) in L:
    l.append(ltmp[0].transpose())
    laml.append(ev)
    
```

Traditionally, we normalize these such that $l^{(\mu)} \cdot r^{(\nu)} = \delta_{\mu\nu}$


```python
for n in range(len(l)):
    if lam[n] == laml[n]:
        ltmp = l[n]
        p = ltmp.dot(r[n])
        l[n] = ltmp/p
```

### 0-th left eigenvector


```python
l[0]
```




$$\left[\begin{matrix}1 & 0 & - \frac{1}{c^{2}}\end{matrix}\right]$$



### 1-st left eigenvector


```python
l[1]
```




$$\left[\begin{matrix}0 & - \frac{\rho}{2 c} & \frac{1}{2 c^{2}}\end{matrix}\right]$$



### 2-nd left eigenvector


```python
l[2]
```




$$\left[\begin{matrix}0 & \frac{\rho}{2 c} & \frac{1}{2 c^{2}}\end{matrix}\right]$$




```python

```

# Entropy formulation

here we write the system in terms of $q_s = (\rho, u, s)^\intercal$, where the system is

$${q_s}_t + A_s(q_s) {q_s}_x = 0$$

and 

$$
A_s = \left (\begin{matrix}u & \rho & 0\\
      \frac{c^{2}}{\rho} & u & \frac{p_{s}}{\rho}\\
         0 & 0 & u\end{matrix}\right)
         $$


```python
ps = symbols('p_s')

As = Matrix([[u, rho, 0], [c**2/rho, u, ps/rho], [0, 0, u]])
As
```




$$\left[\begin{matrix}u & \rho & 0\\\frac{c^{2}}{\rho} & u & \frac{p_{s}}{\rho}\\0 & 0 & u\end{matrix}\right]$$




```python
As.eigenvals()
```




$$\left \{ u : 1, \quad - c + u : 1, \quad c + u : 1\right \}$$




```python
R = As.eigenvects()   # this returns a tuple for each eigenvector with multiplicity -- unpack it
r = []
lam = []
for (ev, _, rtmp) in R:
    r.append(rtmp[0])
    lam.append(ev)
    
# we can normalize them anyway we want, so let's make the first entry 1
for n in range(len(r)):
    v = r[n]
    r[n] = v/v[0]
```


```python
r[0], lam[0]
```




$$\left ( \left[\begin{matrix}1\\0\\- \frac{c^{2}}{p_{s}}\end{matrix}\right], \quad u\right )$$




```python
r[1], lam[1]
```




$$\left ( \left[\begin{matrix}1\\- \frac{c}{\rho}\\0\end{matrix}\right], \quad - c + u\right )$$




```python
r[2], lam[2]
```




$$\left ( \left[\begin{matrix}1\\\frac{c}{\rho}\\0\end{matrix}\right], \quad c + u\right )$$



### left eigenvectors


```python
Bs = As.transpose()
L = B.eigenvects()
l = []
laml = []
for (ev, _, ltmp) in L:
    l.append(ltmp[0].transpose())
    laml.append(ev)
    
```

normalization


```python
for n in range(len(l)):
    if lam[n] == laml[n]:
        ltmp = l[n]
        p = ltmp.dot(r[n])
        l[n] = ltmp/p
```


```python
simplify(l[0])
```




$$\left[\begin{matrix}\frac{p_{s}}{p_{s} + 1} & 0 & - \frac{p_{s}}{c^{2} \left(p_{s} + 1\right)}\end{matrix}\right]$$




```python
l[1]
```




$$\left[\begin{matrix}0 & - \frac{\rho}{c} & \frac{1}{c^{2}}\end{matrix}\right]$$




```python
l[2]
```




$$\left[\begin{matrix}0 & \frac{\rho}{c} & \frac{1}{c^{2}}\end{matrix}\right]$$



# 2-d system


```python
rho, u, v, c = symbols('rho u v c')

A = Matrix([[u, rho, 0, 0], [0, u, 0, rho**-1], [0,0, u, 0], [0, c**2 * rho, 0, u]])
A
```




$$\left[\begin{matrix}u & \rho & 0 & 0\\0 & u & 0 & \frac{1}{\rho}\\0 & 0 & u & 0\\0 & c^{2} \rho & 0 & u\end{matrix}\right]$$




```python
A.eigenvals()
```




$$\left \{ u : 2, \quad - c + u : 1, \quad c + u : 1\right \}$$




```python
R = A.eigenvects()   # this returns a tuple for each eigenvector with multiplicity -- unpack it
r = []
lam = []
for (ev, _, rtmp) in R:
    for rv in rtmp:
        r.append(rv)
        lam.append(ev)
    
# we can normalize them anyway we want, so let's make the first entry 1
for n in range(len(r)):
    v = r[n]
    if not v[0] == 0:
        r[n] = v/v[0]
```


```python
r[0], lam[0]
```




$$\left ( \left[\begin{matrix}1\\0\\0\\0\end{matrix}\right], \quad u\right )$$




```python
r[1], lam[1]
```




$$\left ( \left[\begin{matrix}0\\0\\1\\0\end{matrix}\right], \quad u\right )$$




```python
r[2], lam[2]
```




$$\left ( \left[\begin{matrix}1\\- \frac{c}{\rho}\\0\\c^{2}\end{matrix}\right], \quad - c + u\right )$$




```python
r[3], lam[3]
```




$$\left ( \left[\begin{matrix}1\\\frac{c}{\rho}\\0\\c^{2}\end{matrix}\right], \quad c + u\right )$$




```python

```
