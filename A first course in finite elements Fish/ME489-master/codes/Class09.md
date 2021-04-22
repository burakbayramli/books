

```python
import numpy as np
import sympy as sp
import matplotlib
import random
import matplotlib.pyplot as plt
%matplotlib inline 
```


```python
n_ele = 4
n_nod = 5
elements = np.array([[1,3],[3,4],[4,5],[5,2]])
node_X = np.array([0.,1.,0.25,0.5,0.75])
node_u = np.random.rand(5)
```


```python
## Define shape function
def N1(x,x1e,x2e):
    le = x2e-x1e
    return (1./le)*(x2e-x)

def N2(x,x1e,x2e):
    le = x2e-x1e
    return (1./le)*(x-x1e)
```


```python
## plot a function over the domain by plotting in each element 

# Loop over elements 
for i in range(n_ele):
    
    # Beginning and end 'x' coordinates of this element 
    x1e = node_X[elements[i,0]-1]
    x2e = node_X[elements[i,1]-1]
    
    # Nodal values of the function 'u'
    u1e = node_u[elements[i,0]-1]
    u2e = node_u[elements[i,1]-1]
    
    # Eval the shape function evaluation for multiple x
    x_vec = np.linspace(x1e,x2e,50)
    u_vec = N1(x_vec,x1e,x2e)*u1e + N2(x_vec,x1e,x2e)*u2e
    
    # plot
    plt.plot(x_vec,u_vec)
    
```


![png](output_3_0.png)

