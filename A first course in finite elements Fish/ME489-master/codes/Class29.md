

```python
import numpy as np
```


```python
def Area(x1,y1,x2,y2,x3,y3):
    edge1 = np.array([x2-x1,y2-y1,0.])
    edge2 = np.array([x3-x1,y3-y1,0.])
    A_e = np.cross(edge1,edge2)
    A_e = np.linalg.norm(A_e)/2.
    return A_e
```


```python
node_X = np.array([[0.,0.],[3.,0.],[0.,3.]])
elements = np.array([[1,2,3]])
n_el = 1
E = 3e11
nu = 0.3
for i in range(n_el):
    x1 = node_X[elements[i,0]-1,0]
    y1 = node_X[elements[i,0]-1,1]
    x2 = node_X[elements[i,1]-1,0]
    y2 = node_X[elements[i,1]-1,1]
    x3 = node_X[elements[i,2]-1,0]
    y3 = node_X[elements[i,2]-1,1]
    Ae = Area(x1,y1,x2,y2,x3,y3)
    B = (1./(2.*Ae))*np.array([[y2-y3,0,y3-y1,0,y1-y2,0],
                               [0,x3-x2,0,x1-x3,0,x2-x1],
                               [x3-x2,y2-y3,x1-x3,y3-y1,x2-x1,y1-y2]])
    D = E/(1-nu**2)*np.array([[1.,nu,0.],[nu,1.,0.],[0.,0.,(1.-nu)/2.]])
    Dstar = np.array([[1.,nu,0.],[nu,1.,0.],[0.,0.,(1.-nu)/2.]])
    K = Ae*np.dot(B.transpose(),np.dot(D,B))
    Kstar = Ae*np.dot(B.transpose(),np.dot(Dstar,B))
    print('B')
    print(B)
    print('K without multiplying by (E/(1-nu**2))')
    print(Kstar*2)
    
```

    B
    [[-0.33333333  0.          0.33333333  0.          0.          0.        ]
     [ 0.         -0.33333333  0.          0.          0.          0.33333333]
     [-0.33333333 -0.33333333  0.          0.33333333  0.33333333  0.        ]]
    K without multiplying by (E/(1-nu**2))
    [[ 1.35  0.65 -1.   -0.35 -0.35 -0.3 ]
     [ 0.65  1.35 -0.3  -0.35 -0.35 -1.  ]
     [-1.   -0.3   1.    0.    0.    0.3 ]
     [-0.35 -0.35  0.    0.35  0.35  0.  ]
     [-0.35 -0.35  0.    0.35  0.35  0.  ]
     [-0.3  -1.    0.3   0.    0.    1.  ]]



```python
# Only two equations are needed, those corresponding to u2x and u3y 
# equation for u2x
print(Kstar[2,:])
# equation for u3y
print(Kstar[-1,:])
# NOTE: you can tell from the problem statement that this should be a symmetric solution, i.e.
# the displacement if u2x and u3y should be the same 
Ksmall = E/(1-nu**2)*np.array([[0.5,0.15],[0.15,0.5]])
# The force vector is the integral of the traction over the boundary, 
# note here that the traction is constant, so you don't really need to do the integral, just multiply
# by le/2 where le is the length of the edge 
force = np.array([45./2.,45./2.])
print('Dispalcements')
np.linalg.solve(Ksmall,force)
```

    [-0.5  -0.15  0.5   0.    0.    0.15]
    [-0.15 -0.5   0.15  0.    0.    0.5 ]
    Dispalcements





    array([  1.05000000e-10,   1.05000000e-10])


