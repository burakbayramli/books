

```python
import numpy as np
```


```python
## read in the mesh
meshfile = open("pipe_mesh.txt",'r').readlines()
n_node = int(meshfile[1])
node_X = np.zeros((n_node,2))
for i in range(n_node):
    aux = meshfile[2+i].split()
    node_X[i,0] = float(aux[0])
    node_X[i,1] = float(aux[1])
n_elem = int(meshfile[3+n_node])
elements = np.zeros((n_elem,3),dtype=int)
for i in range(n_elem):
    aux = meshfile[4+n_node+i].split()
    elements[i,0] = int(aux[0])
    elements[i,1] = int(aux[1])
    elements[i,2] = int(aux[2])
print(node_X)
print(elements)
```

    [[ 0.  0.]
     [ 0.  1.]
     [ 0.  3.]
     [ 0.  4.]
     [ 2.  0.]
     [ 1.  1.]
     [ 3.  2.]
     [ 2.  2.]
     [ 2.  4.]
     [ 1.  3.]]
    [[ 1  6  2]
     [ 1  5  6]
     [ 5  8  6]
     [ 5  7  8]
     [ 7  9  8]
     [ 9 10  8]
     [ 9  4 10]
     [10  4  3]]



```python
def source(x,y):
    return x**2+y**2

```


```python
# looping over elements
for i in range(n_elem):
    # Get the nodes making up this element
    node1 = elements[i,0]-1 # the -1 is because indices start from 0
    node2 = elements[i,1]-1 # the -1 is because indices start from 0
    node3 = elements[i,2]-1 # the -1 is because indices start from 0
    # Get the coordinates of the triangle
    x1 = node_X[node1,0]
    y1 = node_X[node1,1]
    x2 = node_X[node2,0]
    y2 = node_X[node2,1]
    x3 = node_X[node3,0]
    y3 = node_X[node3,1]
    
    # Jacobian
    J = np.array([[x1-x3,y1-y3],[x2-x3,y2-y3]])
    detJ = np.linalg.det(J)
    
    # Loop over integration points 
    IPxi = np.array([[0.166666666,0.1666666666],[0.6666666666,0.16666666],[0.16666666666,0.666666666]])
    IPw = np.array([0.1666666666,0.166666666,0.1666666666])
    integral = 0
    for i in range(len(IPxi)):
        xIP = IPxi[i,0]*x1 + IPxi[i,1]*x2 + (1-IPxi[i,0]-IPxi[i,1])*x3
        yIP = IPxi[i,0]*y1 + IPxi[i,1]*y2 + (1-IPxi[i,0]-IPxi[i,1])*y3
        integral += IPw[i]*source(xIP,yIP)*detJ
    print('integral of source over the triangle = ',integral)
    
```

    ('integral of source over the triangle = ', 0.33333333278888894)
    ('integral of source over the triangle = ', 1.3333333311555555)
    ('integral of source over the triangle = ', 3.9999999819022221)
    ('integral of source over the triangle = ', 7.499999981477778)
    ('integral of source over the triangle = ', 12.833333288131112)
    ('integral of source over the triangle = ', 11.99999996572889)
    ('integral of source over the triangle = ', 14.666666626328889)
    ('integral of source over the triangle = ', 5.6666666501422229)

