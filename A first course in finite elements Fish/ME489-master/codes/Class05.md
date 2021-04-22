

```python
import numpy as np
np.set_printoptions(precision=3)
```


```python
## EXAMPLE 1

# number of elements and nodes
n_el = 2
n_node = 3
## connectivity: which nodes form each element
elements = np.array([[1,3],[2,3]])
## node coordinates
L = 2.
nodes_X = np.array([[L,0],[0,0],[L,L]])
## parameters are stiffness for each element (arbitrary, just to show)
k1 = 3.
k2 = 2.
stiffness_vec = np.array([k1,k2])
# array of stiffness matrices
Ke_array = []
for i in range(n_el):
    ## stiffness matrix in the local (rotated system aligned with the truss element)
    Ke_loc = np.array([[stiffness_vec[i],0,-stiffness_vec[i],0],\
                   [0,0,0,0],\
                   [-stiffness_vec[i],0,stiffness_vec[i],0],\
                   [0,0,0,0]])
    ## transformation matrix (get angle based on node coordinates)
    X1e = nodes_X[elements[i,0]-1,0]
    Y1e = nodes_X[elements[i,0]-1,1]
    X2e = nodes_X[elements[i,1]-1,0]
    Y2e = nodes_X[elements[i,1]-1,1]    
    phi = np.arctan2(Y2e-Y1e,X2e-X1e)
    Re = np.array([[np.cos(phi),np.sin(phi),0,0],\
                   [-np.sin(phi),np.cos(phi),0,0],\
                   [0,0,np.cos(phi),np.sin(phi)],\
                   [0,0,-np.sin(phi),np.cos(phi)]])
    ## transform Ke_loc to Ke
    Ke = np.dot(Re.transpose(),np.dot(Ke_loc,Re))
    print('Ke for element ',i)
    print(Ke)
    Ke_array.append(Ke)

# assemble global stiffness
K = np.zeros((n_node*2,n_node*2))
for i in range(n_el):
    # watching out for matlab vs python convention 
    ind1 = elements[i,0]-1 # node 1
    ind2 = elements[i,1]-1 # node 2
    # first row 
    K[ind1*2+0,ind1*2+0] += Ke_array[i][0,0]
    K[ind1*2+0,ind1*2+1] += Ke_array[i][0,1]
    K[ind1*2+0,ind2*2+0] += Ke_array[i][0,2]
    K[ind1*2+0,ind2*2+1] += Ke_array[i][0,3]
    # second row
    K[ind1*2+1,ind1*2+0] += Ke_array[i][1,0]
    K[ind1*2+1,ind1*2+1] += Ke_array[i][1,1]
    K[ind1*2+1,ind2*2+0] += Ke_array[i][1,2]
    K[ind1*2+1,ind2*2+1] += Ke_array[i][1,3]
    # third row
    K[ind2*2+0,ind1*2+0] += Ke_array[i][2,0]
    K[ind2*2+0,ind1*2+1] += Ke_array[i][2,1]
    K[ind2*2+0,ind2*2+0] += Ke_array[i][2,2]
    K[ind2*2+0,ind2*2+1] += Ke_array[i][2,3]
    # fourth row
    K[ind2*2+1,ind1*2+0] += Ke_array[i][3,0]
    K[ind2*2+1,ind1*2+1] += Ke_array[i][3,1]
    K[ind2*2+1,ind2*2+0] += Ke_array[i][3,2]
    K[ind2*2+1,ind2*2+1] += Ke_array[i][3,3]
print('K: global stiffness')
print(K)
```

    ('Ke for element ', 0)
    [[  1.125e-32   1.837e-16  -1.125e-32  -1.837e-16]
     [  1.837e-16   3.000e+00  -1.837e-16  -3.000e+00]
     [ -1.125e-32  -1.837e-16   1.125e-32   1.837e-16]
     [ -1.837e-16  -3.000e+00   1.837e-16   3.000e+00]]
    ('Ke for element ', 1)
    [[ 1.  1. -1. -1.]
     [ 1.  1. -1. -1.]
     [-1. -1.  1.  1.]
     [-1. -1.  1.  1.]]
    K: global stiffness
    [[  1.125e-32   1.837e-16   0.000e+00   0.000e+00  -1.125e-32  -1.837e-16]
     [  1.837e-16   3.000e+00   0.000e+00   0.000e+00  -1.837e-16  -3.000e+00]
     [  0.000e+00   0.000e+00   1.000e+00   1.000e+00  -1.000e+00  -1.000e+00]
     [  0.000e+00   0.000e+00   1.000e+00   1.000e+00  -1.000e+00  -1.000e+00]
     [ -1.125e-32  -1.837e-16  -1.000e+00  -1.000e+00   1.000e+00   1.000e+00]
     [ -1.837e-16  -3.000e+00  -1.000e+00  -1.000e+00   1.000e+00   4.000e+00]]



```python
# Redoing in class 
# number of elements and nodes
n_el = 2
n_node = 3
## connectivity: which nodes form each element
elements = np.array([[1,3],[2,3]])
## node coordinate vector
L = 1 #meter
node_X = np.array([[L,0],[0,0],[L,L]])
## parameters are stiffness for each element (arbitrary, just to show)
k1 = 3. # N/m
k2 = 2. # N/m
stiffness_vec = np.array([k1,k2])
# array of stiffness matrices
Ke_array = []
# loop over elements to get the element stiffness matrix
for i in range(n_el):
    print('element ',i)
    print(elements[i])
    # local stiffness for rotated system
    Ke_loc = stiffness_vec[i]*np.array([[1,0,-1,0],\
                                       [0,0,0,0],\
                                       [-1,0,1,0],\
                                       [0,0,0,0]])
    X1e = node_X[ elements[i,0]-1 ,0]
    Y1e = node_X[ elements[i,0]-1 ,1]
    X2e = node_X[ elements[i,1]-1 ,0]
    Y2e = node_X[ elements[i,1]-1 ,1]
    print(X1e,Y1e,',',X2e,Y2e)
    phie = np.arctan2( Y2e-Y1e,X2e-X1e)
    Re = np.array([[np.cos(phie),np.sin(phie),0,0],\
                   [-np.sin(phie),np.cos(phie),0,0],\
                   [0,0,np.cos(phie),np.sin(phie)],\
                   [0,0,-np.sin(phie),np.cos(phie)]])
    Ke = np.dot(Re.transpose(),np.dot(Ke_loc,Re))
    print('Ke for element ',i)  
    print(Ke)
    Ke_array.append(Ke)
# assemble global stiffness
# be careful with numbering between matlab and python
K = np.zeros((2*n_node,2*n_node))
for i in range(n_el):
    inda = elements[i,0]-1 #index of node a, see notes
    indb = elements[i,1]-1 #index of node b, see notes
    # but now remember that for the assembly 
    # the columns of Ke correspond to ax,ay,bx,by in the global stiffness
    # where a and b are the global node numbers for the current element
    # ax = a*2+0
    # ay = a*2+1
    # bx = b*2+0
    # by = b*2+1
    K[inda*2+0,inda*2+0] += Ke_array[i][0,0]
    K[inda*2+0,inda*2+1] += Ke_array[i][0,1]
    K[inda*2+0,indb*2+0] += Ke_array[i][0,2]
    K[inda*2+0,indb*2+1] += Ke_array[i][0,3]
    # second row 
    K[inda*2+1,inda*2+0] += Ke_array[i][1,0]
    K[inda*2+1,inda*2+1] += Ke_array[i][1,1]
    K[inda*2+1,indb*2+0] += Ke_array[i][1,2]
    K[inda*2+1,indb*2+1] += Ke_array[i][1,3]
    # third row
    K[indb*2+0,inda*2+0] += Ke_array[i][2,0]
    K[indb*2+0,inda*2+1] += Ke_array[i][2,1]
    K[indb*2+0,indb*2+0] += Ke_array[i][2,2]
    K[indb*2+0,indb*2+1] += Ke_array[i][2,3]
    # fourth row
    K[indb*2+1,inda*2+0] += Ke_array[i][3,0]
    K[indb*2+1,inda*2+1] += Ke_array[i][3,1]
    K[indb*2+1,indb*2+0] += Ke_array[i][3,2]
    K[indb*2+1,indb*2+1] += Ke_array[i][3,3]
print('K: global stiffness')
print(K)
```

    ('element ', 0)
    [1 3]
    (1, 0, ',', 1, 1)
    ('Ke for element ', 0)
    [[  1.125e-32   1.837e-16  -1.125e-32  -1.837e-16]
     [  1.837e-16   3.000e+00  -1.837e-16  -3.000e+00]
     [ -1.125e-32  -1.837e-16   1.125e-32   1.837e-16]
     [ -1.837e-16  -3.000e+00   1.837e-16   3.000e+00]]
    ('element ', 1)
    [2 3]
    (0, 0, ',', 1, 1)
    ('Ke for element ', 1)
    [[ 1.  1. -1. -1.]
     [ 1.  1. -1. -1.]
     [-1. -1.  1.  1.]
     [-1. -1.  1.  1.]]
    K: global stiffness
    [[  1.125e-32   1.837e-16   0.000e+00   0.000e+00  -1.125e-32  -1.837e-16]
     [  1.837e-16   3.000e+00   0.000e+00   0.000e+00  -1.837e-16  -3.000e+00]
     [  0.000e+00   0.000e+00   1.000e+00   1.000e+00  -1.000e+00  -1.000e+00]
     [  0.000e+00   0.000e+00   1.000e+00   1.000e+00  -1.000e+00  -1.000e+00]
     [ -1.125e-32  -1.837e-16  -1.000e+00  -1.000e+00   1.000e+00   1.000e+00]
     [ -1.837e-16  -3.000e+00  -1.000e+00  -1.000e+00   1.000e+00   4.000e+00]]

