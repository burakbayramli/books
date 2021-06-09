
## Problem 1 

NOTE: This is the same as the code from class 04, but adjusted for the homework examples


```python
import numpy as np
```


```python
## Problem setup 
## This is the only thing that is different from the class 04

# Setting up the 'mesh'
n_el = 4
n_node = 4
## nodal coordinates, not given and not needed, but useful to see which is which node
# thus I assigned arbitrary values here, just so that you have a better idea of where the nodes are
# and the connectivity becomes clear
node_X = np.array([[0,0],[2,0],[1,0],[0.5,0]])
## connectivity: which nodes form each element
elements = np.array([[1,3],[1,4],[4,3],[3,2]])

# Material parameters 
## parameters are stiffness for each element 
k = 5. #N/m
stiffness_vec = k*np.array([3.,1.,2.,1.])

# External forces. 
# Note, I create a vector which is the size of the number of nodes, I actually don't know the reactions
# in fact I need to solve for the reactions at some point, but it is convinient to create a vector of this
# size so I am just going to put a zero where the reactions are
F = np.zeros((n_node))
F[2] = 50. # This is the force on node '3', not that python indexing starts at 0, so F[2] is for node 3

# Applied displacements.
# Note: for the block partitioning to work you need to number the nodes such that the first set of nodes 
# have an essential boundary condition, and the rest are free nodes. In this case, since the nodes with
# essential boundary conditions appear first I don't need to create a long vector the size of the number of nodes
# I can just create the vector with just the known displacements 
d_E = np.array([0.,0.]) # nodes 1 and 2 have known displacements of zero

# Defining group of essential and free nodes 
# Note: for the block partitioning to work you need to number the nodes such that the first set of nodes 
# have an essential boundary condition, and the rest are free nodes
n_E = 2 # nodes 1 and 2 have essential boundary conditions 
n_F = 2 # the rest of the nodes are free 
```


```python
## Assembling global stiffness matrix

# copy-paste from class 04
# array of stiffness matrices
Ke_array = []
for i in range(n_el):
    Ke = np.array([[stiffness_vec[i],-stiffness_vec[i]],[-stiffness_vec[i],stiffness_vec[i]]])
    Ke_array.append(Ke)
# assemble global stiffness
# be careful with numbering between matlab and python
K = np.zeros((n_node,n_node))
for i in range(n_el):
    K[elements[i,0]-1,elements[i,0]-1] += Ke_array[i][0,0]
    K[elements[i,0]-1,elements[i,1]-1] += Ke_array[i][0,1]
    K[elements[i,1]-1,elements[i,0]-1] += Ke_array[i][1,0]
    K[elements[i,1]-1,elements[i,1]-1] += Ke_array[i][1,1]
print('K: global stiffness')
print(K)
```

    K: global stiffness
    [[ 20.   0. -15.  -5.]
     [  0.   5.  -5.   0.]
     [-15.  -5.  30. -10.]
     [ -5.   0. -10.  15.]]



```python
## Solving the linear system

# since I am doing the block partitioning, I numbered my nodes in a way that all the nodes
# with essential boundary conditions were numbered first, then the free nodes 
# Here I don't have to do anything else, just copy-paste from class 04
K_E = K[0:n_E,0:n_E]
K_F = K[n_E:,n_E:]
K_EF = K[0:n_E,n_E:]
F_F = F[n_E:]
print('K_E')
print(K_E)
print('K_F')
print(K_F)
print('K_EF')
print(K_EF)
print('F_F')
print(F_F)

# given the partioning, the solution is also copy-pase
d_F = np.linalg.solve(K_F,F_F- np.dot(K_EF.transpose(),d_E))
print('Displacement of free nodes')
print(d_F)

# I didn't do this in class but it is in the notes
F_E = np.dot(K_E,d_E) + np.dot(K_EF,d_F)
print('Reaction forces at nodes with essential boundary conditions')
print(F_E)
```

    K_E
    [[20.  0.]
     [ 0.  5.]]
    K_F
    [[ 30. -10.]
     [-10.  15.]]
    K_EF
    [[-15.  -5.]
     [ -5.   0.]]
    F_F
    [50.  0.]
    Displacement of free nodes
    [2.14285714 1.42857143]
    Reaction forces at nodes with essential boundary conditions
    [-39.28571429 -10.71428571]


## Problem 2


```python
## Problem setup 
## This is the only thing that is different from the class 04

# Setting up the 'mesh'
n_el = 3
n_node = 4
## nodal coordinates
node_X = np.array([[0,0],[1.5,0],[0.5,0],[1.0,0]])
## connectivity: which nodes form each element
elements = np.array([[1,3],[3,4],[4,2]])

# Material parameters 
## parameters are stiffness for each element 
L = 0.5 # m
E = 60e9 # Pa
A1 = 1 # m^2 
A2 = 0.5 # m^2
A3 = 0.25 # m^2
k1 = E*A1/L #N/m
k2 = E*A2/L #N/m
k3 = E*A3/L #N/m
stiffness_vec = np.array([k1,k2,k3])

# External forces. 
# Note, I create a vector which is the size of the number of nodes, I actually don't know the reactions
# in fact I need to solve for the reactions, but it is convinient to create a vector of this
# size so I am just going to put a zero where the reactions are
F = np.zeros((n_node))
F[2] = 500. # This is the force on node '3', not that python indexing starts at 0, so F[2] is for node 3
F[3] = 100. # This is the force on node '4'

# Applied displacements.
# Note: for the block partitioning to work you need to number the nodes such that the first set of nodes 
# have an essential boundary condition, and the rest are free nodes. In this case, since the nodes with
# essential boundary conditions appear first I don't need to create a long vector the size of the number of nodes
# I can just create the vector with just the known displacements 
d_E = np.array([0.,0.]) # nodes 1 and 2 have known displacements of zero

# Defining group of essential and free nodes 
# Note: for the block partitioning to work you need to number the nodes such that the first set of nodes 
# have an essential boundary condition, and the rest are free nodes
n_E = 2 # nodes 1 and 2 have essential boundary conditions 
n_F = 2 # the rest of the nodes are free 
```


```python
## Assembling global stiffness matrix

# copy-paste from class 04
# array of stiffness matrices
Ke_array = []
for i in range(n_el):
    Ke = np.array([[stiffness_vec[i],-stiffness_vec[i]],[-stiffness_vec[i],stiffness_vec[i]]])
    Ke_array.append(Ke)
# assemble global stiffness
# be careful with numbering between matlab and python
K = np.zeros((n_node,n_node))
for i in range(n_el):
    K[elements[i,0]-1,elements[i,0]-1] += Ke_array[i][0,0]
    K[elements[i,0]-1,elements[i,1]-1] += Ke_array[i][0,1]
    K[elements[i,1]-1,elements[i,0]-1] += Ke_array[i][1,0]
    K[elements[i,1]-1,elements[i,1]-1] += Ke_array[i][1,1]
print('K: global stiffness')
print(K)
```

    K: global stiffness
    [[ 1.2e+11  0.0e+00 -1.2e+11  0.0e+00]
     [ 0.0e+00  3.0e+10  0.0e+00 -3.0e+10]
     [-1.2e+11  0.0e+00  1.8e+11 -6.0e+10]
     [ 0.0e+00 -3.0e+10 -6.0e+10  9.0e+10]]



```python
## Solving the linear system

# since I am doing the block partitioning, I numbered my nodes in a way that all the nodes
# with essential boundary conditions were numbered first, then the free nodes 
# Here I don't have to do anything else, just copy-paste from class 04
K_E = K[0:n_E,0:n_E]
K_F = K[n_E:,n_E:]
K_EF = K[0:n_E,n_E:]
F_F = F[n_E:]
print('K_E')
print(K_E)
print('K_F')
print(K_F)
print('K_EF')
print(K_EF)
print('F_F')
print(F_F)

# given the partioning, the solution is also copy-pase
d_F = np.linalg.solve(K_F,F_F- np.dot(K_EF.transpose(),d_E))
print('Displacement of free nodes')
print(d_F)

# I didn't do this in class but it is in the notes
F_E = np.dot(K_E,d_E) + np.dot(K_EF,d_F)
print('Reaction forces at nodes with essential boundary conditions')
print(F_E)
```

    K_E
    [[1.2e+11 0.0e+00]
     [0.0e+00 3.0e+10]]
    K_F
    [[ 1.8e+11 -6.0e+10]
     [-6.0e+10  9.0e+10]]
    K_EF
    [[-1.2e+11  0.0e+00]
     [ 0.0e+00 -3.0e+10]]
    F_F
    [500. 100.]
    Displacement of free nodes
    [4.04761905e-09 3.80952381e-09]
    Reaction forces at nodes with essential boundary conditions
    [-485.71428571 -114.28571429]

