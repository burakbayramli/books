

```python
import numpy as np
```


```python
## EXAMPLE 1
# number of elements and nodes
n_el = 3
n_node = 3
## connectivity: which nodes form each element
elements = np.array([[1,3],[1,3],[2,3]])
## parameters are stiffness for each element (arbitrary, just to show)
k1 = 3.
k2 = 2.
k3 = 1.5
stiffness_vec = np.array([k1,k2,k3])
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
    [[ 5.   0.  -5. ]
     [ 0.   1.5 -1.5]
     [-5.  -1.5  6.5]]



```python
## EXAMPLE 2
# number of elements and nodes
n_el = 5
n_node = 4
## connectivity: which nodes form each element
elements = np.array([[1,4],[4,2],[1,3],[4,3],[3,2]])
## parameters are stiffness for each element (arbitrary, just to show)
# stiffness_vec = np.array([k1,k2,k3,k4,k5])
stiffness_vec = 2.*np.array([2.0,1.0,1.,3.,3.])
# array of stiffness matrices for each element
Ke_array = []
# for loop over elements
for i in range(n_el):
    Ke = np.array([[stiffness_vec[i],-stiffness_vec[i]],[-stiffness_vec[i],stiffness_vec[i]]])
    Ke_array.append(Ke)
# assemble global stiffness
# be careful with numbering between matlab and python
K = np.zeros((n_node,n_node))
# loop over elements
for i in range(n_el):
    K[elements[i,0]-1,elements[i,0]-1] += Ke_array[i][0,0]
    K[elements[i,0]-1,elements[i,1]-1] += Ke_array[i][0,1]
    K[elements[i,1]-1,elements[i,0]-1] += Ke_array[i][1,0]
    K[elements[i,1]-1,elements[i,1]-1] += Ke_array[i][1,1]
print(K)
```

    [[  6.   0.  -2.  -4.]
     [  0.   8.  -6.  -2.]
     [ -2.  -6.  14.  -6.]
     [ -4.  -2.  -6.  12.]]



```python
# solving the system with matrix partitioning 
n_E = 2
n_F = 2
K_E = K[0:n_E,0:n_E]
K_F = K[n_E:,n_E:]
K_EF = K[0:n_E,n_E:]
print('K_E')
print(K_E)
print('K_F')
print(K_F)
print('K_EF')
print(K_EF)
```

    K_E
    [[ 6.  0.]
     [ 0.  8.]]
    K_F
    [[ 14.  -6.]
     [ -6.  12.]]
    K_EF
    [[-2. -4.]
     [-6. -2.]]



```python
# known displacement
d_E = np.array([0.,2.])
# known external forces
F_F = np.array([0.,0.])
# solving A\b
np.linalg.solve(K_F,F_F- np.dot(K_EF.transpose(),d_E))
```




    array([ 1.27272727,  0.96969697])




```python
## EXAMPLE 2
# number of elements and nodes
n_el = 3
n_node = 3
## connectivity: which nodes form each element
elements = np.array([[1,3],[3,2],[1,3]])
## parameters are stiffness for each element (arbitrary, just to show)
# stiffness_vec = np.array([k1,k2,k3,k4,k5])
stiffness_vec = np.array([15.0,5.0,1./(1./5.+1./10.)])
# array of stiffness matrices for each element
Ke_array = []
# for loop over elements
for i in range(n_el):
    Ke = np.array([[stiffness_vec[i],-stiffness_vec[i]],[-stiffness_vec[i],stiffness_vec[i]]])
    Ke_array.append(Ke)
# assemble global stiffness
# be careful with numbering between matlab and python
K = np.zeros((n_node,n_node))
# loop over elements
for i in range(n_el):
    K[elements[i,0]-1,elements[i,0]-1] += Ke_array[i][0,0]
    K[elements[i,0]-1,elements[i,1]-1] += Ke_array[i][0,1]
    K[elements[i,1]-1,elements[i,0]-1] += Ke_array[i][1,0]
    K[elements[i,1]-1,elements[i,1]-1] += Ke_array[i][1,1]
print(K)
```

    [[ 18.33333333   0.         -18.33333333]
     [  0.           5.          -5.        ]
     [-18.33333333  -5.          23.33333333]]



```python
# solving the system with matrix partitioning 
n_E = 2
n_F = 1
K_E = K[0:n_E,0:n_E]
K_F = K[n_E:,n_E:]
K_EF = K[0:n_E,n_E:]
print('K_E')
print(K_E)
print('K_F')
print(K_F)
print('K_EF')
print(K_EF)
```

    K_E
    [[ 18.33333333   0.        ]
     [  0.           5.        ]]
    K_F
    [[ 23.33333333]]
    K_EF
    [[-18.33333333]
     [ -5.        ]]



```python
# known displacement
d_E = np.array([0.,0.])
# known external forces
F_F = np.array([50.])
# solving A\b
np.linalg.solve(K_F,F_F- np.dot(K_EF.transpose(),d_E))
```




    array([ 2.14285714])




```python
F = np.zeros((n_node*2))
## add forces 
node_i = 5
F[(node_i-1)*2+0] = # X-force for node i 
F[(node_i-1)*2+1] = # X-force for node i 

F_F = F[n_E:]
```
