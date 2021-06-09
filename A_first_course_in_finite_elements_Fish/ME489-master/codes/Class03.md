

```python
import numpy as np
import copy
```


```python
## Element stiffness matrix

## element 1
ke1 = 1.0
Ke1 = ke1*np.array([[1,-1],[-1,1]])

## element 1
ke2 = 2.0
Ke2 = ke2*np.array([[1,-1],[-1,1]])

```


```python
## Global stiffness matrix 

## approach 1, gather matrices 
Le1 = np.array([[0,0,1],[0,1,0]])
Le2 = np.array([[0,1,0],[1,0,0]])

K = np.dot(Le1.transpose(),np.dot(Ke1,Le1)) + np.dot(Le2.transpose(),np.dot(Ke2,Le2))
print(K)

## approach 2, direct assembly
# connectivity array 
elements = np.array([[3,2],[2,1]])
# array of stiffness matrices
Ke_array = [Ke1,Ke2]
n_el = 2
# be careful with numbering between matlab and python
K_v2 = np.zeros((3,3))
for i in range(n_el):
    K_v2[elements[i,0]-1,elements[i,0]-1] += Ke_array[i][0,0]
    K_v2[elements[i,0]-1,elements[i,1]-1] += Ke_array[i][0,1]
    K_v2[elements[i,1]-1,elements[i,0]-1] += Ke_array[i][1,0]
    K_v2[elements[i,1]-1,elements[i,1]-1] += Ke_array[i][1,1]
print(K_v2)

## approach 3, scatter matrices and assemble
# difficult to program unless nodes are consecutively number for an element
# ends up looking similar to the direct assembly 
```

    [[ 2. -2.  0.]
     [-2.  3. -1.]
     [ 0. -1.  1.]]
    [[ 2. -2.  0.]
     [-2.  3. -1.]
     [ 0. -1.  1.]]



```python
## solving also different approaches

## approach 1, get rid of one equation
Kmod = copy.deepcopy(K)
Kmod[0,0]=1
Kmod[0,1]=0
f1 = 10
f2 = 5
u1bar = 1
F = np.array([1,10,5])
```


```python
np.linalg.solve(Kmod,F)
```




    array([  1. ,   8.5,  13.5])




```python
## approach 1.2, you can also modify the correspondng column 
F[1] -= u1bar*Kmod[1,0]
Kmod[1,0] = 0
np.linalg.solve(Kmod,F)
```




    array([  1. ,   8.5,  13.5])




```python
## approach 2, penalty method 
Kmod2 = copy.deepcopy(K)
beta = 1e7*(Kmod2[0,0]+Kmod2[1,1]+Kmod2[2,2])/3.
Kmod2[0,0] = beta
F = np.array([beta*u1bar,10,5])
np.linalg.solve(Kmod2,F)
```




    array([  1.00000085,   8.50000085,  13.50000085])




```python
## approach 3, block matrices
K_F = K[1:3,1:3]
F_F = np.array([10,5])
K_EF = K[0,1:3]
np.linalg.solve(K_F,F_F-u1bar*K_EF)
```




    array([  8.5,  13.5])


