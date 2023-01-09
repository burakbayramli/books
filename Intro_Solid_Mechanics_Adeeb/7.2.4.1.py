import sympy as sp
import numpy as np
from sympy import *
from numpy.linalg import *
from scipy.linalg import *
sp.init_printing(use_latex = "mathjax")
def vonMises(M):
    return sp.sqrt(1/2*((M[0,0] - M[1,1])**2+(M[1,1] - M[2,2])**2+
                       (M[2,2] - M[0,0])**2+6*(M[0,1]**2+M[0,2]**2+M[1,2]**2)))
e = np.array([[0.002, 0.001, 0.002], [0.001, -0.003, 0.001], [0.002, 0.001, 0.0015]])
print("\u03b5 =", e)
eigen = eig(e) # eigen values and vectors
print("eigenvectors and eigenvalues\n",eigen)
# first index is eigen values
print("Eigenvalues (principal strains)= \n",eigen[0])
# Columns are normalized eigenvectors. Transpose is used to convert them into rows
print("Eigenvectors (principal directions) = \n",eigen[1].T)
Ee = 210000
Nu = 0.3
G = Ee/2/(1+Nu)
strainvector = Matrix([e[0,0], e[1,1], e[2,2], 2*e[0,1], 2*e[0,2], 2*e[1,2]])
print("strainvector:", strainvector)
CC = Matrix([[1/Ee, -Nu/Ee, -Nu/Ee, 0, 0, 0],
             [-Nu/Ee, 1/Ee, -Nu/Ee, 0, 0, 0], 
            [-Nu/Ee, -Nu/Ee, 1/Ee, 0, 0, 0],
            [0, 0, 0, 1/G, 0, 0], 
           [0, 0, 0, 0, 1/G, 0],
             [0, 0, 0, 0, 0, 1/G]])
print("CC: ",CC)
Dd = CC.inv()
print("D: ", Dd)
stressvector = Dd * strainvector
print("stressvector: ", stressvector)
stressmatrix = Matrix([[stressvector[0], stressvector[3], stressvector[4]], 
                       [stressvector[3], stressvector[1], stressvector[5]],
                       [stressvector[4], stressvector[5], stressvector[2]]])
print("stressmatrix: ", stressmatrix)
#convert the stress matrix to a numpy object for faster calculations of the eigenvalues and eigenvectors.
stressmatrix=np.array(stressmatrix).astype(np.float64)
eigen = eig(stressmatrix) # eigen values and vectors
print("eigenvectors and eigenvalues\n",eigen)
# first index is eigen values
print("Eigenvalues (principal stresses)= \n",eigen[0])
# Columns are normalized eigenvectors. Transpose is used to convert them into rows
print("Eigenvectors (principal directions) = \n",eigen[1].T)
vonMises(stressmatrix)
print("vonMises stress: ", vonMises(stressmatrix))
