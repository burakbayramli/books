# Hueckel method for benzene
from eigsys import *
from matutil import *

n = 6

H = [[0]*(n+1) for i in range(n+1)]                      # Hamiltonian matrix
C = [[0]*(n+1) for i in range(n+1)]                  # molecular coefficients
E = [0]*(n+1)                                                 # eigenenergies

alpha = -6.9                     # Coulomb & resonance integrals: F. Brogli &
beta = -3.2                      # E. Heilbronner, Theoret. Chim. Acta 1972

for i in range(1,n+1): H[i][i] = alpha                    # diagonal elements
for i in range(1,n):
   H[i+1][i] = beta; H[i][i+1] = beta                 # off-diagonal elements
H[1][6] = beta; H[6][1] = beta                   # cyclic boundary conditions
                                                        # exact eigenenergies
X = (0, alpha+2*beta, alpha+beta, alpha+beta, \
        alpha-beta, alpha-beta, alpha-2*beta )
print("Eigenenergies (exact):")
VecPrint(X,n)

Jacobi(H,C,E,n)
EigSort(C,E,n,1)                          # sort eigenvalues and eigenvectors

print("Eigenenergies (eV):")
VecPrint(E,n)

print("Eigenvextors:")
MatPrint(C,n,n)

for i in range(2,n+1):                                  # restore Hamiltonian
   for j in range(1,i): H[i][j] = H[j][i]               # from upper triangle

err = 0e0
for i in range(1,n+1):
   for j in range(1,n+1):
      t = - E[j] * C[i][j]
      for k in range(1,n+1): t += H[i][k] * C[k][j]
      if (err < fabs(t)): err = fabs(t)           # err = max|a x - lambda x|

print("\nMaximum error = {0:8.2e}".format(err))
