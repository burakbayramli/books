# Solves the Roothaan equation F C = S C E for the H2O molecule
from eigsys import *
from matutil import *

n = 13

C = [[0]*(n+1) for i in range(n+1)]                  # molecular coefficients
F = [[0]*(n+1) for i in range(n+1)]                             # Fock matrix
S = [[0]*(n+1) for i in range(n+1)]                          # overlap matrix
S0 = [[0]*(n+1) for i in range(n+1)]          # backup copy of overlap matrix
E = [0]*(n+1)                                                 # energy levels

f = open('H2O.dat','r')                                     # read in F and S
line = f.readline()                                             # skip header
for i in range(1,n+1):                          # read in lower triangle of F
   line = f.readline()                              # F not altered by EigSym
   for j in range(1,i+1):
      F[i][j] = float(line.split()[j-1])
      F[j][i] = F[i][j]                                          # symmetrize

line = f.readline()                                             # skip header
for i in range(1,n+1):
   line = f.readline()
   for j in range(1,i+1):                       # read in lower triangle of S
      S[i][j] = float(line.split()[j-1])              # S destroyed by EigSym
      S0[i][j] = S0[j][i] = S[i][j]                                # backup S
f.close()

print("F: Fock matrix (sample)")
MatPrint(F,5,5)
print("S: overlap matrix (sample)")
MatPrint(S,5,5)

EigSym(F,S,C,E,n)                                   # solve Roothaan equation
EigSort(C,E,n,1)                          # sort eigenvalues and eigenvectors

print("\nEigenenergies:")
VecPrint(E,n)

print("\nHOMO-LUMO gap E[6]-E[5] = {0:4.3f} Hartrees".format(E[6]-E[5]))

err = 0e0
for i in range(1,n+1):
   for j in range(1,n+1):
      t = 0e0
      for k in range(1,n+1): t += (F[i][k] - E[j]*S0[i][k]) * C[k][j]
      if (err < fabs(t)): err = fabs(t)           # err = max|a x - lambda x|

print("\nMaximum error = {0:8.2e}".format(err))
