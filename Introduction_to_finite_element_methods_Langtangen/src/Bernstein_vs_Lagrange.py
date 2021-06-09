import sympy as sym 
import mpmath
import numpy
import pylab
import time

def lagrange_series(N): 
  psi = []
#  h = Rational(1, N)
  h = 1.0/N
  points = [i*h for i in range(N+1)]
  for i in range(len(points)): 
    p = 1 
    for k in range(len(points)): 
      if k != i:
        p *= (x - points[k])/(points[i] - points[k])
    psi.append(p)
  psi = psi[1:-1]
  return psi

def bernstein_series(N): 
  # FIXME: check if a normalization constant is common in the definition 
  # advantage is that the basis is always positive 
  psi = []
#  for k in range(0,N+1): 
  for k in range(1,N):  # bc elsewhere  
    psi_k = x**k*(1-x)**(N-k)  
    psi.append(psi_k)
  return psi


def sin_series(N): 
  # FIXME: do not satisfy bc  
  psi = []
  for k in range(1,N): 
    psi_k = sin(pi*k*x)
    psi.append(psi_k)
  return psi

def taylor_series(N): 
  # FIXME: do not satisfy bc  
  print("Cannot with current BC implementation") 
  return 
  psi = []
  for k in range(1,N): 
    psi_k = x**k 
    psi.append(psi_k)
  return psi

def series(series_type, N): 
  if series_type=="Taylor" : return taylor_series(N) # cannot do with current implementation of bc
  elif series_type=="sin"  : return sin_series(N)
  elif series_type=="Bernstein"  : return bernstein_series(N)
  elif series_type=="Lagrange"  : return lagrange_series(N)
  else: print("series type unknown ") # sys.exit(0)




x = sym.Symbol("x")
integrand_type = "stiffness"

bstime = [] 
lstime = [] 

bqtime = [] 
lqtime = [] 



Ns = [2, 4, 8, 16, 32]

for N in Ns: 
  t0 = time.time()
  bpsi = series("Bernstein", N)
  A = sym.zeros((N-1), (N-1))
  for i in range(0, N-1):  
    for j in range(0, N-1): 
      integrand = 0 
      if integrand_type == "mass": integrand = bpsi[i]*bpsi[j] 
      if integrand_type == "stiffness": integrand = sym.diff(bpsi[i],x)*sym.diff(bpsi[j],x) 
      integrand = sym.lambdify([x], integrand)
      A[i,j] = mpmath.quad(integrand, [0, 1]) 
  t1 = time.time()
  bqtime.append(t1-t0)

for N in Ns: 
  t0 = time.time()
  lpsi = series("Lagrange", N)
  A = sym.zeros((N-1), (N-1))
  for i in range(0, N-1):  
    for j in range(0, N-1): 
      integrand = 0 
      if integrand_type == "mass": integrand = lpsi[i]*lpsi[j] 
      if integrand_type == "stiffness" : integrand = sym.diff(lpsi[i],x)*sym.diff(lpsi[j],x) 
      integrand = sym.lambdify([x], integrand)
      A[i,j] = mpmath.quad(integrand, [0,1]) 
  t1 = time.time()
  lqtime.append(t1-t0)
 
for N in Ns: 
  t0 = time.time()
  bpsi = series("Bernstein", N)
  A = sym.zeros((N-1), (N-1))
  for i in range(0, N-1):  
    for j in range(0, N-1): 
      integrand = 0 
      if integrand_type == "mass": integrand = bpsi[i]*bpsi[j] 
      if integrand_type == "stiffness": integrand = sym.diff(bpsi[i],x)*sym.diff(bpsi[j],x) 
      A[i,j] = sym.integrate(integrand, (x, [0, 1])) 
  t1 = time.time()
  bstime.append(t1-t0)

for N in Ns: 
  t0 = time.time()
  lpsi = series("Lagrange", N)
  A = sym.zeros((N-1), (N-1))
  for i in range(0, N-1):  
    for j in range(0, N-1): 
      integrand = 0 
      if integrand_type == "mass": integrand = lpsi[i]*lpsi[j] 
      if integrand_type == "stiffness" : integrand = sym.diff(lpsi[i],x)*sym.diff(lpsi[j],x) 
      A[i,j] = sym.integrate(integrand, (x, [0, 1])) 
  t1 = time.time()
  lstime.append(t1-t0)
 
 
print("Berstein quadrature ", bqtime)
print("Lagrange quadrature ", lqtime)
print("Bernstein symbolic ", bstime)
print("Lagrange symbolic ", lstime)

import pylab 
pylab.loglog(Ns, bqtime)
pylab.loglog(Ns, lqtime)
pylab.loglog(Ns, bstime)
pylab.loglog(Ns, lstime)
pylab.loglog(Ns, [4*10**-4*N**2 for N in Ns])
pylab.loglog(Ns, [10**-4*N**4 for N in Ns])
pylab.legend(["Bernstein quad", "Lagrange quad", "Berstein symb", "Lagrange symb", "N**2", "N**4"], loc="upper left") 
pylab.show()



