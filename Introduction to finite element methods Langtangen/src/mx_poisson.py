from sympy import * 

def bernstein_series(N): 
  # FIXME: check if a normalization constant is common in the definition 
  # advantage is that the basis is always positive 
  psi = []
  for k in range(0,N+1): 
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
  psi = []
  for k in range(1,N): 
    psi_k = x**k 
    psi.append(psi_k)
  return psi

def series(series_type, N): 
  if series_type=="Taylor" : return taylor_series(N)
  elif series_type=="sin"  : return sin_series(N)
  elif series_type=="bernstein"  : return bernstein_series(N)
  else: print("series type unknown ") # sys.exit(0)


N = 10 
M = 8 
series_type = "bernstein"
Omega = [0, 1]
x = Symbol("x")

psi = series(series_type, N)
eta = series(series_type, M)

A = zeros((N+1+M+1), (N+1+M+1))
b = zeros((N+1+M+1))
f = 0 
g = 0 

for i in range(0, N+1):  
  for j in range(0, N+1): 
    integrand = psi[i]*psi[j] 
    A[i,j] = integrate(integrand, (x,Omega[0], Omega[1])) 

for i in range(0, N+1):  
  for j in range(0, M+1): 
    integrand = diff(psi[i],x)*eta[j] 
    A[i,N+1+j] = integrate(integrand, (x,Omega[0], Omega[1])) 
    A[N+1+j,i] = A[i,N+1+j] 


for i in range(0, N+1):  
  integrand = f*psi[i]
  print(integrand)
  b[i,0] = integrate(integrand, (x, Omega[0], Omega[1])) 

for i in range(0, M+1):  
  integrand = g*eta[i]
  print(integrand)
  b[N+1+i,0] = integrate(integrand, (x, Omega[0], Omega[1])) 


   

# bc 
A[0,:] = zeros(1,N+1+M+1)
A[0,0] = 1 
b[0] = 1 

A[N,:] = zeros(1,N+1+M+1)
A[N,N] = 1 
b[N] = 0 


print(A)

#c = A.LUsolve(b)
# FIXME: pinvsolve does not work and there is no info about this function
c = A.pinvsolve(b)
print(c) 
u = sum(c[r,0]*psi[r] for r in range(N+1))

print(u) 


import numpy, pylab 
U = lambdify([x], u)
x = numpy.arange(Omega[0], Omega[1], 1/((N+1)*10.0)) 
UU = U(x)
pylab.plot(UU)
pylab.show()



