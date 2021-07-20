
import sympy as sym
from approx1D import least_squares 
import scipy
import scipy.integrate

def sin_series(N): 
  psi = []
  for k in range(1,N): 
    psi_k = sym.sin(sym.pi*k*x)
    psi.append(psi_k)
  return psi

def taylor_series(N): 
  psi = []
  for k in range(1,N): 
    psi_k = x**k 
    psi.append(psi_k)
  return psi

def series(series_type, N): 
  if series_type=="Taylor" : return taylor_series(N)
  elif series_type=="sin"  : return sin_series(N)
  else: print("series type unknown ") # sys.exit(0)


X = scipy.arange(0, 1, 0.0001)
import pylab
def plot_solution(series_type, N, func): 

  print("series_type ", series_type, " N ", N, " func ", func) 

  psi = series(series_type, N)
  print(psi)
  u, c = least_squares(func, psi, Omega, False) 

  u = sym.lambdify([x], u)
  U = [u(xi) for xi in X]
  pylab.plot(X, U)

def plot_u(u): 

  u = sym.lambdify([x], u)
  U = [u(xi) for xi in X]
  pylab.plot(X, U)



Omega = [0, 1]
x = sym.Symbol("x")
gauss_bell = sym.exp(-(x-0.5)**2) - sym.exp(-0.5**2)
#step = sym.Piecewise( (1, 0.25 < x and x < 0.75), (0, True)  )
step = sym.Piecewise( (1, 0.25 <= x), (0, True)  )- sym.Piecewise( (1, 0.75 < x), (0, True)  )
hat = sym.Piecewise( (x, x <= 0.5), (0, True)  ) + sym.Piecewise( (1-x, 0.5 < x), (0, True)  )

print("hat (0.3) ", hat.subs(x, 0.3))

func=hat

Ns =[5, 10, 15, 20, 25]
#Ns =[2, 3, 4, 5]
for N in Ns: 
#  plot_solution("sin", N, func)
  plot_solution("Taylor", N, func)
legend = ["N=%d" % N for N in Ns]
plot_u(func)
legend.append("analytical solution")
pylab.legend(legend)
pylab.show()
pylab.savefig("step.png", title="sin approximation")






