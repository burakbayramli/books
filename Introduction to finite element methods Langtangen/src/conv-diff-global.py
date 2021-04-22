import sympy as sym
import numpy
import matplotlib.pyplot as plt
import mpmath

def lagrange_series(x, N):
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

def analytical(): 
    eps_values = [1.0, 0.1, 0.01, 0.001]
    for eps in eps_values: 
      x = numpy.arange(Omega[0], Omega[1], 1/((N+1)*100.0))
      ue =  (numpy.exp(-x/eps) - 1)/ (numpy.exp(-1/eps) - 1)
      print(len(x), len(ue))
      plt.plot(x, ue)
    plt.legend(["$\epsilon$=%.1e" % eps for eps in eps_values],
               loc="lower right")
    plt.title("Analytical Solution")
    plt.show()


def bernstein_series(x, N):
    # FIXME: check if a normalization constant is common in the definition
    # advantage is that the basis is always positive
    psi = []
    # for k in range(0,N+1):
    for k in range(1,N):  # bc elsewhere
        psi_k = x**k*(1-x)**(N-k)
        psi.append(psi_k)
    return psi

def sin_series(x, N):
    # FIXME: do not satisfy bc
    psi = []
    for k in range(1,N):
        psi_k = sym.sin(sym.pi*k*x)
        psi.append(psi_k)
    return psi

def series(x, series_type, N):
    if series_type=="sin"  : return sin_series(x, N)
    elif series_type=="Bernstein"  : return bernstein_series(x, N)
    elif series_type=="Lagrange"  : return lagrange_series(x, N)
    else: print("series type unknown ") # sys.exit(0)

def epsilon_experiment(N, series_type, Omega,
                       eps_values = [1.0, 0.1, 0.01, 0.001]):
    # x is global, symbol or array
    psi = series(x, series_type, N)
    f = 1
    for eps in eps_values:
        A = sym.zeros(N-1, N-1)
        b = sym.zeros(N-1)

        for i in range(0, N-1):
            integrand = f*psi[i]
            integrand = sym.lambdify([x], integrand, 'mpmath')
            b[i,0] = mpmath.quad(integrand, [Omega[0], Omega[1]])
            for j in range(0, N-1):
                integrand = eps*sym.diff(psi[i], x)*\
                  sym.diff(psi[j], x) - sym.diff(psi[i], x)*psi[j]
                integrand = sym.lambdify([x], integrand, 'mpmath')
                A[i,j] = mpmath.quad(integrand, [Omega[0], Omega[1]])

        c = A.LUsolve(b)
        u = sum(c[r,0]*psi[r] for r in range(N-1)) + x

        U = sym.lambdify([x], u, modules='numpy')
        x_ = numpy.arange(Omega[0], Omega[1], 1/((N+1)*100.0))
        U_ = U(x_)
        plt.plot(x_, U_)
    plt.legend(["$\epsilon$=%.1e" % eps for eps in eps_values],
               loc="upper left")
    plt.title(series_type)
    plt.show()


if __name__ == '__main__':
    import sys
    if len(sys.argv) > 1:
        series_type = sys.argv[1]
    else:
        series_type = "Bernstein"
    if len(sys.argv) > 2:
        N = int(sys.argv[2])
    else:
        N = 8
    #series_type = "sin"
    #series_type = "Lagrange"

    Omega = [0, 1]
    x = sym.Symbol("x")

    analytical()
    epsilon_experiment(N, series_type, Omega)



