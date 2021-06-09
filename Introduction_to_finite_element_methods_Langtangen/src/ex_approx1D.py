"""
Examples on approximating functions by global basis functions,
using the approx1D.py module.
"""
from approx1D import *
from Lagrange import *
import matplotlib.pyplot as plt
#import scitools.std as plt
import sympy as sym
import sys
x = sym.Symbol('x')
np.random.seed(2)


def sines(x, N):
    return [sym.sin(sym.pi*(i+1)*x) for i in range(N+1)]

def cosines(x, N):
    return [sym.cos(sym.pi*i*x) for i in range(N+1)]

def sines_cosines(x, N):
    c = [sym.cos(sym.pi*i*x) for i in range(N+1)]
    s = [sym.sin(sym.pi*i*x) for i in range(1, N+1)]
    return c + s

def taylor(x, N):
    return [x**i for i in range(N+1)]


# ----------------------------------------------------------------------

def run_parabola_by_linear_leastsq():
    f = 10*(x-1)**2 - 1
    psi = [1, x]
    Omega = [1, 2]
    u, c = least_squares(f, psi, Omega)
    comparison_plot(f, u, Omega, 'parabola_ls_linear')

def run_parabola_by_taylor_leastsq_illconditioning(N=2):
    """
    Test Taylor approx to a parabola and exact symbolics vs
    ill-conditioned numerical approaches.
    """
    f = 10*(x-1)**2 - 1
    u, c = least_squares(f, psi=[x**i for i in range(N+1)], Omega=[1, 2])
    # Note: in least_squares there is extra code for numerical solution
    # of the systems
    print('f:', sym.expand(f))
    print('u:', sym.expand(u))
    comparison_plot(f, u, [1, 2], 'parabola_ls_taylor%d' % N)

def run_parabola_by_sines_leastsq(boundary_term=False):
    for N in (4, 12):
        f = 10*(x-1)**2 - 1
        psi = sines(x, N)
        Omega = [0, 1]
        if boundary_term:
            f0 = 9; f1 = -1
            b = f0*(1-x) + x*f1
            u, c = least_squares_orth(f-b, psi, Omega)
            u = u + b
        else:
            u, c = least_squares_orth(f, psi, Omega)
        plt.figure()
        comparison_plot(f, u, Omega, 'parabola_ls_sines%d%s' %
                        (N, '_wfterm' if boundary_term else ''))


def run_sin_by_powers(N):
    f = sym.sin(x)
    psi = taylor(x, N)
    Omega=[0, 2*sym.pi]
    u, c = least_squares(f, psi, Omega)
    comparison_plot(f, u, Omega)

def run_Lagrange_poly(N):
    # Test of symbolic and numeric evaluation of Lagrange polynomials
    x = sym.Symbol('x')
    psi, points = Lagrange_polynomials_01(x, N)
    print(psi)
    print(points)
    x = 0.5
    psi, points = Lagrange_polynomials_01(x, N)
    print(psi)
    print(points)


def run_sin_by_Lagrange_leastsq(N, ymin=-1.2, ymax=1.2):
    # Least-squares use of Lagrange polynomials
    f = sym.sin(2*sym.pi*x)
    psi, points = Lagrange_polynomials_01(x, N)
    Omega=[0, 1]
    u, c = least_squares(f, psi, Omega)
    comparison_plot(f, u, Omega, filename='Lagrange_ls_sin_%d' % (N+1),
                    plot_title='Least squares approximation by '\
                    'Lagrange polynomials of degree %d' % N,
                    ymin=ymin, ymax=ymax)



def run_abs_by_Lagrange_leastsq(N):
    """Least-squares with of Lagrange polynomials for |1-2x|."""
    f = abs(1-2*x)
    # This f will lead to failure of sympy integrate, fallback on numerical int.
    psi, points = Lagrange_polynomials_01(x, N)
    Omega=[0, 1]
    u, c = least_squares(f, psi, Omega, False)
    comparison_plot(f, u, Omega, filename='Lagrange_ls_abs_%d' % (N+1),
                    plot_title='Least squares approximation by '\
                    'Lagrange polynomials of degree %d' % N)


def run_parabola_by_linear_interp1():
    f = 10*(x-1)**2 - 1
    psi = [1, x]
    Omega = [1, 2]
    points = [1 + sym.Rational(1,3), 1 + sym.Rational(2,3)]
    u, c = interpolation(f, psi, points)
    comparison_plot(f, u, Omega, 'parabola_interp1_linear')


def run_parabola_by_linear_interp2():
    # as run_parabola_by_linear_interp1, but other interpolation points
    f = 10*(x-1)**2 - 1
    psi = [1, x]
    Omega = [1, 2]
    points = [1, 2]
    u, c = interpolation(f, psi, points)
    comparison_plot(f, u, Omega, 'parabola_interp2_linear')

def run_parabola_by_quadratic_interp():
    f = 10*(x-1)**2 - 1
    psi = [1, x, x**2]
    Omega = [1, 2]
    points = [1, 1.2, 2]
    u, c = interpolation(f, psi, points)
    comparison_plot(f, u, Omega, 'parabola_interp3_quadratic')


def run_sin_by_poly_interp(N):
    f = sym.sin(sym.pi*x)
    psi = taylor(x, N)
    Omega = [1, 2]
    points = np.linspace(1, 2, N+1)
    u, c = interpolation(f, psi, points)
    comparison_plot(f, u, Omega, 'sin_interp_poly%d' % N)

def run_parabola_by_linear_regression():
    f = 10*(x-1)**2 - 1
    psi = [1, x]
    Omega = [1, 2]
    mp1_values = [2, 8, 32]  # values of m+1
    # Create m+3 points and use the inner m+1 points
    for mp1 in mp1_values:
        points = np.linspace(Omega[0], Omega[1], mp1+2)[1:-1]
        u, c = regression(f, psi, points)
        comparison_plot(
            f, u, Omega,
            filename='parabola_by_regression_%d' % mp1,
            points=points,
            points_legend='%d interpolation points' % mp1,
            legend_loc='upper left')

def run_noisy_parabola_by_linear_regression():
    """Demonstrate standard statistical linear regression with noise data."""
    f_formula = 10*(x-1)**2 - 1
    f_func = sym.lambdify([x], f_formula, modules='numpy')
    sigma = 0.6  # Add normal noise with this std to f
    psi = [1, x]
    Omega = [1, 2]
    mp1_values = [4, 8, 32]  # values of m+1
    # Create m+3 points and use the inner m+1 points
    for mp1 in mp1_values:
        points = np.linspace(Omega[0], Omega[1], mp1+2)[1:-1]
        f_data = f_func(points) + \
                 np.random.normal(0, sigma, points.size)
        u, c = regression_with_noise(f_data, psi, points)
        comparison_plot(
            f_formula, u, Omega,
            filename='noisy_parabola_by_linear_regression_%d' % mp1,
            points=points, point_values=f_data,
            points_legend='%d data points' % mp1,
            legend_loc='upper left')

def run_noisy_parabola_by_quadratic_regression():
    """Demonstrate standard statistical linear regression with noise data."""
    f_formula = 10*(x-1)**2 - 1
    f_func = sym.lambdify([x], f_formula, modules='numpy')
    sigma = 0.6  # Add normal noise with this std to f
    psi = [1, x, x**2]
    Omega = [1, 2]
    mp1_values = [4, 8, 32]  # values of m+1
    # Create m+3 points and use the inner m+1 points
    for mp1 in mp1_values:
        points = np.linspace(Omega[0], Omega[1], mp1+2)[1:-1]
        f_data = f_func(points) + \
                 np.random.normal(0, sigma, points.size)
        u, c = regression_with_noise(f_data, psi, points)
        comparison_plot(
            f_formula, u, Omega,
            filename='noisy_parabola_by_quadratic_regression_%d' % mp1,
            points=points, point_values=f_data,
            points_legend='%d data points' % mp1,
            legend_loc='upper left')


def run_sin_by_Lagrange_interp_(N, ymin=-1.2, ymax=1.2):
    f = sym.sin(2*sym.pi*x)
    psi, points = Lagrange_polynomials_01(x, N)
    u, c = interpolation(f, psi, points)
    comparison_plot(f, u, Omega=[0, 1],
                    filename='Lagrange_interp_sin_%d' % (N+1),
                    plot_title='Interpolation by Lagrange polynomials '\
                    'of degree %d' % N,
                    ymin=ymin, ymax=ymax)

def run_poly_by_Lagrange_interp_(n, N):
    f = x**n
    psi, points = Lagrange_polynomials_01(x, N)
    u, c = interpolation(f, psi, points)
    comparison_plot(f, u, Omega=[0, 1],
                    filename='Lagrange_interp_p%d_%d' % (n, N+1),
                    plot_title='Interpolation by Lagrange polynomials '\
                    'of degree %d' % N)

def run_abs_by_Lagrange_interp_(N, ymin=None, ymax=None):
    f = abs(1-2*x)
    psi, points = Lagrange_polynomials_01(x, N)
    u, c = interpolation(f, psi, points)
    comparison_plot(f, u, Omega=[0, 1],
                    filename='Lagrange_interp_abs_%d' % (N+1),
                    plot_title='Interpolation by Lagrange polynomials '\
                    'of degree %d' % N, ymin=ymin, ymax=ymax)
    # Make figures of Lagrange polynomials (psi)
    plt.figure()
    xcoor = np.linspace(0, 1, 1001)
    legends = []
    for i in (2, (N+1)//2+1):
        fn = sym.lambdify([x], psi[i])
        ycoor = fn(xcoor)
        plt.plot(xcoor, ycoor)
        legends.append(r'$\psi_%d$' % i)
        plt.hold('on')
    plt.legend(legends)
    plt.plot(points, [0]*len(points), 'ro')
    #if ymin is not None and ymax is not None:
    #    axis([xcoor[0], xcoor[-1], ymin, ymax])
    plt.savefig('Lagrange_basis_%d.pdf' % (N+1))
    plt.savefig('Lagrange_basis_%d.png' % (N+1))

def run_abs_by_Lagrange_interp__Cheb(N, ymin=None, ymax=None):
    f = sym.Abs(1-2*x)
    fn = sym.lambdify([x], f)
    psi, points= Lagrange_polynomials(x, N, [0, 1],
                                      point_distribution='Chebyshev')
    u, c = interpolation(f, psi, points)
    comparison_plot(f, u, Omega=[0, 1],
                    filename='Lagrange_interp_abs_Cheb_%d' % (N+1),
                    plot_title='Interpolation by Lagrange polynomials '\
                    'of degree %d' % N, ymin=ymin, ymax=ymax)
    print('Interpolation points:', points)

    # Make figures of Lagrange polynomials (psi)
    plt.figure()
    xcoor = np.linspace(0, 1, 1001)
    legends = []
    for i in (2, (N+1)//2+1):
        fn = sym.lambdify([x], psi[i])
        ycoor = fn(xcoor)
        plt.plot(xcoor, ycoor)
        legends.append(r'$\psi_%d$' % i)
        plt.hold('on')
    plt.legend(legends)
    plt.plot(points, [0]*len(points), 'ro')
    #if ymin is not None and ymax is not None:
    #    axis([xcoor[0], xcoor[-1], ymin, ymax])
    plt.savefig('Lagrange_basis_Cheb_%d.pdf' % (N+1))
    plt.savefig('Lagrange_basis_Cheb_%d.png' % (N+1))

def run_abs_by_Lagrange_interp__conv(N=[3, 6, 12, 24]):
    f = sym.Abs(1-2*x)
    f = sym.sin(2*sym.pi*x)
    fn = sym.lambdify([x], f, modules='numpy')
    resolution = 50001
    xcoor = np.linspace(0, 1, resolution)
    fcoor = fn(xcoor)
    Einf = []
    E2 = []
    h = []
    for _N in N:
        psi, points = Lagrange_polynomials_01(x, _N)
        u, c = interpolation(f, psi, points)
        un = sym.lambdify([x], u, modules='numpy')
        ucoor = un(xcoor)
        e = fcoor - ucoor
        Einf.append(e.max())
        E2.append(np.sqrt(np.sum(e*e/e.size)))
        h.append(1./_N)
    print(Einf)
    print(E2)
    print(h)
    print(N)
    # Assumption: error = CN**(-N)
    print('convergence rates:')
    for i in range(len(E2)):
        C1 = E2[i]/(N[i]**(-N[i]/2))
        C2 = Einf[i]/(N[i]**(-N[i]/2))
        print(N[i], C1, C2)
    # Does not work properly...


if __name__ == '__main__':
    # Run from command line:
    # python ex_approx1D.py run_parabola_by_linear_regression
    cmd = sys.argv[1]
    args = ''
    if len(sys.argv) > 2:
        args = ','.join(sys.argv[2:])
    eval('{0}({1})'.format(cmd, args))
