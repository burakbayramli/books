from nonlinear_solvers import bisection, Newton, secant, rate

def f(x):
    return x**2 - 9

def dfdx(x):
    return 2*x

def print_rates(method, x, x_exact):
    q = ['%.2f' % q_ for q_ in rate(x, x_exact)]
    print method + ':'
    for q_ in q:
        print q_,
    print

eps = 1e-6

x, it = Newton(f, dfdx, 1000, eps, return_x_list=True)
print_rates('Newton', x, 3)

x0 = 1000; x1 = x0 - 1
x, it = secant(f, x0, x1, eps, return_x_list=True)
print_rates('Secant', x, 3)

# The error model does not work well for Bisection when
# the solution is approached
x, it = bisection(f, 0, 1000, eps, return_x_list=True)
print_rates('Bisection', x, 3)
#e = [abs(x_-3) for x_ in x]
#print [e[i+1]/e[i] for i in range(len(e)-1)]



