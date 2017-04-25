from numpy import linspace, sin, cos, exp, sqrt
import matplotlib.pyplot as plt

def f(x):
    return exp(sqrt(x))*sin(2*x) + cos(x)**5 + 8

a = 0;  b = 7;  n = 200000
dx = float(b-a)/n
eps = 0.001

for i in range(0, n+1, 1):
    x = a + i*dx
    f_value = f(x)
    if abs(f_value) < eps:
        print "x: %f , f_value: %f " % (x, f_value)

x_plot = linspace(a, b, n+1)
plt.plot(x_plot, f(x_plot), 'b-')
plt.xlabel('x')
plt.ylabel('f(x)')
plt.grid('on')
plt.show()
plt.savefig('search_solutions_1eqn.pdf')
