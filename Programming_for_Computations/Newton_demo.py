"""
This is a program for illustrating the convergence of Newton's method
for solving nonlinear algebraic equations of the form f(x) = 0.

Usage:
python Newton_movie.py f_formula df_formula x0 xmin xmax

where f_formula is a string formula for f(x); df_formula is
a string formula for the derivative f'(x), or df_formula can
be the string 'numeric', which implies that f'(x) is computed
numerically; x0 is the initial guess of the root; and the
x axis in the plot has extent [xmin, xmax].
"""
from Newton import Newton
from scitools.std import *
import matplotlib.pyplot as plt
plt.xkcd()  # cartoon style
import sys

def line(x0, y0, dydx):
    """
    Find a and b for a line a*x+b that goes through (x0,y0)
    and has the derivative dydx at this point.

    Formula:  y = y0 + dydx*(x - x0)
    """
    return dydx, y0 - dydx*x0


def illustrate_Newton(info, f, df, xmin, xmax):
    # First make a plot f for the x values that are in info
    xvalues = linspace(xmin, xmax, 401)
    fvalues = f(xvalues)
    ymin = fvalues.min(); ymax = fvalues.max()
    frame_counter = 0

    # Go through all x points (roots) and corresponding values
    # for each iteration and plot a green line from the x axis up
    # to the point (root,value), construct and plot the tangent at
    # this point, then plot the function curve, the tangent,
    # and the green line,
    # repeat this for all iterations and store hardcopies for making
    # a movie.

    for root, value in info:
        a, b = line(root, value, df(root))
        y = a*xvalues + b
        raw_input('Type CR to continue: ')
        plt.figure()
        plt.plot(xvalues, fvalues, 'r-',
                 [root, root], [ymin, value], 'g-',
                 [xvalues[0], xvalues[-1]], [0,0], 'k--',
                 xvalues, y, 'b-')
        plt.legend(['f(x)', 'approx. root', 'y=0', 'approx. line'])
        plt.axis([xmin, xmax, ymin, ymax])
        plt.title("Newton's method, iter. %d: x=%g; f(%g)=%.3E" % (frame_counter+1, root, root, value))
        plt.savefig('tmp_root_%04d.pdf' % frame_counter)
        plt.savefig('tmp_root_%04d.png' % frame_counter)
        frame_counter += 1

try:
    f_formula = sys.argv[1]
    df_formula = sys.argv[2]
    x0 = float(sys.argv[3])
    xmin = float(sys.argv[4])
    xmax = float(sys.argv[5])
except IndexError:
    print 'f_formula df_formula x0 xmin max'
    sys.exit(1)

# Clean up all plot files
import glob, os
for filename in glob.glob('tmp_*.pdf'):
    os.remove(filename)

f = StringFunction(f_formula)
f.vectorize(globals())
if df_formula == 'numeric':
    # Make a numerical differentiation formula
    h = 1.0E-7
    def df(x):
        return (f(x+h) - f(x-h))/(2*h)
else:
    df = StringFunction(df_formula)
    df.vectorize(globals())
x, info = Newton(f, x0, df, store=True)
illustrate_Newton(info, f, df, xmin, xmax)
plt.show()
