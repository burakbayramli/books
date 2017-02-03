import sys
from math import *
from scitools.std import *
usage = 'Arguments: k m x0 force-formula tstart tstop dt'
print sys.argv, len(sys.argv)
if len(sys.argv) < 7:
    print usage; sys.exit(1)

try:
    k = float(sys.argv[1])
    m = float(sys.argv[2])
    x0 = float(sys.argv[3])
    force_formula = sys.argv[4]
    tstart = float(sys.argv[5])
    tstop = float(sys.argv[6])
    dt = float(sys.argv[7])
except IndexError, e:
    print e, '\n', usage; sys.exit(1)

if force_formula != '0':
    from scitools.StringFunction import StringFunction
    F = StringFunction(formula)
else:
    F = None

print F

t = tstart
x = [x0, x0]
while t <= tstop:
    t += dt
    if F is not None:
        try:
            F_value = F(t)
        except:
            raise ValueError('Could not evaluate %s for t=%g' \
                             % (force_formula, t))
    else:
        F_value = 0

    x_new = - x[-2] + 2*x[-1] + dt**2*(1/m)*(F_value - k*x[-1])
    x.append(x_new)

    if F is None:
        exact = x0*cos(sqrt(k/m)*t)
        print '%12g  %14.6e  %14.6e' % (t, x[-1], exact)
    else:
        print '%12g  %14.6e' % (t, x[-1])

    
    # Visualize computations
    tcoor = linspace(tstart, t, len(x))
    if F is None:
        exact = x0*cos(sqrt(k/m)*tcoor)
        plot(tcoor, exact, tcoor, x, axis=[tstart, tstop, -x0, x0])
    else:
        plot(tcoor, x, axis=[tstart, tstop, -x0, x0])

     
