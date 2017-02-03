#!/usr/bin/env python
import numpy as np
from scitools.std import StringFunction

def Explicit_Euler(f, u0, T, n):
    dt = T/float(n)
    t = np.linspace(0, T, n+1)
    u = np.zeros(n+1)
    u[0] = u0
    for k in range(n):
        u[k+1] = u[k] + dt*f(u[k])
    return u, t

if __name__ == '__main__':
    try:
        f_formula = sys.argv[1]
        n = int(sys.argv[2])
        T = eval(sys.argv[3])
        u0 = eval(sys.argv[4])
    except:
        print "usage:", sys.argv[0], "'f(u)' n T u0 "; sys.exit(1)

    f = StringFunction(f_formula, independent_variables='u')
    u, t = Explicit_Euler(f, u0, T, n)
    plot(t, u)
