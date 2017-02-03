from Euler import Explicit_Euler
from scitools.std import *

u0 = 0
T = 1
def f(u):
    return exp(u)

for n in 5, 10, 20, 100:
    u, t = Explicit_Euler(f, u0, T, n)
    plot(u, t)
    hold('on')
    legend('n=%d' % n)
title("Explicit Euler for the differential equation u'=e^u "\
      "using different values for n.")
t = linspace(0, T, n+1)
