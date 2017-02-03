from scitools.std import *
import time

def f(x, m, s):
    return (1.0/(sqrt(2*pi)*s))*exp(-0.5*((x-m)/s)**2)

m = 0
s_start = 2
s_stop = 0.2
s_values = linspace(s_start, s_stop, 30)
x = linspace(m -3*s_start, m + 3*s_start, 1000)

# make a plot of three s values:
for s in s_start, 1, s_stop:
    plot(x, f(x,m,s))
    hold('on')
    legend('s=%s' %s)
title('A Gaussian Bell Function')
hardcopy('tmp4.eps')

