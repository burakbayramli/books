#!/usr/bin/env python

def epidemic(T, n, S0, I0, r, a):
    dt = T/float(n)
    t = linspace(0, T, n+1)
    S = zeros(n+1)
    I = zeros(n+1)
    S[0] = S0
    I[0] = I0
    for k in range(n):
        S[k+1] = S[k] - dt*r*S[k]*I[k]
        I[k+1] = I[k] + dt*(r*S[k]*I[k] - a*I[k])
    return S, I, t
    
from scitools.std import *
try:
    n = int(sys.argv[1])
    T = eval(sys.argv[2])
    S0 = eval(sys.argv[3])
    I0 = eval(sys.argv[4])
    r = eval(sys.argv[5])
    a = eval(sys.argv[6])
except:
    print "usage:", sys.argv[0], "n T S0 I0 r a"
    sys.exit(1)

plot(t, S, xlabel='t', ylabel='Susceptibles')
plot(t, I, xlabel='t', ylabel='Infectives')
