from scitools.std import *   # for curve plotting

def f(t):
    return t**2*exp(-t**2)

t = linspace(0, 3, 51)    # 51 points between 0 and 3
y = zeros(len(t), 'd')    # 51 doubles ('d')
for i in xrange(len(t)):
    y[i] = f(t[i])

plot(t, y)

