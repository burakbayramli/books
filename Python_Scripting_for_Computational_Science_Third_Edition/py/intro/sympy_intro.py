from sympy import *

# some quick examples:
x = Symbol('x')
print cos(acos(x))
dcos = diff(cos(2*x), x)
dcos
print dcos
print dcos.subs(x, pi).evalf()  # x=pi, float evaluation
S = sin(x).series(x, 4)
print S
I = integrate(log(x), x)
print I

# a longer example with a function and its derivatives:
def make_symbols(*args):
    return [Symbol(s) for s in args]

a, A, omega, sigma, m, t = \
   make_symbols('a', 'A', 'omega', 'sigma', 'm', 't')

f = A*exp(-((x-m)/(2*sigma))**2)*exp(-a*t)*sin(2*pi*omega*x)
prms = {'A': 1, 'a': 0.1, 'm': 1, 'sigma': 1,
        'omega': 1, 't': 0.2}


df = diff(f, x)
ddf = diff(f, x, 2)

# turn formulas into string expressions:
from scitools.StringFunction import StringFunction
f = StringFunction(str(f), **prms)
df = StringFunction(str(df), **prms)
ddf = StringFunction(str(ddf), **prms)

print '\n\nf:', f
print repr(f)
print "\n\nf's lambda:", f._lambda  # this is what is exec'ed
print 'df:', df
print repr(df)
print df._lambda
print '\n\nddf:',
print ddf
print repr(ddf)
print ddf._lambda

x = 0.1
print 'ddf(x=%g)=%g' % (x, ddf(x))
