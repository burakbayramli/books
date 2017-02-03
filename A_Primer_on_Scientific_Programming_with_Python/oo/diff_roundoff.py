"""
Make a table of h versus size of a function to investigate
round-off errors for a forward difference formula.
"""
def f(x):
    return a + x

# Allow a command-line formula involving a instead, e.g., exp(a*x)
import sys
from math import *
if len(sys.argv) > 1:
    formula = sys.argv[1]
    def f(x):
        return eval(formula)

x = 2
a_values = [10**i for i in range(6)]
h_values = [10**(-2*i) for i in range(8)]
print 'a=       ',
for a in a_values:  # print heading
    print '%10.0f' % a,
print
for h in h_values:
    print 'h=%7.1e' % h,
    for a in a_values:
        df = (f(x+h) - f(x))/float(h)
        print '%10.3e' % df,
    print
    

