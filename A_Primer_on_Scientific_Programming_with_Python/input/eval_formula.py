formula = raw_input('Give a formula involving x: ')
x = eval(raw_input('Give x: '))
from math import *   # make all math functions available
result = eval(formula)
print '%s for x=%g yields %g' % (formula, x, result)

