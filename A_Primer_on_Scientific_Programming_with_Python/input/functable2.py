import sys
usage = 'Usage: %s formula name-of-independent-variable '\
        'start stop inc' % sys.argv[0]
if len(sys.argv) < 5:
    print usage; sys.exit(1)

try:
    formula = sys.argv[1]
    start, stop, inc = [float(r) for r in sys.argv[3:6]]
except IndexError, e:
    print e, '\n', usage; sys.exit(1)

from scitools.StringFunction import StringFunction
f = StringFunction(formula)
x = start
while x <= stop:
    print 'x=%-12g f=%-12g' % (x, f(x))
    x += inc

    

     
