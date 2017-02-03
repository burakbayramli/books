import sys
s = sum([float(x) for x in sys.argv[1:]])
print 'The sum of %s is %s' % (sys.argv[1:], s)
