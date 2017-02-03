# just print the command-line arguments
import sys
print 'Program "%s" has %d command-line arguments' % \
      (sys.argv[0], len(sys.argv)-1)
for a in sys.argv:
    print '"%s"' % a, type(a)

