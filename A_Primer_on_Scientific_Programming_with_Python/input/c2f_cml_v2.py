import sys
if len(sys.argv) < 2:
    print 'You failed to provide Celsius degrees as input '\
          'on the command line!'
    sys.exit(1)  # abort
C = float(sys.argv[1])
F = 9.0*C/5 + 32
print '%gC is %.1fF' % (C, F)


