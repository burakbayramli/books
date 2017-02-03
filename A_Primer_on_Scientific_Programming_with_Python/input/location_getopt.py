# set default values:
s0 = v0 = 0; a = t = 1
import getopt, sys
options, args = getopt.getopt(sys.argv[1:], '', 
  ['v0=', 'initial_velocity=', 't=', 'time=',
   's0=', 'initial_position=', 'a=', 'acceleration='])

for option, value in options:
    if option in ('--t', '--time'):
        t = float(value)
    elif option in ('--a', '--acceleration'):
        a = float(value)
    elif option in ('--v0', '--initial_velocity'):
        v0 = float(value)
    elif option in ('--s0', '--initial_position'):
        s0 = float(value)

s = s0 + v0*t + 0.5*a*t**2
print """
An object, starting at s=%g at t=0 with initial
velocity %s m/s, and subject to a constant
acceleration %g m/s**2, is found at the
location s=%g m after %s seconds.
""" % (s0, v0, a, s, t)

