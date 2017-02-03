# set default values:
s0 = v0 = 0; a = t = 1
import argparse
parser = argparse.ArgumentParser()
parser.add_argument('--v0', '--initial_velocity', type=float,
                    default=0.0, help='initial velocity')
parser.add_argument('--s0', '--initial_position', type=float,
                    default=0.0, help='initial position')
parser.add_argument('--a', '--acceleration', type=float,
                    default=1.0, help='acceleration')
parser.add_argument('--t', '--time', type=float,
                    default=1.0, help='time')

args = parser.parse_args()
    
s0 = args.s0; v0 = args.v0; a = args.a; t = args.t
s = s0 + v0*t + 0.5*a*t**2
print """
An object, starting at s=%g at t=0 with initial
velocity %s m/s, and subject to a constant
acceleration %g m/s**2, is found at the
location s=%g m after %s seconds.
""" % (s0, v0, a, s, t)

