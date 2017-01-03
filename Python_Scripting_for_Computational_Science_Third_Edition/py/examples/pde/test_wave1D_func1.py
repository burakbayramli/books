from wave1D_func1 import solver
from numpy import sin, pi

def I(x):  return sin(2*x*pi/L)
def f(x,t): return 0

solutions = []
time_level_counter = 0
version = 'scalar'
N = int(sys.argv[1])

def action(u, x, t):
    global time_level_counter
    if time_level_counter % N == 0:
        solutions.append(u.copy())
    time_level_counter += 1

n = 100; tstop = 6; L = 10
dt, x, cpu = solver(I, f, 1.0, lambda t: 0, lambda t: 0,
                    L, n, 0, tstop,
                    user_action=action, version=version)
print 'CPU time:', cpu
