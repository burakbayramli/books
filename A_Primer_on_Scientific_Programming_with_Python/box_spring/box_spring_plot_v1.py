from box_spring import init_prms, solve
from scitools.std import *

def plot_S(S, t, step):
    if step == 0:       # nothing to plot yet
        return

    tstart = 0
    tstop = N*dt
    tcoor = linspace(0, t, step)

    S = array(S[:len(tcoor)])
    Y = w(tcoor) - L - S - b/2. # (w, L, b are global vars.)

    plot(tcoor, Y)

# Default values
m = 1; b = 2; L = 10; k = 1; beta = 0; S0 = 1;
dt = 2*pi/40; g = 9.81; w_formula = '0'; N = 200; 

m, b, L, k, beta, S0, dt, g, w, N = \
   init_prms(m, b, L, k, beta, S0, dt, g, w_formula, N)

w.vectorize(globals())  # w must work with arrays in plot_S

S = solve(m, k, beta, S0, dt, g, w, N, user_action=plot_S)

# Make hardcopy of last plot of S
hardcopy('tmp_S.eps')  # or savefig('tmp_S.eps')
