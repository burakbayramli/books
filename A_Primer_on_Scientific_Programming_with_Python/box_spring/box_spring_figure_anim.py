from box_spring import init_prms, solve
import sys, time, os
# make sure that we find the box_spring_figure module in ../oo:
#sys.path.insert(0, '../oo')
# better:
sys.path.insert(0, os.path.join(os.pardir, 'oo'))
from box_spring_figure import draw, set_figure_size

def animate_figure(S, t, step):
    draw(L, w(t+dt), S[step], box_size)
    time.sleep(0.1)

# Default values
from math import pi
m = 1; b = 2; L = 10; k = 1; beta = 0; S0 = 1;
dt = 2*pi/40; g = 9.81; w_formula = '0'; N = 80;
box_size = 4
w_max = 0
S_max = 15
set_figure_size(L, w_max, S_max, box_size)

m, b, L, k, beta, S0, dt, g, w, N = \
   init_prms(m, b, L, k, beta, S0, dt, g, w_formula, N)

S = solve(m, k, beta, S0, dt, g, w, N,
          user_action=animate_figure)
