# derive the Riemann initial conditions for a slow moving shock from the
# shock jump conditions.
#
# We'll consider a right moving shock.  We know that the shock speed is
# 
# S = u_r + c_r [ (pstar/p_r) (gamma+1)/(2 gamma) + (gamma-1)/(2 gamma) ]^(1/2)
#
# We can pick a (pstar/p_r) and a rho_r, which then defines c_r.  By setting
# S = 0, we find the necessary u_r.  We can then use the remaining jump
# conditions to find the post-shock state.

import math

p_r = 1.0
rho_r = 1.0
gamma = 1.4

c_r = math.sqrt(gamma*p_r/rho_r)

pstar_jump = 100.0

# set shock speed, S = 0
u_r = -c_r*math.sqrt((pstar_jump)*(gamma+1.0)/(2.0*gamma) + 
                     (gamma-1.0)/(2.0*gamma))

# now use the jump conditions to find the post-shock state (star)
rho_star = rho_r * (pstar_jump*(gamma+1.0) + (gamma-1.0)) / \
           ((gamma+1.0) + pstar_jump*(gamma-1.0))

p_star = pstar_jump * p_r

u_star = u_r - c_r * math.sqrt(2.0/(gamma*(gamma-1.0))) * (1.0 - pstar_jump) / \
         math.sqrt(pstar_jump*(gamma+1.0)/(gamma-1.0) + 1.0)

print("left state:  rho_l = {}, u_l = {}, p_l = {}".format(rho_star, u_star, p_star))

print("right state: rho_r = {}, u_r = {}, p_r = {}".format(rho_r, u_r, p_r))



