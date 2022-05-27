import numpy as np
from collections import namedtuple
import matplotlib.pyplot as plt

conserved_variables = ('Density', 'Momentum', 'Energy')
primitive_variables = ('Density', 'Velocity', 'Pressure')
Primitive_State = namedtuple('State', primitive_variables)
Conserved_State = namedtuple('State', conserved_variables)

def primitive_to_conservative(rho, u, p, gamma=1.4):
    mom = rho*u
    E   = p/(gamma-1.) + 0.5*rho*u**2
    return rho, mom, E

def pospart(x):
    return np.maximum(1.e-15,x)

def conservative_to_primitive(rho, mom, E, gamma=1.4):
    u = mom/pospart(rho)
    p = (gamma-1.)*(E - 0.5*rho*u**2)
    return rho, u, p

def cons_to_prim(q, gamma=1.4):
    return conservative_to_primitive(*q,gamma=1.4)

def roe_averages(q_l, q_r, gamma=1.4):
    rho_sqrt_l = np.sqrt(q_l[0])
    rho_sqrt_r = np.sqrt(q_r[0])
    p_l = (gamma-1.)*(q_l[2]-0.5*(q_l[1]**2)/q_l[0])
    p_r = (gamma-1.)*(q_r[2]-0.5*(q_r[1]**2)/q_r[0])
    denom = rho_sqrt_l + rho_sqrt_r
    u_hat = (q_l[1]/rho_sqrt_l + q_r[1]/rho_sqrt_r)/denom
    H_hat = ((q_l[2]+p_l)/rho_sqrt_l + (q_r[2]+p_r)/rho_sqrt_r)/denom
    c_hat = np.sqrt((gamma-1)*(H_hat-0.5*u_hat**2))
    
    return u_hat, c_hat, H_hat
        
def Euler_hlle(q_l, q_r, gamma=1.4):
    """HLLE approximate solver for the Euler equations."""
    
    rho_l = q_l[0]
    rhou_l = q_l[1]
    u_l = rhou_l/rho_l
    rho_r = q_r[0]
    rhou_r = q_r[1]
    u_r = rhou_r/rho_r
    E_r = q_r[2]
    E_l = q_l[2]
    
    u_hat, c_hat, H_hat = roe_averages(q_l, q_r, gamma)
    p_r = (gamma-1.) * (E_r - rho_r*u_r**2/2.)
    p_l = (gamma-1.) * (E_l - rho_l*u_l**2/2.)
    H_r = (E_r+p_r) / rho_r
    H_l = (E_l+p_l) / rho_l
    c_r = np.sqrt((gamma-1.)*(H_r-u_r**2/2.))
    c_l = np.sqrt((gamma-1.)*(H_l-u_l**2/2.))
    
    s1 = min(u_l-c_l,u_hat-c_hat)
    s2 = max(u_r+c_r,u_hat+c_hat)
    
    rho_m = (rhou_r - rhou_l - s2*rho_r + s1*rho_l)/(s1-s2)
    rhou_m = (rho_r*u_r**2 - rho_l*u_l**2 \
              + p_r - p_l - s2*rhou_r + s1*rhou_l)/(s1-s2)
    E_m = ( u_r*(E_r+p_r) - u_l*(E_l+p_l) - s2*E_r + s1*E_l)/(s1-s2)
    q_m = np.array([rho_m, rhou_m, E_m])
    
    states = np.column_stack([q_l,q_m,q_r])
    speeds = [s1, s2]
    wave_types = ['contact','contact']
    
    def reval(xi):
        rho  = (xi<s1)*rho_l + (s1<=xi)*(xi<=s2)*rho_m + (s2<xi)*rho_r
        mom  = (xi<s1)*rhou_l + (s1<=xi)*(xi<=s2)*rhou_m + (s2<xi)*rhou_r
        E  = (xi<s1)*E_l + (s1<=xi)*(xi<=s2)*E_m + (s2<xi)*E_r
        return rho, mom, E

    return states, speeds, reval, wave_types

def compare_solutions(left, right):
    q_l = np.array(primitive_to_conservative(*left))
    q_r = np.array(primitive_to_conservative(*right))

    states, speeds, reval, wave_types = Euler_hlle(q_l, q_r)
    xmax = 0.9
    x = np.linspace(-xmax, xmax, 100)
    t = 0.7 # change this value to see graph at different time points
    q = reval(x/(t+1e-10))
    q = cons_to_prim(q)

    fig, axes = plt.subplots(3, 1, figsize=(5, 6), sharex=True)
    axes[0].plot(x,q[0]); axes[0].set_ylim(0,4)
    axes[1].plot(x,q[1]); axes[0].set_ylim(0,1)
    axes[2].plot(x,q[2]); axes[0].set_ylim(0,4)
    
    plt.savefig('/tmp/sod-out.png')


left  = Primitive_State(Density = 3.,
              Velocity = 0.,
              Pressure = 3.)
right = Primitive_State(Density = 1.,
              Velocity = 0.,
              Pressure = 1.)

compare_solutions(left, right)

