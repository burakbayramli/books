from clawpack import riemann
from clawpack.riemann.euler_with_efix_1D_constants \
    import density, momentum, energy, num_eqn

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

def pospart(x):
    return np.maximum(1.e-15,x)

def primitive_to_conservative(rho, u, p, gamma=1.4):
    mom = rho*u
    E   = p/(gamma-1.) + 0.5*rho*u**2
    return rho, mom, E

def conservative_to_primitive(rho, mom, E, gamma=1.4):
    u = mom/pospart(rho)
    p = (gamma-1.)*(E - 0.5*rho*u**2)
    return rho, u, p

def shocktube(q_l, q_r, N=50, riemann_solver='HLL', 
              solver_type='classic'):

    from clawpack import pyclaw
    from clawpack import riemann

    if riemann_solver == 'Roe':
        rs = riemann.euler_1D_py.euler_roe_1D
    elif riemann_solver == 'HLL':
        rs = riemann.euler_1D_py.euler_hll_1D

    if solver_type == 'classic':
        solver = pyclaw.ClawSolver1D(rs)        
        solver.limiters = pyclaw.limiters.tvd.MC
    else:
        solver = pyclaw.SharpClawSolver1D(rs)

    solver.kernel_language = 'Python'
    
    solver.bc_lower[0]=pyclaw.BC.extrap
    solver.bc_upper[0]=pyclaw.BC.extrap

    x = pyclaw.Dimension(-1.0,1.0,N,name='x')
    domain = pyclaw.Domain([x])
    state = pyclaw.State(domain,num_eqn)

    gamma = 1.4
    state.problem_data['gamma']= gamma
    state.problem_data['gamma1']= gamma-1.

    state.problem_data['efix'] = False

    xc = state.grid.p_centers[0]
    
    velocity = (xc<=0)*q_l[1] + (xc>0)*q_r[1]
    pressure = (xc<=0)*q_l[2] + (xc>0)*q_r[2]

    state.q[density ,:] = (xc<=0)*q_l[0] + (xc>0)*q_r[0]
    state.q[momentum,:] = velocity * state.q[density,:]
    state.q[energy  ,:] = pressure/(gamma - 1.) + \
                          0.5 * state.q[density,:] * velocity**2

    claw = pyclaw.Controller()
    claw.tfinal = 0.5
    claw.solution = pyclaw.Solution(state,domain)
    claw.solver = solver
    claw.num_output_times = 10
    claw.keep_copy = True
    claw.verbosity=0

    return claw


N = 80  # number of grid cells to use

prim_l = [1.,0.,1.]
prim_r = [1./8,0.,1./10]
q_l = conservative_to_primitive(*prim_l)
q_r = conservative_to_primitive(*prim_r)

# Roe-based solution
roe_st = shocktube(q_l,q_r,N=N,riemann_solver='Roe')
roe_st.run()
xc_st = roe_st.solution.state.grid.p_centers[0]

# Exact solution
xc_exact_st = np.linspace(-1,1,2000)

def plot_frame(i):
    t = roe_st.frames[i].t
    fig, ax = plt.subplots(3,1, sharex=True, figsize=(8,6))
    variablenames = ["Density", "Momentum", "Energy"]
    variables = [density, momentum, energy]
    ylims = [[0,1.1], [-0.05,0.35], [0,1.1]]
    plt.subplots_adjust(hspace=0)
    ax[0].title.set_text('Solutions at t={:.2f}'.format(t))
    ax[0].set_xlim((-1,1))
    ax[2].set(xlabel = 'x')
    for j, variable in enumerate(variables):
        ax[j].set_ylim(ylims[j])
        ax[j].plot(xc_st,roe_st.frames[i].q[variable,:],'-or',lw=0.5,markersize=3)
        ax[j].legend(['Exact','HLL','Roe'],loc='best')
        ax[j].set(ylabel=variablenames[j])
    plt.show()

plot_frame(10)


