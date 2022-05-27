# Documents/repos/clawpack/pyclaw/src/pyclaw/classic/solver.py
import numpy as np
import matplotlib.pyplot as plt

density, momentum, energy = 0,1,2

num_eqn = 3

def roe_averages(q_l,q_r,problem_data):
    # Solver parameters
    gamma1 = problem_data['gamma1']

    # Calculate Roe averages
    rhsqrtl = np.sqrt(q_l[0,...])
    rhsqrtr = np.sqrt(q_r[0,...])
    pl = gamma1 * (q_l[2,...] - 0.5 * (q_l[1,...]**2) / q_l[0,...])
    pr = gamma1 * (q_r[2,...] - 0.5 * (q_r[1,...]**2) / q_r[0,...])
    rhsq2 = rhsqrtl + rhsqrtr
    u = (q_l[1,...] / rhsqrtl + q_r[1,...] / rhsqrtr) / rhsq2
    enthalpy = ((q_l[2,...] + pl) / rhsqrtl + (q_r[2,...] + pr) / rhsqrtr) / rhsq2
    a = np.sqrt(gamma1 * (enthalpy - 0.5 * u**2))

    return u, a, enthalpy, pl, pr

def euler_roe_1D(q_l,q_r,aux_l,aux_r,problem_data):
    r"""
    Roe Euler solver in 1d
    *aug_global* should contain -
     - *gamma* - (float) Ratio of the heat capacities
     - *gamma1* - (float) :math:`1 - \gamma`
     - *efix* - (bool) Whether to use an entropy fix or not
    See :ref:`pyclaw_rp` for more details.
    :Version: 1.0 (2009-6-26)
    """

    # Problem dimensions
    num_rp = q_l.shape[1]
    num_waves = 3

    # Return values
    wave = np.empty( (num_eqn, num_waves, num_rp) )
    s = np.empty( (num_waves, num_rp) )
    amdq = np.zeros( (num_eqn, num_rp) )
    apdq = np.zeros( (num_eqn, num_rp) )

    # Solver parameters
    gamma1 = problem_data['gamma1']

    # Calculate Roe averages
    u, a, enthalpy = roe_averages(q_l,q_r,problem_data)[0:3]

    # Find eigenvector coefficients
    delta = q_r - q_l
    a2 = gamma1 / a**2 * ((enthalpy -u**2)*delta[0,...] + u*delta[1,...] - delta[2,...])
    a3 = (delta[1,...] + (a-u) * delta[0,...] - a*a2) / (2.0*a)
    a1 = delta[0,...] - a2 - a3

    # Compute the waves
    wave[0,0,...] = a1
    wave[1,0,...] = a1 * (u-a)
    wave[2,0,...] = a1 * (enthalpy - u*a)
    s[0,...] = u - a

    wave[0,1,...] = a2
    wave[1,1,...] = a2 * u
    wave[2,1,...] = a2 * 0.5 * u**2
    s[1,...] = u

    wave[0,2,...] = a3
    wave[1,2,...] = a3 * (u+a)
    wave[2,2,...] = a3 * (enthalpy + u*a)
    s[2,...] = u + a

    # Entropy fix
    if problem_data['efix']:
        raise NotImplementedError("Entropy fix has not been implemented!")
    else:
        # Godunov update
        s_index = np.zeros((2,num_rp))
        for m in range(num_eqn):
            for mw in range(num_waves):
                s_index[0,:] = s[mw,:]
                amdq[m,:] += np.min(s_index,axis=0) * wave[m,mw,:]
                apdq[m,:] += np.max(s_index,axis=0) * wave[m,mw,:]

    return wave,s,amdq,apdq

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

    if riemann_solver == 'Roe': rs = euler_roe_1D
        
    solver = pyclaw.ClawSolver1D(rs)        
    #solver = mysolver.ClawSolver1D(rs)

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


