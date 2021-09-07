import sys, os
import numpy as np
import matplotlib.pyplot as plt
from utils import riemann_tools

f = lambda q: q*(1-q)
c = lambda q: 1 - 2*q
shock_speed = lambda qr, ql: (f(qr)-f(ql))/(qr-ql)


def exact_riemann_solution(q_l,q_r,D):
    r"""
    Exact solution to the Riemann problem for the LWR traffic model
    with an on-ramp at x=0 that brings a flux D of cars.
    """
    f = lambda q: q*(1-q)
    assert D <= 0.25  # Maximum flux
    if q_r > 0.5: assert D <= f(q_r)

    f = lambda q: q*(1-q)
    c = lambda q: 1 - 2*q
    shock_speed = lambda qr, ql: (f(qr)-f(ql))/(qr-ql)

    # Middle state if adjacent to q_r:
    q_mB = 0.5*(1 + np.sqrt(1.+4.*(D-f(q_r))))

    if q_r >= 0.5:
        if f(q_l) - f(q_r) + D < 0 and q_l <= 0.5:
            # Right-going shock only
            q_m = 0.5*(1 - np.sqrt(1.-4.*(f(q_l)+D)))
            states = [q_l, q_m, q_r]
            wave_types = ['contact','shock']
            speeds = [0, shock_speed(q_m, q_r)]
            def reval(xi):
                q = np.zeros((1,len(xi)))
                q[0,:] = (xi<=0)*q_l \
                       + (xi>0)*(xi<=speeds[1])*q_m \
                       + (xi>speeds[1])*q_r
                return q
        else:
            # Left-going wave only
            q_m = q_mB
            states = [q_l, q_m, q_r]
            if q_l > q_m:
                # Left-going rarefaction only
                wave_types = ['raref','contact']
                speeds = [(c(q_l),c(q_m)),0]
                def reval(xi):
                    q = np.zeros((1,len(xi)))
                    q[0,:] = (xi<=speeds[0][0])*q_l \
                           + (xi>speeds[0][0])*(xi<speeds[0][1])*(1-xi)/2. \
                           + (xi>speeds[0][1])*(xi<=0)*q_m \
                           + (xi>0)*q_r
                    return q
            else:
                # Left-going shock only
                wave_types = ['shock','contact']
                speeds = [shock_speed(q_l,q_m),0]
                def reval(xi):
                    q = np.zeros((1,len(xi)))
                    q[0,:] = (xi<=speeds[0])*q_l \
                           + (xi>speeds[0])*(xi<=0)*q_m \
                           + (xi>0)*q_r
                    return q

    elif q_l > 0.5 - np.sqrt(D):
        # right-going rarefaction + congestion at x=0
        q_m = 0.5 + np.sqrt(D)
        states = [q_l, q_m, 0.5, q_r]
        wave_types = ['','contact','raref']
        speeds = [None, 0, (0, c(q_r))]
        if q_l > q_m:
            # left-going rarefaction
            wave_types[0] = 'raref'
            speeds[0] = (c(q_l),c(q_m))
            def reval(xi):
                q = np.zeros((1,len(xi)))
                q[0,:] = (xi<=speeds[0][0])*q_l \
                          + (xi>speeds[0][0])*(xi<=speeds[0][1])*(1-xi)/2. \
                          + (xi>speeds[0][1])*(xi<=0)*q_m \
                          + (xi>0)*(xi<=speeds[2][1])*(1-xi)/2. \
                          + (xi>speeds[2][1])*q_r
                return q
        else:
            # left-going shock
            wave_types[0] = 'shock'
            speeds[0] = shock_speed(q_l, q_m)
            def reval(xi):
                q = np.zeros((1,len(xi)))
                q[0,:] = (xi<=speeds[0])*q_l \
                          + (xi>speeds[0])*(xi<=0)*q_m \
                          + (xi>0)*(xi<=speeds[2][1])*(1-xi)/2. \
                          + (xi>speeds[2][1])*q_r
                return q

    else:
        # right-going wave only
        q_m = 0.5*(1 - np.sqrt(1.-4.*(f(q_l)+D)))
        states = [q_l, q_m, q_r]
        if q_r > q_m:
            # Right-going shock only
            wave_types = ['contact','shock']
            speeds = [0, shock_speed(q_m, q_r)]
            def reval(xi):
                q = np.zeros((1,len(xi)))
                q[0,:] = (xi<=0)*q_l \
                       + (xi>0)*(xi<=speeds[1])*q_m \
                       + (xi>speeds[1])*q_r
                return q
        else:
            # Right-going rarefaction only
            wave_types = ['contact','raref']
            speeds = [0, (c(q_m),c(q_r))]
            def reval(xi):
                q = np.zeros((1,len(xi)))
                q[0,:] = (xi<=0)*q_l \
                       + (xi>0)*(xi<=speeds[1][0])*q_m \
                       + (xi>speeds[1][0])*(xi<speeds[1][1])*(1-xi)/2. \
                       + (xi>speeds[1][1])*q_r
                return q

    return np.array([states]), speeds, reval, wave_types


def phase_plane_plot(q_l, q_r, D, states=None, speeds=None,
                     reval=None, wave_types=None, axes=None, show=True):
    r"""Plot Riemann solution in the q-f plane."""
    if axes is None:
        fig, axes = plt.subplots()

    if states is None:
        states, speeds, reval, wave_types = exact_riemann_solution(q_l, q_r, D)
    states = states[0]
    colors = {'shock': 'r', 'raref': 'b', 'contact': 'k'}

    f_l = f(q_l)
    f_r = f(q_r)

    # Plot flux curve
    q = np.linspace(0,1,500)
    axes.plot(q, f(q))
    axes.plot([q_l], [f(q_l)],'o')
    axes.plot([q_r], [f(q_r)],'o')

    fluxes = [f(state) for state in states]

    for i, w in enumerate(wave_types):
        if w is 'raref':
            q = np.linspace(states[i],states[i+1],500)
            ff = f(q)
            axes.plot(q,ff,color=colors['raref'],lw=3)
        else:
            axes.plot([states[i],states[i+1]],[fluxes[i],fluxes[i+1]],color=colors[wave_types[i]],lw=3)

    eps = 1.e-7
    speedlist = []
    for s in speeds:
        if not isinstance(s,tuple):
            s = [s]
        speedlist += s

    for s in speedlist:
        for xi in [s-eps, s+eps]:
            q = reval(np.array([xi]))
            axes.plot(q, f(q), 'ok')

    axes.text(q_l, f_l+0.02, '$q_l$')
    axes.text(q_r, f_r+0.02, '$q_r$')
    axes.set_xlim(0,1)
    ymax = 0.3
    axes.set_ylim(0,ymax)
    axes.plot([0.5,0.5],[0,ymax],'--k',linewidth=1,alpha=0.5)
    axes.set_xlabel('q')
    axes.set_ylabel('f(q)')

    if show:
        plt.show()


def plot_car_trajectories(q_l,q_r,D,axes=None,t=None,xmax=None):
    states, speeds, reval, wave_types = exact_riemann_solution(q_l,q_r,D)
    def reval_with_speed(xi):
        q = reval(xi)
        u = 1-q
        qu = np.vstack((q,u))
        return qu

    # density of particles for trajectories:
    rho_left = q_l/2.
    rho_right = q_r/2.

    # compute trajectories:
    x_traj, t_traj, xmax = riemann_tools.compute_riemann_trajectories(states,
                            speeds, reval_with_speed, wave_types,
                            xmax=xmax,rho_left=rho_left, rho_right=rho_right)

    # plot trajectories along with waves in the x-t plane:
    axes = riemann_tools.plot_riemann_trajectories(x_traj, t_traj, speeds, wave_types,
                                                   xmax=xmax, ax=axes, t=t)

    axes.set_title('Vehicle trajectories')
    axes.set_xlabel('$x$',fontsize=15)
    axes.set_ylabel('$t$',fontsize=15)
