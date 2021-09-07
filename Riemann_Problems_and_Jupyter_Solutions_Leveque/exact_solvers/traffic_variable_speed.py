import numpy as np
import matplotlib.pyplot as plt

def exact_riemann_solution(q_l,q_r,v_l,v_r):
    r"""Exact solution to the Riemann problem for the LWR traffic model with variable speed limit.
        Inputs:
            - q_l, q_r : traffic density for left and right states
            - v_l, v_r : speed limits for left and right states
    """
    f = lambda q, v: v*q*(1-q)
    c = lambda q, v: v*(1.-2.*q)

    f_l = f(q_l,v_l)
    f_r = f(q_r,v_r)
    c_l = c(q_l,v_l)
    c_r = c(q_r,v_r)

    if f_r == f_l:
        states = [q_l, q_r]
        speeds = [0]
        wave_types = ['contact']

    elif (f_r < f_l) and (q_r > 0.5):
        # Left-going shock (1)
        q_star = (1. + np.sqrt(1.-4*f_r/v_l))/2.
        states = [q_l, q_star, q_r]
        shock_speed = (f_r-f_l)/(q_star-q_l)
        speeds = [shock_speed,0]
        wave_types = ['shock', 'contact']

    elif (f_r > f_l) and (q_l < 0.5):
        # Right-going shock (2)
        q_star = (1. - np.sqrt(1.-4*f_l/v_r))/2.
        states = [q_l, q_star, q_r]
        shock_speed = (f_r-f_l)/(q_r-q_star)
        speeds = [0,shock_speed]
        wave_types = ['contact', 'shock']

    elif (f_l > v_r/4.) and (q_r <= 0.5):
        # Left-going shock and right-going rarefaction (3)
        q_star = (1. + np.sqrt(1.-v_r/v_l))/2.
        shock_speed = -(f_l-v_r/4.)/(q_star-q_l)
        states = [q_l, q_star, 0.5, q_r]
        speeds = [shock_speed,0,(0,c_r)]
        wave_types = ['shock', 'contact', 'raref']

    elif (f_r > v_l/4.) and (q_l >= 0.5):
        # Left-going rarefaction and right-going shock (4)
        q_star = (1. - np.sqrt(1.-v_l/v_r))/2.
        shock_speed = (f_r-v_l/4.)/(q_r-q_star)
        states = [q_l,0.5,q_star,q_r]
        speeds = [(c_l, 0), 0, shock_speed]
        wave_types = ['raref', 'contact', 'shock']

    elif (f_l<f_r<=v_l/4.) and (q_l>0.5) and (q_r>=0.5):
        # Left-going rarefaction (6)
        q_star = (1. + np.sqrt(1.-4*f_r/v_l))/2.
        states = [q_l, q_star, q_r]
        c_star = c(q_star,v_l)
        speeds = [(c_l,c_star),0]
        wave_types = ['raref', 'contact']

    elif (f_r<f_l<=v_r/4.) and (q_l<=0.5) and (q_r<0.5):
        # Right-going rarefaction (7)
        q_star = (1. - np.sqrt(1.-4*f_l/v_r))/2.
        states = [q_l, q_star, q_r]
        c_star = c(q_star,v_r)
        speeds = [0,(c_star,c_r)]
        wave_types = ['contact', 'raref']

    elif (q_l>=0.5) and (q_r<=0.5) and (f_l<=v_r/4.) and (f_r<=v_l/4.):
        # Transonic rarefaction (5)
        if v_r < v_l:
            # q* on left side (5a)
            q_star = (1. + np.sqrt(1.-v_r/v_l))/2.
            states = [q_l, q_star, 0.5, q_r]
            c_star = c(q_star, v_l)
            speeds = [(c_l,c_star),0,(0,c_r)]
            wave_types = ['raref', 'contact', 'raref']

        elif v_r > v_l:
            # q* on right side (5b)
            q_star = (1. - np.sqrt(1.-v_l/v_r))/2.
            states = [q_l, 0.5, q_star, q_r]
            c_star = c(q_star, v_r)
            speeds = [(c_l,0),0,(c_star,c_r)]
            wave_types = ['raref', 'contact', 'raref']

        else:  # v_r == v_l
            states = [q_l, q_r]
            speeds = [(c_l,c_r),]
            wave_types = ['raref']

    else:
        print(f_l, f_r)
        raise Exception('Unhandled state!')

    def c_raref_fun(xi):
        return (xi<=0)*(1.-xi/v_l)/2. + (xi>0)*(1.-xi/v_r)/2.

    reval = make_reval(states, speeds, wave_types, c_raref_fun)

    return np.array([states]), speeds, reval, wave_types


def phase_plane_plot(q_l, q_r, v_l, v_r, states=None, speeds=None,
                     reval=None, wave_types=None, axes=None, show=True,
                     connect=True):
    r"""Plot Riemann solution in the q-f plane."""
    if axes is None:
        fig, axes = plt.subplots()

    if states is None:
        states, speeds, reval, wave_types = exact_riemann_solution(q_l, q_r, v_l, v_r)
    states = states[0]
    colors = {'shock': 'r', 'raref': 'b', 'contact': 'k'}

    f = lambda q, v: v*q*(1-q)

    f_l = f(q_l,v_l)
    f_r = f(q_r,v_r)

    # Plot flux curves
    q1 = np.linspace(0,0.5,500)
    q2 = np.linspace(0.5,1,500)
    axes.plot(q1, f(q1,v_l),'--k',alpha=0.5)
    axes.plot(q2, f(q2,v_r),'--k',alpha=0.5)

    if q_l > 0.5:
        qlb = np.linspace(0.5, q_l, 500)
        axes.plot(qlb, f(qlb,v_l), '-b',linewidth=0.8)
        qlr = np.linspace(q_l, 1.0, 500)
        axes.plot(qlr, f(qlr,v_l), '-r',linewidth=0.8)
    else:
        axes.plot(q2, f(q2,v_l),'-r',linewidth=0.8)
    if q_r < 0.5:
        qrr = np.linspace(0., q_r, 500)
        axes.plot(qrr, f(qrr,v_r), '-r',linewidth=0.8)
        qrb = np.linspace(q_r, 0.5, 500)
        axes.plot(qrb, f(qrb,v_r), '-b',linewidth=0.8)
    else:
        axes.plot(q1, f(q1,v_r),'-r',linewidth=0.8)
    axes.plot([q_l], [f(q_l,v_l)],'ok')
    axes.plot([q_r], [f(q_r,v_r)],'ok')

    fluxes = [f_l,f_r]
    if len(states) == 4:
        fluxes = [f_l, f(states[1],v_l), f(states[2],v_r), f_r]
    elif len(states) == 3:
        if isinstance(speeds[0],tuple):
            if speeds[0][1] >= 0:
                fluxes.insert(1,f(states[1],v_r))
            else:
                fluxes.insert(1,f(states[1],v_l))
        else:
            if speeds[0] < 0:
                fluxes.insert(1,f(states[1],v_l))
            else:
                fluxes.insert(1,f(states[1],v_r))

    if connect:
        for i, w in enumerate(wave_types):
            if w is 'raref':
                q = np.linspace(states[i],states[i+1],500)
                if min(speeds[i])<0:  # left-going
                    ff = f(q,v_l)
                else:  # right-going
                    ff = f(q,v_r)
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
                if xi<0:
                    v = v_l
                else:
                    v = v_r
                axes.plot(q, f(q,v), 'ok')

    which = np.sign(v_r-v_l)
    ymax = 0.3*max(v_l,v_r)
    axes.text(q_l, f_l-0.06*which*ymax, r'$\rho_l$', horizontalalignment='center',
              verticalalignment='center',
              fontsize=15)
    axes.text(q_r, f_r+0.06*which*ymax,r'$\rho_r$', horizontalalignment='center',
              verticalalignment='center',
              fontsize=15)
    axes.set_xlim(0,1)
    axes.set_ylim(0,ymax)
    axes.plot([0.5,0.5],[0,ymax],'--k',linewidth=1,alpha=0.5)
    axes.set_xlabel(r'$\rho$')
    axes.set_ylabel(r'$f(\rho)$')

    if show:
        plt.show()


def make_reval(states, speeds, wave_types, c_raref_fun=None):
    """
    Generate a function reval(xi) that yields the exact solution for any value of
    xi = x/t.  Currently only for scalar problems.
    """
    def reval(xi):
        q = np.zeros((1,len(xi)))

        for i in range(len(wave_types)):
            if i == 0:
                s_last = -np.Inf
            if wave_types[i] == 'raref':
                q[0,:] += (xi>s_last)*(xi<=speeds[i][0])*states[i]
                q[0,:] += (xi>speeds[i][0])*(xi<=speeds[i][1])*c_raref_fun(xi)
                s_last = speeds[i][1]
            else:
                q[0,:] += (xi>s_last)*(xi<=speeds[i])*states[i]
                s_last = speeds[i]

        q[0,:] += (xi>s_last)*states[-1]  # = q_r
        return q
    return reval
