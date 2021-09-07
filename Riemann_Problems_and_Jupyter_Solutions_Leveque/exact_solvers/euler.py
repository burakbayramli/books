import numpy as np
from scipy.optimize import fsolve
import matplotlib.pyplot as plt
from collections import namedtuple
from utils import riemann_tools
from ipywidgets import interact
from ipywidgets import widgets

conserved_variables = ('Density', 'Momentum', 'Energy')
primitive_variables = ('Density', 'Velocity', 'Pressure')

Primitive_State = namedtuple('State', primitive_variables)
Conserved_State = namedtuple('State', conserved_variables)

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

def cons_to_prim(q, gamma=1.4):
    return conservative_to_primitive(*q,gamma=1.4)

def sound_speed(rho, p, gamma=1.4):
    return np.sqrt(gamma*p/pospart(rho))

def beta(gamma):
    return (gamma+1.)/(gamma-1.)

def lambda1(q, xi, gamma=1.4):
    "Characteristic speed for 1-waves."
    rho, u, p = conservative_to_primitive(*q, gamma=gamma)
    c = sound_speed(rho, p, gamma)
    return u - c

def lambda2(q, xi, gamma=1.4):
    "Characteristic speed for 2-waves."
    rho, u, p = conservative_to_primitive(*q, gamma=gamma)
    return u

def lambda3(q, xi, gamma=1.4):
    "Characteristic speed for 3-waves."
    rho, u, p = conservative_to_primitive(*q, gamma=gamma)
    c = sound_speed(rho, p, gamma)
    return u + c

def integral_curve_1(p, rhostar, ustar, pstar, gamma=1.4):
    """Velocity as a function of pressure for the 1-integral curve passing
       through (rhostar, ustar, pstar)"""
    c = sound_speed(rhostar, pstar, gamma)
    return ustar + 2*c/(gamma-1.)* (1.-(pospart(p)/pstar)**((gamma-1.)/(2.*gamma)))

def integral_curve_3(p, rhostar, ustar, pstar, gamma=1.4):
    c = sound_speed(rhostar, pstar, gamma)
    return ustar - 2*c/(gamma-1.)* (1.-(pospart(p)/pstar)**((gamma-1.)/(2.*gamma)))

def hugoniot_locus_1(p, rhostar, ustar, pstar, gamma=1.4):
    c = sound_speed(rhostar, pstar, gamma)
    return ustar + 2*c/np.sqrt(2*gamma*(gamma-1.)) * ((1-p/pstar)/np.sqrt(1+beta(gamma)*p/pstar))

def hugoniot_locus_3(p, rhostar, ustar, pstar, gamma=1.4):
    c = sound_speed(rhostar, pstar, gamma)
    return ustar - 2*c/np.sqrt(2*gamma*(gamma-1.)) * ((1-p/pstar)/np.sqrt(1+beta(gamma)*p/pstar))

def exact_riemann_solution(q_l, q_r, gamma=1.4, phase_plane_curves=False):
    """Return the exact solution to the Riemann problem with initial states
       q_l, q_r.  The solution is given in terms of a list of states, a list of
       speeds (each of which may be a pair in case of a rarefaction fan), and a
       function reval(xi) that gives the solution at a point xi=x/t.

       The input and output vectors are the conserved quantities.

       If phase_plane_curves==True, then the appropriate Hugoniot Locus and/or
       integral curve is returned for the 1- and 3-waves.
    """
    rho_l, u_l, p_l = conservative_to_primitive(*q_l)
    rho_r, u_r, p_r = conservative_to_primitive(*q_r)

    # Compute left and right state sound speeds
    c_l = sound_speed(rho_l, p_l, gamma)
    c_r = sound_speed(rho_r, p_r, gamma)

    ws = np.zeros(5)
    wave_types = ['', 'contact', '']

    if rho_l == 0:
        # 3-rarefaction connecting right state to vacuum
        p = 0.
        rho_l_star = 0.
        rho_r_star = 0.
        u_vacuum_r = integral_curve_3(0., rho_r, u_r, p_r, gamma)
        u = u_vacuum_r
        ws[0] = 0.
        ws[1] = 0.
        ws[2] = 0.
        ws[3] = u_vacuum_r
        ws[4] = u_r + c_r
        wave_types = ['contact', 'contact', 'raref']

    elif rho_r == 0:
        # 1-rarefaction connecting left state to vacuum
        p = 0
        rho_l_star = 0.
        rho_r_star = 0.
        u_vacuum_l = integral_curve_1(0., rho_l, u_l, p_l, gamma)
        u = u_vacuum_l
        ws[0] = u_l - c_l
        ws[1] = u_vacuum_l
        ws[2] = 0.
        ws[3] = 0.
        ws[4] = 0.
        wave_types = ['raref', 'contact', 'contact']

    elif u_l - u_r + 2*(c_l+c_r)/(gamma-1.) < 0:
        # Middle states are vacuum
        p = 0.
        rho_l_star = 0.
        rho_r_star = 0.
        u_vacuum_l = integral_curve_1(0., rho_l, u_l, p_l, gamma)
        u_vacuum_r = integral_curve_3(0., rho_r, u_r, p_r, gamma)
        u = 0.5*(u_vacuum_l + u_vacuum_r)
        ws[0] = u_l - c_l
        ws[1] = u_vacuum_l
        ws[2] = u
        ws[3] = u_vacuum_r
        ws[4] = u_r + c_r
        wave_types = ['raref', 'contact', 'raref']

    else:
        # Check whether the 1-wave is a shock or rarefaction
        def phi_l(p):
            if p >= p_l: return hugoniot_locus_1(p, rho_l, u_l, p_l, gamma)
            else: return integral_curve_1(p, rho_l, u_l, p_l, gamma)

        # Check whether the 1-wave is a shock or rarefaction
        def phi_r(p):
            if p >= p_r: return hugoniot_locus_3(p, rho_r, u_r, p_r, gamma)
            else: return integral_curve_3(p, rho_r, u_r, p_r, gamma)

        phi = lambda p: phi_l(p)-phi_r(p)

        exp = (1.-gamma)/(2.*gamma)
        guess = ((c_l + c_r - (gamma-1.)*(u_r-u_l)/2.)/(c_l*p_l**exp+c_r*p_r**exp))**(-1./exp)
        # Compute middle state p, u by finding curve intersection
        p, info, ier, msg = fsolve(phi, guess, full_output=True, xtol=1.e-14)
        # For strong rarefactions, sometimes fsolve needs help
        if ier != 1:
            p, info, ier, msg = fsolve(phi, guess, full_output=True, factor=0.1, xtol=1.e-10)
            # This should not happen:
            if ier != 1:
                print('Warning: fsolve did not converge.')
                print(msg)

        u = phi_l(p)

        ws[2] = u

        # Find shock and rarefaction speeds
        if p > p_l:
            wave_types[0] = 'shock'
            rho_l_star = rho_l*(1+beta(gamma)*p/p_l)/(p/p_l+beta(gamma))
            ws[0] = (rho_l*u_l - rho_l_star*u)/(rho_l - rho_l_star)
            ws[1] = ws[0]
        else:
            wave_types[0] = 'raref'
            rho_l_star = (p/p_l)**(1./gamma) * rho_l
            c_l_star = sound_speed(rho_l_star, p, gamma)
            ws[0] = u_l - c_l
            ws[1] = u - c_l_star

        if p > p_r:
            wave_types[2] = 'shock'
            rho_r_star = rho_r*(1+beta(gamma)*p/p_r)/(p/p_r+beta(gamma))
            ws[4] = (rho_r*u_r - rho_r_star*u)/(rho_r - rho_r_star)
            ws[3] = ws[4]
        else:
            wave_types[2] = 'raref'
            rho_r_star = (p/p_r)**(1./gamma) * rho_r
            c_r_star = sound_speed(rho_r_star, p, gamma)
            ws[3] = u + c_r_star
            ws[4] = u_r + c_r

    # Find solution inside rarefaction fans (in primitive variables)
    def raref1(xi):
        u1 = ((gamma-1.)*u_l + 2*(c_l + xi))/(gamma+1.)
        rho1 = (rho_l**gamma*(u1-xi)**2/pospart(gamma*p_l))**(1./(gamma-1.))
        p1 = p_l*(rho1/pospart(rho_l))**gamma
        return rho1, u1, p1

    def raref3(xi):
        u3 = ((gamma-1.)*u_r - 2*(c_r - xi))/(gamma+1.)
        rho3 = (rho_r**gamma*(xi-u3)**2/pospart(gamma*p_r))**(1./(gamma-1.))
        p3 = p_r*(rho3/pospart(rho_r))**gamma
        return rho3, u3, p3

    q_l_star = np.squeeze(np.array(primitive_to_conservative(rho_l_star,u,p)))
    q_r_star = np.squeeze(np.array(primitive_to_conservative(rho_r_star,u,p)))

    states = np.column_stack([q_l,q_l_star,q_r_star,q_r])
    speeds = [[], ws[2], []]
    if wave_types[0] in ['shock','contact']:
        speeds[0] = ws[0]
    else:
        speeds[0] = (ws[0],ws[1])
    if wave_types[2] in ['shock','contact']:
        speeds[2] = ws[3]
    else:
        speeds[2] = (ws[3],ws[4])

    def reval(xi):
        r"""Returns the Riemann solution in primitive variables for any
            value of xi = x/t.
        """
        rar1 = raref1(xi)
        rar3 = raref3(xi)
        rho_out =  (xi<=ws[0]                  )*rho_l      \
                 + (xi>ws[0])*(xi<=ws[1])*rar1[0]    \
                 + (xi>ws[1])*(xi<=ws[2]   )*rho_l_star \
                 + (xi>ws[2])   *(xi<=ws[3])*rho_r_star \
                 + (xi>ws[3])*(xi<=ws[4])*rar3[0]    \
                 + (xi>ws[4]                   )*rho_r

        u_out   =  (xi<=ws[0]                  )*u_l     \
                 + (xi>ws[0])*(xi<=ws[1])*rar1[1] \
                 + (xi>ws[1])*(xi<=ws[2]   )*u       \
                 + (xi>ws[2]   )*(xi<=ws[3])*u       \
                 + (xi>ws[3])*(xi<=ws[4])*rar3[1] \
                 + (xi>ws[4]                   )*u_r

        p_out   =  (xi<=ws[0]                  )*p_l     \
                 + (xi>ws[0])*(xi<=ws[1])*rar1[2] \
                 + (xi>ws[1])*(xi<=ws[2]   )*p       \
                 + (xi>ws[2]   )*(xi<=ws[3])*p       \
                 + (xi>ws[3])*(xi<=ws[4])*rar3[2] \
                 + (xi>ws[4]                   )*p_r
        return primitive_to_conservative(rho_out,u_out,p_out)

    if phase_plane_curves:
        if wave_types[0] == 'raref':
            phi1 = lambda p: integral_curve_1(p, rho_l, u_l, p_l, gamma)
        elif wave_types[0] == 'shock':
            phi1 = lambda p: hugoniot_locus_1(p, rho_l, u_l, p_l, gamma)
        else:
            phi1 = lambda p: p
        if wave_types[2] == 'raref':
            phi3 = lambda p: integral_curve_3(p, rho_r, u_r, p_r, gamma)
        elif wave_types[2] == 'shock':
            phi3 = lambda p: hugoniot_locus_3(p, rho_r, u_r, p_r, gamma)
        else:
            phi3 = lambda p: p
        return states, speeds, reval, wave_types, (p, phi1, phi3)
    else:
        return states, speeds, reval, wave_types


def phase_plane_plot(left_state, right_state, gamma=1.4, ax=None, approx_states=None,
                     cons_inputs=False):
    r"""Plot the Hugoniot loci or integral curves in the p-u plane."""
    import matplotlib.lines as mlines
    
    if ax is None:
        fig, ax = plt.subplots()

    if cons_inputs:
        q_left = left_state.copy()
        q_right = right_state.copy()
        left_state = Primitive_State(*conservative_to_primitive(*q_left))
        right_state = Primitive_State(*conservative_to_primitive(*q_right))
    else:
        q_left  = primitive_to_conservative(*left_state)
        q_right = primitive_to_conservative(*right_state)
    # Solve Riemann problem
    ex_states, ex_speeds, reval, wave_types, ppc = \
                        exact_riemann_solution(q_left, q_right, gamma,
                                               phase_plane_curves=True)
    pm, w1, w3 = ppc

    x = [left_state.Pressure,pm,right_state.Pressure]
    y = [left_state.Velocity, w1(pm), right_state.Velocity]
    if left_state.Pressure == 0:
        c_r = sound_speed(right_state.Density, right_state.Pressure, gamma)
        y[1] = right_state.Velocity - 2*c_r/(gamma-1.)
    if right_state.Pressure == 0:
        c_l = sound_speed(left_state.Density, left_state.Pressure, gamma)
        y[1] = left_state.Velocity - 2*c_l/(gamma-1.)
    xmax, xmin = max(x), min(x)
    ymax, ymin = max(y), min(y)
    dx, dy = xmax - xmin, ymax - ymin

    w1v, w3v = (np.vectorize(w1), np.vectorize(w3))
    ax.set_xlabel('Pressure (p)')
    ax.set_ylabel('Velocity (u)')
    ax.set_title('Phase plane')

    pa = np.linspace(0.,left_state.Pressure,500)
    pb = np.linspace(left_state.Pressure,xmax+0.5*dx)
    ua = w1v(pa)
    ub = w1v(pb)
    if wave_types[0] == 'shock':
        style1 = '--r'
        style2 = '-r'
    elif wave_types[0] == 'raref':
        style1 = '-b'
        style2 = '--b'
    else:
        style1 = '-w'
        style2 = '-w'
    ax.plot(pa,ua,style1)
    ax.plot(pb,ub,style2)

    pa = np.linspace(0.,right_state.Pressure,500)
    pb = np.linspace(right_state.Pressure,xmax+0.5*dx)
    ua = w3v(pa)
    ub = w3v(pb)
    if wave_types[2] == 'shock':
        style1 = '--r'
        style2 = '-r'
    elif wave_types[2] == 'raref':
        style1 = '-b'
        style2 = '--b'
    else:
        style1 = '-w'
        style2 = '-w'
    ax.plot(pa,ua,style1)
    ax.plot(pb,ub,style2)

          
    msize = 8
    ax.plot(x[0],y[0],'<k',markersize=msize,label='Left')
    ax.plot(x[1],y[1],'ok',markersize=msize,label='Middle')
    ax.plot(x[2],y[2],'>k',markersize=msize,label='Right')
        
    # add legends only for Left, Middle, Right:
    handles = []
    handle = mlines.Line2D([], [], color='k', linestyle='', marker='<',
                    label='Left state')
    handles.append(handle)
    handle = mlines.Line2D([], [], color='k', linestyle='', marker='o',
                    label='Middle state')
    handles.append(handle)
    handle = mlines.Line2D([], [], color='k', linestyle='', marker='>',
                    label='Right state')
    handles.append(handle)
    plt.legend(handles=handles, fontsize=8)
    
    if approx_states is not None:
        p_approx = []
        u_approx = []
        for j in range(approx_states.shape[1]):
            rho, u, p = cons_to_prim(approx_states[:,j],gamma=gamma)
            p_approx.append(p)
            u_approx.append(u)
        ax.plot(p_approx,u_approx,'-g',zorder=0)
        # don't plot the left and right states as dots, only middle states:
        ax.plot(p_approx[1:-1],u_approx[1:-1],'og',markersize=8,zorder=0)

    xlimits = ax.get_xlim()
    if xlimits[0] <= 0.:
        # shift xlimits to better show vacuum state:
        x0 = min(xlimits[0], -0.05*(xlimits[1] - xlimits[0]))
        ax.set_xlim(x0,xlimits[1])
        ylimits = ax.get_ylim()
        ax.plot([0,0], ylimits, 'k-', linewidth=0.6)  # y-axis
        
def plot_integral_curves(plot_1=True,plot_3=False,gamma=1.4,rho_0=1.):
    N = 400
    p = np.linspace(0.,5,N)
    p_0 = 1.
    uu = np.linspace(-3,3,15)
    c_0 = np.sqrt(gamma*p_0/rho_0)
    if plot_1:
        for u_0 in uu:
            u = u_0 + (2*c_0)/(gamma-1.)* \
                (1.-(p/p_0)**((gamma-1)/(2*gamma)))
            plt.plot(p,u,color='coral')
    if plot_3:
        for u_0 in uu:
            u = u_0 - (2*c_0)/(gamma-1.)* \
                (1.-(p/p_0)**((gamma-1)/(2*gamma)))
            plt.plot(p,u,color='maroon')
    plt.xlabel('p'); plt.ylabel('u')
    plt.title('Integral curves projected to p-u plane')
    plt.show()

def plot_hugoniot_loci(plot_1=True,plot_3=False,gamma=1.4,rho_0=1.):
    N = 400
    p = np.linspace(1.e-3,5,N)
    p_0 = 1.
    uu = np.linspace(-3,3,15)
    c_0 = np.sqrt(gamma*p_0/rho_0)
    beta = (gamma+1.)/(gamma-1.)
    if plot_1:
        for u_0 in uu:
            u_1 = u_0 + (2*c_0)/np.sqrt(2*gamma*(gamma-1.))* \
                (1.-p/p_0)/(np.sqrt(1+beta*p/p_0))
            plt.plot(p,u_1,color='coral')
    if plot_3:
        for u_0 in uu:
            u_1 = u_0 - (2*c_0)/np.sqrt(2*gamma*(gamma-1.))* \
                (1.-p/p_0)/(np.sqrt(1+beta*p/p_0))
            plt.plot(p,u_1,color='maroon')
    plt.xlabel('p'); plt.ylabel('u')
    plt.title('Hugoniot Loci projected to p-u plane')
    plt.show()

def riemann_solution(left_state, right_state, gamma=1.4):
    q_left  = primitive_to_conservative(*left_state)
    q_right = primitive_to_conservative(*right_state)

    ex_states, ex_speeds, reval, wave_types = exact_riemann_solution(q_left ,q_right, gamma)

    plot_function = riemann_tools.make_plot_function(ex_states, ex_speeds, reval, wave_types,
                                                     layout='vertical',
                                                     vertical_spacing=0.15,
                                                     variable_names=primitive_variables,
                                                     plot_chars=[lambda1,lambda2,lambda3],
                                                     derived_variables=cons_to_prim)

    interact(plot_function, t=widgets.FloatSlider(value=0.5,min=0,max=.9),
             which_char=widgets.Dropdown(options=[None,1,2,3],description='Show characteristics:',
                                         style={'description_width':'initial'}))

def plot_riemann_trajectories(q_l, q_r, gamma=1.4, primitive=False):
    if primitive:
        q_left  = primitive_to_conservative(*q_l)
        q_right = primitive_to_conservative(*q_r)
    else:
        q_left = q_l
        q_right = q_r

    ex_states, ex_speeds, reval, wave_types = exact_riemann_solution(q_left ,q_right, gamma=gamma)

    def reval_rho_u(x):
        q = reval(x)
        rho = q[0]
        u = q[1]/q[0]
        rho_u = np.vstack((rho,u))
        return rho_u

    # Specify density of trajectories to left and right:
    rho_l = q_left[0] / 10.
    rho_r = q_right[0] / 10.
    x_traj, t_traj, xmax = riemann_tools.compute_riemann_trajectories(ex_states,
                                                                      ex_speeds,
                                                                      reval_rho_u,
                                                                      wave_types,
                                                                      i_vel=1,
                                                                      rho_left=rho_l,
                                                                      rho_right=rho_r)

    riemann_tools.plot_riemann_trajectories(x_traj, t_traj, ex_speeds, wave_types)
