import sys, os
import numpy as np
from scipy.optimize import fsolve
import matplotlib.pyplot as plt
import warnings
from ipywidgets import interact
from ipywidgets import widgets, Checkbox, fixed
from utils import riemann_tools
from collections import namedtuple
warnings.filterwarnings("ignore")

conserved_variables = ('Depth', 'Momentum')
primitive_variables = ('Depth', 'Velocity')
left, middle, right = (0, 1, 2)
State = namedtuple('State', conserved_variables)
Primitive_State = namedtuple('PrimState', primitive_variables)

def pospart(x):
    return np.maximum(1.e-15,x)

def primitive_to_conservative(h, u):
    hu = h*u
    return h, hu

def conservative_to_primitive(h, hu):
    # Check that h>=0 where it is not np.nan:
    assert np.nanmin(h)>=0
    # We should instead check that hu is zero everywhere that h is
    u = hu/pospart(h)
    return h, u

def cons_to_prim(q):
    return conservative_to_primitive(*q)

def exact_riemann_solution(q_l, q_r, grav=1., force_waves=None,
                           primitive_inputs=False, include_contact=False):
    """Return the exact solution to the Riemann problem with initial states q_l, q_r.
       The solution is given in terms of a list of states, a list of speeds (each of which
       may be a pair in case of a rarefaction fan), and a function reval(xi) that gives the
       solution at a point xi=x/t.

       The input and output vectors are the conserved quantities.
    """
    if primitive_inputs:
        h_l, u_l = q_l
        h_r, u_r = q_r
        hu_l = h_l*u_l
        hu_r = h_r*u_r
    else:
        h_l, u_l = conservative_to_primitive(*q_l)
        h_r, u_r = conservative_to_primitive(*q_r)
        hu_l = q_l[1]
        hu_r = q_r[1]

    # Compute left and right state sound speeds
    c_l = np.sqrt(grav*h_l)
    c_r = np.sqrt(grav*h_r)

    # Define the integral curves and hugoniot loci
    # Avoid warnings due to negative depths in fsolve calls
    integral_curve_1   = lambda h: u_l + 2*(np.sqrt(grav*h_l) -
                                            np.sqrt(grav*np.maximum(h,0)))
    integral_curve_2   = lambda h: u_r - 2*(np.sqrt(grav*h_r) -
                                            np.sqrt(grav*np.maximum(h,0)))
    hugoniot_locus_1 = lambda h: (h_l*u_l + (h-h_l)*(u_l -
                                  np.sqrt(grav*h_l*(1 + (h-h_l)/h_l) * (1 + (h-h_l)/(2*h_l)))))/h
    hugoniot_locus_2 = lambda h: (h_r*u_r + (h-h_r)*(u_r +
                                  np.sqrt(grav*h_r*(1 + (h-h_r)/h_r) * (1 + (h-h_r)/(2*h_r)))))/h

    # Check whether the 1-wave is a shock or rarefaction
    def phi_l(h):
        if (h>=h_l and force_waves!='raref') or force_waves=='shock':
            return hugoniot_locus_1(h)
        else:
            return integral_curve_1(h)

    # Check whether the 2-wave is a shock or rarefaction
    def phi_r(h):
        if (h>=h_r and force_waves!='raref') or force_waves=='shock':
            return hugoniot_locus_2(h)
        else:
            return integral_curve_2(h)

    ws = np.zeros(4)
    wave_types = ['', '']

    dry_velocity_l = u_l + 2*np.sqrt(grav*h_l)
    dry_velocity_r = u_r - 2*np.sqrt(grav*h_r)
    if dry_velocity_l < dry_velocity_r:
        # Dry middle state
        h_m = 0
        # This is a bit arbitrary:
        u_m = 0.5*(dry_velocity_l + dry_velocity_r)

        hu_m = u_m * h_m
        ws[0] = u_l - c_l
        ws[1] = dry_velocity_l
        ws[2] = dry_velocity_r
        ws[3] = u_r + c_r

    elif h_l == 0:
        # Dry left state; 2-rarefaction only
        h_m = 0
        u_m = dry_velocity_r
        hu_m = u_m * h_m
        ws[0] = 0
        ws[1] = 0
        ws[2] = dry_velocity_r
        ws[3] = u_r + c_r

    elif h_r == 0:
        # Dry right state; 1-rarefaction only
        h_m = 0
        u_m = dry_velocity_l
        hu_m = u_m * h_m
        ws[0] = u_l - c_l
        ws[1] = dry_velocity_l
        ws[2] = 0
        ws[3] = 0

    else:
        phi = lambda h: phi_l(h)-phi_r(h)

        # Compute middle state h, hu by finding curve intersection
        guess = (u_l-u_r+2.*np.sqrt(grav)*(np.sqrt(h_l)+np.sqrt(h_r)))**2./16./grav
        h_m, _, ier, msg = fsolve(phi, guess, full_output=True, xtol=1.e-14)
        # For strong rarefactions, sometimes fsolve needs help
        if ier!=1:
            h_m, _, ier, msg = fsolve(phi, guess,full_output=True,factor=0.1,xtol=1.e-10)
            # This should not happen:
            if ier!=1:
                print('Warning: fsolve did not converge.')
                print(msg)

        u_m = phi_l(h_m)
        hu_m = u_m * h_m

        # Find shock and rarefaction speeds
        if (h_m>h_l and force_waves!='raref') or force_waves=='shock':
            wave_types[0] = 'shock'
            ws[0] = (hu_l - hu_m) / (h_l - h_m)
            ws[1] = ws[0]
        else:
            wave_types[0] = 'raref'
            c_m = np.sqrt(grav * h_m)
            ws[0] = u_l - c_l
            ws[1] = u_m - c_m

        if (h_m>h_r and force_waves!='raref') or force_waves=='shock':
            wave_types[1] = 'shock'
            ws[2] = (hu_r - hu_m) / (h_r - h_m)
            ws[3] = ws[2]
        else:
            wave_types[0] = 'raref'
            c_m = np.sqrt(grav * h_m)
            ws[2] = u_m + c_m
            ws[3] = u_r + c_r

    # Find solution inside rarefaction fans (in primitive variables)
    def raref1(xi):
        RiemannInvariant = u_l + 2*np.sqrt(grav*h_l)
        h = ((RiemannInvariant - xi)**2 / (9*grav))
        u = (xi + np.sqrt(grav*h))
        hu = h*u
        return h, hu

    def raref2(xi):
        RiemannInvariant = u_r - 2*np.sqrt(grav*h_r)
        h = ((RiemannInvariant - xi)**2 / (9*grav))
        u = (xi - np.sqrt(grav*h))
        hu = h*u
        return h, hu

    q_m = np.squeeze(np.array((h_m, hu_m)))

    states = np.column_stack([q_l,q_m,q_r])
    speeds = [[], []]
    if wave_types[0] is 'shock':
        speeds[0] = ws[0]
    else:
        speeds[0] = (ws[0],ws[1])
    if wave_types[1] is 'shock':
        speeds[1] = ws[2]
    else:
        speeds[1] = (ws[2],ws[3])

    if include_contact:
        states = np.column_stack([q_l,q_m,q_m,q_r])
        um = q_m[1]/q_m[0]
        speeds = [speeds[0],um,speeds[1]]
        wave_types = [wave_types[0],'contact',wave_types[1]]
        
    def reval(xi):
        """
        Function that evaluates the Riemann solution for arbitrary xi = x/t.
        Sets the solution to nan in an over-turning rarefaction wave
        for illustration purposes of this non-physical solution.
        """
        rar1 = raref1(xi)
        rar2 = raref2(xi)
        h_out = (xi<=ws[0])*h_l + \
            (xi>ws[0])*(xi<=ws[1])*rar1[0] + \
            (xi>ws[1])*(xi<=ws[0])*1e9 +  \
            (xi>ws[1])*(xi<=ws[2])*h_m +  \
            (xi>ws[2])*(xi<=ws[3])*rar2[0] +  \
            (xi>ws[3])*(xi<=ws[2])*1e9 +  \
            (xi>ws[3])*h_r
        h_out[h_out>1e8] = np.nan
        hu_out = (xi<=ws[0])*hu_l + \
            (xi>ws[0])*(xi<=ws[1])*rar1[1] + \
            (xi>ws[1])*(xi<=ws[0])*1e9 +  \
            (xi>ws[1])*(xi<=ws[2])*hu_m +  \
            (xi>ws[2])*(xi<=ws[3])*rar2[1] +  \
            (xi>ws[3])*(xi<=ws[2])*1e9 +  \
            (xi>ws[3])*hu_r
        hu_out[hu_out>1e8] = np.nan
        return h_out, hu_out

    return states, speeds, reval, wave_types

def integral_curve(h, hstar, hustar, wave_family, g=1., y_axis='u'):
    """
    Return u or hu as a function of h for integral curves through
    (hstar, hustar).
    """
    ustar = hustar / pospart(hstar)
    if wave_family == 1:
        if y_axis == 'u':
            return ustar + 2*(np.sqrt(g*hstar) - np.sqrt(g*h))
        else:
            return h*ustar + 2*h*(np.sqrt(g*hstar) - np.sqrt(g*h))
    else:
        if y_axis == 'u':
            return ustar - 2*(np.sqrt(g*hstar) - np.sqrt(g*h))
        else:
            return h*ustar - 2*h*(np.sqrt(g*hstar) - np.sqrt(g*h))


def hugoniot_locus(h, hstar, hustar, wave_family, g=1., y_axis='u'):
    """
    Return u or hu as a function of h for the Hugoniot locus through
    (hstar, hustar).
    """
    ustar = hustar / hstar
    alpha = h - hstar
    d = np.sqrt(g*hstar*(1 + alpha/hstar)*(1 + alpha/(2*hstar)))
    if wave_family == 1:
        if y_axis == 'u':
            return (hustar + alpha*(ustar - d))/pospart(h)
        else:
            return hustar + alpha*(ustar - d)
    else:
        if y_axis == 'u':
            return (hustar + alpha*(ustar + d))/pospart(h)
        else:
            return hustar + alpha*(ustar + d)


def phase_plane_curves(hstar, hustar, state, g=1., wave_family='both', y_axis='u', ax=None,
                       plot_unphysical=False):
    """
    Plot the curves of points in the h - u or h-hu phase plane that can be
    connected to (hstar,hustar).
    state = 'qleft' or 'qright' indicates whether the specified state is ql or qr.
    wave_family = 1, 2, or 'both' indicates whether 1-waves or 2-waves should be plotted.
    Colors in the plots indicate whether the states can be connected via a shock or rarefaction.
    """
    if ax is None:
        fig, ax = plt.subplots()

    h = np.linspace(0, hstar, 200)

    if wave_family in [1,'both']:
        if state == 'qleft' or plot_unphysical:
            u = integral_curve(h, hstar, hustar, 1, g, y_axis=y_axis)
            ax.plot(h,u,'b', label='1-rarefactions')
        if state == 'qright' or plot_unphysical:
            u = hugoniot_locus(h, hstar, hustar, 1, g, y_axis=y_axis)
            ax.plot(h,u,'--r', label='1-shocks')

    if wave_family in [2,'both']:
        if state == 'qleft' or plot_unphysical:
            u = hugoniot_locus(h, hstar, hustar, 2, g, y_axis=y_axis)
            ax.plot(h,u,'--r', label='2-shocks')
        if state == 'qright' or plot_unphysical:
            u = integral_curve(h, hstar, hustar, 2, g, y_axis=y_axis)
            ax.plot(h,u,'b', label='2-rarefactions')

    h = np.linspace(hstar, 3, 200)

    if wave_family in [1,'both']:
        if state == 'qright' or plot_unphysical:
            u = integral_curve(h, hstar, hustar, 1, g, y_axis=y_axis)
            ax.plot(h,u,'--b', label='1-rarefactions')
        if state == 'qleft' or plot_unphysical:
            u = hugoniot_locus(h, hstar, hustar, 1, g, y_axis=y_axis)
            ax.plot(h,u,'r', label='1-shocks')

    if wave_family in [2,'both']:
        if state == 'qright' or plot_unphysical:
            u = hugoniot_locus(h, hstar, hustar, 2, g, y_axis=y_axis)
            ax.plot(h,u,'r', label='2-shocks')
        if state == 'qleft' or plot_unphysical:
            u = integral_curve(h, hstar, hustar, 2, g, y_axis=y_axis)
            ax.plot(h,u,'--b', label='2-rarefactions')

    # plot and label the point (hstar, hustar)
    ax.set_xlabel('Depth (h)')
    if y_axis == 'u':
        ustar = hustar/hstar
        ax.set_ylabel('Velocity (u)')
    else:
        ustar = hustar  # Fake it
        ax.set_ylabel('Momentum (hu)')
    if state == 'qleft':
        ax.plot([hstar],[ustar],'k<',markersize=6)
    elif state == 'qright':
        ax.plot([hstar],[ustar],'k>',markersize=6)
    #ax.plot([hstar],[ustar],'ko',markersize=5) #old way
    ax.text(hstar + 0.1, ustar - 0.2, state, fontsize=13)
    


def make_axes_and_label(x1=-.5, x2=6., y1=-2.5, y2=2.5):
    plt.plot([x1,x2],[0,0],'k')
    plt.plot([0,0],[y1,y2],'k')
    plt.axis([x1,x2,y1,y2])
    plt.legend()
    plt.xlabel("h = depth",fontsize=15)
    plt.ylabel("hu = momentum",fontsize=15)

def phase_plane_plot(q_l, q_r, g=1., ax=None, force_waves=None, y_axis='u', 
                     approx_states=None, hmin=0, color='g', include_contact=False):
    r"""Plot the Hugoniot loci or integral curves in the h-u or h-hu plane."""
    
    import matplotlib.lines as mlines

    # Solve Riemann problem
    states, speeds, reval, wave_types = \
                        exact_riemann_solution(q_l, q_r, g, force_waves=force_waves,
                                                include_contact=include_contact)

    # Set plot bounds
    if ax is None:
        fig, ax = plt.subplots()
    x = states[0,:]
    if y_axis == 'hu':
        y = states[1,:]
    else:
        y = states[1,:]/pospart(states[0,:])
        if states[0,middle] == 0:
            dry_velocity_l = states[1,left]/pospart(states[0,left]) + 2*np.sqrt(g*states[0,left])
            dry_velocity_r = states[1,right]/pospart(states[0,right]) - 2*np.sqrt(g*states[0,right])
            y[1] = 1./(np.abs(np.sign(dry_velocity_l))+np.abs(np.sign(dry_velocity_r))) * \
                    (dry_velocity_l+dry_velocity_r)

    xmax, xmin = max(x), min(x)
    ymax = max(abs(y))
    dx = xmax - xmin
    ymax = max(abs(y))
    ax.set_xlim(hmin, xmax + 0.5*dx)
    ax.set_ylim(-1.5*ymax, 1.5*ymax)
    ax.set_xlabel('Depth (h)')
    if y_axis == 'u':
        ax.set_ylabel('Velocity (u)')
    else:
        ax.set_ylabel('Momentum (hu)')

    # Plot curves
    h_l = states[0,left]
    h1 = np.linspace(1.e-2,h_l)
    h2 = np.linspace(h_l,xmax+0.5*dx)
    if wave_types[0] == 'shock':
        hu1 = hugoniot_locus(h1, h_l, states[1,left], wave_family=1, g=g, y_axis=y_axis)
        hu2 = hugoniot_locus(h2, h_l, states[1,left], wave_family=1, g=g, y_axis=y_axis)
        ax.plot(h1,hu1,'--r', label='Hugoniot locus (unphysical)')
        ax.plot(h2,hu2,'r', label='Hugoniot locus (physical)')
    else:
        hu1 = integral_curve(h1, h_l, states[1,left], wave_family=1, g=g, y_axis=y_axis)
        hu2 = integral_curve(h2, h_l, states[1,left], wave_family=1, g=g, y_axis=y_axis)
        ax.plot(h1,hu1,'b', label='Integral curve (physical)')
        ax.plot(h2,hu2,'--b', label='Integral curve (unphysical)')

    h_r = states[0,right]
    h1 = np.linspace(1.e-2,h_r)
    h2 = np.linspace(h_r,xmax+0.5*dx)
    if wave_types[1] == 'shock':
        hu1 = hugoniot_locus(h1, states[0,right], states[1,right], wave_family=2, g=g, y_axis=y_axis)
        hu2 = hugoniot_locus(h2, states[0,right], states[1,right], wave_family=2, g=g, y_axis=y_axis)
        ax.plot(h1,hu1,'--r', label='Hugoniot locus (unphysical)')
        ax.plot(h2,hu2,'r', label='Hugoniot locus (physical)')
    else:
        hu1 = integral_curve(h1, states[0,right], states[1,right], wave_family=2, g=g, y_axis=y_axis)
        hu2 = integral_curve(h2, states[0,right], states[1,right], wave_family=2, g=g, y_axis=y_axis)
        ax.plot(h1,hu1,'b', label='Integral curve (physical)')
        ax.plot(h2,hu2,'--b', label='Integral curve (unphysical)')


    msize = 6
    Lp = ax.plot(x[0],y[0],'<k',markersize=msize,label='Left')
    Mp = ax.plot(x[1],y[1],'ok',markersize=msize,label='Middle')
    Rp = ax.plot(x[-1],y[-1],'>k',markersize=msize,label='Right')
        
    # add legends only for Left, Middle, Right:
    handles = []
    handle = mlines.Line2D([], [], color='k', linestyle='', marker='<',
                            markersize=msize, label='Left state')
    handles.append(handle)
    handle = mlines.Line2D([], [], color='k', linestyle='', marker='o',
                            markersize=msize, label='Middle state')
    handles.append(handle)
    handle = mlines.Line2D([], [], color='k', linestyle='', marker='>',
                            markersize=msize, label='Right state')
    handles.append(handle)
    plt.legend(handles=handles, fontsize=6)

    if approx_states is not None:
        u = approx_states[1,:]/(approx_states[0,:]+1.e-15)
        h = approx_states[0,:]
        ax.plot(h,u,'-g',zorder=0)
        # don't plot the left and right states as dots, only middle states:
        ax.plot(h[1:-1],u[1:-1],'og',markersize=8,zorder=0)

    xlimits = ax.get_xlim()
    if xlimits[0] <= 0.:
        # shift xlimits to better show dry state:
        x0 = min(xlimits[0],  -0.05*(xlimits[1] - xlimits[0]))
        ax.set_xlim(x0,xlimits[1])
        ylimits = ax.get_ylim()
        ax.plot([0,0], ylimits, 'k-', linewidth=0.6)  # y-axis
        
    # The code below generates a legend with just one
    # entry for each kind of line/marker, even if
    # multiple plotting calls were made with that type
    # of line/marker.  
    # It avoids making entries for lines/markers that
    # don't actually appear on the given plot.
    # It also alphabetizes the entries.
    handles, labels = ax.get_legend_handles_labels()
    i = np.arange(len(labels))
    filter = np.array([])
    unique_labels = list(set(labels))
    for ul in unique_labels:
        filter = np.append(filter,[i[np.array(labels)==ul][0]])
    handles = [handles[int(f)] for f in filter]
    labels = [labels[int(f)] for f in filter]
    order = sorted(range(len(labels)), key=labels.__getitem__)
    handles = [handles[i] for i in order]
    labels = [labels[i] for i in order]
    #ax.legend(handles,labels)

def plot_hugoniot_loci(plot_1=True,plot_2=False,y_axis='hu'):
    h = np.linspace(0.001,3,100)
    hstar = 1.0
    legend = plot_1*['1-loci'] + plot_2*['2-loci']
    for hustar in np.linspace(-4,4,15):
        if plot_1:
            hu = hugoniot_locus(h,hstar,hustar,wave_family=1,y_axis=y_axis)
            plt.plot(h,hu,'-',color='coral')
        if plot_2:
            hu = hugoniot_locus(h,hstar,hustar,wave_family=2,y_axis=y_axis)
            plt.plot(h,hu,'-',color='maroon')
        plt.axis((0,3,-3,3))
        plt.xlabel('depth h')
        if y_axis=='hu':
            plt.ylabel('momentum hu')
        else:
            plt.ylabel('velocity u')
        plt.title('Hugoniot loci')
        plt.legend(legend,loc=1)
    plt.show()

def lambda_1(q, _, g=1.):
    "Characteristic speed for shallow water 1-waves."
    h, hu = q
    if h > 0:
        u = hu/h
        return u - np.sqrt(g*h)
    else:
        return 0

def lambda_2(q, _, g=1.):
    "Characteristic speed for shallow water 2-waves."
    h, hu = q
    if h > 0:
        u = hu/h
        return u + np.sqrt(g*h)
    else:
        return 0

def lambda_tracer(q, _, g=1.):
    h, hu = q
    if h > 0:
        u = hu/h
        return u
    else:
        return 0

# == Move to demos?

def make_demo_plot_function(h_l=3., h_r=1., u_l=0., u_r=0,
                            figsize=(10,3), hlim=(0,3.5), ulim=(-1,1),
                            force_waves=None, stripes=True):

    g = 1.

    q_l = primitive_to_conservative(h_l,u_l)
    q_r = primitive_to_conservative(h_r,u_r)

    x = np.linspace(-1.,1.,1000)
    states, speeds, reval, wave_types = \
        exact_riemann_solution(q_l,q_r,g,force_waves=force_waves)

    # compute particle trajectories:
    def reval_rho_u(x):
        q = reval(x)
        rho = q[0]
        u = q[1]/q[0]
        rho_u = np.vstack((rho,u))
        return rho_u

    if stripes:
        x_traj, t_traj, xmax = \
            riemann_tools.compute_riemann_trajectories(states, speeds, 
                                                       reval_rho_u,
                                                       wave_types, i_vel=1, 
                                                       xmax=2,
                                                       rho_left=h_l/4.,
                                                       rho_right=h_r/4.)
    else:
        x_traj, t_traj, xmax = \
            riemann_tools.compute_riemann_trajectories(states, speeds, 
                                                       reval_rho_u,
                                                       wave_types, i_vel=1, 
                                                       xmax=2,
                                                       num_left=3,
                                                       num_right=3)

    num_vars = len(primitive_variables)

    def plot_shallow_water_demo(t=0.5, fig=0):
        if t == 0:
            q = np.zeros((2,len(x)))
            q[0,:] = q_l[0]*(x<=0) + q_r[0]*(x>0)
            q[1,:] = q_l[1]*(x<=0) + q_r[1]*(x>0)
        else:
            q = np.array(reval(x/t))

        if t<0.02:
            q[1] = np.where(x<0, q_l[1], q_r[1])

        primitive = conservative_to_primitive(q[0],q[1])

        if fig == 0:
            fig = plt.figure(figsize=figsize)
            show_fig = True
        else:
            show_fig = False

        axes = [0]*num_vars
        for i in range(num_vars):
            axes[i] = fig.add_subplot(1,num_vars,i+1)
            q = primitive[i]
            plt.plot(x,q,'-k',linewidth=3)
            plt.title(primitive_variables[i])
            if t != 0:
                plt.suptitle('Solution at time $t='+str(t)+'$',fontsize=12)
            else:
                plt.suptitle('Initial data',fontsize=12)
            axes[i].set_xlim(-1,1)

            if i==0 and force_waves != 'raref':
                # plot stripes only on depth plot
                # (and suppress if nonphysical solution plotted)
                n = np.where(t > t_traj)[0]
                if len(n)==0:
                    n = 0
                else:
                    n = min(n.max(), len(t_traj)-1)

                for j in range(1, x_traj.shape[1]-1):
                    j1 = np.where(x_traj[n,j] > x)[0]
                    if len(j1)==0:
                        j1 = 0
                    else:
                        j1 = min(j1.max(), len(x)-1)
                    j2 = np.where(x_traj[n,j+1] > x)[0]
                    if len(j2)==0:
                        j2 = 0
                    else:
                        j2 = min(j2.max(), len(x)-1)

                    # set advected color for density plot:
                    if x_traj[0,j]<0:
                        # shades of blue for fluid starting from x<0
                        if np.mod(j,2)==0:
                            c = 'dodgerblue'
                            alpha = 1.0
                        else:
                            c = 'lightblue'
                            alpha = 1.0
                    else:
                        # shades of blue for fluid starting from x<0
                        if np.mod(j,2)==0:
                            c = 'blue'
                            alpha = 1.0
                        else:
                            c = 'cornflowerblue'
                            alpha = 1.0
                    plt.fill_between(x[j1:j2],q[j1:j2],0,color=c,alpha=alpha)

        axes[0].set_ylim(hlim)
        axes[1].set_ylim(ulim)
        if show_fig:
            plt.show()

    return plot_shallow_water_demo

def macro_riemann_plot(which,context='notebook',figsize=(10,3)):
    """
    Some simulations to show that the Riemann solution describes macroscopic behavior
    in the Cauchy problem.
    """
    from IPython.display import HTML
    from clawpack import pyclaw
    from matplotlib import animation
    from clawpack.riemann import shallow_roe_tracer_1D

    depth = 0
    momentum = 1
    tracer = 2

    solver = pyclaw.ClawSolver1D(shallow_roe_tracer_1D)
    solver.num_eqn = 3
    solver.num_waves = 3
    solver.bc_lower[0] = pyclaw.BC.wall
    solver.bc_upper[0] = pyclaw.BC.wall
    x = pyclaw.Dimension(-1.0,1.0,2000,name='x')
    domain = pyclaw.Domain(x)
    state = pyclaw.State(domain,solver.num_eqn)

    state.problem_data['grav'] = 1.0

    grid = state.grid
    xc = grid.p_centers[0]

    hl = 3.
    hr = 1.
    ul = 0.
    ur = 0.

    xs = 0.1

    alpha = (xs-xc)/(2.*xs)
    if which=='linear':
        state.q[depth,:] = hl*(xc<=-xs) + hr*(xc>xs) + (alpha*hl + (1-alpha)*hr)*(xc>-xs)*(xc<=xs)
        state.q[momentum,:] = hl*ul*(xc<=-xs) + hr*ur*(xc>xs) + (alpha*hl*ul + (1-alpha)*hr*ur)*(xc>-xs)*(xc<=xs)
    elif which=='oscillatory':
        state.q[depth,:] = hl*(xc<=-xs) + hr*(xc>xs) + (alpha*hl + (1-alpha)*hr+0.2*np.sin(8*np.pi*xc/xs))*(xc>-xs)*(xc<=xs)
        state.q[momentum,:] = hl*ul*(xc<=-xs) + hr*ur*(xc>xs) + (alpha*hl*ul + (1-alpha)*hr*ur+0.2*np.cos(8*np.pi*xc/xs))*(xc>-xs)*(xc<=xs)

    state.q[tracer,:] = xc

    claw = pyclaw.Controller()
    claw.tfinal = 0.5
    claw.solution = pyclaw.Solution(state,domain)
    claw.solver = solver
    claw.keep_copy = True
    claw.num_output_times = 5
    claw.verbosity = 0

    claw.run()

    fig = plt.figure(figsize=figsize)
    ax_h = fig.add_subplot(121)
    ax_u = fig.add_subplot(122)
    fills = []
    frame = claw.frames[0]
    h = frame.q[0,:]
    u = frame.q[1,:]/h
    b = 0*h
    surface = h+b
    tracer = frame.q[2,:]

    x, = frame.state.grid.p_centers

    line, = ax_h.plot(x, surface,'-k',linewidth=3)
    line_u, = ax_u.plot(x, u,'-k',linewidth=3)

    fills = {'navy': None,
             'blue': None,
             'cornflowerblue': None,
             'deepskyblue': None}
    colors = fills.keys()

    def set_stripe_regions(tracer):
        widthl = 0.3/hl
        widthr = 0.3/hr
        # Designate areas for each color of stripe
        stripes = {}
        stripes['navy'] = (tracer>=0)
        stripes['blue'] = (tracer % widthr>=widthr/2.)*(tracer>=0)
        stripes['cornflowerblue'] = (tracer<=0)
        stripes['deepskyblue'] = (tracer % widthl>=widthl/2.)*(tracer<=0)
        return stripes

    stripes = set_stripe_regions(tracer)

    for color in colors:
        fills[color] = ax_h.fill_between(x,b,surface,facecolor=color,where=stripes[color],alpha=0.5)

    ax_h.set_xlabel('$x$'); ax_u.set_xlabel('$x$')
    ax_h.set_xlim(-1,1); ax_h.set_ylim(0,3.5)
    ax_u.set_xlim(-1,1); ax_u.set_ylim(-1,1)
    ax_u.set_title('Velocity'); ax_h.set_title('Depth')

    def fplot(frame_number):
        fig.suptitle('Solution at time $t='+str(frame_number/10.)+'$',fontsize=12)
        # Remove old fill_between plots
        for color in colors:
            fills[color].remove()

        frame = claw.frames[frame_number]
        h = frame.q[0,:]
        u = frame.q[1,:]/h
        b = 0*h
        tracer = frame.q[2,:]
        surface = h+b
        line.set_data(x,surface)
        line_u.set_data(x,u)
        stripes = set_stripe_regions(tracer)
        for color in colors:
            fills[color] = ax_h.fill_between(x,b,surface,facecolor=color,where=stripes[color],alpha=0.5)
        return line,

    if context in ['notebook','html']:
        anim = animation.FuncAnimation(fig, fplot, frames=len(claw.frames), interval=200, repeat=False)
        plt.close()
        return HTML(anim.to_jshtml())
    else:  # PDF output
        fplot(0)
        plt.show()
        fplot(2)
        return fig

def make_plot_functions(h_l, h_r, u_l, u_r,
                        g=1.,force_waves=None,extra_lines=None,stripes=True,
                        include_contact=False):
    
    q_l  = State(Depth = h_l,
                 Momentum = h_l*u_l)
    q_r = State(Depth = h_r,
                Momentum = h_r*u_r)
    states, speeds, reval, wave_types = \
        exact_riemann_solution(q_l,q_r,g, force_waves=force_waves, 
                               include_contact=include_contact)
        
    plot_function_stripes = make_demo_plot_function(h_l,h_r,u_l,u_r,
                                            figsize=(7,2),hlim=(0,4.5),ulim=(-2,2),
                                            force_waves=force_waves,stripes=stripes)
    def plot_function_xt_phase(plot_1_chars=False,plot_2_chars=False,plot_tracer_chars=False):
        plt.figure(figsize=(7,2))
        ax = plt.subplot(121)
        riemann_tools.plot_waves(states, speeds, reval, wave_types, t=0,
                                 ax=ax, color='multi')
        if plot_1_chars:
            riemann_tools.plot_characteristics(reval,lambda_1,
                                               axes=ax,extra_lines=extra_lines)
        if plot_2_chars:
            riemann_tools.plot_characteristics(reval,lambda_2,
                                               axes=ax,extra_lines=extra_lines)
        if plot_tracer_chars:
            riemann_tools.plot_characteristics(reval,lambda_tracer,
                                               axes=ax,extra_lines=extra_lines)
        ax = plt.subplot(122)
        phase_plane_plot(q_l,q_r,g,ax=ax,
                                       force_waves=force_waves,y_axis='u',
                                       include_contact=include_contact)
        plt.title('Phase plane')
        plt.show()
    return plot_function_stripes, plot_function_xt_phase


def plot_riemann_SW(h_l,h_r,u_l,u_r,g=1.,force_waves=None,extra_lines=None, 
                    tracer=False, particle_paths=True,plot1=False,plot2=False):
    stripes = not tracer
    plot_function_stripes, plot_function_xt_phase = \
                make_plot_functions(h_l,h_r,u_l,u_r,g,
                                    force_waves,extra_lines,stripes=stripes,
                                    include_contact=tracer)
    interact(plot_function_stripes, 
             t=widgets.FloatSlider(value=0.,min=0,max=.5), fig=fixed(0))
    if tracer:
        interact(plot_function_xt_phase, 
                 plot_1_chars=Checkbox(description='1-characteristics',
                                       value=plot1),
                 plot_2_chars=Checkbox(description='2-characteristics',
                                       value=plot2),
                 plot_tracer_chars=Checkbox(description='Tracer characteristics'))
    elif particle_paths:
    
        interact(plot_function_xt_phase, 
                 plot_1_chars=Checkbox(description='1-characteristics',
                                       value=plot1),
                 plot_2_chars=Checkbox(description='2-characteristics',
                                       value=plot2),
                 plot_tracer_chars=Checkbox(description='Particle paths'))
    else:
        interact(plot_function_xt_phase, 
                 plot_1_chars=Checkbox(description='1-characteristics',
                                       value=plot1),
                 plot_2_chars=Checkbox(description='2-characteristics',
                                       value=plot2),
                 plot_tracer_chars=fixed(value=False))  # suppress checkbox
