import sys, os
import matplotlib.pyplot as plt
import numpy as np

from . import shallow_water

from utils import riemann_tools

figsize =(8,4)

def plot_int_curves(plot_1=True,plot_2=False,y_axis='hu'):
    N = 400
    g = 1.
    if y_axis == 'hu':
        h, hu = np.meshgrid(np.linspace(0.01,3,N),
                            np.linspace(-3,3,N))
        u = hu/h
        yvar = hu
        ylabel = 'momentum (hu)'
    else:
        h, u = np.meshgrid(np.linspace(0.01,3,N),
                           np.linspace(-3,3,N))
        yvar = u
        ylabel = 'velocity (u)'
    w1 = u + 2 * np.sqrt(g*h)
    w2 = u - 2 * np.sqrt(g*h)
    if plot_1:
        clines = np.linspace(-4,6,21)
        plt.contour(h,yvar,w1,clines,colors='cornflowerblue',
                    linestyles='solid')
    if plot_2:
        clines = np.linspace(-6,4,21)
        plt.contour(h,yvar,w2,clines,colors='lightblue',
                    linestyles='solid')

    plt.axis((0,3,-3,3))
    plt.xlabel('depth h')
    plt.ylabel(ylabel)
    legend = plot_1*['1-curves'] + plot_2*['2-curves']
    plt.title('Integral curves: %s' % legend)
    plt.show()
    
    
def compare_curves(wave_family=1, h0=1., u0=0., y_axis='u'):
    h = np.linspace(1.e-2,3,400)

    u = shallow_water.integral_curve(h,h0,h0*u0,
                                     wave_family,y_axis=y_axis)
    plt.plot(h,u,'b')
    u = shallow_water.hugoniot_locus(h,h0,h0*u0,
                                     wave_family,y_axis=y_axis)
    plt.plot(h,u,'r')
    plt.legend(['Integral curve','Hugoniot locus'])
    if y_axis == 'u':
        plt.plot(h0,u0,'ok')
        plt.ylabel('velocity (u)')
    else:
        plt.plot(h0,h0*u0,'ok')
        plt.ylabel('momentum (hu)')
    plt.xlabel('depth h')
    plt.title('Wave family %i' % wave_family)
    
    xlimits = plt.xlim()
    if xlimits[0] <= 0.:
        # shift xlimits to better show dry state:
        x0 = -0.05*(xlimits[1] - xlimits[0])
        plt.xlim(x0,xlimits[1])
        ylimits = plt.ylim()
        plt.plot([0,0], ylimits, 'k-', linewidth=0.6)  # y-axis
    plt.show()
    
def connect_states(h_l=1.,u_l=-1.,h_r=1.,u_r=1.):
    q_l = np.array([h_l,h_l*u_l])
    q_r = np.array([h_r,h_r*u_r])
    fig, ax = plt.subplots(1,2,figsize=figsize)
    shallow_water.phase_plane_curves(q_l[0], q_l[1], 'qleft', 
                  wave_family=1,ax=ax[0])
    shallow_water.phase_plane_curves(q_r[0], q_r[1], 'qright', 
                  wave_family=2,ax=ax[0])
    shallow_water.phase_plane_curves(q_l[0], q_l[1], 'qleft', 
                  wave_family=1,y_axis='hu',ax=ax[1])
    shallow_water.phase_plane_curves(q_r[0], q_r[1], 'qright', 
                  wave_family=2,y_axis='hu',ax=ax[1])
    ax[0].set_title('h-u plane'); ax[1].set_title('h-hu plane'); 
    ax[0].set_xlim(0,3); ax[1].set_xlim(0,3);
    ax[0].set_ylim(-10,10); ax[1].set_ylim(-10,10); 
    plt.tight_layout(); plt.show()
    
    
#============

def plot_waves(f,q_left, q_right, xi_left, xi_right, n=1000, axes=None, t=0.2):
    qtilde = nonconvex.osher_solution(f, q_left, q_right, 1000)
    xi = np.linspace(xi_left, xi_right, n)
    qvals = qtilde(xi)
    fvals = f(qvals)
    dxi = xi[1]-xi[0]
    smoothness = riemann_tools.detect_smoothness(qvals,dxi,dmax=10)
    values, ranges = riemann_tools.intervals(smoothness)
    
    # filter out extraneous constant states between rarefactions:
    # For complicated nonconvex fluxes, 
    # values tend to contain sequences [1,0,1] indicating a constant
    # state between two rarefaction waves, which shouldn't happen,
    # so merge such ranges into a single rarefaction wave.
    
    jd = []
    for j in range(len(values)):
        try:
            if values[j]==0 and values[j+1]==1 and values[j-1]==1:
                jd.append(j)         
        except:
            pass
    jd.reverse()
    for j in jd:
        ranges[j-1] = (ranges[j-1][0], ranges[j+1][1])  # merge
        values.pop(j+1)
        values.pop(j)
        ranges.pop(j+1)
        ranges.pop(j)  

    wave_types, speeds = riemann_tools.make_waves(values, ranges, xi)
    riemann_tools.plot_waves(None,speeds,None,wave_types,ax=axes,
                             t_pointer=False,t=t)
    #plt.xlim(xi_left, xi_right)
    plt.xlabel('x')

def make_plot_function(f, q_left, q_right, xi_left, xi_right):
    
    xi, qxi, q_char, xi_char = nonconvex.nonconvex_solutions(f, q_left, q_right, 
                                                             xi_left, xi_right)
    def plot_function(t=0.2, fig=0):
        """
        Create plot at time t.
        Nonzero fig is used only by jsanimate_widgets when
        converting to html files.
        """
        if fig==0: 
            #plt.figure(figsize=(14,6))
            plt.figure()
            
        # plot solution q(x,t):
        plt.subplot(1,3,1)
        # from characteristic-based solution:
        plt.plot(xi_char*t,q_char,'k--') 

        # single-valued solution, extended full domain:
        xi_plot = np.hstack((xi_left,xi*t,xi_right))
        q_plot = np.hstack((q_left,qxi,q_right))
        plt.plot(xi_plot, q_plot, 'k', linewidth=2)
        plt.title('Solution q(x,t) at t = %4.2f' % t)
        plt.xlabel('x')

        plt.xlim(xi_left,xi_right)
        
        # plot flux function and convex hull:
        plt.subplot(1,3,3)
        q_plot = np.linspace(q_left, q_right, 1000)
        f_plot = f(q_plot)
        plt.plot(q_plot, f_plot, 'k--', label='f(q)')
        plt.plot(qxi, f(qxi),'k', label='Convex hull')
        plt.plot([q_left,q_right],[f(q_left),f(q_right)],'bo')
        plt.title('Flux function')
        plt.xlabel('q')
        plt.legend()

        ax = plt.subplot(1,3,2)
        plot_waves(f,q_left,q_right,xi_left,xi_right,n=1000,axes=ax,t=t)
        #ax.set_xlim(xi_left,xi_right)
        
        if fig==0: plt.show()
        return None
    return plot_function

def demo1():
    f = lambda q: q*(1-q)

    #plt.figure(figsize=(12,5))
    plt.subplot(121)

    q_left = 0.6;  q_right = 0.1
    xi, qxi, q_char, xi_char = nonconvex.nonconvex_solutions(f, q_left, q_right,
                                                             -1.5,1.5)
    plt.plot(xi_char, q_char,'r')
    plt.plot(xi, qxi, 'k', linewidth=2)
    plt.ylim(0.,0.7)
    plt.title('Rarefaction solution')

    plt.subplot(122)

    q_left = 0.1;  q_right = 0.6
    xi, qxi, q_char, xi_char = nonconvex.nonconvex_solutions(f, q_left, q_right,
                                                             -1.5,1.5)
    plt.plot(xi_char, q_char,'k--')
    plt.plot(xi, qxi, 'k', linewidth=2)
    plt.ylim(0.,0.7)
    plt.title('Shock solution');
    
def plot_flux(f, q_left, q_right, plot_zero=True):
    qvals = np.linspace(q_right, q_left, 200)
    fvals = f(qvals)
    dfdq = np.diff(fvals) / (qvals[1]-qvals[0])  # approximate df/dq
    qmid = 0.5*(qvals[:-1] + qvals[1:])   # midpoints for plotting dfdq

    #plt.figure(figsize=(12,4))
    plt.subplot(131)
    plt.plot(qvals,fvals)
    plt.xlabel('q')
    plt.ylabel('f(q)')
    plt.title('flux function f(q)')

    plt.subplot(132)
    plt.plot(qmid, dfdq)
    plt.xlabel('q')
    plt.ylabel('df/dq')
    plt.title('characteristic speed df/dq')

    plt.subplot(133)
    plt.plot(dfdq, qmid)
    plt.xlabel('df/dq')
    plt.ylabel('q')
    plt.title('q vs. df/dq')
    if plot_zero:
        plt.plot([0,0],[qmid.min(), qmid.max()],'k--')

    plt.subplots_adjust(left=0.)
    plt.tight_layout()

