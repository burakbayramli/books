import sys, os
import matplotlib.pyplot as plt
import numpy as np

from . import nonconvex

from utils import riemann_tools

figsize =(8,4)

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
            plt.figure(figsize=figsize);
            
        # plot solution q(x,t):
        plt.subplot(1,3,1)
        # from characteristic-based solution:
        plt.plot(xi_char*t,q_char,'k--') 

        # single-valued solution, extended full domain:
        xi_plot = np.hstack((xi_left,xi*t,xi_right))
        q_plot = np.hstack((q_left,qxi,q_right))
        plt.plot(xi_plot, q_plot, 'k', linewidth=2)
        plt.title('Solution q(x,t)\n at t = %4.2f' % t)
        plt.xlabel('x')
        plt.xlim(xi_left,xi_right)
        
        ax = plt.subplot(1,3,2)
        plot_waves(f,q_left,q_right,xi_left,xi_right,n=1000,axes=ax,t=t)
        #ax.set_xlim(xi_left,xi_right)
                
        # plot flux function and convex hull:
        plt.subplot(1,3,3)
        q_plot = np.linspace(q_left, q_right, 1000)
        f_plot = f(q_plot)
        plt.plot(q_plot, f_plot, 'k--', label='f(q)')
        plt.plot(qxi, f(qxi),'k', label='Convex hull')
        plt.plot([q_left,q_right],[f(q_left),f(q_right)],'bo')
        plt.title('Flux function (dashed)\n Convex hull (solid)')
        plt.xlabel('q')
        plt.tight_layout()
        #plt.legend()

        if fig==0: plt.show()
        return None
    return plot_function

def demo1():
    f = lambda q: q*(1-q)

    plt.figure(figsize=figsize)
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

    plt.figure(figsize=(8,3))  # figures look better this way
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


#===

def make_plot_function_qsliders(f):
    
    def plot_function(t=0.2, q_left=1, q_right=0, fig=0):
        """
        Create plot at time t.
        Nonzero fig is used only by jsanimate_widgets when
        converting to html files.
        """
        if fig==0: 
            plt.figure(figsize=figsize)

        xi_left = -3.
        xi_right = 3.
        xi, qxi, q_char, xi_char = \
                    nonconvex.nonconvex_solutions(f, q_left, q_right, 
                                                  xi_left, xi_right)
        # plot solution q(x,t):
        plt.subplot(1,3,1)
        # from characteristic-based solution:
        plt.plot(xi_char*t,q_char,'k--') 

        # single-valued solution, extended full domain:
        xi_plot = np.hstack((xi_left,xi*t,xi_right))
        q_plot = np.hstack((q_left,qxi,q_right))
        plt.plot(xi_plot, q_plot, 'k', linewidth=2)
        plt.title('Solution q(x,t)\n at t = %4.2f' % t)
        plt.xlabel('x')
        plt.ylim(-4,4)

        plt.xlim(xi_left,xi_right)
        
        ax = plt.subplot(1,3,2)
        plot_waves(f,q_left,q_right,xi_left,xi_right,n=1000,axes=ax,t=t)
        ax.set_xlim(-1.5,1.5)

        # plot flux function and convex hull:
        plt.subplot(1,3,3)
        ql_plot = -4
        qr_plot = 4
        q_plot = np.linspace(ql_plot,qr_plot, 1000)
        f_plot = f(q_plot)
        plt.plot(q_plot, f_plot, 'k--', label='f(q)')
        plt.plot(qxi, f(qxi),'k', label='Convex hull')
        plt.plot([q_left,q_right],[f(q_left),f(q_right)],'bo')
        plt.title('Flux function (dashed)\n Convex hull (solid)')
        plt.xlabel('q')
        plt.xlim(ql_plot,qr_plot)
        plt.ylim(-0.8,1.2)
        plt.tight_layout()
        #plt.legend()


        
        if fig==0: plt.show()
        return None
    return plot_function
