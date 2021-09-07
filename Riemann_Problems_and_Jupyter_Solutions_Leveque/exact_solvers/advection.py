
import sys, os
import matplotlib.pyplot as plt
import numpy as np
from utils import riemann_tools
from ipywidgets import interact
from ipywidgets import widgets

def characteristics(a=1.):
    x = np.linspace(-2*np.abs(a)-1,2*np.abs(a)+1,41)
    t = np.linspace(0,1,20)
    for ix in x:
        plt.plot(ix+a*t,t,'-k',lw=0.5)
    plt.xlim(0,1)
    plt.ylim(0,1)
    plt.title('Characteristics $x-at=C$')
    plt.xlabel('$x$')
    plt.ylabel('$t$')

def solution(a=1.):
    characteristics(a)
    plt.title('Propagation along characteristics')
    xx = np.linspace(-2*np.abs(a),2*np.abs(a)+1,1000)
    q = 0.1*np.exp(-100*(xx-0.5)**2)
    plt.plot(xx,q,'-r')
    spacing = 0.04
    number = 20
    for i in range(number):
        plt.plot(xx+spacing*i*a,q+spacing*i,'-r')

def riemann_demo(a=1.):
    characteristics(a)
    plt.xlim(-0.8,0.8)
    plt.title('Solution of the Riemann problem')
    xx = np.linspace(-2*np.abs(a)-1,2*np.abs(a)+1,1000)
    #q = 0.05+0.05*(xx<0.)
    q = 0.05*(xx<0.)
    spacing = 0.04
    number = 20
    for i in range(number):
        plt.plot(xx+spacing*i*a,q+spacing*i,'-r')

def riemann_solution(q_l, q_r, a=1.):
    """
    Solve the Riemann problem for the advection equation, with velocity a.
    """
    states = np.array([[q_l, q_r]])
    speeds = [a]
    wave_types = ['contact']
    def reval(xi):
        q = np.zeros((1,len(xi)))
        q[0,:] = (xi<a)*q_l + (xi>=a)*q_r
        return q

    return states, speeds, reval, wave_types

def plot_riemann_solution(ql, qr, a):
    c = lambda q, xi: a
    soln = riemann_solution(ql ,qr, a)

    plot_advection = riemann_tools.make_plot_function(*soln,
                                                      plot_chars=[c])

    return interact(plot_advection,
                    t=widgets.FloatSlider(value=0.0,min=0,max=1.0),
                    which_char=widgets.fixed(True))
