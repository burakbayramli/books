"""
Exact Riemann solver for the acoustics wave equation in 1D, 
including some plotting functionality.
"""
import sys, os
import numpy as np
from utils import riemann_tools

def lambda1(q, xi, aux):
    """Characteristic speed for 1-waves."""
    rho, bulk = aux
    return -np.sqrt(bulk/rho)

def lambda2(q, xi, aux):
    """Characteristic speed for 2-waves."""
    rho, bulk = aux
    return np.sqrt(bulk/rho)

def exact_riemann_solution(ql, qr, aux):
    """ Exact solution of Riemann problem for acoustics  equations."""
    
    # Define delta q, speeds and impedance
    rho, bulk = aux
    dq = qr - ql
    c = np.sqrt(bulk/rho)
    Z = rho*c

    # Define the 2 eigenvectors
    r1 = np.array([-Z, 1])
    r2 = np.array([Z,  1])

    alpha1 = (-dq[0] + dq[1]*Z)/(2*Z)
    alpha2 = (dq[0] + dq[1]*Z)/(2*Z)

    # Compute middle state qm
    qm = ql + alpha1*r1
    # It is equivalent to
    #qm = qr - alpha2*r2

    # Compute wave speeds
    speeds = np.zeros(2)
    speeds[0] = -c
    speeds[1] = c

    # Concatenate states for plotting
    states = np.column_stack([ql,qm,qr])

    # Calculate reval function (used for plotting the solution)
    def reval(xi):
            r"""Returns the Riemann solution for any value of xi = x/t.
            """
            p_out =  (xi<=speeds[0]                 )*ql[0]      \
                    + (xi>speeds[0])*(xi<=speeds[1])*qm[0]    \
                    + (xi>speeds[1]                 )*qr[0]

            u_out =  (xi<=speeds[0]                 )*ql[1]      \
                    + (xi>speeds[0])*(xi<=speeds[1])*qm[1]    \
                    + (xi>speeds[1]                 )*qr[1]
            return p_out, u_out

    return states, speeds, reval


def riemann_plot_func(q_l, q_r, aux):
    """Return Riemann plot function for (only) time-dependent interacts. """
    ex_states, ex_speeds, reval = exact_riemann_solution(q_l ,q_r, aux)

    plot_function = riemann_tools.make_plot_function(ex_states, ex_speeds, reval, layout='vertical',
                                                    variable_names=['pressure', 'velocity'],
                                                    aux=(np.array(aux),np.array(aux)), 
                                                    plot_chars=[lambda1, lambda2])
    return plot_function




