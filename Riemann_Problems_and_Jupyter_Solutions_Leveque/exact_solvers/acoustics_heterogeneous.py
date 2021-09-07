"""
Exact Riemann solver for the acoustic wave equation in 1D in
heterogeneous media.
"""
import sys, os
import numpy as np
from utils import riemann_tools

def lambda1_het(q, xi, aux):
    """Characteristic speed for 1-waves in a heterogeneous medium."""
    rho, K = aux
    return -np.sqrt(K/rho)

def lambda2_het(q, xi, aux):
    """Characteristic speed for 2-waves in a heterogeneous medium."""
    rho, K = aux
    return np.sqrt(K/rho)

def exact_riemann_solution(ql, qr, auxl, auxr):
    """ Exact solution of Riemann problem for acoustics equations in
    heterogeneous media."""
    
    # Define delta q, speeds and impedance (left and right)
    dq = qr - ql
    rhol, bulkl = auxl
    rhor, bulkr = auxr
    cl = np.sqrt(bulkl/rhol)
    cr = np.sqrt(bulkr/rhor)
    Zl = rhol*cl
    Zr = rhor*cr

    # Define the 2 eigenvectors
    r1 = np.array([-Zl, 1])
    r2 = np.array([Zr,  1])

    # Compute the alphas
    alpha1 = (-dq[0] + dq[1]*Zr)/(Zl + Zr)
    alpha2 = (dq[0] + dq[1]*Zl)/(Zl + Zr)

    # Compute middle state qm
    qm = ql + alpha1*r1
    # It is equivalent to
    #qm = qr - alpha2*r2

    # Compute waves speeds (characteristics)
    speeds = np.zeros(2)
    speeds[0] = -cl
    speeds[1] = cr

    # Concatenate states for plotting
    states = np.column_stack([ql,qm,qr])

    # Calculate reval function (only necessary for plotting)
    def reval(xi):
            r"""Returns the Riemann solution in for any value of xi = x/t.
            """
            p_out =  (xi<=speeds[0]                 )*ql[0]      \
                    + (xi>speeds[0])*(xi<=speeds[1])*qm[0]    \
                    + (xi>speeds[1]                 )*qr[0]

            u_out =  (xi<=speeds[0]                 )*ql[1]      \
                    + (xi>speeds[0])*(xi<=speeds[1])*qm[1]    \
                    + (xi>speeds[1]                 )*qr[1]
            return p_out, u_out

    return states, speeds, reval

def riemann_plot_func(q_l, q_r, auxl, auxr):
    """Return Riemann plot function for (only) time-dependent interacts. """
    ex_states, ex_speeds, reval = exact_riemann_solution(q_l ,q_r, auxl, auxr)

    plot_function = riemann_tools.make_plot_function(ex_states, ex_speeds, reval, layout='vertical',
                                                    variable_names=['pressure', 'velocity'],
                                                    aux=(np.array(auxl),np.array(auxr)), 
                                                    plot_chars=[lambda1_het, lambda2_het])
    return plot_function

