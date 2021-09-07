import sys, os
import numpy as np
from scipy.optimize import fsolve
import matplotlib.pyplot as plt

conserved_variables = ('Strain', 'Momentum')
primitive_variables = ('Strain', 'Velocity')
colors = {'shock': 'r', 'raref': 'b', 'contact': 'k'}


def primitive_to_conservative(eps, u, rho):
    mom = rho*u
    return eps, mom


def conservative_to_primitive(eps, mom, rho):
    u = mom/rho
    return eps, u

def sigma(eps, K1, K2):
    "Stress."
    return K1*eps + K2*eps**2

def dsigma(eps, K1, K2):
    "Derivative of stress w.r.t. strain."
    return K1 + 2*K2*eps

def sound_speed(eps, K1, K2, rho):
    return np.sqrt(dsigma(eps, K1, K2)/rho)


def exact_riemann_solution(q_l,q_r,aux_l,aux_r,phase_plane_curves=False):
    """
    Return the exact solution to the Riemann problem with initial states
    q_l, q_r.  The solution is given in terms of a list of states, a list of
    speeds (each of which may be a pair in case of a rarefaction fan), and a
    function reval(xi) that gives the solution at a point xi=x/t.

    The input and output vectors are the conserved quantities.

    If phase_plane_curves==True, then the appropriate Hugoniot Locus and/or
    integral curve is returned for the 1- and 2-waves.

    For the variable-coefficient nonlinear elasicity problem:

           eps_t - u_x = 0
           (\rho(x) u)_t - \sigma(eps,x) = 0

    the solution has two intermediate states.  These must be connected to
    the left and right states by an integral curve or hugoniot locus, and
    to each other by continuity of the velocity and stress.
    """
    eps_l, mom_l = q_l
    eps_r, mom_r = q_r
    rho_l, K1_l, K2_l = aux_l
    rho_r, K1_r, K2_r = aux_r

    u_l = mom_l / rho_l
    u_r = mom_r / rho_r

    sigma_l = sigma(eps_l, K1_l, K2_l)
    sigma_r = sigma(eps_r, K1_r, K2_r)

    # Define the integral curves and hugoniot loci (u as a function of eps)
    integral_curve_1   = lambda eps: u_l + 1./(3*K2_l*np.sqrt(rho_l))*( dsigma(eps,K1_l,K2_l)**1.5 - dsigma(eps_l,K1_l,K2_l)**1.5 )
    integral_curve_2   = lambda eps: u_r - 1./(3*K2_r*np.sqrt(rho_r))*( dsigma(eps,K1_r,K2_r)**1.5 - dsigma(eps_r,K1_r,K2_r)**1.5 )
    hugoniot_locus_1   = lambda eps: u_l - np.sqrt((sigma(eps,K1_l,K2_l)-sigma_l)*(eps-eps_l)/rho_l)
    hugoniot_locus_2   = lambda eps: u_r - np.sqrt((sigma(eps,K1_r,K2_r)-sigma_r)*(eps-eps_r)/rho_r)

    # Check whether the 1-wave is a shock or rarefaction
    # It is a shock if eps^*_l > eps_l
    def phi_l(eps):
        if eps >= eps_l: return hugoniot_locus_1(eps)
        else: return integral_curve_1(eps)

    # Check whether the 2-wave is a shock or rarefaction
    def phi_r(eps):
        if eps >= eps_r: return hugoniot_locus_2(eps)
        else: return integral_curve_2(eps)

    def rsol(eps):
        eps_star_l = eps[0]
        eps_star_r = eps[1]
        resid = np.zeros(2)
        resid[0] = phi_l(eps_star_l) - phi_r(eps_star_r)  # Continuity of u (velocity)
        resid[1] = sigma(eps_star_l,K1_l,K2_l) - sigma(eps_star_r,K1_r,K2_r)  # Continuity of stress
        return resid

    # Compute middle states by finding curve intersection
    guess = np.array([(eps_l+eps_r)/2., (eps_l+eps_r)/2.])
    eps, info, ier, msg = fsolve(rsol, guess, full_output=True, xtol=1.e-10)
    # This should not happen:
    if ier != 1:
        print('Warning: fsolve did not converge.')
        print(msg)

    eps_star_l, eps_star_r = eps
    u_star_l = phi_l(eps_star_l)
    u_star_r = phi_r(eps_star_r)  # These should be equal

    # compute the wave speeds
    ws = np.zeros(5)
    # The stationary wave at x=0:
    ws[2] = 0.
    wave_types = ['', 'contact', '']

    # Find shock and rarefaction speeds
    if eps_star_l > eps_l:  # 1-Shock
        wave_types[0] = 'shock'
        ws[0] = np.sqrt((sigma(eps_star_l,K1_l,K2_l)-sigma_l)/(rho_l*(eps_star_l-eps_l)))
        ws[1] = ws[0]
    else:  # 1-Rarefaction
        wave_types[0] = 'raref'
        ws[0] = -sound_speed(eps_l,K1_l,K2_l,rho_l)
        ws[1] = -sound_speed(eps_star_l,K1_l,K2_l,rho_l)

    if eps_star_r > eps_r:  # 2-shock
        wave_types[2] = 'shock'
        ws[4] = np.sqrt((sigma(eps_star_r,K1_r,K2_r)-sigma_r)/(rho_r*(eps_star_r-eps_r)))
        ws[3] = ws[4]
    else:  # 2-rarefaction
        wave_types[2] = 'raref'
        ws[3] = sound_speed(eps_star_r,K1_r,K2_r,rho_r)
        ws[4] = sound_speed(eps_r,K1_r,K2_r,rho_r)

    # Find solution inside rarefaction fans (in primitive variables)
    def raref1(xi):
        eps = (rho_l*xi**2-K1_l)/(2.*K2_l)
        u = integral_curve_1(eps)
        return eps, u

    def raref2(xi):
        eps = (rho_r*xi**2-K1_r)/(2.*K2_r)
        u = integral_curve_2(eps)
        return eps, u

    q_star_l = np.array([eps_star_l, u_star_l*rho_l])
    q_star_r = np.array([eps_star_r, u_star_r*rho_r])

    states = np.column_stack([q_l, q_star_l, q_star_r, q_r])
    speeds = [[], ws[2], []]
    if wave_types[0] is 'shock':
        speeds[0] = ws[0]
    else:
        speeds[0] = (ws[0],ws[1])
    if wave_types[2] is 'shock':
        speeds[2] = ws[3]
    else:
        speeds[2] = (ws[3],ws[4])

    def reval(xi):
        r"""Returns the Riemann solution in primitive variables for any
            value of xi = x/t.
        """
        rar1 = raref1(xi)
        rar2 = raref2(xi)
        eps_out =  (xi<=ws[0]                  )*eps_l      \
                 + (xi> ws[0])*(xi<=ws[1])*rar1[0]    \
                 + (xi> ws[1])*(xi<=speeds[1]   )*eps_star_l \
                 + (xi>speeds[1])   *(xi<=ws[3])*eps_star_r \
                 + (xi> ws[3])*(xi<=ws[4])*rar2[0]    \
                 + (xi> ws[4]                   )*eps_r

        u_out   =  (xi<=ws[0]                  )*u_l     \
                 + (xi> ws[0])*(xi<=ws[1])*rar1[1] \
                 + (xi> ws[1])*(xi<=speeds[1]   )*u_star_l       \
                 + (xi>speeds[1]   )*(xi<=ws[3])*u_star_r       \
                 + (xi> ws[3])*(xi<=ws[4])*rar2[1] \
                 + (xi> ws[4]                   )*u_r

        rho_out = (xi<=0)*rho_l + (xi>0)*rho_r
        return primitive_to_conservative(eps_out, u_out, rho_out)

    if phase_plane_curves:
        return states, speeds, reval, wave_types, (eps_star_l, eps_star_r, phi_l, phi_r)
    else:
        return states, speeds, reval, wave_types


def phase_plane_plot(q_l, q_r, aux_l, aux_r, ax=None):
    r"""Plot the Hugoniot loci or integral curves in the epsilon-u plane."""

    eps_l, mom_l = q_l
    eps_r, mom_r = q_r
    rho_l, K1_l, K2_l = aux_l
    rho_r, K1_r, K2_r = aux_r

    u_l = mom_l / rho_l
    u_r = mom_r / rho_r

    sigma_l = sigma(eps_l, K1_l, K2_l)
    sigma_r = sigma(eps_r, K1_r, K2_r)

    integral_curve_1   = lambda eps: u_l + 1./(3*K2_l*np.sqrt(rho_l))*( dsigma(eps,K1_l,K2_l)**1.5 - dsigma(eps_l,K1_l,K2_l)**1.5 )
    integral_curve_2   = lambda eps: u_r - 1./(3*K2_r*np.sqrt(rho_r))*( dsigma(eps,K1_r,K2_r)**1.5 - dsigma(eps_r,K1_r,K2_r)**1.5 )
    hugoniot_locus_1   = lambda eps: u_l - np.sqrt((sigma(eps,K1_l,K2_l)-sigma_l)*(eps-eps_l)/rho_l)
    hugoniot_locus_2   = lambda eps: u_r - np.sqrt((sigma(eps,K1_r,K2_r)-sigma_r)*(eps-eps_r)/rho_r)

    # Solve Riemann problem
    ex_states, ex_speeds, reval, wave_types, ppc = \
        exact_riemann_solution(q_l, q_r, aux_l, aux_r, phase_plane_curves=True)
    eps_star_l, eps_star_r, w1, w2 = ppc

    # Set plot bounds
    if ax is None:
        fig, ax = plt.subplots()
    x = (q_l[0], eps_star_l, eps_star_r, q_r[0])
    y = (u_l, w1(eps_star_l), w2(eps_star_r), u_r)
    xmax, xmin = max(x), min(x)
    ymax, ymin = max(y), min(y)
    ymax = max(abs(ymax), abs(ymin))
    dx, dy = xmax - xmin, ymax - ymin
    scalefac = 1.2
    ax.set_xlim(0, xmax*scalefac)
    ax.set_ylim(-scalefac*ymax, scalefac*ymax)
    ax.set_xlabel('Strain ($\epsilon$)')
    ax.set_ylabel('Velocity (u)')

    # Plot curves
    w1v, w2v = (np.vectorize(w1), np.vectorize(w2))

    eps1 = np.linspace(1.e-2, eps_star_l)
    eps2 = np.linspace(eps_star_l, xmax*scalefac)
    if wave_types[0] == 'shock':
        ax.plot(eps1, hugoniot_locus_1(eps1), '-r', lw=2)
        ax.plot(eps2, hugoniot_locus_1(eps2), '--r', lw=2)
    else:  # 1-rarefaction
        ax.plot(eps1, integral_curve_1(eps1), '--b', lw=2)
        ax.plot(eps2, integral_curve_1(eps2), '-b', lw=2)

    eps1 = np.linspace(1.e-2, eps_star_r)
    eps2 = np.linspace(eps_star_r, xmax*scalefac)
    if wave_types[2] == 'shock':
        ax.plot(eps1, hugoniot_locus_2(eps1), '-r', lw=2)
        ax.plot(eps2, hugoniot_locus_2(eps2), '--r', lw=2)
    else:  # 2-rarefaction
        ax.plot(eps1, integral_curve_2(eps1), '--b', lw=2)
        ax.plot(eps2, integral_curve_2(eps2), '-b', lw=2)

    ax.plot([eps_star_l,eps_star_r],[w1v(eps_star_l),w2v(eps_star_r)],colors[wave_types[1]])
    for xp, yp in zip(x, y):
        ax.plot(xp, yp, 'ok', markersize=10)
    # Label states
    for i, label in enumerate(('$q_l$', '$q_l^*$', '$q_r^*$', '$q_r$')):
        ax.text(x[i] + 0.05*dx, y[i] + 0.05*dy, label)
