from __future__ import print_function
import sys, os
import numpy as np
from scipy.optimize import fsolve
import matplotlib.pyplot as plt
from utils import riemann_tools
from ipywidgets import widgets
from ipywidgets import interact
from IPython.display import display

conserved_variables = ('Density', 'Momentum', 'Energy')
primitive_variables = ('Density', 'Velocity', 'Pressure')

def pospart(x):
    return np.maximum(1.e-15,x)

def primitive_to_conservative(rho, u, p, gamma, pinf):
    mom = rho*u
    E   = (p - gamma * pinf)/(gamma-1.) + 0.5*rho*u**2
    return rho, mom, E

def conservative_to_primitive(rho, mom, E, gamma, pinf):
    u = mom/pospart(rho)
    p = (gamma-1.)*(E - 0.5*rho*u**2) - gamma*pinf
    return rho, u, p

def cons_to_prim(q, gamma = 1.4, pinf = 0.0):
    return conservative_to_primitive(*q,gamma=gamma,pinf=pinf)

def sound_speed(rho, p, gamma = 1.4, pinf = 0.0):
    return np.sqrt(gamma*(p+pinf)/pospart(rho))

def alpha(rho, gamma):
    return 2.0/(pospart(rho)*(gamma + 1.0))

def beta(p, gamma, pinf):
    return pospart(p + pinf)*(gamma - 1.0)/(gamma + 1.0)

def lambda1(q, xi, aux, varout='primitive'):
    "Characteristic speed for 1-waves."
    gamma, pinf = aux
    if varout == 'primitive':
            rho, u, p = q
    else:
        rho, u, p = conservative_to_primitive(*q, gamma=gamma,pinf=pinf)
    al = alpha(rho, gamma)
    be = beta(p, gamma, pinf)
    return u - np.sqrt(gamma*(p + pinf)/pospart(rho))

def lambda2(q, xi, aux, varout='primitive'):
    "Characteristic speed for 2-waves."
    gamma, pinf = aux
    if varout == 'primitive':
            rho, u, p = q
    else:
        rho, u, p = conservative_to_primitive(*q, gamma=gamma,pinf=pinf)
    return u

def lambda3(q, xi, aux, varout='primitive'):
    "Characteristic speed for 3-waves."
    gamma, pinf = aux
    if varout == 'primitive':
            rho, u, p = q
    else:
        rho, u, p = conservative_to_primitive(*q, gamma=gamma,pinf=pinf)
    al = alpha(rho, gamma)
    be = beta(p, gamma, pinf)
    return u + np.sqrt(gamma*(p + pinf)/pospart(rho))

def integral_curve_1(p, rhol, ul, pl, gammal = 1.4, pinfl = 0.0):
    """Velocity as a function of pressure for the 1-integral curve passing
       through (rhostar, ustar, pstar)"""
    cl = sound_speed(rhol, pl, gammal, pinfl)
    pbl = pospart(pl + pinfl)
    gl1 = gammal - 1.0  
    return ul + 2*cl/gl1*(1 - (pospart(p + pinfl)/pbl)**(gl1/(2.0*gammal)))

def integral_curve_3(p, rhor, ur, pr, gammar = 1.4, pinfr = 0.0):
    c = sound_speed(rhor, pr, gammar, pinfr)
    pbr = pospart(pr + pinfr)
    gr1 = gammar - 1.0  
    return ur - 2*cr/gr1*(1 - (pospart(p + pinfr)/pbr)**(gr1/(2.0*gammar)))

def hugoniot_locus_1(p, rhol, ul, pl, gammar = 1.4, pinfr = 0.0):
    c = sound_speed(rhol, pl, gammal, pinfl)
    al = alpha(rhol, gammal)
    be = beta(pl, gammal, pinfl)
    return ul - (p - pl)*np.sqrt(al/(p + pinfl + be))

def hugoniot_locus_3(p, rhor, ur, pr, gammar = 1.4, pinfr = 0.0):
    c = sound_speed(rhor, pr, gammar, pinfr)
    ar = alpha(rhor, gammar)
    be = beta(pr, gammar, pinfr)
    return ur + (p - pr)*np.sqrt(ar/(p + pinfr + be))


def exact_riemann_solution(ql, qr, auxl, auxr, varin = 'primitive', varout = 'primitive'):

    # Get intial data
    gammal, pinfl = auxl
    gammar, pinfr = auxr
    if varin == 'conservative':
        rhol, ul, pl = conservative_to_primitive(*ql, gamma = gammal, pinf = pinfl)
        rhor, ur, pr = conservative_to_primitive(*qr, gamma = gammar, pinf = pinfr)
    else:
        rhol, ul, pl = ql
        rhor, ur, pr = qr

    # Bar pressure (convenient change of variable)
    pbl = pl + pinfl
    pbr = pr + pinfr

    # Useful parameters
    gl1 = gammal - 1.0
    gr1 = gammar - 1.0
    bl = (gammal + 1.0)/(gammal - 1.0)
    br = (gammar + 1.0)/(gammar - 1.0)
    betal = pbl/bl
    betar = pbr/br
    al = 2.0/((gammal + 1.0)*rhol)
    ar = 2.0/((gammar + 1.0)*rhor)
    # Calculate velocities (sound speed)
    cl =  np.sqrt(gammal*(pl + pinfl)/rhol)
    cr =  np.sqrt(gammar*(pr + pinfr)/rhor)

    # Functions to calculate integral curves (rarefactions) and hugoniot locii (shocks)
    integral_curve_1 = lambda p : ul + 2*cl/gl1*(1 - ((p + pinfl)/pbl)**(gl1/(2.0*gammal)))
    integral_curve_3 = lambda p : ur - 2*cr/gr1*(1 - ((p + pinfr)/pbr)**(gr1/(2.0*gammar)))
    hugoniot_locus_1 = lambda p : ul - (p - pl)*np.sqrt(al/(p + pinfl + betal))
    hugoniot_locus_3 = lambda p : ur + (p - pr)*np.sqrt(ar/(p + pinfr + betar))

    # Check whether the 1-wave is a shock or rarefaction
    def phi_l(p):
        global wave1
        if p >= pl:  # 1-Shock
            wave1 = 'shock'
            return hugoniot_locus_1(p)
        else:  # 1-Rarefaction
            wave1 = 'raref'
            return integral_curve_1(p)

    # Check whether the 3-wave is a shock or rarefaction
    def phi_r(p):
        global wave3
        if p >= pr:
            wave3 = 'shock'
            return hugoniot_locus_3(p)
        else:
            wave3 = 'raref'
            return integral_curve_3(p)

    phi = lambda p : phi_l(p) - phi_r(p)

    # Use fsolve to find p_star such that Phi(p_star)=0
    p0 = (pl + pr)/2.0  # initial guess is the average of initial pressures
    p_star, info, ier, msg = fsolve(phi, p0, full_output=True, xtol=1.e-14)
    # For strong rarefactions, sometimes fsolve needs help
    if ier != 1:
        p_star, info, ier, msg = fsolve(phi, p0, full_output=True, factor=0.1, xtol=1.e-10)
        # This should not happen:
        if ier != 1:
            print('Warning: fsolve did not converge.')
            print(msg)

    # Calculate middle states ustar and rho_star in terms of p_star
    pbsl = p_star + pinfl
    pbsr = p_star + pinfr
    u_star = 0.5*(phi_l(p_star) + phi_r(p_star))
    if wave1 == 'shock':
        rhol_star = rhol*(pbsl/pbl + 1.0/bl)/(pbsl/(pbl*bl) + 1.0)
    elif wave1 == 'raref':
        rhol_star = rhol*(pbsl/pbl)**(1.0/gammal)
    if wave3 == 'shock':
        rhor_star = rhor*(pbsr/pbr + 1.0/br)/(pbsr/(pbr*br) + 1.0)
    elif wave3 == 'raref':
        rhor_star = rhor*(pbsr/pbr)**(1.0/gammar)

    # Arrange final states for output
    # Output correct name of variables
    if varout == 'conservative':
        outvars = conserved_variables
        ql      = primitive_to_conservative(rhol, ul, pl, gammal, pinfl)
        ql_star = primitive_to_conservative(rhol_star, u_star, p_star, gammal, pinfl)
        qr_star = primitive_to_conservative(rhor_star, u_star, p_star, gammar, pinfr)
        qr      = primitive_to_conservative(rhor, ur, pr, gammar, pinfr)
    else:
        outvars = primitive_variables
        ql      = [rhol, ul, pl]
        ql_star = [rhol_star, u_star, p_star]
        qr_star = [rhor_star, u_star, p_star]
        qr      = [rhor, ur, pr]
    states = np.column_stack([ql,ql_star,qr_star,qr])

    # Calculate wave speeds for output and rho_star states
    ws = np.zeros(5)
    betal = (pl + pinfl)*(gammal - 1.0)/(gammal + 1.0)
    betar = (pr + pinfr)*(gammar - 1.0)/(gammar + 1.0)
    alphal = 2.0/(rhol*(gammal + 1.0))
    alphar = 2.0/(rhor*(gammar + 1.0))
    cl_star = np.sqrt(gammal*(pbsl)/rhol_star)
    cr_star = np.sqrt(gammar*(pbsr)/rhor_star)
    ws[2] = u_star  # Contact discontinuity
    if wave1 == 'shock':
        ws[0] = ul - np.sqrt((pbsl + betal)/alphal)/rhol
        ws[1] = ws[0]
    elif wave1 == 'raref':
        ws[0] = ul - cl
        ws[1] = u_star - cl_star
    if wave3 == 'shock':
        ws[3] = ur + np.sqrt((pbsr + betar)/alphar)/rhor
        ws[4] = ws[3]
    elif wave3 == 'raref':
        ws[3] = u_star + cr_star
        ws[4] = ur + cr

    #speeds = [(ws[0],ws[1]),ws[2],(ws[3],ws[4])]
    speeds = [[], ws[2], []]
    wave_types = [wave1, 'contact', wave3]
    if wave_types[0] is 'shock':
        speeds[0] = ws[0]
    else:
        speeds[0] = (ws[0],ws[1])
    if wave_types[2] is 'shock':
        speeds[2] = ws[3]
    else:
        speeds[2] = (ws[3],ws[4])

    # Functions to find solution inside rarefaction fans
    def raref1(xi):
        u1 = (ul*gl1 + 2*(xi +  cl))/(gammal + 1.)
        rho1 = rhol*(abs(u1 - xi)/cl)**(2.0/gl1)
        p1 = pbl*(abs(u1 - xi)/cl)**(2.0*gammal/gl1) - pinfl
        return rho1, u1, p1

    def raref3(xi):
        u3 = (ur*gr1 + 2*(xi -  cr))/(gammar + 1.)
        rho3 = rhor*(abs(xi - u3)/cr)**(2.0/gr1)
        p3 = pbr*(abs(xi - u3)/cr)**(2.0*gammar/gr1) - pinfr
        return rho3, u3, p3

    #Returns the Riemann solution in primitive variables for any value of xi = x/t.
    def reval(xi):
        rar1 = raref1(xi)
        rar3 = raref3(xi)
        rho_out =  (xi<=ws[0]                  )*rhol       \
                 + (xi>ws[0])*(xi<=ws[1])*rar1[0]    \
                 + (xi>ws[1])*(xi<=speeds[1]   )*rhol_star  \
                 + (xi>speeds[1]   )*(xi<=ws[3])*rhor_star  \
                 + (xi>ws[3])*(xi<=ws[4])*rar3[0]    \
                 + (xi>ws[4]                   )*rhor

        u_out   =  (xi<=ws[0]                  )*ul      \
                 + (xi>ws[0])*(xi<=ws[1])*rar1[1] \
                 + (xi>ws[1])*(xi<=speeds[1]   )*u_star  \
                 + (xi>speeds[1]   )*(xi<=ws[3])*u_star  \
                 + (xi>ws[3])*(xi<=ws[4])*rar3[1] \
                 + (xi>ws[4]                   )*ur

        p_out   =  (xi<=ws[0]                  )*pl      \
                 + (xi>ws[0])*(xi<=ws[1])*rar1[2] \
                 + (xi>ws[1])*(xi<=speeds[1]   )*p_star  \
                 + (xi>speeds[1]   )*(xi<=ws[3])*p_star  \
                 + (xi>ws[3])*(xi<=ws[4])*rar3[2] \
                 + (xi>ws[4]                   )*pr
        gamma   =  (xi<=0                             )*gammal  \
                 + (xi>0                              )*gammar
        pinf    =  (xi<=0                             )*pinfl  \
                 + (xi>0                              )*pinfr 
        if varout == 'conservative':
            return primitive_to_conservative(rho_out,u_out,p_out,gamma,pinf)
        else:
            return rho_out,u_out,p_out

    return states, speeds, reval, wave_types, outvars


