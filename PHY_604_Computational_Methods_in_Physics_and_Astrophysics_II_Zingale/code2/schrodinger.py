# A lot of this follows from Pang s 4.9, but they use the Numerov
# algorithm (and sort of skirt around the issue of how to start the
# multistep integration off).
#
# Landau and Paez Ch. 10 also gives some insight -- they explicitly
# seek the symmetric solutions.
#

from __future__ import print_function

import numpy as np


class Schrodinger(object):
    """ 
    A class to solve the time-independent Schrodinger equation for
    a general potential well.  This takes a potential function,
    V(x), and integrates (shoots) from the left and right (starting 
    at x_far >> 0) and meets at a matching point, x_match, where 
    the logarithmic derivative is matched (dpsi/dx/psi).  

    Odd and even wavefunctions are supported by making sure that
    the sign of psi matches at the matching point.

    Note: we assume that V(x) is symmetric here.

    references: Pang, section 4.9
                Landay and Paez, Ch. 10

    (although, note: those use the Numerov algorithm, we 
     use RK4 here)

    """

    def __init__(self, V, x_match=0.5, x_far=10, nsteps=100, tol=1.e-6):
        self.V = V
        self.x_match = x_match
        self.x_far = x_far
        self.nsteps = nsteps
        self.tol = tol

        # grids
        self.xi_left = np.linspace(-x_far, x_match, nsteps+1)
        self.xi_right = np.linspace(x_far, x_match, nsteps+1)

    def rhs(self, xi, y, E):
        """ our RHS function """
        psi, phi = y
        return np.array([phi, (self.V(xi)-E)*psi])

    def integrate(self, y0, E, side="left"):
        """ integrate our system from xi = a to xi = b,
            with the choice of the eigenvalue = E using RK4
            with even steps, nsteps total """

        if side == "left":
            xi_s = self.xi_left
        else:
            xi_s = self.xi_right

        psi_s = [y0[0]]

        y = y0

        for n in range(self.nsteps):
            xi = xi_s[n]
            xihalf = 0.5*(xi_s[n] + xi_s[n+1])
            xinew = xi_s[n+1]

            dxi = xinew - xi

            # RK4
            k1 = self.rhs(xi, y, E)
            k2 = self.rhs(xihalf, y + 0.5*dxi*k1, E)
            k3 = self.rhs(xihalf, y + 0.5*dxi*k2, E)
            k4 = self.rhs(xinew, y + dxi*k3, E)

            ynew = y + (dxi/6.0)*(k1 + 2.0*k2 + 2.0*k3 + k4)

            y = ynew

            psi_s.append(y[0])

        return np.array(y), np.array(xi_s), np.array(psi_s)

    def match(self, E):
        """ integrate from -x0 to 0 and from +x0 to 0, and meet in the middle
            and return the discrepancy in the log derivative there -- that is
            what we will zero. """

        # note: we are assuming a symmetric potential here
        beta = np.sqrt(self.V(self.x_far) - E)

        # boundary conditions [psi, phi]
        y0_left = [np.exp(-beta*self.x_far), beta*np.exp(-beta*self.x_far)]
        y0_right = [np.exp(-beta*self.x_far), -beta*np.exp(-beta*self.x_far)]

        # left integration
        yl, xi_l, psi_l = self.integrate(y0_left, E, side="left")

        # right integration
        yr, xi_r, psi_r = self.integrate(y0_right, E, side="right")

        # note, the far solution was setup with even parity as |x| -> oo,
        # but we could have odd or even solutions, so we need to allow the
        # entire solution to flip.  We'll first make the right solution
        # have the same sign as the left (and apply the same sign change
        # to the derivative at the matching point), and then check our
        # continuity condition.  This also renormalizes, but since we
        # consider the log derivative, normalization cancels.
        f = yl[0]/yr[0]

        yr = f*yr
        psi_r = f*psi_r
    
        # put the solution together
        xi = np.concatenate((xi_l, xi_r[::-1]))
        psi = np.concatenate((psi_l, psi_r[::-1]))

        return yl[1]/yl[0] - yr[1]/yr[0], xi, psi

    def solve(self, E1, E2):
        """ do the secant iteration to find an eigenvalue/function """

        dE = 1.e10

        m_old, _, _ = self.match(E1)
        while np.abs(dE) > self.tol:
            m_new, xi_s, psi_s = self.match(E2)

            dmdE = (m_new - m_old)/(E2 - E1)
            dE = -m_new/dmdE

            E1 = E2
            m_old = m_new

            E2 += dE

        return E2, xi_s, psi_s

