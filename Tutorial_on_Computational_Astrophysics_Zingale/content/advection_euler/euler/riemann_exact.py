"""An exact Riemann solver for the Euler equations with a gamma-law
gas.  The left and right states are stored as State objects.  We then
create a RiemannProblem object with the left and right state:

> rp = RiemannProblem(left_state, right_state)

Next we solve for the star state:

> rp.find_star_state()

Finally, we sample the solution to find the interface state, which
is returned as a State object:

> q_int = rp.sample_solution()
"""

import numpy as np
import scipy.optimize as optimize

class State:
    """ a simple object to hold a primitive variable state """

    def __init__(self, p=1.0, u=0.0, rho=1.0):
        self.p = p
        self.u = u
        self.rho = rho

    def __str__(self):
        return f"rho: {self.rho}; u: {self.u}; p: {self.p}"

class RiemannProblem:
    """ a class to define a Riemann problem.  It takes a left
        and right state.  Note: we assume a constant gamma """

    def __init__(self, left_state, right_state, gamma=1.4):
        self.left = left_state
        self.right = right_state
        self.gamma = gamma

        self.ustar = None
        self.pstar = None

    def __str__(self):
        return f"pstar = {self.pstar}, ustar = {self.ustar}"

    def u_hugoniot(self, p, side):
        """define the Hugoniot curve, u(p)."""

        if side == "left":
            state = self.left
            s = 1.0
        elif side == "right":
            state = self.right
            s = -1.0

        c = np.sqrt(self.gamma*state.p/state.rho)

        if p < state.p:
            # rarefaction
            u = state.u + s*(2.0*c/(self.gamma-1.0))* \
                (1.0 - (p/state.p)**((self.gamma-1.0)/(2.0*self.gamma)))
        else:
            # shock
            beta = (self.gamma+1.0)/(self.gamma-1.0)
            u = state.u + s*(2.0*c/np.sqrt(2.0*self.gamma*(self.gamma-1.0)))* \
                (1.0 - p/state.p)/np.sqrt(1.0 + beta*p/state.p)

        return u

    def find_star_state(self, p_min=0.001, p_max=1000.0):
        """ root find the Hugoniot curve to find ustar, pstar """

        # we need to root-find on
        self.pstar = optimize.brentq(
            lambda p: self.u_hugoniot(p, "left") - self.u_hugoniot(p, "right"),
            p_min, p_max)
        self.ustar = self.u_hugoniot(self.pstar, "left")


    def shock_solution(self, sgn, state):
        """return the interface solution considering a shock"""

        p_ratio = self.pstar/state.p
        c = np.sqrt(self.gamma*state.p/state.rho)

        # Toro, eq. 4.52 / 4.59
        S = state.u + sgn*c*np.sqrt(0.5*(self.gamma + 1.0)/self.gamma*p_ratio +
                                    0.5*(self.gamma - 1.0)/self.gamma)

        # are we to the left or right of the shock?
        if (self.ustar < 0 and S < 0) or (self.ustar > 0 and S > 0):
            # R/L region
            solution = state
        else:
            # * region -- get rhostar from Toro, eq. 4.50 / 4.57
            gam_fac = (self.gamma - 1.0)/(self.gamma + 1.0)
            rhostar = state.rho * (p_ratio + gam_fac)/(gam_fac * p_ratio + 1.0)
            solution = State(rho=rhostar, u=self.ustar, p=self.pstar)

        return solution

    def rarefaction_solution(self, sgn, state):
        """return the interface solution considering a rarefaction wave"""

        # find the speed of the head and tail of the rarefaction fan

        # isentropic (Toro eq. 4.54 / 4.61)
        p_ratio = self.pstar/state.p
        c = np.sqrt(self.gamma*state.p/state.rho)
        cstar = c*p_ratio**((self.gamma-1.0)/(2*self.gamma))

        lambda_head = state.u + sgn*c
        lambda_tail = self.ustar + sgn*cstar

        gam_fac = (self.gamma - 1.0)/(self.gamma + 1.0)

        if (sgn > 0 and lambda_head < 0) or (sgn < 0 and lambda_head > 0):
            # R/L region
            solution = state

        elif (sgn > 0 and lambda_tail > 0) or (sgn < 0 and lambda_tail < 0):
            # * region, we use the isentropic density (Toro 4.53 / 4.60)
            solution = State(rho = state.rho*p_ratio**(1.0/self.gamma),
                             u = self.ustar, p = self.pstar)

        else:
            # we are in the fan -- Toro 4.56 / 4.63
            rho = state.rho * (2/(self.gamma + 1.0) -
                               sgn*gam_fac*state.u/c)**(2.0/(self.gamma-1.0))
            u = 2.0/(self.gamma + 1.0) * ( -sgn*c + 0.5*(self.gamma - 1.0)*state.u)
            p = state.p * (2/(self.gamma + 1.0) -
                           sgn*gam_fac*state.u/c)**(2.0*self.gamma/(self.gamma-1.0))
            solution = State(rho=rho, u=u, p=p)

        return solution

    def sample_solution(self):
        """given the star state (ustar, pstar), find the state on the interface"""

        if self.ustar < 0:
            # we are in the R* or R region
            state = self.right
            sgn = 1.0
        else:
            # we are in the L* or L region
            state = self.left
            sgn = -1.0

        # is the non-contact wave a shock or rarefaction?
        if self.pstar > state.p:
            # compression! we are a shock
            solution = self.shock_solution(sgn, state)

        else:
            # rarefaction
            solution = self.rarefaction_solution(sgn, state)

        return solution


if __name__ == "__main__":

    q_l = State(rho=1.0, u=0.0, p=1.0)
    q_r = State(rho=0.125, u=0.0, p=0.1)

    rp = RiemannProblem(q_l, q_r, gamma=1.4)

    rp.find_star_state()
    q_int = rp.sample_solution()
    print(q_int)
