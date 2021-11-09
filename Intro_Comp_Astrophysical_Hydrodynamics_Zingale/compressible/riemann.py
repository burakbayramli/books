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

import matplotlib.pyplot as plt
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

    def u_hugoniot(self, p, side, shock=False):
        """define the Hugoniot curve, u(p).  If shock=True, we do a 2-shock
        solution"""

        if side == "left":
            state = self.left
            s = 1.0
        elif side == "right":
            state = self.right
            s = -1.0

        c = np.sqrt(self.gamma*state.p/state.rho)

        if shock:
            # shock
            beta = (self.gamma+1.0)/(self.gamma-1.0)
            u = state.u + s*(2.0*c/np.sqrt(2.0*self.gamma*(self.gamma-1.0)))* \
                (1.0 - p/state.p)/np.sqrt(1.0 + beta*p/state.p)

        else:
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


    def find_2shock_star_state(self, p_min=0.001, p_max=1000.0):
        """ root find the Hugoniot curve to find ustar, pstar """

        # we need to root-find on
        self.pstar = optimize.brentq(
            lambda p: self.u_hugoniot(p, "left", shock=True) - self.u_hugoniot(p, "right", shock=True),
            p_min, p_max)
        self.ustar = self.u_hugoniot(self.pstar, "left", shock=True)

    def shock_solution(self, sgn, xi, state):
        """return the interface solution considering a shock"""

        p_ratio = self.pstar/state.p
        c = np.sqrt(self.gamma*state.p/state.rho)

        # Toro, eq. 4.52 / 4.59
        S = state.u + sgn*c*np.sqrt(0.5*(self.gamma + 1.0)/self.gamma*p_ratio +
                                    0.5*(self.gamma - 1.0)/self.gamma)

        # are we to the left or right of the shock?
        if (sgn > 0 and xi > S) or (sgn < 0 and xi < S):
            # R/L region
            solution = state
        else:
            # * region -- get rhostar from Toro, eq. 4.50 / 4.57
            gam_fac = (self.gamma - 1.0)/(self.gamma + 1.0)
            rhostar = state.rho * (p_ratio + gam_fac)/(gam_fac * p_ratio + 1.0)
            solution = State(rho=rhostar, u=self.ustar, p=self.pstar)

        return solution

    def rarefaction_solution(self, sgn, xi, state):
        """return the interface solution considering a rarefaction wave"""

        # find the speed of the head and tail of the rarefaction fan

        # isentropic (Toro eq. 4.54 / 4.61)
        p_ratio = self.pstar/state.p
        c = np.sqrt(self.gamma*state.p/state.rho)
        cstar = c*p_ratio**((self.gamma-1.0)/(2*self.gamma))

        lambda_head = state.u + sgn*c
        lambda_tail = self.ustar + sgn*cstar

        gam_fac = (self.gamma - 1.0)/(self.gamma + 1.0)

        if (sgn > 0 and xi > lambda_head) or (sgn < 0 and xi < lambda_head):
            # R/L region
            solution = state

        elif (sgn > 0 and xi < lambda_tail) or (sgn < 0 and xi > lambda_tail):
            # * region, we use the isentropic density (Toro 4.53 / 4.60)
            solution = State(rho = state.rho*p_ratio**(1.0/self.gamma),
                             u = self.ustar, p = self.pstar)

        else:
            # we are in the fan -- Toro 4.56 / 4.63
            rho = state.rho * (2/(self.gamma + 1.0) -
                               sgn*gam_fac*(state.u - xi)/c)**(2.0/(self.gamma-1.0))
            u = 2.0/(self.gamma + 1.0) * ( -sgn*c + 0.5*(self.gamma - 1.0)*state.u + xi)
            p = state.p * (2/(self.gamma + 1.0) -
                           sgn*gam_fac*(state.u - xi)/c)**(2.0*self.gamma/(self.gamma-1.0))
            solution = State(rho=rho, u=u, p=p)

        return solution

    def sample_solution(self, time, npts, xmin=0.0, xmax=1.0):
        """given the star state (ustar, pstar), sample the solution for npts
        points between xmin and xmax at the given time.

        this is a similarity solution in xi = x/t """

        # we write it all explicitly out here -- this could be vectorized
        # better.

        dx = (xmax - xmin)/npts
        xjump = 0.5*(xmin + xmax)

        x = np.linspace(xmin, xmax, npts, endpoint=False) + 0.5*dx
        xi = (x - xjump)/time

        # which side of the contact are we on?
        chi = np.sign(xi - self.ustar)

        gam = self.gamma
        gam_fac = (gam - 1.0)/(gam + 1.0)

        rho_v = []
        u_v = []
        p_v = []

        for n in range(npts):

            if xi[n] > self.ustar:
                # we are in the R* or R region
                state = self.right
                sgn = 1.0
            else:
                # we are in the L* or L region
                state = self.left
                sgn = -1.0

            # is non-contact wave a shock or rarefaction?
            if self.pstar > state.p:
                # compression! we are a shock
                solution = self.shock_solution(sgn, xi[n], state)

            else:
                # rarefaction
                solution = self.rarefaction_solution(sgn, xi[n], state)


            # store
            rho_v.append(solution.rho)
            u_v.append(solution.u)
            p_v.append(solution.p)

        return x, np.array(rho_v), np.array(u_v), np.array(p_v)

    def plot_hugoniot(self, p_min = 0.0, p_max=1.5, N=500, gray=False):
        """ plot the Hugoniot curves """

        p = np.linspace(p_min, p_max, num=N)
        u_left = np.zeros_like(p)
        u_right = np.zeros_like(p)

        for n in range(N):
            u_left[n] = self.u_hugoniot(p[n], "left")

        # shock for p > p_s; rarefaction otherwise
        ish = np.where(p > self.left.p)
        ir = np.where(p < self.left.p)

        if gray:
            color = "0.5"
        else:
            color = "C0"

        plt.plot(p[ish], u_left[ish], c=color, ls="-", lw=2)
        plt.plot(p[ir], u_left[ir], c=color, ls=":", lw=2)
        plt.scatter([self.left.p], [self.left.u], marker="x", c=color, s=40)

        du = 0.025*(max(np.max(u_left), np.max(u_right)) -
                    min(np.min(u_left), np.min(u_right)))

        if not gray:
            plt.text(self.left.p, self.left.u+du, "left",
                     horizontalalignment="center", color=color)


        for n in range(N):
            u_right[n] = self.u_hugoniot(p[n], "right")
        ish = np.where(p > self.right.p)
        ir = np.where(p < self.right.p)

        if gray:
            color = "0.5"
        else:
            color = "C1"

        plt.plot(p[ish], u_right[ish], c=color, ls="-", lw=2)
        plt.plot(p[ir], u_right[ir], c=color, ls=":", lw=2)
        plt.scatter([self.right.p], [self.right.u], marker="x", c=color, s=40)

        if not gray:
            plt.text(self.right.p, self.right.u+du, "right",
                     horizontalalignment="center", color=color)

        plt.xlim(p_min, p_max)

        plt.xlabel(r"$p$", fontsize="large")
        plt.ylabel(r"$u$", fontsize="large")

        if not gray:
            legs = []
            legnames = []

            legs.append(plt.Line2D((0,1),(0,0), color="0.5", ls="-", marker=None))
            legnames.append("shock")

            legs.append(plt.Line2D((0,1),(0,0), color="0.5", ls=":", marker=None))
            legnames.append("rarefaction")

            plt.legend(legs, legnames, frameon=False, loc="best")

        plt.tight_layout()


    def plot_2shock_hugoniot(self, p_min = 0.0, p_max=1.5, N=500):
        """ plot the Hugoniot curves under the 2-shock approximation"""

        p = np.linspace(p_min, p_max, num=N)
        u_left = np.zeros_like(p)
        u_right = np.zeros_like(p)

        for n in range(N):
            u_left[n] = self.u_hugoniot(p[n], "left", shock=True)

        plt.plot(p, u_left, c="C0", ls="-", lw=2, zorder=100)
        plt.scatter([self.left.p], [self.left.u], marker="x", c="C0", s=40, zorder=100)

        for n in range(N):
            u_right[n] = self.u_hugoniot(p[n], "right", shock=True)

        plt.plot(p, u_right, c="C1", ls="-", lw=2, zorder=100)
        plt.scatter([self.right.p], [self.right.u], marker="x", c="C1", s=40, zorder=100)

        du = 0.025*(max(np.max(u_left), np.max(u_right)) -
                    min(np.min(u_left), np.min(u_right)))

        plt.text(self.left.p, self.left.u+du, "left",
                 horizontalalignment="center", color="C0")

        plt.text(self.right.p, self.right.u+du, "right",
                 horizontalalignment="center", color="C1")

        plt.xlim(p_min, p_max)

        plt.xlabel(r"$p$", fontsize="large")
        plt.ylabel(r"$u$", fontsize="large")

        plt.tight_layout()
