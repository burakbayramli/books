# orbital motion.  We consider low mass objects orbiting the Sun.  We
# work in units of AU, yr, and solar masses.  From Kepler's third law:
#
# 4 pi**2 a**3 = G M P**2
#
# if a is in AU, P is in yr, and M is in solar masses, then
#
# a**3 = P**2
#
# and therefore
#
# 4 pi**2 = G
#
# we work in coordinates with the Sun at the origin
#
# This version implements adaptive timestepping, following the text by
# Garcia
#
# M. Zingale

from __future__ import print_function

import math
import numpy as np

# global parameters
GM = 4.0*math.pi**2  #(assuming M = 1 solar mass)

# adaptive timestepping parameters
S1 = 0.9
S2 = 4.0

class OrbitHistory(object):
    """ a simple container to store the integrated history of an
        orbit """

    def __init__(self, t=None, x=None, y=None, u=None, v=None):
        self.t = np.array(t)
        self.x = np.array(x)
        self.y = np.array(y)
        self.u = np.array(u)
        self.v = np.array(v)

    def finalR(self):
        """ the radius at the final integration time """
        N = len(self.t)
        return math.sqrt(self.x[N-1]**2 + self.y[N-1]**2)


    def displacement(self):
        """ distance between the starting and ending point """
        N = len(self.t)
        return math.sqrt( (self.x[0] - self.x[N-1])**2 +
                          (self.y[0] - self.y[N-1])**2 )

    def energy(self):
        """ return the energy (per unit mass) at each point in time """
        return 0.5*(self.u**2 + self.v**2) \
            - GM/np.sqrt(self.x**2 + self.y**2)


def RK4_singlestep(X0, V0, t, dt, rhs):
    """ take a single RK-4 timestep from t to t+dt for the system
        ydot = rhs """

    x = X0[0]
    y = X0[1]

    u = V0[0]
    v = V0[1]

    # get the RHS at several points
    xdot1, ydot1, udot1, vdot1 = rhs([x,y], [u,v])

    xdot2, ydot2, udot2, vdot2 = \
        rhs([x+0.5*dt*xdot1,y+0.5*dt*ydot1],
            [u+0.5*dt*udot1,v+0.5*dt*vdot1])

    xdot3, ydot3, udot3, vdot3 = \
        rhs([x+0.5*dt*xdot2,y+0.5*dt*ydot2],
            [u+0.5*dt*udot2,v+0.5*dt*vdot2])

    xdot4, ydot4, udot4, vdot4 = \
        rhs([x+dt*xdot3,y+dt*ydot3],
            [u+dt*udot3,v+dt*vdot3])

    # advance
    unew = u + (dt/6.0)*(udot1 + 2.0*udot2 + 2.0*udot3 + udot4)
    vnew = v + (dt/6.0)*(vdot1 + 2.0*vdot2 + 2.0*vdot3 + vdot4)

    xnew = x + (dt/6.0)*(xdot1 + 2.0*xdot2 + 2.0*xdot3 + xdot4)
    ynew = y + (dt/6.0)*(ydot1 + 2.0*ydot2 + 2.0*ydot3 + ydot4)

    return xnew, ynew, unew, vnew


class Orbit(object):
    """ hold the initial conditions of a planet/comet/etc. orbiting
        the Sun and integrate """

    def __init__(self, a, e):
        """ a = semi-major axis (in AU),
            e = eccentricity """

        self.x0 = 0.0          # start at x = 0 by definition
        self.y0 = a*(1.0 - e)  # start at perihelion

        self.a = a
        self.e = e

        # perihelion velocity (see C&O Eq. 2.33 for ex)
        self.u0 = -math.sqrt( (GM/a)* (1.0 + e) / (1.0 - e) )
        self.v0 = 0.0


    def kepler_period(self):
        """ return the period of the orbit in yr """
        return math.sqrt(self.a**3)


    def circular_velocity(self):
        """ return the circular velocity (in AU/yr) corresponding to
            the initial radius -- assuming a circle """
        return math.sqrt(GM/self.a)


    def escape_velocity(self):
        """ return the escape velocity (in AU/yr) corresponding to
            the initial radius -- assuming a circle """
        return math.sqrt(2.0*GM/self.a)


    def int_RK4(self, dt, err, tmax):
        """ integrate the equations of motion using 4th order R-K
            method with an adaptive stepsize, to try to achieve the
            relative error err.  dt here is the initial timestep

            if err < 0, then we don't do adaptive stepping, but rather
            we always walk at the input dt
            """

        # initial conditions
        t = 0.0
        x = self.x0
        y = self.y0
        u = self.u0
        v = self.v0

        # store the history for plotting
        tpoints = [t]
        xpoints = [x]
        ypoints = [y]
        upoints = [u]
        vpoints = [v]

        # start with the old timestep
        dt_new = dt

        n_reset = 0

        while t < tmax:

            if err > 0.0:
                # adaptive stepping
                # iteration loop -- keep trying to take a step until
                # we achieve our desired error
                rel_error = 1.e10

                n_try = 0
                while rel_error > err:
                    dt = dt_new
                    if t+dt > tmax:
                        dt = tmax-t

                    # take 2 half steps
                    xtmp, ytmp, utmp, vtmp = \
                        RK4_singlestep([x,y], [u,v],
                                       t, 0.5*dt, self.rhs)

                    xnew, ynew, unew, vnew = \
                        RK4_singlestep([xtmp,ytmp], [utmp,vtmp],
                                       t+0.5*dt, 0.5*dt, self.rhs)

                    # now take just a single step to cover dt
                    xsingle, ysingle, usingle, vsingle = \
                        RK4_singlestep([x,y], [u,v],
                                       t, dt, self.rhs)

                    # {x,y,u,v}double should be more accurate than
                    # {x,y,u,v}single, since it used smaller steps.

                    # estimate the relative error now
                    rel_error = max(abs((xnew-xsingle)/xnew),
                                    abs((ynew-ysingle)/ynew),
                                    abs((unew-usingle)/unew),
                                    abs((vnew-vsingle)/vnew) )


                    #print (len(tpoints), rel_error)

                    # adaptive timestep algorithm from Garcia (Eqs. 3.30
                    # and 3.31)
                    dt_est = dt*abs(err/rel_error)**0.2
                    dt_new = min(max(S1*dt_est, dt/S2), S2*dt)

                    n_try += 1

                if n_try > 1:
                    # n_try = 1 if we took only a single try at the step
                    n_reset += (n_try-1)   

            else:
                if t + dt > tmax:
                    dt = tmax-t

                # take just a single step to cover dt
                xnew, ynew, unew, vnew = \
                    RK4_singlestep([x,y], [u,v],
                                   t, dt, self.rhs)


            # successful step
            t += dt

            # store
            tpoints.append(t)
            xpoints.append(xnew)
            ypoints.append(ynew)
            upoints.append(unew)
            vpoints.append(vnew)

            # set for the next step
            x = xnew; y = ynew; u = unew; v = vnew


        # return a OrbitHistory object with the trajectory
        H = OrbitHistory(tpoints, xpoints, ypoints, upoints, vpoints)
        H.n_reset = n_reset

        return H


    def rhs(self, X, V):
        """ RHS of the equations of motion.  X is the input coordinate
            vector and V is the input velocity vector """

        # current radius
        r = math.sqrt(X[0]**2 + X[1]**2)

        # position
        xdot = V[0]
        ydot = V[1]

        # velocity
        udot = -GM*X[0]/r**3
        vdot = -GM*X[1]/r**3

        return xdot, ydot, udot, vdot
