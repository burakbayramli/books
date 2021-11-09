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
# M. Zingale (2013-02-19)

import math
import numpy

# global parameters
GM = 4.0*math.pi**2  #(assuming M = 1 solar mass)


class OrbitHistory(object):
    """ a simple container to store the integrated history of an
        orbit """
    
    def __init__(self):
        self.t = None
        self.x = None
        self.y = None
        self.u = None
        self.v = None

    def final_R(self):
        """ the radius at the final integration time """
        N = len(self.t)
        return math.sqrt(self.x[N-1]**2 + self.y[N-1]**2)
    

    def displacement(self):
        """ distance between the starting and ending point """
        N = len(self.t)
        return math.sqrt( (self.x[0] - self.x[N-1])**2 +
                          (self.y[0] - self.y[N-1])**2 )


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
        

    def int_Euler(self, dt, tmax):
        """ integrate the equations of motion using Euler's method.
            We integrate until t = tmax """

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

        while (t < tmax):

            # make sure that the next step doesn't take us past where
            # we want to be, because of roundoff
            if t+dt > tmax:
                dt = tmax-t            

            # get the RHS
            xdot, ydot, udot, vdot = self.rhs([x,y], [u,v])

            # advance
            unew = u + dt*udot
            vnew = v + dt*vdot
            
            xnew = x + dt*xdot
            ynew = y + dt*ydot

            t += dt

            # store
            tpoints.append(t)
            xpoints.append(xnew)
            ypoints.append(ynew)
            upoints.append(unew)
            vpoints.append(vnew)

            # set for the next step
            x = xnew; y = ynew; u = unew; v = vnew

        # return an OrbitHistory object with the trajectory
        H = OrbitHistory()
        H.t = numpy.array(tpoints)
        H.x = numpy.array(xpoints)
        H.y = numpy.array(ypoints)
        H.u = numpy.array(upoints)
        H.v = numpy.array(vpoints)

        return H


    def int_Euler_Cromer(self, dt, tmax):
        """ integrate the equations of motion using Euler's method.
            We integrate until t = tmax """

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

        while (t < tmax):
            
            # make sure that the next step doesn't take us past where
            # we want to be, because of roundoff
            if t+dt > tmax:
                dt = tmax-t            

            # get the RHS
            xdot, ydot, udot, vdot = self.rhs([x,y], [u,v])

            # advance
            unew = u + dt*udot
            vnew = v + dt*vdot
            
            # these two lines are the only change from Euler
            xnew = x + dt*unew
            ynew = y + dt*vnew

            t += dt

            # store
            tpoints.append(t)
            xpoints.append(xnew)
            ypoints.append(ynew)
            upoints.append(unew)
            vpoints.append(vnew)

            # set for the next step
            x = xnew; y = ynew; u = unew; v = vnew

        # return an OrbitHistory object with the trajectory
        H = OrbitHistory()
        H.t = numpy.array(tpoints)
        H.x = numpy.array(xpoints)
        H.y = numpy.array(ypoints)
        H.u = numpy.array(upoints)
        H.v = numpy.array(vpoints)

        return H


    def int_RK2(self, dt, tmax):
        """ integrate the equations of motion using 2nd order R-K
            method (also know as the midpoint method) We integrate
            until we reach tmax """

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

        while (t < tmax):
            
            # make sure that the next step doesn't take us past where
            # we want to be, because of roundoff
            if t+dt > tmax:
                dt = tmax-t            

            # get the RHS
            xdot, ydot, udot, vdot = self.rhs([x,y], [u,v])

            # advance to dt/2 to get intermediate position
            utmp = u + 0.5*dt*udot
            vtmp = v + 0.5*dt*vdot
            
            xtmp = x + 0.5*dt*xdot
            ytmp = y + 0.5*dt*ydot

            # get the RHS with the intermediate state
            xdot, ydot, udot, vdot = self.rhs([xtmp,ytmp], [utmp,vtmp])

            unew = u + dt*udot
            vnew = v + dt*vdot
            
            xnew = x + dt*xdot
            ynew = y + dt*ydot

            t += dt

            # store
            tpoints.append(t)
            xpoints.append(xnew)
            ypoints.append(ynew)
            upoints.append(unew)
            vpoints.append(vnew)

            # set for the next step
            x = xnew; y = ynew; u = unew; v = vnew

        # return an OrbitHistory object with the trajectory
        H = OrbitHistory()
        H.t = numpy.array(tpoints)
        H.x = numpy.array(xpoints)
        H.y = numpy.array(ypoints)
        H.u = numpy.array(upoints)
        H.v = numpy.array(vpoints)

        return H


    def int_RK4(self, dt, tmax):
        """ integrate the equations of motion using 4th order R-K
            method.  """

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

        while (t < tmax):

            # make sure that the next step doesn't take us past where
            # we want to be, because of roundoff
            if t+dt > tmax:
                dt = tmax-t            
            
            # get the RHS at several points
            xdot1, ydot1, udot1, vdot1 = self.rhs([x,y], [u,v])

            xdot2, ydot2, udot2, vdot2 = \
                self.rhs([x+0.5*dt*xdot1,y+0.5*dt*ydot1], 
                         [u+0.5*dt*udot1,v+0.5*dt*vdot1])

            xdot3, ydot3, udot3, vdot3 = \
                self.rhs([x+0.5*dt*xdot2,y+0.5*dt*ydot2], 
                         [u+0.5*dt*udot2,v+0.5*dt*vdot2])

            xdot4, ydot4, udot4, vdot4 = \
                self.rhs([x+dt*xdot3,y+dt*ydot3], 
                         [u+dt*udot3,v+dt*vdot3])
            

            # advance
            unew = u + (dt/6.0)*(udot1 + 2.0*udot2 + 2.0*udot3 + udot4)
            vnew = v + (dt/6.0)*(vdot1 + 2.0*vdot2 + 2.0*vdot3 + vdot4)
            
            xnew = x + (dt/6.0)*(xdot1 + 2.0*xdot2 + 2.0*xdot3 + xdot4)
            ynew = y + (dt/6.0)*(ydot1 + 2.0*ydot2 + 2.0*ydot3 + ydot4)

            t += dt

            # store
            tpoints.append(t)
            xpoints.append(xnew)
            ypoints.append(ynew)
            upoints.append(unew)
            vpoints.append(vnew)

            # set for the next step
            x = xnew; y = ynew; u = unew; v = vnew

        # return an OrbitHistory object with the trajectory
        H = OrbitHistory()
        H.t = numpy.array(tpoints)
        H.x = numpy.array(xpoints)
        H.y = numpy.array(ypoints)
        H.u = numpy.array(upoints)
        H.v = numpy.array(vpoints)

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



