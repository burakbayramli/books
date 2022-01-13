# integrate the equations of motion of a pendulum, w/o the small angle
# approximation

import numpy as np
import matplotlib.pyplot as plt
import math

# global parameters 
g = 9.81     # gravitational acceleration [m/s]
L = 9.81     # length of pendulum [m]


class PendulumHistory(object):
    """ simple container to store the pendulum history """

    def __init__(self, theta0=None):
        self.t = None
        self.theta = None
        self.omega = None
        self.g = g
        self.L = L
        self.theta0 = theta0

    def energy(self):
        """ return the energy (per unit mass) """
        return 0.5*self.L**2*self.omega**2 - \
            self.g*self.L*np.cos(self.theta)

    def period(self):
        """ return the period up to the theta**2 term """
        
        # this is Garcia Eq. 2.38
        return 2.0*math.pi*math.sqrt(self.L/self.g)*(1.0 + theta0**2/16.0)
           

def rhs(theta, omega):
    """ equations of motion for a pendulum
        dtheta/dt = omega
        domega/dt = - (g/L) sin theta """

    return omega, -(g/L)*np.sin(theta)


def int_RK4(theta0, dt, tmax, rhs):
    """ integrate the equations of motion using 4th-order
        Runge-Kutta. Here, theta0 is the initial angle from vertical,
        in radians. 

        Note: we use uniform steps here -- this is important to
        allow for an FFT (which needs uniformly spaced samples).
    """
        
    
    # initial conditions
    t = 0.0
    theta = theta0
    omega = 0.0    # at the maximum angle, the angular velocity is 0


    # store the history for plotting
    t_s = [t]
    theta_s = [theta]
    omega_s = [omega]

    while t < tmax:
        
        # get the RHS at time-level n
        thetadot1, omegadot1 = rhs(theta, omega)

        thetadot2, omegadot2 = rhs(theta + 0.5*dt*thetadot1, 
                                   omega + 0.5*dt*omegadot1)

        thetadot3, omegadot3 = rhs(theta + 0.5*dt*thetadot2, 
                                   omega + 0.5*dt*omegadot2)

        thetadot4, omegadot4 = rhs(theta + dt*thetadot3, 
                                   omega + dt*omegadot3)

        theta += (dt/6.0)*(thetadot1 + 2.0*thetadot2 + 
                           2.0*thetadot3 + thetadot4)

        omega += (dt/6.0)*(omegadot1 + 2.0*omegadot2 + 
                           2.0*omegadot3 + omegadot4)

        t += dt

        # store
        t_s.append(t)
        theta_s.append(theta)
        omega_s.append(omega)


    # return a PendulumHistory object with the trajectory
    H = PendulumHistory(theta0 = theta0)
    H.t = np.array(t_s)
    H.theta = np.array(theta_s)
    H.omega = np.array(omega_s)

    return H



#-----------------------------------------------------------------------------
# 5 degree pendulum
theta0 = np.radians(5.0)
dt = 0.025
tmax = 30.0

H = int_RK4(theta0, dt, tmax, rhs)

plt.plot(H.t, H.theta)
plt.plot([H.period(), H.period()], 
           [np.min(H.theta), np.max(H.theta)], ls=":")

plt.xlabel("time")
plt.ylabel(r"$\theta$")
plt.savefig("pendulum-theta-5.png")

# power spectrum
N = len(H.t)
F = np.fft.rfft(H.theta)

# normalization is 2/N, since we are real, so 1/2 of the domain is
# redundant
F = 2.0*F/N   

# rfftfreq returns just the postive frequencies
# http://docs.scipy.org/doc/numpy-dev/reference/generated/np.fft.rfftfreq.html)

k = np.fft.rfftfreq(N)
kfreq = k*N/max(H.t)

print(N)

plt.clf()

f_analytic = 1.0/H.period()

plt.plot(kfreq, np.abs(F)**2)
plt.plot([f_analytic, f_analytic], [0.0,max(np.abs(F)**2)], ls=":")

plt.xlabel(r"$\nu_k$")
plt.ylabel(r"$|F(k)|^2$")

plt.xlim(0,1)
plt.savefig("pendulum-powerspectrum-5.png")


#-----------------------------------------------------------------------------
# 25 degree pendulum
theta0 = np.radians(25.0)
dt = 0.025
tmax = 50*(2.0*math.pi*math.sqrt(L/g))

H = int_RK4(theta0, dt, tmax, rhs)

plt.clf()

plt.plot(H.t, H.theta)
plt.plot([H.period(), H.period()], 
           [np.min(H.theta), np.max(H.theta)], ls=":")

plt.xlabel("time")
plt.ylabel(r"$\theta$")
plt.savefig("pendulum-theta-25.png")

t_out = [1, 5, 25, 50]

print("total number of integration points = ", len(H.t))

P_analytic = H.period()

for N_periods in t_out:

    points = H.t < N_periods*P_analytic

    # power spectrum
    N = len(H.t[points])
    F = np.fft.rfft(H.theta[points])

    # normalization is 2/N, since we are real, so 1/2 of the domain is
    # redundant
    F = 2.0*F/N   

    # rfftfreq returns just the positive frequncies
    # http://docs.scipy.org/doc/numpy-dev/reference/generated/np.fft.rfftfreq.html)

    k = np.fft.rfftfreq(N)
    kfreq = k*N/max(H.t[points])

    print(N)

    plt.clf()

    f_analytic = 1.0/H.period()

    plt.plot(kfreq, np.abs(F)**2)
    plt.plot([f_analytic, f_analytic], [0.0,max(np.abs(F)**2)], ls=":")

    plt.xlabel(r"$\nu_k$")
    plt.ylabel(r"$|F(k)|^2$")

    plt.xlim(0,1)
    plt.savefig("pendulum-power-theta_25-P_{}.png".format(N_periods))



    


