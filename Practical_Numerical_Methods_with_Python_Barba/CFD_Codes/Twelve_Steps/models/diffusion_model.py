"""
Author: Rohan
Date: 19/03/16
"""

# Standard imports
import numpy as np
import sympy


class Diffusion(object):
    def __init__(self):
        pass

    @staticmethod
    def mass_diffusivity(T, d_0=16, e=160000.0):
        """
        Function used to get the mass diffusivity constant of a given substance based on the
        temperature relationship:
                    D = D_0 * e ^ (- E / RT )

        The default diffusivity terms are for carbon dioxide
        :param d_0: reference diffusivity constant
        :param e: Energy per mole
        :return: The Diffusivity constant for a given material as it varies with temperature
        """
        R = 8.314

        exponent = - e / (R * T)
        d = d_0 * np.exp(exponent)

        return d

    @staticmethod
    def diffusion_1d(x_loc, time_steps, u_distribution, nu=0.3):
        """
        Model used to implement 1D diffusion, as given by:

                        du/dt = nu * d2u/dx2

        :param x_loc: Discrete spatial locations in domain
        :param time_steps: Discrete time steps in simulation
        :param u_distribution: initial distribution of diffuse quantity u
        :param nu: diffusivity constant
        :return: 2D diffusion solution for each time step
        """

        assert u_distribution.shape == x_loc.shape
        u_solution = np.zeros((x_loc.shape[0], time_steps.shape[0]))

        delta_t = time_steps[1:] - time_steps[0:-1]
        dx = x_loc[1:] - x_loc[0:-1]

        u_solution[:, 0] = u_distribution
        i = 1

        while i < time_steps.shape[0]:
            dt = delta_t[i - 1]
            u_solution[1:-1, i] = u_solution[1:-1, i - 1] \
                                  + nu * dt / (dx[1:] ** 2) * (u_solution[2:, i - 1] \
                                                             - 2 * u_solution[1:-1, i - 1] \
                                                             + u_solution[0:-2, i - 1])

            # Apply boundary condition at first x_location
            u_solution[0, i] = u_solution[0, 0]
            u_solution[-1, i] = u_solution[-1, 0]

            i += 1

        return u_solution

    @staticmethod
    def diffusion_2d(x_loc, y_loc, time_steps, u_initial, nu=0.05):
        """
        A model used to solve the 2D diffusion model:

            du/dt = v * [d2u/dx2 + d2u/dy2]

        :param x_loc: the x location of cell centres
        :param y_loc: the y location of cell centres
        :param time_steps: the time steps during the simulation
        :param u_initial: the initial distribution of u in the system
        :param nu: the diffusivity constant
        :return: a 3D array of solutions for every time step
        """

        assert u_initial.shape == (x_loc.shape[0], y_loc.shape[0])

        dx = x_loc[1:] - x_loc[:-1]
        dy = y_loc[1:] - y_loc[:-1]
        delta_t = time_steps[1:] - time_steps[:-1]

        u = np.zeros((time_steps.shape[0], x_loc.shape[0], y_loc.shape[0]))
        u[0, :, :] = u_initial

        i = 1
        while i < time_steps.shape[0]:
            dt = delta_t[i - 1]
            u[i, 1:-1, 1:-1] = u[i-1, 1:-1, 1:-1] + nu * dt * ((u[i - 1, 2:, 1:-1] \
                                    - 2 * u[i - 1, 1:-1, 1:-1] + u[i - 1, 0:-2, 1:-1]) / dx[1:] ** 2 \
                                    + (u[i - 1, 1:-1, 2:] \
                                    - 2 * u[i - 1, 1:-1, 1:-1] + u[i - 1, 1:-1, 0:-2]) / dy[1:] ** 2)
            # Apply boundary condition at first x_location
            u[i, 0, :] = u[0, 0, :]
            u[i, -1, :] = u[0, -1, :]
            u[i, :, 0] = u[0, :, 0]
            u[i, :, -1] = u[0, :, -1]

            i += 1
        return u

    @staticmethod
    def burgers_equation_1d(nx, nt, dx, nu_value=0.07):
        """
        This function implements the 1d burger's equation:

            du/dt + u * du/dx = nu * d2u/dx2

        :param nx: Number of x samples in periodic region between 0 and 2*pi
        :param nt: Number of time steps
        :param dx: delta x
        :param nu_value: value of kinematic viscosity
        :return: x values of samples points, computational and analytical solution to equation
        """
        # Use sympy to differentiate and generate initial condition
        x, nu, t = sympy.symbols('x nu t')
        phi = sympy.exp(-(x-4*t)**2/(4*nu*(t+1))) + sympy.exp(-(x-4*t-2*np.pi)**2/(4*nu*(t+1)))

        phiprime = phi.diff(x)
        u = -2*nu*(phiprime/phi) + 4

        ufunc = sympy.utilities.lambdify((t, x, nu), u)

        nu = nu_value
        dt = nu * dx
        x = np.linspace(0, 2*np.pi, nx)
        t = 0
        u = np.asarray([ufunc(t, x0, nu) for x0 in x])

        for n in range(nt):
            un = u.copy()
            u[1:-1] = un[1:-1] - un[1:-1] * dt/dx * (un[1:-1] - un[0:-2]) \
                       + nu * dt / (dx ** 2) * (un[2:] - 2 * un[1:-1] + un[0:-2])

            # Apply boundary condition at first x_location
            u[0] = un[0] - un[0] * dt/dx * (un[0] - un[-2]) \
                       + nu * dt / (dx ** 2) * (un[1] - 2 * un[0] + un[-2])
            u[-1] = un[-1] - un[-1] * dt/dx * (un[-1] - un[-2]) \
                       + nu * dt / (dx ** 2) * (un[0] - 2 * un[-1] + un[-2])

        u_analytical = np.asarray([ufunc(nt*dt, xi, nu) for xi in x])

        return x, u, u_analytical

    @staticmethod
    def burgers_equation_2d(x_loc, y_loc, time_steps, u_initial, v_initial, nu=0.07):
        """
        A model for Burger's equation in 2D:

            du/dt + u * du/dx + v * du/dy = nu * (d2u/dx2 + d2u/dy2)
            dv/dt + u * dv/dx + v * dv/dy = nu * (d2v/dx2 + d2v/dy2)

        :param x_loc: location of x cell centres within the simulation
        :param y_loc: location of y cell centres within the simulation
        :param time_steps: number of timesteps during the simulation
        :param u_initial: the initial distribution of u
        :param v_initial: the initial distribution of v
        :param nu: the diffusivity constant
        :return: two 3D arrays giving he solution for u and v at each time step
        """

        assert u_initial.shape == (x_loc.shape[0], y_loc.shape[0])
        assert v_initial.shape == (x_loc.shape[0], y_loc.shape[0])

        delta_t = time_steps[1:] - time_steps[0:-1]
        dx = x_loc[1:] - x_loc[0:-1]
        dy = y_loc[1:] - y_loc[0:-1]

        u_solution = np.zeros((time_steps.shape[0], x_loc.shape[0], y_loc.shape[0]))
        v_solution = np.zeros((time_steps.shape[0], x_loc.shape[0], y_loc.shape[0]))
        u_solution[0, :, :] = u_initial
        v_solution[0, :, :] = v_initial
        i=1
        while i < time_steps.shape[0]:
            dt = delta_t[i - 1]

            u_advective = u_solution[i - 1, 1:-1, 1:-1] * dt/dx[:-1] * \
                                    (u_solution[i - 1, 1:-1, 1:-1] -
                                     u_solution[i - 1, :-2, 1:-1]) + \
                                    v_solution[i - 1, 1:-1, 1:-1] * dt/dy[:-1] * \
                                    (u_solution[i - 1, 1:-1, 1:-1] -
                                     u_solution[i - 1, 1:-1, :-2])

            v_advective = u_solution[i - 1, 1:-1, 1:-1] * dt/dx[:-1] * \
                                    (v_solution[i - 1, 1:-1, 1:-1] -
                                     v_solution[i - 1, :-2, 1:-1]) + \
                                    v_solution[i - 1, 1:-1, 1:-1] * dt/dy[:-1] * \
                                    (v_solution[i - 1, 1:-1, 1:-1] -
                                     v_solution[i - 1, 1:-1, :-2])

            u_diffuse = nu * dt * ((u_solution[i - 1, 2:, 1:-1] \
                                    - 2 * u_solution[i - 1, 1:-1, 1:-1] + u_solution[i - 1, 0:-2, 1:-1]) / dx[1:] ** 2 \
                                    + (u_solution[i - 1, 1:-1, 2:] \
                                    - 2 * u_solution[i - 1, 1:-1, 1:-1] + u_solution[i - 1, 1:-1, 0:-2]) / dy[1:] ** 2)

            v_diffuse = nu * dt * ((v_solution[i - 1, 2:, 1:-1] \
                                    - 2 * v_solution[i - 1, 1:-1, 1:-1] + v_solution[i - 1, 0:-2, 1:-1]) / dx[1:] ** 2 \
                                    + (v_solution[i - 1, 1:-1, 2:] \
                                    - 2 * v_solution[i - 1, 1:-1, 1:-1] + v_solution[i - 1, 1:-1, 0:-2]) / dy[1:] ** 2)

            # Combine diffuse and advective terms
            u_solution[i, 1:-1, 1:-1] = u_solution[i - 1, 1:-1, 1:-1] - u_advective + u_diffuse
            v_solution[i, 1:-1, 1:-1] = v_solution[i - 1, 1:-1, 1:-1] - v_advective + v_diffuse

            u_solution[i, 0, :] = 1
            u_solution[i, -1, :] = 1
            u_solution[i, :, 0] = 1
            u_solution[i, :, -1] = 1
            v_solution[i, 0, :] = 1
            v_solution[i, -1, :] = 1
            v_solution[i, :, 0] = 1
            v_solution[i, :, -1] = 1
            i += 1

        return u_solution, v_solution

        pass
