"""
Author: Rohan
Date: 20/03/16
"""
# Standard imports
import numpy as np


class NavierStokes(object):
    def __init__(self):
        pass

    @staticmethod
    def convection_1d(x_loc, time_steps, initial_velocities, c=1, linear=True):
        """
        Function to solve the one dimensional convection equation:

        Linear:       du/dt + c * du/dx = 0
        Non-Linear:   du/dt + u * du/dx = 0

        :param x_loc: discrete cell locations being solved for
        :param time_steps: discrete time values being solved
        :param initial_velocities: initial velocity condition in domain
        :param c: wave speed
        :paramL linear: boolean to determine whether the solution is linear or not
        :return: a 2 dimensional array containing the spatial solution for each
                time step
        """

        assert initial_velocities.shape == x_loc.shape
        u_solution = np.zeros((x_loc.shape[0], time_steps.shape[0]))

        delta_t = time_steps[1:] - time_steps[0:-1]
        dx = x_loc[1:] - x_loc[0:-1]

        u_solution[:, 0] = initial_velocities
        i = 1
        while i < time_steps.shape[0]:
            dt = delta_t[i - 1]

            if linear is True:
                u_solution[1:, i] = u_solution[1:, i - 1] \
                                    - c * dt / dx * (u_solution[1:, i - 1] \
                                                        - u_solution[0:-1, i - 1])
            else:
                u_solution[1:, i] = u_solution[1:, i - 1] \
                                    - u_solution[1:, i - 1] * dt / dx * (u_solution[1:, i - 1] \
                                                                         - u_solution[0:-1, i - 1])

            # Apply boundary condition at first x_location
            u_solution[0, i] = u_solution[0, 0]
            i += 1

        return u_solution

    @staticmethod
    def convection_2d(x_loc, y_loc, time_steps, u_initial, v_initial, c=1, linear=True):
        """
        Function to solve the 2D convection equation:

                du/dt + c*du/dx + c*du/dy = 0

        :param x_loc: the location of x cell centres in the mesh
        :param y_loc: the location of y cell centres in the mesh
        :param time_steps: the time steps used in the simulation
        :param u_initial: the initial x velocity condition to solve for
        :param v_initial: the initial y velocity condition to solve for
        :param c: the wave speed
        :param linear: boolean to determin whether the equation is linear or not
        :return: a 3 dimensional array containing the spatial solution for each
                time step
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
            if linear:
                u_solution[i, 1:, 1:] = u_solution[i - 1, 1:, 1:] - \
                                        c*dt/dx*(u_solution[i - 1, 1:, 1:] -
                                                 u_solution[i - 1, :-1, 1:]) - \
                                        c*dt/dy*(u_solution[i - 1, 1:, 1:] -
                                                 u_solution[i - 1, 1:, :-1])
                v_solution[i, 1:, 1:] = v_solution[i - 1, 1:, 1:] - \
                                        c*dt/dx*(v_solution[i - 1, 1:, 1:] -
                                                 v_solution[i - 1, :-1, 1:]) - \
                                        c*dt/dy*(v_solution[i - 1, 1:, 1:] -
                                                 v_solution[i - 1, 1:, :-1])
            else:
                u_solution[i, 1:, 1:] = u_solution[i - 1, 1:, 1:] - \
                                        u_solution[i - 1, 1:, 1:] * dt/dx * \
                                        (u_solution[i - 1, 1:, 1:] -
                                         u_solution[i - 1, :-1, 1:]) - \
                                        v_solution[i - 1, 1:, 1:] * dt/dy * \
                                        (u_solution[i - 1, 1:, 1:] -
                                         u_solution[i - 1, 1:, :-1])
                v_solution[i, 1:, 1:] = v_solution[i - 1, 1:, 1:] - \
                                        u_solution[i - 1, 1:, 1:] * dt/dx * \
                                        (v_solution[i - 1, 1:, 1:] -
                                         v_solution[i - 1, :-1, 1:]) - \
                                        v_solution[i - 1, 1:, 1:] * dt/dy * \
                                        (v_solution[i - 1, 1:, 1:] -
                                         v_solution[i - 1, 1:, :-1])
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

    @staticmethod
    def laplace_2d(x_loc, y_loc, u):
        """
        A function to solve the two dimensional Laplace equation

                        d2u/dx2 + d2u/dy2 = 0
        :param x_loc: A 1D array of the x coordinates in the system
        :param y_loc: A 1D array of the y coordinates in the system
        :param u: A 2D array of variable states in the system
        :return: A two dimensional array containing the iterative solution to the system given
                the boundary conditions specified in the function.
        """

        assert u.shape == (x_loc.shape[0], y_loc.shape[0])

        dx = x_loc[1:] - x_loc[0:-1]
        dy = y_loc[1:] - y_loc[0:-1]

        u_sol = u.copy()
        error = 1
        while error > 1e-3:
            u = u_sol.copy()
            u_sol[1:-1, 1:-1] = (dy[0:-1] * dy[1:] * (u_sol[2:, 1:-1] + u_sol[0:-2, 1:-1]) \
                                + dx[0:-1] * dx[1:] * (u_sol[1:-1, 2:] + u_sol[1:-1, 0:-2])) \
                                / (2 * (dy[0:-1] * dy[1:] + dx[0:-1] * dx[1:]))

            u_sol[0, :] = 0
            u_sol[-1, :] = y_loc
            u_sol[:, 0] = u_sol[:, 1]
            u_sol[:, -1] = u_sol[:, -2]

            error = np.abs(np.sum(np.abs(u_sol[:]) - np.abs([u])) / np.sum(np.abs(u[:])))
        return u_sol

    @staticmethod
    def poisson_2d(x_loc, y_loc, u, b, num_iter, u_x_min=0, u_x_max=0, u_y_min=0, u_y_max=0,
                   periodic_x=False, periodic_y=False, wall_x=False, wall_y=False):
        """
        Function to iteratively solve the Poisson equation:
                    d2u/dx2 + d2u/dy2 = b

        :param x_loc: location of x positions
        :param y_loc: location of y positions
        :param u: state variable being iteratively solved
        :param b: source term
        :param num_iter: number of iterations made before returning solution
        :return: a 2D array giving the solution for the boundary conditions specified in this
                function
        """
        assert u.shape == (x_loc.shape[0], y_loc.shape[0])
        assert b.shape == (x_loc.shape[0], y_loc.shape[0])
        assert not (periodic_x and wall_x)
        assert not (periodic_y and wall_y)

        dx = x_loc[1:] - x_loc[0:-1]
        dy = y_loc[1:] - y_loc[0:-1]

        u_sol = u.copy()
        for i in range(num_iter):
            u_sol[1:-1, 1:-1] = (dy[0:-1] * dy[1:] * (u_sol[2:, 1:-1] + u_sol[0:-2, 1:-1]) \
                                + dx[0:-1] * dx[1:] * (u_sol[1:-1, 2:] + u_sol[1:-1, 0:-2]) \
                                 - (dy[0:-1] * dy[1:] * dx[0:-1] * dx[1:] * b[1:-1, 1:-1])) \
                                / (2 * (dy[0:-1] * dy[1:] + dx[0:-1] * dx[1:]))
            # Apply Boundary Conditions
            if periodic_x:
                u_sol[0, 1:-1] = (dy[0:-1] * dy[1:] * (u_sol[1, 1:-1] + u_sol[-1, 1:-1]) \
                                    + dx[0:-1] * dx[1:] * (u_sol[0, 2:] + u_sol[0, 0:-2]) \
                                     - (dy[0:-1] * dy[1:] * dx[0] * dx[1] * b[0, 1:-1])) \
                                    / (2 * (dy[0:-1] * dy[1:] + dx[0] * dx[1]))
                u_sol[-1, 1:-1] = (dy[0:-1] * dy[1:] * (u_sol[0, 1:-1] + u_sol[-2, 1:-1]) \
                                    + dx[0] * dx[1] * (u_sol[-1, 2:] + u_sol[-1, 0:-2]) \
                                     - (dy[0:-1] * dy[1:] * dx[0] * dx[1] * b[-1, 1:-1])) \
                                    / (2 * (dy[0:-1] * dy[1:] + dx[0] * dx[1]))
            elif wall_x:
                u_sol[0, 1:-1] = u_sol[1, 1:-1]
                u_sol[-1, 1:-1] = u_sol[-2, 1:-1]
            else:
                u_sol[0, :] = u_x_min
                u_sol[-1, :] = u_x_max

            if periodic_y:
                u_sol[1:-1, 0] = (dy[0] * dy[1] * (u_sol[2:, 0] + u_sol[0:-2, 0]) \
                                    + dx[0:-1] * dx[1:] * (u_sol[1:-1, 1] + u_sol[1:-1, -1]) \
                                     - (dy[0] * dy[1] * dx[0:-1] * dx[1:] * b[1:-1, 0])) \
                                    / (2 * (dy[0] * dy[1] + dx[0:-1] * dx[1:]))
                u_sol[1:-1, -1] = (dy[-2] * dy[-1] * (u_sol[2:, -1] + u_sol[0:-2, -1]) \
                                    + dx[0:-1] * dx[1:] * (u_sol[1:-1, 0] + u_sol[1:-1, -2]) \
                                     - (dy[-2] * dy[-1] * dx[0:-1] * dx[1:] * b[1:-1, -1])) \
                                    / (2 * (dy[-2] * dy[-1] + dx[0:-1] * dx[1:]))
            elif wall_y:
                u_sol[1:-1, 0] = u_sol[1:-1, 1]
                u_sol[1:-1, -1] = u_sol[1:-1, -2]
            else:
                u_sol[:, 0] = u_y_min
                u_sol[:, -1] = u_y_max

        return u_sol

    @staticmethod
    def cavity_flow(x_loc, y_loc, time_steps, u, v, p, rho, nu, nit):
        """
        Function to calculate the flow within a cavity using standard N-S equations
        :param x_loc: locations of x positions within mesh in a 1D array.
        :param y_loc: locations of y positions within mesh in a 1D array.
        :param time_steps: the time steps specified for the simulation in a 1D array/
        :param u: velocities at grid points in the x direction in a 2D array.
        :param v: velocities at grid points in the y direction in a 2D array.
        :param p: pressures at grid points in a 2D array.
        :param rho: density of the incompressible fluid.
        :param nu: viscosity of the fluid.
        :param nit: number of iterations within the poisson equation solution to the
                    pressure field.
        :return: u and v velocity fields once the solutions has converged to a steady
                state.
        """

        assert u.shape == (x_loc.shape[0], y_loc.shape[0])
        assert v.shape == (x_loc.shape[0], y_loc.shape[0])

        dt = time_steps[1:] - time_steps[0:-1]
        dx = x_loc[1:] - x_loc[0:-1]
        dy = y_loc[1:] - y_loc[0:-1]

        b = np.zeros((x_loc.shape[0], y_loc.shape[0]))
        i = 0
        u_sol = u.copy()
        v_sol = v.copy()
        p_sol = p.copy()
        while i < dt.shape[0]:
            # Get new iteration fields
            u = u_sol.copy()
            v = v_sol.copy()
            p = p_sol.copy()

            # Get b field for poisson equation
            b[1:-1, 1:-1] = (1 / dt[i]) * ((u[2:, 1:-1] - u[0:-2, 1:-1]) \
                            / (2 * dx[0:-1]) + (v[1:-1, 2:] - v[1:-1, 0:-2]) / (2 * dy[0:-1])) \
                            - \
                            ((u[2:, 1:-1] - u[0:-2, 1:-1]) ** 2) / (4 * dx[0:-1] ** 2) \
                            - (u[1:-1, 2:] - u[1:-1, 0:-2]) * (v[2:, 1:-1] - v[0:-2, 1:-1]) \
                            / (4 * dx[0:-1] * dy[0:-1]) \
                            - ((v[1:-1, 2:] - v[1:-1, 0:-2]) ** 2) / (4 * dy[0:-1] ** 2)

            # apply poisson equation
            p_sol = NavierStokes.poisson_2d(x_loc, y_loc, p, b, nit, p[1, :], p[-2, :],
                                            p[:, 1], 0)

            # solve velocity field
            u_sol[1:-1, 1:-1] = u[1:-1, 1:-1] - \
                                u[1:-1, 1:-1] * (dt[i]) / (dx[0:-1]) * (u[1:-1, 1:-1] - u[0:-2, 1:-1]) \
                                - v[1:-1, 1:-1] * (dt[i]) / (dy[0:-1]) * (u[1:-1, 1:-1] - u[1:-1, 0:-2]) \
                                - dt[i] * (p[2:, 1:-1] - p[0:-2, 1:-1]) / (rho * 2 * dx[0:-1]) \
                                + nu * (dt[i]/(dx[0:-1] ** 2) * (u[2:, 1:-1] - 2 * u[1:-1, 1:-1] + u[0:-2, 1:-1]) \
                                + (dt[i]/(dy[0:-1] ** 2)) * (u[1:-1, 2:] - 2 * u[1:-1, 1:-1] + u[1:-1, 0:-2]))

            v_sol[1:-1, 1:-1] = v[1:-1, 1:-1] - \
                                u[1:-1, 1:-1] * (dt[i]) / (dx[0:-1]) * (v[1:-1, 1:-1] - v[0:-2, 1:-1]) \
                                - v[1:-1, 1:-1] * (dt[i]) / (dy[0:-1]) * (v[1:-1, 1:-1] - v[1:-1, 0:-2]) \
                                - dt[i] * (p[1:-1, 2:] - p[1:-1, 0:-2]) / (rho * 2 * dy[0:-1]) \
                                + nu * (dt[i]/(dx[0:-1] ** 2) * (v[2:, 1:-1] - 2 * v[1:-1, 1:-1] + v[0:-2, 1:-1]) \
                                + (dt[i]/(dy[0:-1] ** 2)) * (v[1:-1, 2:] - 2 * v[1:-1, 1:-1] + v[1:-1, 0:-2]))

            # B.Cs for cavity flow
            u_sol[0, :] = 0
            u_sol[:, 0] = 0
            u_sol[:, -1] = 1
            u_sol[-1, :] = 0
            v_sol[0, :] = 0
            v_sol[-1, :] = 0
            v_sol[:, 0] = 0
            v_sol[:, -1] = 0

            i += 1

        return u_sol, v_sol, p_sol

    @staticmethod
    def channel_flow(x_loc, y_loc, error, dt, u, v, p, rho, nu, nit, F):
        """
        Function to calculate Poisseulle flow
        :param x_loc: 1D array of x locations within the grid.
        :param y_loc: 1D array of y locations within the grid.
        :param error: Tolerance used in while loop for velocities to evaluate convergence.
        :param dt: Float representing the time step in the simulation.
        :param u: 2D array of x velocities in the grid cells.
        :param v: 2D array of y velocities in the grid cells.
        :param p: 2D array of pressure perturbations from steady state values in the grid cells.
        :param rho: Float representing density.
        :param nu: Float representing viscosity.
        :param nit: Integer representing the number of iterations within the Poisson Pressure
                    calculation.
        :param F: Steady state pressure gradient
        :return: 3 two-dimensional arrays u_sol, v_sol and p_sol
        """
        assert u.shape == (x_loc.shape[0], y_loc.shape[0])
        assert v.shape == (x_loc.shape[0], y_loc.shape[0])

        dx = x_loc[1:] - x_loc[0:-1]
        dy = y_loc[1:] - y_loc[0:-1]

        b = np.zeros((x_loc.shape[0], y_loc.shape[0]))
        u_diff = 1
        num_it = 0
        u_sol = u.copy()
        v_sol = v.copy()
        p_sol = p.copy()
        while u_diff > error:
            u = u_sol.copy()
            v = v_sol.copy()
            p = p_sol.copy()

            # Calculate b term in periodic poisson calculation
            b[1:-1, 1:-1] = (1 / dt) * ((u[2:, 1:-1] - u[0:-2, 1:-1]) \
                            / (2 * dx[0:-1]) + (v[1:-1, 2:] - v[1:-1, 0:-2]) / (2 * dy[0:-1])) \
                            - \
                            ((u[2:, 1:-1] - u[0:-2, 1:-1]) ** 2) / (4 * dx[0:-1] ** 2) \
                            - (u[1:-1, 2:] - u[1:-1, 0:-2]) * (v[2:, 1:-1] - v[0:-2, 1:-1]) \
                            / (4 * dx[0:-1] * dy[0:-1]) \
                            - ((v[1:-1, 2:] - v[1:-1, 0:-2]) ** 2) / (4 * dy[0:-1] ** 2)

            # Solve boundary conditions of b
            b[0, 1:-1] = (1 / dt) * ((u[1, 1:-1] - u[-1, 1:-1]) \
                            / (2 * dx[-1]) + (v[0, 2:] - v[0, 0:-2]) / (2 * dy[-1])) \
                            - \
                            ((u[1, 1:-1] - u[-1, 1:-1]) ** 2) / (4 * dx[-1] ** 2) \
                            - (u[0, 2:] - u[0, 0:-2]) * (v[1, 1:-1] - v[-1, 1:-1]) \
                            / (4 * dx[-1] * dy[-1]) \
                            - ((v[0, 2:] - v[0, 0:-2]) ** 2) / (4 * dy[-1] ** 2)

            b[-1, 1:-1] = (1 / dt) * ((u[0, 1:-1] - u[-2, 1:-1]) \
                            / (2 * dx[-1]) + (v[-1, 2:] - v[-1, 0:-2]) / (2 * dy[-1])) \
                            - \
                            ((u[0, 1:-1] - u[-2, 1:-1]) ** 2) / (4 * dx[-1] ** 2) \
                            - (u[-1, 2:] - u[-1, 0:-2]) * (v[0, 1:-1] - v[-2, 1:-1]) \
                            / (4 * dx[-1] * dy[-1]) \
                            - ((v[-1, 2:] - v[-1, 0:-2]) ** 2) / (4 * dy[-1] ** 2)

            # Calculate periodic poisson solution to pressure
            p_sol = NavierStokes.poisson_2d(x_loc, y_loc, p, b, nit, periodic_x=True, wall_y=True)

            # Calculate velocities
            u_sol[1:-1, 1:-1] = u[1:-1, 1:-1] - \
                                u[1:-1, 1:-1] * dt / (dx[0:-1]) * (u[1:-1, 1:-1] - u[0:-2, 1:-1]) \
                                - v[1:-1, 1:-1] * dt / (dy[0:-1]) * (u[1:-1, 1:-1] - u[1:-1, 0:-2]) \
                                - dt * (p[2:, 1:-1] - p[0:-2, 1:-1]) / (rho * 2 * dx[0:-1]) \
                                + nu * (dt/(dx[0:-1] ** 2) * (u[2:, 1:-1] - 2 * u[1:-1, 1:-1] + u[0:-2, 1:-1]) \
                                + (dt/(dy[0:-1] ** 2)) * (u[1:-1, 2:] - 2 * u[1:-1, 1:-1] + u[1:-1, 0:-2])) + F*dt

            v_sol[1:-1, 1:-1] = v[1:-1, 1:-1] - \
                                u[1:-1, 1:-1] * dt / (dx[0:-1]) * (v[1:-1, 1:-1] - v[0:-2, 1:-1]) \
                                - v[1:-1, 1:-1] * dt / (dy[0:-1]) * (v[1:-1, 1:-1] - v[1:-1, 0:-2]) \
                                - dt * (p[1:-1, 2:] - p[1:-1, 0:-2]) / (rho * 2 * dy[0:-1]) \
                                + nu * (dt/(dx[0:-1] ** 2) * (v[2:, 1:-1] - 2 * v[1:-1, 1:-1] + v[0:-2, 1:-1]) \
                                + (dt/(dy[0:-1] ** 2)) * (v[1:-1, 2:] - 2 * v[1:-1, 1:-1] + v[1:-1, 0:-2]))

            # B.Cs for channel flow
            u_sol[0, 1:-1] = u[0, 1:-1] - \
                                u[0, 1:-1] * dt / (dx[0]) * (u[0, 1:-1] - u[-1, 1:-1]) \
                                - v[0, 1:-1] * dt / (dy[0:-1]) * (u[0, 1:-1] - u[0, 0:-2]) \
                                - dt * (p[1, 1:-1] - p[-1, 1:-1]) / (rho * 2 * dx[0]) \
                                + nu * (dt/(dx[0] ** 2) * (u[1, 1:-1] - 2 * u[0, 1:-1] + u[-1, 1:-1]) \
                                + (dt/(dy[0:-1] ** 2)) * (u[0, 2:] - 2 * u[0, 1:-1] + u[0, 0:-2])) + F*dt

            u_sol[-1, 1:-1] = u[-1, 1:-1] - \
                                u[-1, 1:-1] * dt / (dx[-1]) * (u[-1, 1:-1] - u[-2, 1:-1]) \
                                - v[-1, 1:-1] * dt / (dy[0:-1]) * (u[-1, 1:-1] - u[-1, 0:-2]) \
                                - dt * (p[0, 1:-1] - p[-2, 1:-1]) / (rho * 2 * dx[-1]) \
                                + nu * (dt/(dx[-1] ** 2) * (u[0, 1:-1] - 2 * u[-1, 1:-1] + u[-2, 1:-1]) \
                                + (dt/(dy[0:-1] ** 2)) * (u[-1, 2:] - 2 * u[-1, 1:-1] + u[-1, 0:-2])) + F*dt

            v_sol[0, 1:-1] = v[0, 1:-1] - \
                                u[0, 1:-1] * dt / (dx[0]) * (v[0, 1:-1] - v[-1, 1:-1]) \
                                - v[0, 1:-1] * dt / (dy[0:-1]) * (v[0, 1:-1] - v[0, 0:-2]) \
                                - dt * (p[0, 2:] - p[0, 0:-2]) / (rho * 2 * dy[0:-1]) \
                                + nu * (dt/(dx[0] ** 2) * (v[1, 1:-1] - 2 * v[0, 1:-1] + v[-1, 1:-1]) \
                                + (dt/(dy[0:-1] ** 2)) * (v[0, 2:] - 2 * v[0, 1:-1] + v[0, 0:-2]))

            v_sol[-1, 1:-1] = v[-1, 1:-1] - \
                                u[-1, 1:-1] * dt / (dx[-1]) * (v[-1, 1:-1] - v[-2, 1:-1]) \
                                - v[-1, 1:-1] * dt / (dy[0:-1]) * (v[-1, 1:-1] - v[-1, 0:-2]) \
                                - dt * (p[-1, 2:] - p[-1, 0:-2]) / (rho * 2 * dy[0:-1]) \
                                + nu * (dt/(dx[-1] ** 2) * (v[0, 1:-1] - 2 * v[-1, 1:-1] + v[-2, 1:-1]) \
                                + (dt/(dy[0:-1] ** 2)) * (v[-1, 2:] - 2 * v[-1, 1:-1] + v[-1, 0:-2]))

            u_sol[:, 0] = 0
            u_sol[:, -1] = 0
            v_sol[:, 0] = 0
            v_sol[:, -1] = 0

            # Calculate difference between solutions
            u_diff = np.abs(np.sum(np.sqrt(u_sol**2 + v_sol**2)) - np.sum(np.sqrt(u ** 2 + v ** 2)))/u.size

            num_it += 1

        print num_it
        return u_sol, v_sol, p_sol
