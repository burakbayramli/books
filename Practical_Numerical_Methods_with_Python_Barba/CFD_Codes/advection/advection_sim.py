"""
Author: Rohan
Date: 24/06/17

This file contains a simulation class for running 1D linear advection equation simulations
"""

import numpy as np
from CFD_Projects.advection.flux_calculator import AdvectionFluxCalculator, WENOFluxCalculator


class LinearAdvectionSim(object):
    """
    This class is used to run simple linear advection simulations on a uniform one dimensional.
    """
    def __init__(self, x, initial_grid, flux_calculator_model, final_time, a=None, CFL=0.8):
        """
        Constuctor for linear advection simulation solving:

        Ut + aUx = 0

        :param x: Grid points - assumed to be uniform
        :param initial_grid: Values of state u on grid
        :param flux_calculator_model: Flux calculator method used in simulation
        :param final_time: Final time of the simulation
        :param num_pts: Number of points in time
        """
        assert isinstance(final_time, float) and final_time > 0.0
        # assert isinstance(flux_calculator_model, AdvectionFluxCalculator)
        assert isinstance(x, np.ndarray)
        assert isinstance(initial_grid, np.ndarray)
        assert x.shape == initial_grid.shape

        self.flux_calculator = flux_calculator_model
        self.x = x
        self.u = initial_grid
        self.final_time = final_time
        self.times = list()
        self.times.append(0.0)
        self.dx = self.x[1] - self.x[0]
        self.a = a
        self.CFL = CFL

    def calculate_time_step(self):
        """
        Function to evaluate the dt for the next time step
        """
        if self.a is not None:
            S = self.a
        else:
            S = np.max(self.u)
        return self.CFL * self.dx / S

    def update_states(self, fluxes, dt):
        """
        Function to update the states on the grid based on fluxes between cells
        """
        for i, state in enumerate(self.u):
            self.u[i] += (fluxes[i] - fluxes[i + 1]) * dt / self.dx

    def run_simulation(self):
        """
        Main function to run simulation and return final grid state
        """
        t = 0.0
        ts = 0
        while t < self.final_time:
            dt = self.calculate_time_step()
            dt = dt if t + dt < self.final_time else self.final_time - t

            if not isinstance(self.flux_calculator, WENOFluxCalculator):
                fluxes = self.flux_calculator.evaluate_fluxes(self.u, self.dx, dt, a=self.a)
                self.update_states(fluxes, dt)
            else:
                # This section needs to implement an Upwind type scheme, with Runge Kutta state updates
                raise NotImplementedError()

            t += dt
            ts += 1
            print("Time Step: {}, {}".format(ts, t))

        return self.x, self.u