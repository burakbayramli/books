"""
Author: Rohan
Date: 01/09/16

This file contains a class used to simulate a random fluid problem type, in which the cells within the domain are
assigned random states. In theory, the states should smooth to a uniform grid.
"""

import random

import numpy as np
from matplotlib import pyplot as plt

from CFD_Projects.compressible_hydro.boundary_conditions.boundary_condition import BoundaryCondition2D
from CFD_Projects.compressible_hydro.boundary_conditions.boundary_condition import BoundaryConditionND
from CFD_Projects.compressible_hydro.eos.thermodynamic_state import ThermodynamicState2D
from CFD_Projects.compressible_hydro.flux_calculator.flux_calculator import FluxCalculator2D
from CFD_Projects.compressible_hydro.simulations.base_simulation import BaseSimulation2D
from compressible_hydro.controller.controller_2d import Controller2D


class RandomFluid2D(BaseSimulation2D):
    def __init__(self, mean_state, standard_deviation, final_time, CFL, flux_calculator, num_pts):
        assert isinstance(mean_state, ThermodynamicState2D)
        assert isinstance(standard_deviation, float)
        assert isinstance(final_time, float)
        assert isinstance(CFL, float)
        assert isinstance(flux_calculator, int)
        assert isinstance(num_pts, int)
        assert 0.0 < CFL < 1.0

        super(RandomFluid2D, self).__init__()

        # Set up mesh
        x = np.linspace(1.0 / num_pts, 1.0, num_pts)
        y = np.linspace(1.0 / num_pts, 1.0, num_pts)
        self.x = x
        self.y = y
        self.dx = x[1] - x[0]
        self.dy = y[1] - y[0]

        self.final_time = final_time
        self.CFL = CFL
        self.flux_calculator = flux_calculator

        self.gamma = np.zeros((num_pts, num_pts))
        self.densities = np.zeros((num_pts, num_pts))
        self.pressures = np.zeros((num_pts, num_pts))
        self.vel_x = np.zeros((num_pts, num_pts))
        self.vel_y = np.zeros((num_pts, num_pts))
        self.internal_energies = np.zeros((num_pts, num_pts))
        for i in range(num_pts):
            for j in range(num_pts):
                self.densities[i, j] = np.abs(random.normalvariate(mean_state.rho, standard_deviation * mean_state.rho))
                self.pressures[i, j] = np.abs(random.normalvariate(mean_state.p, standard_deviation * mean_state.p))
                self.vel_x[i, j] = random.normalvariate(mean_state.u, standard_deviation)
                self.vel_y[i, j] = random.normalvariate(mean_state.v, standard_deviation)
                self.internal_energies[i, j] = np.abs(random.normalvariate(mean_state.e_int, standard_deviation * mean_state.e_int))
                self.gamma[i, j] = mean_state.gamma

        self.boundary_functions = {
            BoundaryConditionND.X_LOW: lambda state, y_loc: BoundaryCondition2D.reflecting_boundary_condition(state,
                                                                                                        BoundaryCondition2D.X_LOW),
            BoundaryConditionND.X_HIGH: lambda state, y_loc: BoundaryCondition2D.reflecting_boundary_condition(state,
                                                                                                        BoundaryCondition2D.X_HIGH),
            BoundaryConditionND.Y_LOW: lambda state, x_loc: BoundaryCondition2D.reflecting_boundary_condition(state,
                                                                                                        BoundaryCondition2D.Y_LOW),
            BoundaryConditionND.Y_HIGH: lambda state, x_loc: BoundaryCondition2D.reflecting_boundary_condition(state,
                                                                                                        BoundaryCondition2D.Y_HIGH)
        }

        self.is_initialised = True


def example_2d():
    mean_state = ThermodynamicState2D(1e5, 1.225, 0.0, 0.0, 1.4)
    std_dev = 0.25

    random_sim = RandomFluid2D(mean_state, std_dev, 0.3, 0.5, FluxCalculator2D.GODUNOV, 8)
    controller = Controller2D(random_sim)

    initial_rho = controller.densities
    initial_p = controller.pressures
    initial_vel_x = controller.vel_x
    initial_vel_y = controller.vel_y
    initial_e = controller.internal_energies

    t, x, y, rho, p, u, v, e_int = controller.run_sim()

    X, Y = np.meshgrid(x, y)
    fig, ax = plt.subplots(5, 2)

    im = ax[0, 0].contourf(X, Y, initial_rho, 100)
    fig.colorbar(im, ax=ax[0, 0])
    im = ax[0, 1].contourf(X, Y, rho, 100)
    fig.colorbar(im, ax=ax[0, 1])

    im = ax[1, 0].contourf(X, Y, initial_p, 100)
    fig.colorbar(im, ax=ax[1, 0])
    im = ax[1, 1].contourf(X, Y, p, 100)
    fig.colorbar(im, ax=ax[1, 1])

    im = ax[2, 0].contourf(X, Y, initial_vel_x, 100)
    fig.colorbar(im, ax=ax[2, 0])
    im = ax[2, 1].contourf(X, Y, u, 100)
    fig.colorbar(im, ax=ax[2, 1])

    im = ax[3, 0].contourf(X, Y, initial_vel_y)
    fig.colorbar(im, ax=ax[3, 0])
    im = ax[3, 1].contourf(X, Y, v, 100)
    fig.colorbar(im, ax=ax[3, 1])

    im = ax[4, 0].contourf(X, Y, initial_e, 100)
    fig.colorbar(im, ax=ax[4, 0])
    im = ax[4, 1].contourf(X, Y, e_int, 100)
    fig.colorbar(im, ax=ax[4, 1])

    plt.show()


if __name__ == '__main__':
    example_2d()