"""
Author: Rohan
Date: 01/09/16

This file contains a class used to simulate a random fluid problem type, in which the cells within the domain are
assigned random states. In theory, the states should smooth to a uniform grid.
"""

import numpy as np
from matplotlib import pyplot as plt

from CFD_Projects.compressible_hydro.boundary_conditions.boundary_condition import BoundaryCondition2D
from CFD_Projects.compressible_hydro.boundary_conditions.boundary_condition import BoundaryConditionND
from CFD_Projects.compressible_hydro.eos.thermodynamic_state import ThermodynamicState2D
from CFD_Projects.compressible_hydro.flux_calculator.flux_calculator import FluxCalculator2D
from CFD_Projects.compressible_hydro.simulations.base_simulation import BaseSimulation2D
from compressible_hydro.controller.controller_2d import Controller2D


class DoubleMach2D(BaseSimulation2D):

    def __init__(self, final_time, CFL, flux_calculator, num_x, num_y):
        assert isinstance(final_time, float)
        assert isinstance(CFL, float)
        assert isinstance(flux_calculator, int)
        assert isinstance(num_x, int)
        assert isinstance(num_y, int)
        assert 0.0 < CFL < 1.0

        super(DoubleMach2D, self).__init__()

        theta = np.pi / 6.0

        # Set up mesh
        x = np.linspace(1.0 / num_x, 4.0, num_x)
        y = np.linspace(1.0 / num_y, 1.0, num_y)
        self.x = x
        self.y = y
        self.dx = x[1] - x[0]
        self.dy = y[1] - y[0]

        self.final_time = final_time
        self.CFL = CFL
        self.flux_calculator = flux_calculator

        self.gamma = np.zeros((num_x, num_y))
        self.densities = np.zeros((num_x, num_y))
        self.pressures = np.zeros((num_x, num_y))
        self.vel_x = np.zeros((num_x, num_y))
        self.vel_y = np.zeros((num_x, num_y))
        self.internal_energies = np.zeros((num_x, num_y))

        # Initialise states
        gamma = 1.4
        ahead_state = ThermodynamicState2D(1.0, 1.4, 0.0, 0.0, gamma)
        rho_shock = 5.714
        p_shock = 116.5
        # sound_speed = np.sqrt(self.gamma * p_shock / rho_shock)
        sound_speed = ahead_state.sound_speed()
        u_shock = sound_speed * 10.0 * np.cos(theta)
        v_shock = -sound_speed * 10.0 * np.sin(theta)
        self.shock_state = ThermodynamicState2D(p_shock, rho_shock, u_shock, v_shock, gamma)
        for i in range(num_x):
            for j in range(num_y):
                x_pt = self.x[i]
                y_pt = self.y[j]
                self.gamma[i, j] = gamma
                if x_pt <= 0.6 + y_pt * np.tan(theta):
                    self.densities[i, j] = self.shock_state.rho
                    self.pressures[i, j] = self.shock_state.p
                    self.vel_x[i, j] = self.shock_state.u
                    self.vel_y[i, j] = self.shock_state.v
                    self.internal_energies[i, j] = self.shock_state.e_int
                else:
                    self.densities[i, j] = ahead_state.rho
                    self.pressures[i, j] = ahead_state.p
                    self.vel_x[i, j] = ahead_state.u
                    self.vel_y[i, j] = ahead_state.v
                    self.internal_energies[i, j] = ahead_state.e_int

        self.boundary_functions = {
            BoundaryConditionND.X_LOW: lambda state, y_loc: self.shock_state,
            BoundaryConditionND.X_HIGH: lambda state, y_loc : BoundaryCondition2D.transmissive_boundary_condition(state,
                                                                                                        BoundaryCondition2D.X_HIGH),
            BoundaryConditionND.Y_LOW: lambda state, x_loc: BoundaryCondition2D.reflecting_boundary_condition(state, BoundaryCondition2D.Y_LOW)
                                                            if x_loc >= 1.0/6.0 else
                                                            self.shock_state,
            BoundaryConditionND.Y_HIGH: lambda state, x_loc: BoundaryCondition2D.transmissive_boundary_condition(state,
                                                                                                        BoundaryCondition2D.Y_HIGH)
        }

        self.is_initialised = True


def example_2d():
    # This sim is very computationally expensive
    mach_sim = DoubleMach2D(0.2, 0.5, FluxCalculator2D.GODUNOV, 240, 100)
    controller = Controller2D(mach_sim)

    initial_rho = controller.densities
    initial_p = controller.pressures
    initial_vel_x = controller.vel_x
    initial_vel_y = controller.vel_y
    initial_e = controller.internal_energies

    t, x, y, rho, p, u, v, e_int = controller.run_sim()

    X, Y = np.meshgrid(x, y)
    X = np.transpose(X)
    Y = np.transpose(Y)
    fig, ax = plt.subplots(5, 2)
    num_contours = 200

    im = ax[0, 0].contourf(X, Y, initial_rho, num_contours)
    fig.colorbar(im, ax=ax[0, 0])
    im = ax[0, 1].contourf(X, Y, rho, num_contours)
    fig.colorbar(im, ax=ax[0, 1])
    ax[0, 1].set_title("Density")

    im = ax[1, 0].contourf(X, Y, initial_p, num_contours)
    fig.colorbar(im, ax=ax[1, 0])
    im = ax[1, 1].contourf(X, Y, p, num_contours)
    fig.colorbar(im, ax=ax[1, 1])
    ax[1, 1].set_title("Pressure")

    im = ax[2, 0].contourf(X, Y, initial_vel_x, num_contours)
    fig.colorbar(im, ax=ax[2, 0])
    im = ax[2, 1].contourf(X, Y, u, num_contours)
    fig.colorbar(im, ax=ax[2, 1])
    ax[2, 1].set_title("X Velocity")

    im = ax[3, 0].contourf(X, Y, initial_vel_y)
    fig.colorbar(im, ax=ax[3, 0])
    im = ax[3, 1].contourf(X, Y, v, num_contours)
    fig.colorbar(im, ax=ax[3, 1])
    ax[3, 1].set_title("Y Velocity")

    im = ax[4, 0].contourf(X, Y, initial_e, num_contours)
    fig.colorbar(im, ax=ax[4, 0])
    im = ax[4, 1].contourf(X, Y, e_int, num_contours)
    fig.colorbar(im, ax=ax[4, 1])
    ax[4, 1].set_title("Internal Energy")

    fig.savefig("Double_Mach")
    plt.show()


if __name__ == '__main__':
    example_2d()