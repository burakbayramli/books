"""
Author: Rohan
Date: 04/12/16

This file contains a class used to simulate a 1D Noh problem as outlined in the Tri-Lab test suite
"""

import numpy as np
from matplotlib import pyplot as plt

from CFD_Projects.compressible_hydro.boundary_conditions.boundary_condition import BoundaryCondition1D
from CFD_Projects.compressible_hydro.boundary_conditions.boundary_condition import BoundaryCondition2D
from CFD_Projects.compressible_hydro.boundary_conditions.boundary_condition import BoundaryConditionND
from CFD_Projects.compressible_hydro.eos.thermodynamic_state import ThermodynamicState1D
from CFD_Projects.compressible_hydro.eos.thermodynamic_state import ThermodynamicState2D
from CFD_Projects.compressible_hydro.flux_calculator.flux_calculator import FluxCalculator1D
from CFD_Projects.compressible_hydro.flux_calculator.flux_calculator import FluxCalculator2D
from CFD_Projects.compressible_hydro.simulations.base_simulation import BaseSimulation1D
from CFD_Projects.compressible_hydro.simulations.base_simulation import BaseSimulation2D
from compressible_hydro.controller.controller_1d import Controller1D
from compressible_hydro.controller.controller_2d import Controller2D


class AnalyticNoh(object):
    def __init__(self, state, x_max, num_pts):
        assert isinstance(state, ThermodynamicState1D)
        assert isinstance(x_max, float)
        assert isinstance(num_pts, int)

        self.initial_state = state
        self.num_pts = num_pts
        self.gamma = state.gamma
        self.x = np.linspace(0, x_max, num_pts)
        self.rho = np.zeros(num_pts)
        self.u = np.zeros(num_pts)
        self.p = np.zeros(num_pts)
        self.e = np.zeros(num_pts)

    def get_solution(self, t):
        assert isinstance(t, float)

        v_shock = 0.5 * np.abs(self.initial_state.u) * (self.gamma - 1)
        r_shock = v_shock * t

        for i, x in enumerate(self.x):
            if x <= r_shock:
                # Sim is assumed to be planar
                self.rho[i] = self.initial_state.rho * ((self.gamma + 1) / (self.gamma - 1))
                self.u[i] = 0
                self.e[i] = 0.5 * self.initial_state.u ** 2
                self.p[i] = (self.gamma - 1) * self.rho[i] * self.e[i]
            else:
                self.rho[i] = self.initial_state.rho
                self.u[i] = self.initial_state.u
                self.p[i] = self.initial_state.p
                self.e[i] = self.initial_state.e_int

        return self.x, self.rho, self.u, self.p, self.e


class Noh1D(BaseSimulation1D):
    def __init__(self, initial_state, final_time, CFL, flux_calculator):
        assert(isinstance(initial_state, ThermodynamicState1D))
        assert(isinstance(CFL, float))
        assert(isinstance(final_time, float))
        assert(0.0 < CFL < 1.0)

        super(Noh1D, self).__init__()

        self.x = np.linspace(0.002, 0.4, 100)
        self.dx = self.x[1] - self.x[0]
        self.final_time = final_time
        self.CFL = CFL
        self.flux_calculator = flux_calculator

        # Initialise physical states
        self.number_of_fluids = 1
        self.molar_masses = np.asarray([29.0])
        self.specific_heats = np.asarray([2.5])
        self.densities = list()
        self.pressures = list()
        self.vel_x = list()
        self.internal_energies = list()
        self.mass_ratios = np.zeros((self.x.shape[0], self.number_of_fluids))
        self.gamma = list()
        for i, x_loc in enumerate(self.x):
            self.densities.append(initial_state.rho)
            self.pressures.append(initial_state.p)
            self.vel_x.append(initial_state.u)
            self.internal_energies.append(initial_state.e_int)
            self.gamma.append(initial_state.gamma)
            self.mass_ratios[i, 0] = 1.0
        self.densities = np.asarray(self.densities)
        self.pressures = np.asarray(self.pressures)
        self.vel_x = np.asarray(self.vel_x)
        self.internal_energies = np.asarray(self.internal_energies)
        self.gamma = np.asanyarray(self.gamma)

        self.boundary_functions = {
            BoundaryConditionND.X_LOW: lambda state : BoundaryCondition1D.reflecting_boundary_condition(state),
            BoundaryConditionND.X_HIGH: lambda state : BoundaryCondition1D.transmissive_boundary_condition(state)
        }

        self.is_initialised = True


class Noh2D(BaseSimulation2D):
    def __init__(self, initial_state, final_time, CFL, flux_calculator):
        assert(isinstance(initial_state, ThermodynamicState2D))
        assert(isinstance(CFL, float))
        assert(isinstance(final_time, float))
        assert(0.0 < CFL < 1.0)

        super(Noh2D, self).__init__()

        num_x = 100
        num_y = 2
        x = np.linspace(0.4 / num_x, 0.4, num_x)
        y = np.linspace(0.4 / num_y, 0.4, num_y)
        self.x = x
        self.y = y
        self.dx = x[1] - x[0]
        self.dy = y[1] - y[0]
        self.final_time = final_time
        self.CFL = CFL
        self.flux_calculator = flux_calculator

        # Initialise physical states
        self.densities = np.zeros((num_x, num_y))
        self.pressures = np.zeros(self.densities.shape)
        self.vel_x = np.zeros(self.densities.shape)
        self.vel_y = np.zeros(self.densities.shape)
        self.internal_energies = np.zeros(self.densities.shape)
        self.gamma = np.zeros(self.densities.shape)
        for i in range(num_x):
            for j in range(num_y):
                self.densities[i, j] = initial_state.rho
                self.pressures[i, j] = initial_state.p
                self.vel_x[i, j] = initial_state.u
                self.vel_y[i, j] = initial_state.v
                self.internal_energies[i, j] = initial_state.e_int
                self.gamma[i, j] = initial_state.gamma

        self.boundary_functions = {
            BoundaryConditionND.X_LOW: lambda state, y_loc : BoundaryCondition2D.reflecting_boundary_condition(state,
                                                                                                        BoundaryConditionND.X_LOW),
            BoundaryConditionND.X_HIGH: lambda state, y_loc : BoundaryCondition2D.transmissive_boundary_condition(state,
                                                                                                        BoundaryConditionND.X_HIGH),
            BoundaryConditionND.Y_LOW: lambda state, x_loc : BoundaryCondition2D.transmissive_boundary_condition(state,
                                                                                                        BoundaryConditionND.Y_LOW),
            BoundaryConditionND.Y_HIGH: lambda state, x_loc : BoundaryCondition2D.transmissive_boundary_condition(state,
                                                                                                        BoundaryConditionND.Y_HIGH)
        }
        self.is_initialised = True


def test_noh_1d():
    """
    This function runs through the tri lab version of the Noh problem. See the Tri Lab verification test suite.
    """
    run_god = True
    run_rc = True
    run_hllc = False
    run_muscl = True

    # Use small pressure value for numerical stability (and to be physically meaningful)
    initial_state = ThermodynamicState1D(1e-4, 1.0, -1.0, 5.0 / 3.0)

    # Run Noh sim with Godunov and Random Choice
    if run_god:
        noh_god = Noh1D(initial_state, final_time=0.3, CFL=0.45, flux_calculator=FluxCalculator1D.GODUNOV)
        godunov_sim = Controller1D(noh_god)
        (times_god, x_god, densities_god, pressures_god, velocities_god, internal_energies_god, kinetic_energies_god, mass_ratios_god) = godunov_sim.run_sim()

    if run_rc:
        noh_rc = Noh1D(initial_state, final_time=0.3, CFL=0.45, flux_calculator=FluxCalculator1D.RANDOM_CHOICE)
        rc_sim = Controller1D(noh_rc)
        (times_rc, x_rc, densities_rc, pressures_rc, velocities_rc, internal_energies_rc, kinetic_energies_rc, mass_ratios_rc) = rc_sim.run_sim()

    if run_hllc:
        noh_hllc = Noh1D(initial_state, final_time=0.3, CFL=0.45, flux_calculator=FluxCalculator1D.HLLC)
        hllc_sim = Controller1D(noh_hllc)
        (times_hllc, x_hllc, densities_hllc, pressures_hllc, velocities_hllc, internal_energies_hllc, kinetic_energies_hllc, mass_ratios_hllc) = hllc_sim.run_sim()

    if run_muscl:
        noh_muscl = Noh1D(initial_state, final_time=0.3, CFL=0.45, flux_calculator=FluxCalculator1D.MUSCL)
        muscl_sim = Controller1D(noh_muscl)
        (times_muscl, x_muscl, densities_muscl, pressures_muscl, velocities_muscl, internal_energies_muscl, kinetic_energies_muscl, mass_ratios_muscl) = muscl_sim.run_sim()

    # Get analytic solution
    noh_test = AnalyticNoh(initial_state, 0.4, 100)
    x_sol, rho_sol, u_sol, p_sol, e_sol = noh_test.get_solution(0.3)

    title = "Noh Test"
    num_plts_x = 2
    num_plts_y = 2
    plt.figure(figsize=(20, 10))
    plt.suptitle(title)
    plt.subplot(num_plts_x, num_plts_y, 1)
    plt.title("Density")
    plt.plot(x_sol, rho_sol)
    if run_god:
        plt.scatter(x_god, densities_god, c='g', label='Godunov')
    if run_rc:
        plt.scatter(x_rc, densities_rc, c='r', label='Random Choice')
    if run_hllc:
        plt.scatter(x_hllc, densities_hllc, c='k', label='HLLC')
    if run_muscl:
        plt.scatter(x_muscl, densities_muscl, c='c', label='MUSCL')
    plt.legend()
    plt.subplot(num_plts_x, num_plts_y, 2)
    plt.title("Velocity")
    plt.plot(x_sol, u_sol)
    if run_god:
        plt.scatter(x_god, velocities_god, c='g')
    if run_rc:
        plt.scatter(x_rc, velocities_rc, c='r')
    if run_hllc:
        plt.scatter(x_hllc, velocities_hllc, c='k')
    if run_muscl:
        plt.scatter(x_muscl, velocities_muscl, c='c')
    plt.subplot(num_plts_x, num_plts_y, 3)
    plt.title("Pressure")
    plt.plot(x_sol, p_sol)
    if run_god:
        plt.scatter(x_god, pressures_god, c='g')
    if run_rc:
        plt.scatter(x_rc, pressures_rc, c='r')
    if run_hllc:
        plt.scatter(x_hllc, pressures_hllc, c='k')
    if run_muscl:
        plt.scatter(x_muscl, pressures_muscl, c='c')
    plt.subplot(num_plts_x, num_plts_y, 4)
    plt.title("Energy")
    plt.plot(x_sol, e_sol)
    if run_god:
        plt.scatter(x_god, internal_energies_god, c='g')
    if run_rc:
        plt.scatter(x_rc, internal_energies_rc, c='r')
    if run_hllc:
        plt.scatter(x_hllc, internal_energies_hllc, c='k')
    if run_muscl:
        plt.scatter(x_muscl, internal_energies_muscl, c='c')
    plt.show()


def test_noh_2d():
    """
    This function runs through the tri lab version of the Noh problem. See the Tri Lab verification test suite.
    """
    # Use small pressure value for numerical stability (and to be physically meaningful)
    initial_state = ThermodynamicState2D(1e-4, 1.0, -1.0, 0.0, 5.0 / 3.0)

    # Run Noh sim with Godunov and Random Choice
    noh_god = Noh2D(initial_state, final_time=0.3, CFL=0.45, flux_calculator=FluxCalculator2D.GODUNOV)
    godunov_sim = Controller2D(noh_god)
    (times_god, x_god, y_god, densities_god, pressures_god, vel_x_god, vel_y_god, internal_energies_god) = godunov_sim.run_sim()

    X, Y = np.meshgrid(x_god, y_god)
    X = np.transpose(X)
    Y = np.transpose(Y)
    fig, ax = plt.subplots(5)

    im = ax[0].contourf(X, Y, densities_god, 100)
    fig.colorbar(im, ax=ax[0])
    ax[0].set_title("Densities")

    im = ax[1].contourf(X, Y, pressures_god, 100)
    fig.colorbar(im, ax=ax[1])
    ax[1].set_title("Pressures")

    im = ax[2].contourf(X, Y, vel_x_god, 100)
    fig.colorbar(im, ax=ax[2])
    ax[2].set_title("X Velocities")

    im = ax[3].contourf(X, Y, vel_y_god)
    fig.colorbar(im, ax=ax[3])
    ax[3].set_title("Y Velocities")

    im = ax[4].contourf(X, Y, internal_energies_god, 100)
    fig.colorbar(im, ax=ax[4])
    ax[4].set_title("Internal Energies")

    fig.suptitle("Noh Problem 2D")
    plt.show()


if __name__ == '__main__':
    test_noh_1d()
    # test_noh_2d()