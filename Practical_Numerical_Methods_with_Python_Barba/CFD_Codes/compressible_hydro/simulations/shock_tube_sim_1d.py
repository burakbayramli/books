"""
Author: Rohan
Date: 01/09/16

This file contains a class used to simulate a 1D shock tube problem using a Godunov method Riemann solution. The code in
this file should replicate the results from Toro - Chapter 6
"""

import numpy as np
from matplotlib import pyplot as plt

import cProfile, pstats

from CFD_Projects.compressible_hydro.boundary_conditions.boundary_condition import BoundaryCondition1D
from CFD_Projects.compressible_hydro.boundary_conditions.boundary_condition import BoundaryConditionND
from CFD_Projects.compressible_hydro.eos.thermodynamic_state import ThermodynamicState1D
from CFD_Projects.compressible_hydro.flux_calculator.flux_calculator import FluxCalculator1D
from CFD_Projects.compressible_hydro.simulations.analytic_shock_tube import AnalyticShockTube
from CFD_Projects.compressible_hydro.simulations.base_simulation import BaseSimulation1D
from CFD_Projects.compressible_hydro.controller.controller_1d import Controller1D


class ShockTube1D(BaseSimulation1D):
    def __init__(self, left_state, right_state, membrane_location, final_time, CFL, flux_calculator):
        assert(isinstance(left_state, ThermodynamicState1D))
        assert(isinstance(right_state, ThermodynamicState1D))
        assert(isinstance(membrane_location, float))
        assert(isinstance(CFL, float))
        assert(isinstance(final_time, float))
        assert(0.0 < membrane_location < 1.0)
        assert(0.0 < CFL < 1.0)

        super(ShockTube1D, self).__init__()

        self.x = np.linspace(0.005, 0.995, 100)
        self.dx = self.x[0] * 2
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
        self.gamma = list()
        self.mass_ratios = np.zeros((self.x.shape[0], self.number_of_fluids))
        for i, x_loc in enumerate(self.x):
            self.gamma.append(left_state.gamma)
            if x_loc < membrane_location:
                self.densities.append(left_state.rho)
                self.pressures.append(left_state.p)
                self.vel_x.append(left_state.u)
                self.internal_energies.append(left_state.e_int)
                self.mass_ratios[i, 0] = 1.0
            else:
                self.densities.append(right_state.rho)
                self.pressures.append(right_state.p)
                self.vel_x.append(right_state.u)
                self.internal_energies.append(right_state.e_int)
                self.mass_ratios[i, 0] = 1.0
        self.densities = np.asarray(self.densities)
        self.pressures = np.asarray(self.pressures)
        self.vel_x = np.asarray(self.vel_x)
        self.internal_energies = np.asarray(self.internal_energies)
        self.gamma = np.asarray(self.gamma)

        self.boundary_functions = {
            BoundaryConditionND.X_LOW: lambda state : BoundaryCondition1D.transmissive_boundary_condition(state),
            BoundaryConditionND.X_HIGH: lambda state : BoundaryCondition1D.transmissive_boundary_condition(state)
        }

        self.is_initialised = True


def example(plot_sims=True, profile_sims=False):
    """
    Runs the problems from Toro Chapter 6 to validate this simulation
    """
    gamma = 1.4
    p_left = [1.0, 0.4, 1000.0, 460.894, 1000.0]
    rho_left = [1.0, 1.0, 1.0, 5.99924, 1.0]
    u_left = [0.75, -2.0, 0.0, 19.5975, -19.5975]
    p_right = [0.1, 0.4, 0.01, 46.0950, 0.01]
    rho_right = [0.125, 1.0, 1.0, 5.99242, 1.0]
    u_right = [0.0, 2.0, 0.0, -6.19633, -19.59745]
    membrane_location = [0.3, 0.5, 0.5, 0.4, 0.8]
    end_times = [0.25, 0.15, 0.012, 0.035, 0.012]

    # Options - (GODUNOV, "g"), (RANDOM_CHOICE, "r") (HLLC, "k"), (MUSCL, "c"), (LAX_WENDROFF, "b")
    flux_calculators = [(FluxCalculator1D.GODUNOV, "g")]
    for i in range(0, 5):
        # Get initial conditions for shock tube
        left_state = ThermodynamicState1D(p_left[i], rho_left[i], u_left[i], gamma)
        right_state = ThermodynamicState1D(p_right[i], rho_right[i], u_right[i], gamma)
        
        # Set up plot
        if plot_sims:
            fig, ax = plt.subplots(2, 3, figsize=(20, 10))
            fig.suptitle("Sod Test: {}".format(i + 1))
            ax[0, 0].set_title("Density")
            ax[0, 1].set_title("Velocity")
            ax[0, 2].set_title("Pressure")
            ax[1, 0].set_title("Specific Internal Energy")
            ax[1, 1].set_title("Specific Kinetic Energy")
            ax[1, 2].set_title("Mass Ratios")
            for axis_columns in ax:
                for axis in axis_columns:
                    print(axis)
                    axis.set_xlim([0.0, 1.0])
            ax[1, 2].set_ylim([0.0, 1.05])

        # Generate profiler
        if profile_sims:
            profile = cProfile.Profile()
            profile.enable()

        # Run simulations
        for flux_calculator in flux_calculators:
            shock_tube = ShockTube1D(left_state, right_state, membrane_location[i],
                                     final_time=end_times[i], CFL=0.45,
                                     flux_calculator=flux_calculator[0])
            shock_tube_sim = Controller1D(shock_tube)
            (_, x, densities, pressures, velocities, internal_energies, kinetic_energies, mass_ratios) = shock_tube_sim.run_sim()

            if plot_sims:
                ax[0, 0].scatter(x, densities, c=flux_calculator[1])
                ax[0, 1].scatter(x, velocities, c=flux_calculator[1])
                ax[0, 2].scatter(x, pressures, c=flux_calculator[1])
                ax[1, 0].scatter(x, internal_energies, c=flux_calculator[1])
                ax[1, 1].scatter(x, kinetic_energies / densities, c=flux_calculator[1])
                ax[1, 2].scatter(x, mass_ratios, c=flux_calculator[1])
        
        # Output stats
        if profile_sims:
            profile.disable()
            profile.create_stats()
            with open("sod{}_profile.txt".format(i + 1), 'w') as fp:
                stats = pstats.Stats(profile, stream=fp)
                stats.sort_stats('cumtime')  # cumtime, ncalls, percall
                stats.print_stats()
            
        # Get analytic solution
        sod_test = AnalyticShockTube(left_state, right_state, membrane_location[i], 1000)
        x_sol, rho_sol, u_sol, p_sol, e_int_sol, e_kin_sol = sod_test.get_solution(end_times[i], membrane_location[i])
        
        if plot_sims:
            ax[0, 0].plot(x_sol, rho_sol)
            ax[0, 1].plot(x_sol, u_sol)
            ax[0, 2].plot(x_sol, p_sol)
            ax[1, 0].plot(x_sol, e_int_sol)
            ax[1, 1].plot(x_sol, e_kin_sol)
            plt.show()


if __name__ == '__main__':
    example()