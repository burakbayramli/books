"""
Author: Rohan
Date: 24/06/17

This file contains a controller class used to simulate fluid systems in 1D.
"""

import numpy as np

from CFD_Projects.compressible_hydro.eos.thermodynamic_state import ThermodynamicState1D
from CFD_Projects.compressible_hydro.flux_calculator.flux_calculator import FluxCalculator1D
from CFD_Projects.compressible_hydro.boundary_conditions.boundary_condition import BoundaryConditionND

from CFD_Projects.compressible_hydro.simulations.base_simulation import BaseSimulation1D

from CFD_Projects.compressible_hydro.controller.controller_nd import ControllerND


class Controller1D(ControllerND):
    def __init__(self, simulation):
        assert isinstance(simulation, BaseSimulation1D)
        assert simulation.is_initialised

        # Initialise mesh variables
        self.x = simulation.x
        self.densities = simulation.densities
        self.pressures = simulation.pressures
        self.velocities = simulation.vel_x
        self.internal_energies = simulation.internal_energies
        self.kinetic_energies = np.ones(simulation.internal_energies.shape)
        self.dx = simulation.dx
        self.gamma = simulation.gamma

        # Initialise multi-phase variables
        self.number_of_fluids = simulation.number_of_fluids
        self.molar_masses = simulation.molar_masses
        self.specific_heats = simulation.specific_heats
        self.mass_ratios = simulation.mass_ratios

        # Sim time step variables
        self.final_time = simulation.final_time
        self.CFL = simulation.CFL

        # Initialise flux arrays
        self.density_fluxes = np.zeros((len(self.densities) + 1))
        self.momentum_fluxes = np.zeros(len(self.densities) + 1)
        self.total_energy_fluxes = np.zeros(len(self.densities) + 1)
        self.mass_ratio_fluxes = np.zeros((len(self.densities) + 1, self.number_of_fluids))

        self.flux_calculator = simulation.flux_calculator
        self.boundary_functions = simulation.boundary_functions

        self.validate()

    def validate(self):
        """
        Check that variables passed to Controller by simulation are the correct types and sizes
        """
        assert isinstance(self.x, np.ndarray) and self.x.ndim == 1
        assert isinstance(self.densities, np.ndarray) and self.densities.ndim == 1
        assert isinstance(self.velocities, np.ndarray) and self.velocities.ndim == 1
        assert isinstance(self.pressures, np.ndarray) and self.pressures.ndim == 1
        assert isinstance(self.internal_energies, np.ndarray) and self.internal_energies.ndim == 1
        assert isinstance(self.kinetic_energies, np.ndarray) and self.internal_energies.ndim == 1
        assert isinstance(self.gamma, np.ndarray) and self.gamma.ndim == 1

        assert isinstance(self.number_of_fluids, int)
        assert isinstance(self.molar_masses, np.ndarray)
        assert self.molar_masses.shape[0] == self.number_of_fluids
        assert isinstance(self.specific_heats, np.ndarray)
        assert self.specific_heats.shape[0] == self.number_of_fluids

        if self.number_of_fluids > 1:
            assert self.mass_ratios.shape == (self.x.shape[0], self.number_of_fluids)

        assert self.x.shape == self.densities.shape == self.velocities.shape
        assert self.velocities.shape == self.pressures.shape == self.internal_energies.shape

        assert isinstance(self.dx, float)
        assert isinstance(self.CFL, float)
        assert isinstance(self.final_time, float)

    def _set_boundary_conditions(self):
        """
        Function used to extend the grid at the boundary conditions
        """
        i_length = len(self.densities) - 1
        start_state = ThermodynamicState1D(self.pressures[0], self.densities[0], self.velocities[0], self.gamma[0],
                                           self.mass_ratios[0, :])
        end_state = ThermodynamicState1D(self.pressures[i_length], self.densities[i_length],
                                         self.velocities[i_length], self.gamma[i_length],
                                         self.mass_ratios[i_length, :])

        if self.flux_calculator != FluxCalculator1D.MUSCL:
            new_mass_ratios = np.zeros((i_length + 3, self.number_of_fluids))
            new_mass_ratios[1:-1, :] = self.mass_ratios
            # Low end
            start_state = self.boundary_functions[BoundaryConditionND.X_LOW](start_state)
            self.densities = np.append([start_state.rho], self.densities, 0)
            self.pressures = np.append([start_state.p], self.pressures, 0)
            self.velocities = np.append([start_state.u], self.velocities, 0)
            self.internal_energies = np.append([start_state.e_int], self.internal_energies, 0)
            self.kinetic_energies = np.append([start_state.e_kin], self.kinetic_energies, 0)
            self.gamma = np.append([start_state.gamma], self.gamma, 0)
            new_mass_ratios[0, :] = start_state.mass_ratios

            # High end
            end_state = self.boundary_functions[BoundaryConditionND.X_HIGH](end_state)
            self.densities = np.append(self.densities, [end_state.rho], 0)
            self.pressures = np.append(self.pressures, [end_state.p], 0)
            self.velocities = np.append(self.velocities, [end_state.u], 0)
            self.internal_energies = np.append(self.internal_energies, [end_state.e_int], 0)
            self.kinetic_energies = np.append(self.kinetic_energies, [end_state.e_kin], 0)
            self.gamma = np.append(self.gamma, [end_state.gamma], 0)
            new_mass_ratios[-1, :] = end_state.mass_ratios
        else:
            new_mass_ratios = np.zeros((i_length + 5, self.number_of_fluids))
            new_mass_ratios[2:-2, :] = self.mass_ratios
            # Low end
            start_state = self.boundary_functions[BoundaryConditionND.X_LOW](start_state)
            self.densities = np.append([start_state.rho, start_state.rho], self.densities, 0)
            self.pressures = np.append([start_state.p, start_state.p], self.pressures, 0)
            self.velocities = np.append([start_state.u, start_state.u], self.velocities, 0)
            self.internal_energies = np.append([start_state.e_int, start_state.e_int], self.internal_energies, 0)
            self.kinetic_energies = np.append([start_state.e_kin, start_state.e_kin], self.kinetic_energies, 0)
            self.gamma = np.append([start_state.gamma, start_state.gamma], self.gamma, 0)
            new_mass_ratios[0, :] = start_state.mass_ratios
            new_mass_ratios[1, :] = start_state.mass_ratios

            # High end
            end_state = self.boundary_functions[BoundaryConditionND.X_HIGH](end_state)
            self.densities = np.append(self.densities, [end_state.rho, end_state.rho], 0)
            self.pressures = np.append(self.pressures, [end_state.p, end_state.p], 0)
            self.velocities = np.append(self.velocities, [end_state.u, end_state.u], 0)
            self.internal_energies = np.append(self.internal_energies, [end_state.e_int, end_state.e_int], 0)
            self.kinetic_energies = np.append(self.kinetic_energies, [end_state.e_kin, end_state.e_kin], 0)
            self.gamma = np.append(self.gamma, [end_state.gamma, end_state.gamma], 0)
            new_mass_ratios[-1, :] = end_state.mass_ratios
            new_mass_ratios[-2, :] = end_state.mass_ratios
        self.mass_ratios = new_mass_ratios

    def _calculate_fluxes(self, dt, ts):
        """
        Function used to calculate the fluxes between cells using a flux based method
        """
        assert isinstance(ts, int)

        if self.flux_calculator == FluxCalculator1D.LAX_WENDROFF:
            self.density_fluxes, \
            self.momentum_fluxes, \
            self.total_energy_fluxes, \
            self.mass_ratio_fluxes = FluxCalculator1D.calculate_lax_wendroff_fluxes(self.densities,
                                                                                    self.pressures,
                                                                                    self.velocities,
                                                                                    self.gamma,
                                                                                    self.mass_ratios,
                                                                                    dt,
                                                                                    self.dx)
        elif self.flux_calculator == FluxCalculator1D.GODUNOV:
            self.density_fluxes, \
            self.momentum_fluxes, \
            self.total_energy_fluxes, \
            self.mass_ratio_fluxes = FluxCalculator1D.calculate_godunov_fluxes(self.densities,
                                                                               self.pressures,
                                                                               self.velocities,
                                                                               self.gamma,
                                                                               self.mass_ratios)
        elif self.flux_calculator == FluxCalculator1D.RANDOM_CHOICE:
            dx_over_dt = self.dx / dt
            self.density_fluxes, \
            self.momentum_fluxes, \
            self.total_energy_fluxes, \
            self.mass_ratio_fluxes = FluxCalculator1D.calculate_random_choice_fluxes(self.densities,
                                                                                       self.pressures,
                                                                                       self.velocities,
                                                                                       self.gamma,
                                                                                       self.mass_ratios,
                                                                                       ts,
                                                                                       dx_over_dt)
        elif self.flux_calculator == FluxCalculator1D.HLLC:
            self.density_fluxes, \
            self.momentum_fluxes, \
            self.total_energy_fluxes, \
            self.mass_ratio_fluxes = FluxCalculator1D.calculate_hllc_fluxes(self.densities,
                                                                              self.pressures,
                                                                              self.velocities,
                                                                              self.gamma,
                                                                              self.mass_ratios)
        elif self.flux_calculator == FluxCalculator1D.MUSCL:
            dt_over_dx = dt / self.dx
            self.density_fluxes, \
            self.momentum_fluxes, \
            self.total_energy_fluxes, \
            self.mass_ratio_fluxes = FluxCalculator1D.calculate_muscl_fluxes(self.densities,
                                                                              self.pressures,
                                                                              self.velocities,
                                                                              self.gamma,
                                                                              self.mass_ratios,
                                                                              self.specific_heats,
                                                                              self.molar_masses,
                                                                              dt_over_dx)
        else:
            raise RuntimeError("Flux calculator does not exist")

        grid_length = len(self.densities)
        if self.flux_calculator != FluxCalculator1D.MUSCL:
            self.densities = self.densities[1:grid_length - 1]
            self.pressures = self.pressures[1:grid_length - 1]
            self.velocities = self.velocities[1:grid_length - 1]
            self.internal_energies = self.internal_energies[1:grid_length - 1]
            self.kinetic_energies = self.kinetic_energies[1:grid_length - 1]
            self.gamma = self.gamma[1:grid_length - 1]
            self.mass_ratios = self.mass_ratios[1:grid_length - 1]
        else:
            self.densities = self.densities[2:grid_length - 2]
            self.pressures = self.pressures[2:grid_length - 2]
            self.velocities = self.velocities[2:grid_length - 2]
            self.internal_energies = self.internal_energies[2:grid_length - 2]
            self.kinetic_energies = self.kinetic_energies[2:grid_length - 2]
            self.gamma = self.gamma[2:grid_length - 2]
            self.mass_ratios = self.mass_ratios[2:grid_length - 2]

    def _calculate_time_step(self):
        """
        Calculates the time step from the approximation in Toro 6 using max(|u| + a) in the domain
        """
        max_wave_speed = 0.0
        for i, dens in enumerate(self.densities):
            sound_speed = np.sqrt(self.gamma[i] * self.pressures[i] / self.densities[i])
            wave_speed = np.fabs(self.velocities[i]) + sound_speed
            if wave_speed > max_wave_speed:
                max_wave_speed = wave_speed
        if self.flux_calculator == FluxCalculator1D.RANDOM_CHOICE:
            return 0.5 * self.CFL * self.dx / max_wave_speed
        else:
            return self.CFL * self.dx / max_wave_speed

    def _update_states(self, dt):
        """
        Uses the time step and calculated fluxes to update states in each cell
        """
        assert(isinstance(dt, float))

        for i, state in enumerate(self.densities):
            if self.flux_calculator == FluxCalculator1D.RANDOM_CHOICE:
                rho = self.density_fluxes[i]
                momentum = self.momentum_fluxes[i]
                total_energy = self.total_energy_fluxes[i]
                mass_ratio = self.mass_ratio_fluxes[i]
                gamma = self.gamma[i]

                velocity = momentum / rho
                kinetic_energy = 0.5 * rho * velocity ** 2
                internal_energy = total_energy - kinetic_energy
                pressure = internal_energy * (gamma - 1)

                state = ThermodynamicState1D(pressure, rho, velocity, gamma, mass_ratio)
            else:
                if self.number_of_fluids == 1:
                    total_density_flux = (self.density_fluxes[i] - self.density_fluxes[i + 1])
                else:
                    total_density_flux = (self.density_fluxes[i] * self.mass_ratio_fluxes[i, :] - self.density_fluxes[i + 1] * self.mass_ratio_fluxes[i + 1, :])
                total_density_flux *= dt / self.dx
                total_momentum_flux = (self.momentum_fluxes[i] - self.momentum_fluxes[i + 1]) * dt / self.dx
                total_energy_flux = (self.total_energy_fluxes[i] - self.total_energy_fluxes[i + 1]) * dt / self.dx

                state = ThermodynamicState1D(self.pressures[i], self.densities[i], self.velocities[i], self.gamma[i], self.mass_ratios[i, :])
                state.update_states(total_density_flux, total_momentum_flux, total_energy_flux, self.specific_heats, self.molar_masses)

            self.densities[i] = state.rho
            self.pressures[i] = state.p
            self.velocities[i] = state.u
            self.internal_energies[i] = state.e_int
            self.kinetic_energies[i] = state.e_kin
            self.gamma[i] = state.gamma
            self.mass_ratios[i, :] = state.mass_ratios

    def run_sim(self):
        """
        High level simulation function that runs the simulation
        """
        t = 0.0
        ts = 1
        times = [t]
        while t < self.final_time:
            dt = self._evolve_time_step(ts)
            t += dt
            ts += 1
            times.append(t)
            print("Step " + str(ts) + ": " + str(dt))

        return times, self.x, self.densities, self.pressures, self.velocities, self.internal_energies, self.kinetic_energies, self.mass_ratios
