"""
Author: Rohan
Date: 04/10/16

This file contains a a class providing methods to calculate the fluxes using Riemann solver based methods 
"""

from CFD_Projects.compressible_hydro.eos.thermodynamic_state import ThermodynamicState1D
from CFD_Projects.compressible_hydro.flux_calculator.riemann_solver import IterativeRiemannSolver
from CFD_Projects.compressible_hydro.flux_calculator.riemann_solver import HLLCRiemannSolver
from CFD_Projects.compressible_hydro.flux_calculator.van_der_corput import VanDerCorput
from CFD_Projects.compressible_hydro.flux_calculator.flux_limiter import *

import numpy as np


class FluxCalculatorND(object):
    GODUNOV = 1
    RANDOM_CHOICE = 2
    HLLC = 3
    MUSCL = 4
    LAX_WENDROFF = 5


class FluxCalculator1D(FluxCalculatorND):
    def __init__(self):
        pass

    @staticmethod
    def calculate_lax_wendroff_fluxes(densities, pressures, velocities, gamma, mass_ratios, dt, dx, alpha=1.5):
        staggered_densities = np.zeros(len(densities) - 1)
        staggered_momentum = np.zeros(len(densities) - 1)
        staggered_energies = np.zeros(len(densities) - 1)

        # Predictor
        for i, rho_stag in enumerate(staggered_densities):
            rho_minus = densities[i]
            rho_0 = densities[i + 1]
            u_minus = velocities[i]
            u_0 = velocities[i + 1]
            p_minus = pressures[i]
            p_0 = pressures[i + 1]
            energy_minus = p_minus / (gamma[i] - 1) + 0.5 * rho_minus * u_minus ** 2
            energy_0 = p_0 / (gamma[i + 1] - 1) + 0.5 * rho_0 * u_0 ** 2

            staggered_densities[i] = 0.5 * dt / dx * (rho_minus * u_minus - rho_0 * u_0)
            staggered_densities[i] += 0.5 * (rho_minus + rho_0)
            staggered_momentum[i] = 0.5 * dt / dx * (rho_minus * u_minus ** 2 + p_minus - rho_0 * u_0 ** 2 - p_0)
            staggered_momentum[i] += 0.5 * (rho_minus * u_minus + rho_0 * u_0)
            staggered_energies[i] = 0.5 * dt / dx * ((energy_minus + p_minus) * u_minus - (energy_0 - p_0) * u_0)
            staggered_energies[i] += 0.5 * (energy_minus + energy_0)

        # Corrector
        density_fluxes = np.zeros(len(densities) - 1)
        momentum_fluxes = np.zeros(len(densities) - 1)
        total_energy_fluxes = np.zeros(len(densities) - 1)
        mass_ratio_fluxes = None
        for i, flux in enumerate(density_fluxes):
            e_kin_stag = 0.5 * staggered_momentum[i] ** 2 / staggered_densities[i]
            e_int_stag = staggered_energies[i] - e_kin_stag
            p_stag = e_int_stag * (gamma[i] - 1)

            density_fluxes[i] = staggered_momentum[i]
            momentum_fluxes[i] = 2 * e_kin_stag + p_stag
            total_energy_fluxes[i] = staggered_energies[i] + p_stag
            total_energy_fluxes[i] *= staggered_momentum[i] / staggered_densities[i]

        # Artificial Viscosity
        for i, flux in enumerate(density_fluxes):
            rho_bar = 0.5 * (densities[i] + densities[i + 1])
            u_minus = velocities[i]
            u_0 = velocities[i + 1]
            u_bar = 0.5 * (u_0 + u_minus)

            viscosity = (u_0 - u_minus) * np.abs(u_0 - u_minus)
            momentum_fluxes[i] -= alpha * rho_bar * viscosity
            total_energy_fluxes[i] -= alpha * rho_bar * u_bar * viscosity

        return density_fluxes, momentum_fluxes, total_energy_fluxes, mass_ratio_fluxes

    @staticmethod
    def calculate_godunov_fluxes(densities, pressures, velocities, gamma, mass_ratios):
        """
        Function used to calculate fluxes for a 1D simulation using Godunov's scheme as in Toro Chapter 6
        """
        density_fluxes = np.zeros(len(densities) - 1)
        momentum_fluxes = np.zeros(len(densities) - 1)
        total_energy_fluxes = np.zeros(len(densities) - 1)
        mass_ratio_fluxes = np.zeros((len(densities) - 1, mass_ratios.shape[1]))

        for i, dens_flux in enumerate(density_fluxes):
            solver = IterativeRiemannSolver()

            # Generate left and right states from cell averaged values
            left_state = ThermodynamicState1D(pressures[i], densities[i], velocities[i], gamma[i])
            right_state = ThermodynamicState1D(pressures[i + 1], densities[i + 1], velocities[i + 1], gamma[i + 1])

            # Solve Riemann problem for star states
            p_star, u_star = solver.get_star_states(left_state, right_state)

            # Calculate fluxes using solver sample function
            p_flux, u_flux, rho_flux, is_left = solver.sample(0.0, left_state, right_state, p_star, u_star)

            # Store fluxes in array
            flux_gamma = left_state.gamma if is_left else right_state.gamma
            mass_ratio_fluxes[i, :] = mass_ratios[i, :] if is_left else mass_ratios[i + 1, :]
            density_fluxes[i] = rho_flux * u_flux
            momentum_fluxes[i] = rho_flux * u_flux * u_flux + p_flux
            e_tot = p_flux / (flux_gamma - 1) + 0.5 * rho_flux * u_flux * u_flux
            total_energy_fluxes[i] = (p_flux + e_tot) * u_flux

        return density_fluxes, momentum_fluxes, total_energy_fluxes, mass_ratio_fluxes

    @staticmethod
    def calculate_muscl_fluxes(densities, pressures, velocities, gamma,
                               mass_ratios, specific_heats, molar_masses, dt_over_dx):
        """
        Function used to calculate fluxes for a 1D simulation using a MUSCL Hancock Scheme - Toro 14.4
        """
        # Get half step densities
        limiter = UltraBeeLimiter()
        half_step_densities_L = np.zeros(len(densities) - 2)
        half_step_velocities_L = np.zeros(half_step_densities_L.shape)
        half_step_pressures_L = np.zeros(half_step_densities_L.shape)
        half_step_mass_ratios_L = np.zeros((len(densities) - 2, len(specific_heats)))
        half_step_densities_R = np.zeros(half_step_densities_L.shape)
        half_step_velocities_R = np.zeros(half_step_densities_L.shape)
        half_step_pressures_R = np.zeros(half_step_densities_L.shape)
        half_step_mass_ratios_R = np.zeros(half_step_mass_ratios_L.shape)
        for i, dens in enumerate(half_step_densities_L):
            idx = i + 1

            # Calculate slopes
            left_slopes = dict()
            left_slopes["rho"] = (densities[idx] - densities[idx - 1]) / 2
            left_slopes["mom"] = (densities[idx] * velocities[idx] - densities[idx - 1] * velocities[idx - 1]) / 2
            cell_energy = 0.5 * densities[idx] * velocities[idx] * velocities[idx] + pressures[idx] / (gamma[idx] - 1)
            behind_energy = 0.5 * densities[idx - 1] * velocities[idx - 1] * velocities[idx - 1] + pressures[idx - 1] / (gamma[idx - 1] - 1)
            left_slopes["energy"] = (cell_energy - behind_energy) / 2

            right_slopes = dict()
            right_slopes["rho"] = (densities[idx + 1] - densities[idx]) / 2
            right_slopes["mom"] = (densities[idx + 1] * velocities[idx + 1] - densities[idx] * velocities[idx]) / 2
            forward_energy = 0.5 * densities[idx + 1] * velocities[idx + 1] * velocities[idx + 1] + pressures[idx + 1] / (gamma[idx + 1] - 1)
            right_slopes["energy"] = (forward_energy - cell_energy) / 2

            average_density_slope, average_momentum_slope, average_energy_slope = limiter.calculate_limited_slopes(left_slopes, right_slopes)

            # Interpolate left and right densities
            left_density = densities[idx] - average_density_slope
            left_momentum = densities[idx] * velocities[idx] - average_momentum_slope
            left_energy = cell_energy - average_energy_slope
            left_mass_ratios = mass_ratios[idx, :]
            assert left_density > 0, left_density
            assert left_energy > 0, left_energy
            assert np.isclose(1.0, left_mass_ratios.sum(), 1e-14)

            right_density = densities[idx] + average_density_slope
            right_momentum = densities[idx] * velocities[idx] + average_momentum_slope
            right_energy = cell_energy + average_energy_slope
            right_mass_ratios = mass_ratios[idx, :]
            assert right_density > 0, right_density
            assert right_energy > 0, right_energy
            assert np.isclose(1.0, right_mass_ratios.sum(), 1e-14)

            # Perform half step flux
            left_velocity = left_momentum / left_density
            left_density_flux = left_momentum
            left_internal_energy = left_energy - 0.5 * left_momentum * left_velocity
            left_pressure = left_internal_energy * (gamma[idx] - 1)
            left_momentum_flux = left_momentum * left_velocity + left_pressure
            left_energy_flux = (left_energy + left_pressure) * left_velocity

            right_velocity = right_momentum / right_density
            right_density_flux = right_momentum
            right_internal_energy = right_energy - 0.5 * right_momentum * right_velocity
            right_pressure = right_internal_energy * (gamma[idx] - 1)
            right_momentum_flux = right_momentum * right_velocity + right_pressure
            right_energy_flux = (right_energy + right_pressure) * right_velocity

            half_step_density_flux = (left_density_flux - right_density_flux) * dt_over_dx * 0.5
            half_step_momentum_flux = (left_momentum_flux - right_momentum_flux) * dt_over_dx * 0.5
            half_step_energy_flux = (left_energy_flux - right_energy_flux) * dt_over_dx * 0.5

            state = ThermodynamicState1D(left_pressure, left_density, left_velocity, gamma[idx], left_mass_ratios)
            state.update_states(half_step_density_flux,
                                half_step_momentum_flux,
                                half_step_energy_flux,
                                specific_heats, molar_masses)
            half_step_densities_L[i] = state.rho
            half_step_velocities_L[i] = state.u
            half_step_pressures_L[i] = state.p
            half_step_mass_ratios_L[i, :] = state.mass_ratios

            state = ThermodynamicState1D(right_pressure, right_density, right_velocity, gamma[idx], right_mass_ratios)
            state.update_states(half_step_density_flux,
                                half_step_momentum_flux,
                                half_step_energy_flux,
                                specific_heats, molar_masses)
            half_step_densities_R[i] = state.rho
            half_step_velocities_R[i] = state.u
            half_step_pressures_R[i] = state.p
            half_step_mass_ratios_R[i, :] = state.mass_ratios

        # Calculate final fluxes
        density_fluxes = np.zeros(len(half_step_densities_R) - 1)
        momentum_fluxes = np.zeros(len(half_step_densities_R) - 1)
        total_energy_fluxes = np.zeros(len(half_step_densities_R) - 1)
        mass_ratio_fluxes = np.zeros((len(half_step_densities_R) - 1, mass_ratios.shape[1]))

        for i, dens_flux in enumerate(density_fluxes):
            solver = IterativeRiemannSolver()

            # Generate left and right states from cell averaged values
            left_state = ThermodynamicState1D(half_step_pressures_R[i],
                                              half_step_densities_R[i],
                                              half_step_velocities_R[i],
                                              gamma[i],
                                              half_step_mass_ratios_L[i, :])
            right_state = ThermodynamicState1D(half_step_pressures_L[i + 1],
                                               half_step_densities_L[i + 1],
                                               half_step_velocities_L[i + 1],
                                               gamma[i + 1],
                                               half_step_mass_ratios_R[i + 1, :])

            # Solve Riemann problem for star states
            p_star, u_star = solver.get_star_states(left_state, right_state)

            # Calculate fluxes using solver sample function
            p_flux, u_flux, rho_flux, is_left = solver.sample(0.0, left_state, right_state, p_star, u_star)

            # Store fluxes in array
            mass_ratio_fluxes[i, :] = left_state.mass_ratios if is_left else right_state.mass_ratios
            flux_gamma = left_state.gamma if is_left else right_state.gamma
            density_fluxes[i] = rho_flux * u_flux
            momentum_fluxes[i] = rho_flux * u_flux * u_flux + p_flux
            e_tot = p_flux / (flux_gamma - 1) + 0.5 * rho_flux * u_flux * u_flux
            total_energy_fluxes[i] = (p_flux + e_tot) * u_flux

        return density_fluxes, momentum_fluxes, total_energy_fluxes, mass_ratio_fluxes

    @staticmethod
    def calculate_hllc_fluxes(densities, pressures, velocities, gamma, mass_ratios):
        """
        Calculated the fluxes bases on the HLLC approximate Riemann solver
        """
        density_fluxes = np.zeros(len(densities) - 1)
        momentum_fluxes = np.zeros(len(densities) - 1)
        total_energy_fluxes = np.zeros(len(densities) - 1)
        mass_ratio_fluxes = np.ones((len(densities) - 1, mass_ratios.shape[1]))

        for i, dens_flux in enumerate(density_fluxes):
            solver = HLLCRiemannSolver(gamma[i + 1])
            # Generate left and right states from cell averaged values
            left_state = ThermodynamicState1D(pressures[i], densities[i], velocities[i], gamma[i], mass_ratios=mass_ratios[i])
            right_state = ThermodynamicState1D(pressures[i + 1], densities[i + 1], velocities[i + 1], gamma[i + 1], mass_ratios=mass_ratios[i + 1])

            density_fluxes[i], momentum_fluxes[i], total_energy_fluxes[i], mass_ratio_fluxes[i, :] = solver.evaluate_flux(left_state, right_state)

        return density_fluxes, momentum_fluxes, total_energy_fluxes, mass_ratio_fluxes


    @staticmethod
    def calculate_random_choice_fluxes(densities, pressures, velocities, gamma, mass_ratios, ts, dx_over_dt):
        """
        Function used to calculate states for a 1D simulation using Glimm's random choice scheme as in Toro Chapter 7
        """
        density_fluxes = np.zeros(len(densities) - 1)
        momentum_fluxes = np.zeros(len(densities) - 1)
        total_energy_fluxes = np.zeros(len(densities) - 1)
        mass_ratio_fluxes = np.zeros((len(densities) - 1, mass_ratios.shape[1]))

        theta = VanDerCorput.calculate_theta(ts, 2, 1)
        for i in range(len(densities) - 2):
            solver = IterativeRiemannSolver()

            # Generate left and right states from cell averaged values
            left_state = ThermodynamicState1D(pressures[i], densities[i], velocities[i], gamma[i])
            mid_state = ThermodynamicState1D(pressures[i + 1], densities[i + 1], velocities[i + 1], gamma[i + 1])
            right_state = ThermodynamicState1D(pressures[i + 2], densities[i + 2], velocities[i + 2], gamma[i + 2])

            # Solve Riemann problem for star states on either side of the cell
            p_star_left, u_star_left = solver.get_star_states(left_state, mid_state)
            p_star_right, u_star_right = solver.get_star_states(mid_state, right_state)

            # Calculate fluxes using solver sample function
            if theta <= 0.5:
                p_flux, u_flux, rho_flux, is_left = solver.sample(theta * dx_over_dt, left_state, mid_state,
                                                                  p_star_left, u_star_left)
            else:
                p_flux, u_flux, rho_flux, is_left = solver.sample((theta - 1) * dx_over_dt, mid_state, right_state,
                                                                  p_star_right, u_star_right)

            # Store fluxes in array
            if theta <= 0.5:
                flux_gamma = left_state.gamma if is_left else mid_state.gamma
                mass_ratio_fluxes[i] = mass_ratios[i] if is_left else mass_ratios[i + 1]
            else:
                flux_gamma = mid_state.gamma if is_left else right_state.gamma
                mass_ratio_fluxes[i] = mass_ratios[i + 1] if is_left else mass_ratios[i + 2]
            density_fluxes[i] = rho_flux
            momentum_fluxes[i] = rho_flux * u_flux
            total_energy_fluxes[i] = p_flux / (flux_gamma - 1) + 0.5 * rho_flux * u_flux * u_flux

        return density_fluxes, momentum_fluxes, total_energy_fluxes, mass_ratio_fluxes


class FluxCalculator2D(FluxCalculatorND):
    def __init__(self):
        pass

    @staticmethod
    def calculate_godunov_fluxes(densities, pressures, vel_x, vel_y, gamma):
        """
        Function used to calculate fluxes for a 2D simulation using Godunov's scheme
        """
        density_fluxes = np.zeros((densities.shape[0] - 1, densities.shape[1] - 1, 2))
        momentum_flux_x = np.zeros(density_fluxes.shape)
        momentum_flux_y = np.zeros(density_fluxes.shape)
        total_energy_fluxes = np.zeros(density_fluxes.shape)

        i_length, j_length = np.shape(densities)
        for i in range(i_length - 1):
            for j in range(j_length - 1):
                solver = IterativeRiemannSolver()

                # Generate left and right states from cell averaged values
                left_state = ThermodynamicState1D(pressures[i, j], densities[i, j], vel_x[i, j], gamma[i, j])
                right_state = ThermodynamicState1D(pressures[i + 1, j], densities[i + 1, j], vel_x[i + 1, j], gamma[i + 1, j])

                # Solve Riemann problem for star states
                p_star, u_star = solver.get_star_states(left_state, right_state)

                # Calculate fluxes using solver sample function
                p_flux, u_flux, rho_flux, is_left = solver.sample(0.0, left_state, right_state, p_star, u_star)

                # Store fluxes in array
                v_y = vel_y[i, j] if is_left else vel_y[i + 1, j]
                flux_gamma = left_state.gamma if is_left else right_state.gamma
                density_fluxes[i, j - 1, 0] = rho_flux * u_flux
                momentum_flux_x[i, j - 1, 0] = rho_flux * u_flux * u_flux + p_flux
                momentum_flux_y[i, j - 1, 0] = rho_flux * u_flux * v_y
                e_tot = p_flux / (flux_gamma - 1) + 0.5 * rho_flux * u_flux * u_flux + 0.5 * rho_flux * v_y ** 2
                total_energy_fluxes[i, j - 1, 0] = (p_flux + e_tot) * u_flux

                # Generate left and right states from cell averaged values
                left_state = ThermodynamicState1D(pressures[i, j], densities[i, j], vel_y[i, j], gamma[i, j])
                right_state = ThermodynamicState1D(pressures[i, j + 1], densities[i, j + 1], vel_y[i, j + 1], gamma[i, j + 1])

                # Solve Riemann problem for star states
                p_star, v_star = solver.get_star_states(left_state, right_state)

                # Calculate fluxes using solver sample function
                p_flux, v_flux, rho_flux, is_left = solver.sample(0.0, left_state, right_state, p_star, v_star)

                # Store fluxes in array
                v_x = vel_x[i, j] if is_left else vel_x[i, j + 1]
                flux_gamma = left_state.gamma if is_left else right_state.gamma
                density_fluxes[i - 1, j, 1] = rho_flux * v_flux
                momentum_flux_x[i - 1, j, 1] = rho_flux * v_x * v_flux
                momentum_flux_y[i - 1, j, 1] = rho_flux * v_flux * v_flux + p_flux
                e_tot = p_flux / (flux_gamma - 1) + 0.5 * rho_flux * v_flux * v_flux + 0.5 * rho_flux * v_x ** 2
                total_energy_fluxes[i - 1, j, 1] = (p_flux + e_tot) * v_flux

        return density_fluxes, momentum_flux_x, momentum_flux_y, total_energy_fluxes
