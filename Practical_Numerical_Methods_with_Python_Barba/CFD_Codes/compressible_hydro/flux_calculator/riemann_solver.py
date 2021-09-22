"""
Author: Rohan
Date: 29/06/16

This file contains a class hierarchy used to model a numerical solution to Riemann problems. The implementation of
the Riemann solvers here follows those in Toro. There Riemann solvers assume an ideal gas equation of state.
"""

import numpy as np

from CFD_Projects.compressible_hydro.eos.thermodynamic_state import ThermodynamicState1D


class BaseRiemannSolver(object):
    def __init__(self):
        pass

    def _estimate_p_star(self, left, right):
        """
        :return: an estimate for p_star used in the iterative scheme
        """
        gamma = left.gamma
        G1 = (gamma - 1.0) / (2.0 * gamma)
        G3 = (2.0 * gamma) / (gamma - 1.0)
        G4 = 2.0 / (gamma - 1.0)
        G5 = 2.0 / (gamma + 1.0)
        G6 = (gamma - 1.0) / (gamma + 1.0)
        G7 = (gamma - 1.0) / 2.0
        CUP = 0.25 * (left.rho + right.rho) * (left.sound_speed() + right.sound_speed())
        PPV = 0.5 * (left.p + right.p) + 0.5 * (left.u - right.u) * CUP
        PPV = max(1e-6, PPV)
        PMIN = min(left.p, right.p)
        PMAX = max(left.p, right.p)
        QMAX = PMAX / PMIN

        if QMAX <= 2.0 and PMIN <= PPV <= PMAX:
            PM = PPV
        else:
            if (PPV < PMIN):
                PQ = (left.p / right.p) ** G1
                UM = (PQ * left.u / left.sound_speed() + right.u / right.sound_speed() + G4 * (PQ - 1.0)) \
                     / (PQ / left.sound_speed() + 1.0 / right.sound_speed())
                PTL = 1.0 + G7 * (left.u - UM) / left.sound_speed()
                PTR = 1.0 + G7 * (UM - right.u) / right.sound_speed()
                PM = 0.5 * (left.p * PTL ** G3 + right.p * PTR ** G3)
            else:
                GEL = np.sqrt((G5 / left.rho) / (G6 * left.p + PPV))
                GER = np.sqrt((G5 / right.rho) / (G6 * right.p + PPV))
                PM = (GEL * left.p + GER * right.p - (right.u - left.u)) / (GEL + GER)
                PM = max(1e-6, PM)
        return PM


class IterativeRiemannSolver(BaseRiemannSolver):
    """
    Exact Riemann solver based on a Newton Raphson iterative scheme, outlined in Toro - Chapter 4
    """
    def __init__(self):
        super(IterativeRiemannSolver, self).__init__()

    def __get_A(self, rho, gamma):
        """
        rho: density in the system outside of the star region on either the right or left.
        
        :return: the coefficient A for the solution of f 
        """
        return 2.0 / ((gamma + 1) * rho)
    
    def __get_B(self, pressure, gamma):
        """
        pressure: the pressure in the system outside the star region on either the right or left.
        
        :return: the coefficient B for the solution of f 
        """
        return (gamma - 1) / (gamma + 1) * pressure
    
    def __f(self, p_star, outer):
        """
        p_star: pressure in the star region
        p_outer: pressure outside star region on either the right or left
        rho_outer: density outside the star region on either the right or left
        a: sound speed outside the star region on either the right ofr left
        
        :return: the solution to the function f in the iterative scheme for calculating p star 
        """
        
        if p_star <= outer.p:
            return 2.0 * outer.sound_speed() / (outer.gamma - 1) * \
                   ((p_star / outer.p) ** ((outer.gamma - 1) / (2 * outer.gamma)) - 1)
        else:
            A = self.__get_A(outer.rho, outer.gamma)
            B = self.__get_B(outer.p, outer.gamma)
            
            return (p_star - outer.p) * (A / (p_star + B)) ** 0.5

    def __f_total(self, p_star, left, right):

        f_r = self.__f(p_star, right)
        f_l = self.__f(p_star, left)

        return f_r + f_l + (right.u - left.u)

    def __f_derivative(self, p_star, outer):
        """
        :param p_star:
        :param p_outer:
        :param rho_outer:
        :param a_outer:

        :return: the derivative of the function f in the iterative scheme for calculating p star
        """

        if p_star <= outer.p:
            return 1.0 / (outer.rho * outer.sound_speed()) * (p_star / outer.p) ** (-(outer.gamma + 1) / (2 * outer.gamma))
        else:
            A = self.__get_A(outer.rho, outer.gamma)
            B = self.__get_B(outer.p, outer.gamma)

            return (1 - (p_star - outer.p) / (2 * (B + p_star))) * (A / (p_star + B)) ** 0.5

    def __f_total_derivative(self, p_star, left, right):

        f_deriv_r = self.__f_derivative(p_star, right)
        f_deriv_l = self.__f_derivative(p_star, left)

        return f_deriv_r + f_deriv_l

    def __get_p_star(self, left_state, right_state):
        """
        :return: the pressure in the star region.
        """
        TOL = 1e-6

        p_sol = self._estimate_p_star(left_state, right_state)
        delta = 1.0
        i = 0
        while delta > TOL:
            f_total = self.__f_total(p_sol, left_state, right_state)
            f_total_derivative = self.__f_total_derivative(p_sol, left_state, right_state)

            p_prev = p_sol
            p_sol -= f_total / f_total_derivative
            delta = np.abs(p_prev - p_sol)
            i += 1

        return p_sol
    
    def __get_u_star(self, p_star, left_state, right_state):
        """

        :return: the velocity in the star region.
        """

        return 0.5 * (left_state.u + right_state.u) + 0.5 * (self.__f(p_star, right_state) - self.__f(p_star, left_state))

    def get_star_states(self, left_state, right_state):
        """
        :param left_state: thermodynamic conditions to the left of IVP
        :param right_state: thermodynamic conditions to the right of IVP

        :return: the star pressure and velocity states
        """
        assert isinstance(left_state, ThermodynamicState1D)
        assert isinstance(right_state, ThermodynamicState1D)

        # Check for vacuum generation
        if IterativeRiemannSolver.is_vacuum_generated(left_state, right_state):
            self.sample_vacuum(0.0, left_state, right_state)

        p_star = self.__get_p_star(left_state, right_state)
        u_star = self.__get_u_star(p_star, left_state, right_state)
        
        return p_star, u_star

    @staticmethod
    def is_vacuum_generated(left_state, right_state):
        """
        Function that checks the positivity condition to determine whether the left and right states lead to a vacuum
        """
        # raise NotImplementedError("Fix gamma implementation to be variable gamma!")
        vacuum_condition = (left_state.sound_speed() + right_state.sound_speed()) * 2.0 / (left_state.gamma - 1)
        velocity_difference = right_state.u - left_state.u

        # zero_energy_state = left_state.sound_speed() == 0.0 or right_state.sound_speed() == 0.0
        if vacuum_condition <= velocity_difference:
            return True
        else:
            return False

    def sample_vacuum(self, x_over_t, left_state, right_state):
        """
        Function used to sample in a vacuum state
        """
        S_L = left_state.u + 2 * left_state.sound_speed() / (left_state.gamma - 1.0)
        S_R = right_state.u - 2 * right_state.sound_speed() / (right_state.gamma - 1.0)

        if x_over_t <= (left_state.u - left_state.sound_speed()):
            return left_state.p, left_state.u, left_state.rho, True
        elif x_over_t <= S_L:
            gamma = left_state.gamma
            multiplier = ((2.0 / (gamma + 1)) + (gamma - 1) * (left_state.u - x_over_t) / (left_state.sound_speed() * (gamma + 1))) ** (2.0 / (gamma - 1.0))
            rho = left_state.rho * multiplier
            u = (2.0 / (gamma + 1)) * (left_state.sound_speed() + (gamma - 1) * left_state.u / 2.0 + x_over_t)
            p = left_state.p * multiplier ** gamma
            return p, u, rho, True
        elif S_L < x_over_t < S_R:
            return 0.0, S_L, 0.0, True
        elif x_over_t < right_state.u + right_state.sound_speed():
            gamma = right_state.gamma
            multiplier = ((2.0 / (gamma + 1)) - (gamma - 1) * (right_state.u - x_over_t) / (right_state.sound_speed() * (gamma + 1))) ** (2.0 / (gamma - 1.0))
            rho = right_state.rho * multiplier
            u = (2.0 / (gamma + 1)) * (-right_state.sound_speed() + (gamma - 1) * right_state.u / 2.0 + x_over_t)
            p = right_state.p * multiplier ** gamma
            return p, u, rho, False
        elif x_over_t >= right_state.u + right_state.sound_speed():
            return right_state.p, right_state.u, right_state.rho, False
        else:
            raise RuntimeError("Shouldn't be possible to get here")

    def sample(self, x_over_t, left_state, right_state, p_star, u_star):
        """
        Function used to sample Riemann problem at a specific wave speed, to get the state

        Last returned value is a boolean determining whether the state returned is on the left side of the contact
        """
        # Check if state is a vacuum
        if IterativeRiemannSolver.is_vacuum_generated(left_state, right_state):
            return self.sample_vacuum(x_over_t, left_state, right_state)
        else:
            # Find state along wave line
            if u_star < x_over_t:
                # Consider right wave structures
                rho = right_state.rho
                p = right_state.p
                gamma = right_state.gamma
                if right_state.p >= p_star:
                    rho_star = rho * (p_star / p) ** (1 / gamma)
                    a_star = np.sqrt(gamma * p_star / rho_star)
                    wave_right_high = right_state.u + right_state.sound_speed()
                    wave_right_low = u_star + a_star
                    if wave_right_high <= x_over_t:
                        return right_state.p, right_state.u, right_state.rho, False
                    else:
                        if wave_right_low >= x_over_t:
                            return p_star, u_star, rho_star, False
                        else:
                            multiplier = ((2.0 / (gamma + 1)) - (gamma - 1) * (right_state.u - x_over_t) / (right_state.sound_speed() * (gamma + 1))) ** (2.0 / (gamma - 1.0))
                            rho = right_state.rho * multiplier
                            u = (2.0 / (gamma + 1)) * (-right_state.sound_speed() + (gamma - 1) * right_state.u / 2.0 + x_over_t)
                            p = right_state.p * multiplier ** gamma
                            return p, u, rho, False
                else:
                    rho_star = rho * ((p_star / p + (gamma - 1) / (gamma + 1)) / ((gamma - 1) / (gamma + 1) * (p_star / p) + 1))
                    wave_right_shock = right_state.u + right_state.sound_speed() * ((gamma + 1) * p_star / (2 * gamma * right_state.p) + (gamma - 1) / (2 * gamma)) ** 0.5
                    if wave_right_shock <= x_over_t:
                        return right_state.p, right_state.u, right_state.rho, False
                    else:
                        return p_star, u_star, rho_star, False
            else:
                # Consider left wave structures
                rho = left_state.rho
                p = left_state.p
                gamma = left_state.gamma
                if left_state.p >= p_star:
                    rho_star = rho * (p_star / p) ** (1 / gamma)
                    a_star = np.sqrt(gamma * p_star / rho_star)
                    wave_left_high = left_state.u - left_state.sound_speed()
                    wave_left_low = u_star - a_star
                    if wave_left_high >= x_over_t:
                        return left_state.p, left_state.u, left_state.rho, True
                    else:
                        if wave_left_low <= x_over_t:
                            return p_star, u_star, rho_star, True
                        else:
                            multiplier = ((2.0 / (gamma + 1)) + (gamma - 1) * (left_state.u - x_over_t) / (left_state.sound_speed() * (gamma + 1))) ** (2.0 / (gamma - 1.0))
                            rho = left_state.rho * multiplier
                            u = (2.0 / (gamma + 1)) * (left_state.sound_speed() + (gamma - 1) * left_state.u / 2.0 + x_over_t)
                            p = left_state.p * multiplier ** gamma
                            return p, u, rho, True
                else:
                    rho_star = rho * ((p_star / p + (gamma - 1) / (gamma + 1)) / ((gamma - 1) / (gamma + 1) * (p_star / p) + 1))
                    wave_left_shock = left_state.u - left_state.sound_speed() * ((gamma + 1) * p_star / (2 * gamma * left_state.p) + (gamma - 1) / (2 * gamma)) ** 0.5
                    if wave_left_shock >= x_over_t:
                        return left_state.p, left_state.u, left_state.rho, True
                    else:
                        return p_star, u_star, rho_star, True


class HLLCRiemannSolver(BaseRiemannSolver):
    """
    Approximate Riemann solver that estimates the fluxes based on the assumption of uniform star states, outlined
    in Toro - Chapter 10

    This riemann solver is currently only implemented in 1D
    """
    def __init__(self, gamma):
        assert isinstance(gamma, float)
        self.gamma = gamma
        super(HLLCRiemannSolver, self).__init__()

    def __evaluate_S_star(self, left, right, S_L, S_R):
        """
        Function to evaluate the contact velocity S_Star based on the estimated wave speeds and state data
        """
        S_Star = right.p - left.p + left.rho * left.u * (S_L - left.u) - right.rho * right.u * (S_R - right.u)
        S_Star /= left.rho * (S_L - left.u) - right.rho * (S_R - right.u)

        return S_Star

    def __evaluate_wave_speeds(self, left, right, p_estimate):
        """
        Evaluate wave speeds based on pressure estimate
        """
        gamma_coeff = (self.gamma + 1) / (2 * self.gamma)

        q_L = 1 if p_estimate <= left.p else (1 + gamma_coeff * (p_estimate / left.p - 1)) ** 0.5
        q_R = 1 if p_estimate <= right.p else (1 + gamma_coeff * (p_estimate / right.p - 1)) ** 0.5

        S_L = left.u - left.sound_speed() * q_L
        S_R = right.u + right.sound_speed() * q_R

        S_Star = self.__evaluate_S_star(left, right, S_L, S_R)

        return S_L, S_R, S_Star

    def __evaluate_davis_wave_speeds(self, left, right):
        """
        Evaluate the wave speeds based directly on the left and right states
        """
        S_L = left.u - left.sound_speed()
        S_R = right.u + right.sound_speed()
        S_Star = self.__evaluate_S_star(left, right, S_L, S_R)

        return S_L, S_R, S_Star

    def __evaluate_einfeldt_wave_speeds(self, left, right):
        """
        Evaluate the wave speeds based on Einfeldt's estimates
        :return:
        """
        # Calculate d_bar
        rho_L_root = np.sqrt(left.rho)
        rho_R_root = np.sqrt(right.rho)
        eta = 0.5 * rho_L_root * rho_R_root / ((rho_L_root + rho_R_root) ** 2)
        eta_term = eta * (right.u - left.u) ** 2

        d_squared = (rho_L_root * left.sound_speed() ** 2 + rho_R_root * right.sound_speed() ** 2) / (rho_L_root + rho_R_root)
        d_squared += eta_term
        d_bar = np.sqrt(d_squared)

        # Calculate u_bar
        u_bar = (rho_L_root * left.u + rho_R_root * right.u)
        u_bar /= (rho_L_root + rho_R_root)

        assert not np.isnan(u_bar)
        assert not np.isnan(d_bar)

        # Get wave speeds
        S_L = u_bar - d_bar
        S_R = u_bar + d_bar
        S_Star = self.__evaluate_S_star(left, right, S_L, S_R)

        return S_L, S_R, S_Star

    def __HLLC_flux(self, left, right, S_L, S_Star, S_R):
        """
        Get the HLLC flux by using the constant star state assumption and integrating across the left and right regions
        """
        if 0 <= S_Star:
            rho_flux = left.u * left.rho
            mom_flux = rho_flux * left.u + left.p
            energy_flux = (left.p / (left.gamma - 1) + 0.5 * rho_flux * left.u + left.p) * left.u
            mass_ratio_flux = left.mass_ratios
            if 0 < S_L:
                return rho_flux, mom_flux, energy_flux, mass_ratio_flux
            else:
                flux_coeff_1 = S_Star / (S_L - S_Star)
                flux_coeff_2 = S_L / (S_L - S_Star) * (left.p + left.rho * (S_L - left.u) * (S_Star - left.u))

                rho_flux = flux_coeff_1 * (S_L * left.rho - rho_flux)
                mom_flux = flux_coeff_1 * (S_L * left.u * left.rho - mom_flux) + flux_coeff_2
                energy_flux = flux_coeff_1 * (S_L * (left.e_int * left.rho + left.e_kin) - energy_flux) + flux_coeff_2 * S_Star

                return rho_flux, mom_flux, energy_flux, mass_ratio_flux
        else:
            rho_flux = right.u * right.rho
            mom_flux = rho_flux * right.u + right.p
            energy_flux = (right.p / (right.gamma - 1) + 0.5 * rho_flux * right.u + right.p) * right.u
            mass_ratio_flux = right.mass_ratios
            if S_R <= 0:
                return rho_flux, mom_flux, energy_flux, mass_ratio_flux
            else:
                flux_coeff_1 = S_Star / (S_R - S_Star)
                flux_coeff_2 = S_R / (S_R - S_Star) * (right.p + right.rho * (S_R - right.u) * (S_Star - right.u))

                rho_flux = flux_coeff_1 * (S_R * right.rho - rho_flux)
                mom_flux = flux_coeff_1 * (S_R * right.u * right.rho - mom_flux) + flux_coeff_2
                energy_flux = flux_coeff_1 * (S_R * (right.e_int * right.rho + right.e_kin) - energy_flux) + flux_coeff_2 * S_Star

                return rho_flux, mom_flux, energy_flux, mass_ratio_flux

    def evaluate_flux(self, left, right):
        """
        Public function for exavluation of flux between left and right states
        """
        p_estimate = self._estimate_p_star(left, right)

        S_L, S_R, S_Star = self.__evaluate_wave_speeds(left, right, p_estimate)
        # S_L, S_R, S_Star = self.__evaluate_davis_wave_speeds(left, right)
        # S_L, S_R, S_Star = self.__evaluate_einfeldt_wave_speeds(left, right)  # Currently buggy

        return self.__HLLC_flux(left, right, S_L, S_Star, S_R)


def test_iterative_scheme():
    """
    This function runs through the five shock tube problems outlined in Toro - Chapter 4. The results for contact
    velocity, pressure, and number of iterations should match those on p130-131.
    """
    gamma = 1.4
    p_left = [1.0, 0.4, 1000.0, 0.01, 460.894]
    rho_left = [1.0, 1.0, 1.0, 1.0, 5.99924]
    u_left = [0.0, -2.0, 0.0, 0.0, 19.5975]
    p_right = [0.1, 0.4, 0.01, 100.0, 46.0950]
    rho_right = [0.125, 1.0, 1.0, 1.0, 5.99242]
    u_right = [0.0, 2.0, 0.0, 0.0, -6.19633]

    solver = IterativeRiemannSolver(1.4)
    print('*' * 50)
    for i in range(0, 5):
        print("Riemann Test: " + str(i + 1))

        left_state = ThermodynamicState1D(p_left[i], rho_left[i], u_left[i], gamma)
        right_state = ThermodynamicState1D(p_right[i], rho_right[i], u_right[i], gamma)

        p_star, u_star = solver.get_star_states(left_state, right_state)

        print("Converged Star Pressure: " + str(p_star))
        print("Converged Star Velocity: " + str(u_star))
        print('*' * 50)


if __name__ == '__main__':
    test_iterative_scheme()