"""
Author: Rohan
Date: 18/03/17

This file contains flux limiter classes for producing high order TVD methods
"""

import numpy as np


class BaseLimiter(object):
    @staticmethod
    def _calculate_slope_ratios(left_slopes, right_slopes):
        """
        Function used to calculate to ratio of the different conservative variables
        """
        TOL = 1e-14

        slope_ratios = dict()
        slope_ratios["rho"] = 0.0 if np.isclose(right_slopes["rho"], 0.0, atol=TOL) else left_slopes["rho"] / right_slopes["rho"]
        slope_ratios["mom"] = 0.0 if np.isclose(right_slopes["mom"], 0.0, atol=TOL) else left_slopes["mom"] / right_slopes["mom"]
        slope_ratios["energy"] = 0.0 if np.isclose(right_slopes["energy"], 0.0, atol=TOL) else left_slopes["energy"] / right_slopes["energy"]

        return slope_ratios

    @staticmethod
    def _calculate_eta_values(r_params):
        """
        Function to calculate max allowable slope values for the left and right states - Toro 14.51
        """
        eta_params_L = dict()
        eta_params_R = dict()
        beta_R = 1.0
        beta_L = 1.0
        eta_params_L["rho"] = 2.0 * beta_L * r_params["rho"] / (1 + r_params["rho"])
        eta_params_L["mom"] = 2.0 * beta_L * r_params["mom"] / (1 + r_params["mom"])
        eta_params_L["energy"] = 2.0 * beta_L * r_params["energy"] / (1 + r_params["energy"])
        eta_params_R["rho"] = 2.0 * beta_R / (1 + r_params["rho"])
        eta_params_R["mom"] = 2.0 * beta_R / (1 + r_params["mom"])
        eta_params_R["energy"] = 2.0 * beta_R / (1 + r_params["energy"])

        return eta_params_L, eta_params_R

    @staticmethod
    def _calculate_limiting_factors(r_params, eta_params_L, eta_params_R):
        """
        Function to calculate the limiting factors for each conservative variable, to be implemented in sub-classes
        """
        raise NotImplementedError("Called from the base class!")


    @classmethod
    def calculate_limited_slopes(cls, left_slopes, right_slopes):
        """
        Function that performs an operation to bound the slopes within the TVD region
        """
        assert isinstance(left_slopes, dict)
        assert isinstance(right_slopes, dict)
        r_params = BaseLimiter._calculate_slope_ratios(left_slopes, right_slopes)

        eta_params_L, eta_params_R = BaseLimiter._calculate_eta_values(r_params)

        eta_density, eta_momentum, eta_energy = cls._calculate_limiting_factors(r_params, eta_params_L, eta_params_R)

        average_density_slope = 0.5 * (left_slopes["rho"] + right_slopes["rho"]) * eta_density
        average_momentum_slope = 0.5 * (left_slopes["mom"] + right_slopes["mom"]) * eta_momentum
        average_energy_slope = 0.5 * (left_slopes["energy"] + right_slopes["energy"]) * eta_energy

        return average_density_slope, average_momentum_slope, average_energy_slope


class MinBeeLimiter(BaseLimiter):
    def __init__(self):
        super(MinBeeLimiter, self).__init__()

    @staticmethod
    def _calculate_limiting_factors(r_params, eta_params_L, eta_params_R):
        """
        Toro 13.221
        """
        eta_density = min(1.0, eta_params_R["rho"]) if r_params["rho"] > 1.0 else r_params["rho"]
        eta_momentum = min(1.0, eta_params_R["mom"]) if r_params["mom"] > 1.0 else r_params["mom"]
        eta_energy = min(1.0, eta_params_R["energy"]) if r_params["energy"] > 1.0 else r_params["energy"]
        if r_params["rho"] <= 0.0 or r_params["mom"] <= 0.0 or r_params["energy"] <= 0.0:
            eta_density = 0.0
            eta_momentum = 0.0
            eta_energy = 0.0

        return eta_density, eta_momentum, eta_energy


class UltraBeeLimiter(BaseLimiter):
    def __init__(self):
        super(UltraBeeLimiter, self).__init__()

    @staticmethod
    def _calculate_limiting_factors(r_params, eta_params_L, eta_params_R):
        """
        Toro 13.217
        """
        if r_params["rho"] <= 0.0 or r_params["mom"] <= 0.0 or r_params["energy"] <= 0.0:
            eta_density = 0.0
            eta_momentum = 0.0
            eta_energy = 0.0
        else:
            eta_density = min(eta_params_L["rho"], eta_params_R["rho"])
            eta_momentum = min(eta_params_L["mom"], eta_params_R["mom"])
            eta_energy = min(eta_params_L["energy"], eta_params_R["energy"])

        return eta_density, eta_momentum, eta_energy


class SuperBeeLimiter(BaseLimiter):
    def __init__(self):
        super(SuperBeeLimiter, self).__init__()

    @staticmethod
    def _calculate_limiting_factors(r_params, eta_params_L, eta_params_R):
        """
        Toro 13.218
        """
        if r_params["rho"] <= 0.0 or r_params["mom"] <= 0.0 or r_params["energy"] <= 0.0:
            eta_density = 0.0
            eta_momentum = 0.0
            eta_energy = 0.0
        else:
            if r_params["rho"] <= 0.5:
                eta_density = 2 * r_params["rho"]
            else:
                eta_density = 1.0 if r_params["rho"] <= 1.0 else min(r_params["rho"], eta_params_R["rho"], 2.0)

            if r_params["mom"] <= 0.5:
                eta_momentum = 2 * r_params["mom"]
            else:
                eta_momentum = 1.0 if r_params["mom"] <= 1.0 else min(r_params["mom"], eta_params_R["mom"], 2.0)

            if r_params["energy"] <= 0.5:
                eta_energy = 2 * r_params["energy"]
            else:
                eta_energy = 1.0 if r_params["energy"] <= 1.0 else min(r_params["energy"], eta_params_R["energy"], 2.0)

        return eta_density, eta_momentum, eta_energy


