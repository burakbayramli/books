"""
Author: Rohan Ramasamy
Date: 19/03/16
This file contains a class to simulate turbulence in one, two and three dimensions
"""
# Standard imports
import numpy as np


class EOS(object):
    def __init__(self):
        pass

    @staticmethod
    def ideal_EOS(T, v, M=287.0):
        """
        Function to return the Ideal Gas Equation of State, given by:

                    P = RT / v
        :param T: temperature of the gas
        :param v: mass specific volume of the gas
        :param M: molar mass of the gas
        :return: pressure of the gas
        """
        R = 8.315 / M

        return R * T / v

    @staticmethod
    def redlich_kwong_EOS(T, V_m, T_c=150.7, P_c=4870):
        """
        Function to return the Redlich-Kwong Equation of state, given by:

                    P = RT/(V_m - b) - a/(T^(1/2) * V_m * (V_m + b))
        :param T: the temperature of the gas
        :param V_m: the molar volume of the gas
        :param T_c: the critical temperature of the gas (default for Argon)
        :param P_c: the critical pressure of the gas (default for Argon)
        :return: the pressure of the gas
        """
        R = 8.315
        a = 0.4275 * (R ** 2) * (T_c ** (5.0/2.0)) / P_c
        b = 0.08664 * R * T_c / P_c

        P = R * T / (V_m - b) - a / (np.sqrt(T) * V_m * (V_m + b))

        if (P.any() / P_c) < (T / (2 * T_c)):
            return P
        else:
            raise RuntimeError("Invalid for given result for given conditions")

