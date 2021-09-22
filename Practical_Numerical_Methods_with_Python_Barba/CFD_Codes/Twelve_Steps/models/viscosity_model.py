"""
Author: Rohan
Date: 19/03/16
"""


class Viscosity(object):
    def __init__(self):
        pass

    @staticmethod
    def sutherland_viscosity(T, T_0=297.15, C=120, mu_0=18.27):
        """
        Implements the Sutherland Equation for the dynamic viscosity

        :param T: Temperatures being interpolated
        :param T_0: Reference temperatures
        :param C: Empirical coefficient (default is for air)
        :param mu_0: Empirical coefficient (default is for viscosity)
        :return: The Dynamic viscosity mu
        """

        mu = mu_0 * (T_0 + C) / (T + C) * (T / T_0) ** (3.0/2.0)
        return mu