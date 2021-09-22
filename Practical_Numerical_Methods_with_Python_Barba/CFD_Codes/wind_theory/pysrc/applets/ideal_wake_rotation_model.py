"""
Author: Rohan
Date: 19/12/16

This file contains a simple model to consider ideal turbines with annular wake rotation
"""

import numpy as np
from matplotlib import pyplot as plt


def get_induction_factor(lambda_r):
    """
    This function calculates the maximum induction factor of the turbine from the tip speed ratio
    :param lambda_r: Ratio of blade velocity to incoming wind velocity: wr/U
    :return:
    """
    lambda_r_squared = lambda_r ** 2
    tip_speed_squared = lambda a: (1 - a) * (1 - 4 * a) ** 2 / (1 - 3 * a)
    TOL = 1e-8

    # Physical values are constrained between 0.25 and 0.333
    a_min = 0.25
    a_max = 1.0 / 3.0
    max_num_iter = 1000
    iter_num = 0
    while iter_num < max_num_iter:
        a_mid = (a_max + a_min) / 2.0

        lambda_guess = tip_speed_squared(a_mid)
        if np.abs(lambda_guess - lambda_r_squared) < TOL:
            return a_mid

        if lambda_guess < lambda_r_squared:
            a_min = a_mid
        else:
            a_max = a_mid

        iter_num += 1
    raise RuntimeError("Induction Factor failed to converge!")


def calculate_C_p(tip_speed_ratio):
    """
    This function calculates the power coefficient for a given tip speed ratio
    :param tip_speed_ratio: Ratio of blade velocity to incoming wind velocity at blade tip: wR/U
    :return:
    """
    a_min = get_induction_factor(0.0)
    a_max = get_induction_factor(tip_speed_ratio)

    # Calculate integral
    integral = lambda a: ((1 - a) * (1 - 2 * a) * (1 - 4 * a) / (1 - 3 * a)) ** 2
    a = np.linspace(a_min, a_max, 100000)
    da = a[1] - a[0]
    dCp = integral(a) * da

    Cp = np.sum(dCp) * 24.0 / tip_speed_ratio ** 2
    return Cp


def example():
    num_pts = 100
    tip_speed_ratios = np.linspace(0.01, 10.0, num_pts)
    Cp = np.zeros(num_pts)

    for i, tsr in enumerate(tip_speed_ratios):
        Cp[i] = calculate_C_p(tsr)

    plt.figure()
    plt.plot(tip_speed_ratios, Cp)
    plt.ylabel("Power Coefficient (Cp)")
    plt.xlabel("Tip Speed Ratio")
    plt.axhline(16.0 / 27.0, color='k')
    plt.show()


if __name__ == '__main__':
    example()