"""
Author: Rohan Ramasamy
Date: 29/01/17

A plotting tool to visualise hugoniot relations
"""

import numpy as np
from matplotlib import pyplot as plt

from compressible_hydro.eos.thermodynamic_state import ThermodynamicState1D


def hugoniot_curves(state, num_pts=100, p_multiplier=1000.0):
    """
    Plots the pressure velocity curve for potential shocks, given an initial thermodynamic state ahead of the shock
    """
    assert isinstance(state, ThermodynamicState1D)
    assert isinstance(p_multiplier, float)
    assert p_multiplier > 1.0, "The pressure behind the shock must be greater than the ahead state pressure"

    p_star = np.linspace(state.p, 1000 * state.p, num_pts)
    gamma_plus_one = state.gamma + 1
    gamma_minus_one = state.gamma - 1
    rho_star = (gamma_plus_one * p_star + gamma_minus_one * state.p) / (gamma_plus_one * state.p + gamma_minus_one * p_star) * state.rho

    W = np.sqrt((p_star - state.p) / (1 / state.rho - 1 / rho_star))
    u_star = state.u + (p_star - state.p) / W

    return p_star, u_star, rho_star


def rarefaction_curves(state, num_pts=100, p_multiplier=0.001):
    """
    Plots pressure velocity and pressure density curves for the possible rarefactions from an thermodynamic state
    ahead of the rarefaction
    """
    assert isinstance(state, ThermodynamicState1D)
    assert isinstance(p_multiplier, float)
    assert p_multiplier < 1.0, "The pressure behind the rarefaction must be less than the ahead state pressure"

    gamma = state.gamma
    p_star = np.linspace(state.p * p_multiplier, state.p, num_pts)
    rho_star = (state.rho ** gamma * p_star / state.p) ** (1 / gamma)

    f_l = 2 / (gamma - 1) * np.sqrt(gamma * state.p / state.rho)
    f_l_star = 2 / (gamma - 1) * np.sqrt(gamma * p_star / rho_star)
    u_star = state.u - f_l_star + f_l

    return p_star, u_star, rho_star


if __name__ == '__main__':
    state_1 = ThermodynamicState1D(1e5, 1.225, 0.0, 1.4)
    state_2 = ThermodynamicState1D(1e8, 1.225, 0.0, 1.4)

    p_shock, u_shock, rho_shock = hugoniot_curves(state_1)
    p_rar, u_rar, rho_rar = rarefaction_curves(state_2)

    fig, ax = plt.subplots(1, 2, figsize=(18, 10))

    ax[0].plot(rho_shock, p_shock, label="Shock")
    ax[0].plot(rho_rar, p_rar, label="Rarefaction")
    ax[0].legend()
    ax[0].set_title("Pressure against Density")

    ax[1].plot(u_shock, p_shock, label="Shock")
    ax[1].plot(u_rar, p_rar, label="Rarefaction")
    ax[1].legend()
    ax[1].set_title("Pressure against Velocity")

    fig.suptitle("Rarefaction Relations for an Ideal Gas Equation of State")
    plt.show()

