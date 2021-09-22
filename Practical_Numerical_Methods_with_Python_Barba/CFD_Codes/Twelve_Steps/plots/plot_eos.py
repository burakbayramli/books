"""
Author: Rohan Ramasamy
Date: 19/03/16
This file contains a class to simulate turbulence in one, two and three dimensions
"""
# Standard imports
import numpy as np
from matplotlib import pyplot as plt

# Project imports
from CFD_Projects.Twelve_Steps.models.equation_of_state_model import EOS


def plot_redlich_kwong_ideal_gas_comparison():
    """
    This function is used to plot the Redlich Kwong equation for Argon (the default gas)
    """
    T = 273
    molar_mass = 40.0
    V_m = np.linspace(50, 600, 1000)
    V = V_m / molar_mass

    P_redlich = EOS.redlich_kwong_EOS(T, V_m)
    P_ideal = EOS.ideal_EOS(T, V, M=molar_mass)

    plt.figure(figsize=(10, 8))
    plt.plot(V_m, P_redlich, label="Redlich")
    plt.plot(V_m, P_ideal, label="Ideal")
    plt.title("Pressure vs. Molar Volume")
    plt.ylabel("Pressure")
    plt.xlabel("Volume")
    plt.legend()
    plt.show()


if __name__ == '__main__':
    plot_redlich_kwong_ideal_gas_comparison()