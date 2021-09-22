"""
Author: Rohan
Date: 19/03/16
"""
# Standard includes
from matplotlib import pyplot as plt
import numpy as np

# Project includes
from CFD_Projects.Twelve_Steps.models.viscosity_model import Viscosity


def plot_air_sutherland_viscosity():
    temperature = np.linspace(291.15, 1291.15, 1000)

    mu = Viscosity.sutherland_viscosity(temperature)

    plt.figure(figsize=(10, 8))
    plt.plot(temperature, mu)
    plt.xlabel("Temperature (K)")
    plt.ylabel("Viscosity (Pas)")
    plt.title("Dynamic Viscosity vs Temperature")
    plt.show()


if __name__ == '__main__':
    plot_air_sutherland_viscosity()