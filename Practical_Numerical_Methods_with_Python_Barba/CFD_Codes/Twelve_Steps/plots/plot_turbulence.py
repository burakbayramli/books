"""
Author: Rohan Ramasamy
Date: 19/03/16
This file contains a script for plotting turbulence data
"""

# Standard Includes
import numpy as np
from matplotlib import pyplot as plt

# Project includes
from CFD_Projects.Twelve_Steps.models.turbulence_model import Turbulence


def plot_1d_velocities():
    """
    void function used to plot 1D velocities of a single point to screen
    """
    t = np.linspace(0, 5, 1000)
    velocities = Turbulence.generate_1d_velocities(t)
    # Factor in mean speed
    velocities += 12
    mean = np.mean(velocities)

    plt.figure(figsize=(14, 8))
    plt.plot(t, velocities)
    plt.axhline(mean)
    plt.ylabel("Velocity (m/s)")
    plt.xlabel("Time (s)")
    plt.title("Plot of Turbulence")
    plt.show()


def plot_2d_velocities():
    """
    void function used to plot 2D velocities at a single point
    """
    t = np.linspace(0, 5, 300)
    velocities = Turbulence.generate_2d_velocities(t)

    plt.figure(figsize=(14, 8))
    for i in range(3):
        plt.plot(t, velocities[i, :])
    plt.ylabel("Velocity (m/s)")
    plt.xlabel("Time (s)")
    plt.title("Plot of Turbulence")
    plt.show()


def plot_1d_turbulence():
    """
    void function used to plot 1d turbulence at different x-locations
    """
    t = np.linspace(0, 5, 100)
    x = np.linspace(0, 5, 5)
    print x
    velocities = Turbulence.generate_1d_turbulence(t, x.shape[0])

    # Plot different x locations velocities over time
    plt.figure(figsize=(14, 8))
    for i in range(velocities.shape[0]):
        plt.plot(t, velocities[i, :], label=str(i))
    plt.legend()
    plt.ylabel("Velocity (m/s)")
    plt.xlabel("Time (s)")
    plt.title("Plot of 1D Turbulence against Time")
    plt.show()

    # Plot different x locations velocities over first 3 time steps
    plt.figure(figsize=(14, 8))
    for i in range(3):
        plt.plot(x, velocities[:, i], label=str(i))
    plt.legend()
    plt.ylabel("Velocity (m/s)")
    plt.xlabel("Position (m)")
    plt.title("Plot of 1D Turbulence against Location")
    plt.show()


def plot_2d_turbulence():
    """
    void function used to plot 2d velocities at different x and y locations
    """
    t = np.linspace(0, 3, 3)
    x = np.linspace(0, 10, 10)
    y = np.linspace(0, 10, 10)

    velocities = Turbulence.generate_2d_turbulence(t, x.shape[0], y.shape[0])

    for ts in range(t.shape[0]):
        fig = plt.figure(figsize=(14, 8))
        ax = fig.add_subplot(111)
        ax.set_title("Turbulent Velocity Colour Map at Time Step: " + str(ts))
        plt.imshow(velocities[:, :, 2, ts], interpolation='nearest')
        ax.set_aspect('equal')
        plt.colorbar()
        plt.show()

if __name__ == '__main__':
    # plot_1d_velocities()
    # plot_2d_velocities()
    # plot_1d_turbulence()
    plot_2d_turbulence()