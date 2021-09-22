"""
Author: Rohan Ramasamy
Date: 09/04/16
This file contains a class to plot data
"""

from matplotlib import cm
from matplotlib import pyplot as plt
import numpy as np


class Plot(object):
    def __init__(self):
        pass

    @staticmethod
    def plot2d(x, y, u, name):
        """
        Function to plot 2D data on a contour plot

        :param x: x locations of plotted points
        :param y: y locations of plotted points
        :param u: State variable to be plotted as contour
        :param name: A string containing the name of the file
        """
        fig = plt.figure(figsize=(13, 10), dpi=100)
        ax = fig.gca(projection = '3d')
        X, Y = np.meshgrid(x, y)
        surf = ax.plot_surface(X, Y, u[:], rstride=1, cstride=1, cmap=cm.coolwarm,
                               linewidth=0, antialiased=False)
        fig.savefig(name)

    @staticmethod
    def plot2d_vector(x, y, u, v, name):
        """
        Function to plot 2D data on a contour plot with vectors.

        :param x: x locations of plotted points
        :param y: y locations of plotted points
        :param u: Vector variable name for x direction
        :param v: Vector variable name for y direction
        :param name: A string containing the name of the file
        """
        X, Y = np.meshgrid(x, y)

        fig = plt.figure(figsize=(13,10), dpi=100)
        plt.quiver(X[::2, ::2], Y[::2, ::2], u[::2, ::2], v[::2, ::2])
        plt.xlabel('X')
        plt.ylabel('Y')
        fig.savefig(name)

    @staticmethod
    def plot2d_vector_contour(x, y, p, u, v, name):
        """
        Function to plot 2D data on a contour plot with vectors.

        :param x: x locations of plotted points
        :param y: y locations of plotted points
        :param p: State variable to be plotted as contour
        :param u: Vector variable name for x direction
        :param v: Vector variable name for y direction
        :param name: A string containing the name of the file
        """
        X, Y = np.meshgrid(x, y)

        fig = plt.figure(figsize=(13,10), dpi=100)
        plt.contourf(X, Y, p, alpha=0.5)
        plt.colorbar()
        plt.contour(X, Y, p)
        plt.quiver(X[::2, ::2], Y[::2, ::2], u[::2, ::2], v[::2, ::2])
        plt.xlabel('X')
        plt.ylabel('Y')
        fig.savefig(name)
