"""
Author: Rohan Ramasamy
Date: 19/03/16
This file contains a class to simulate turbulence in one, two and three dimensions
"""
# Standard imports
import numpy as np


class Turbulence(object):
    def __init__(self):
        pass

    @staticmethod
    def generate_1d_velocities(t):
        """
        This function is used to generate data for the turbulence model in the form of white noise
        :param t: a 1D array that represents time
        :return: an array representing the variation in time of a point in space
        """
        mu = 0
        sigma = 1
        v_rand = np.random.normal(mu, sigma, t.shape[0])

        return v_rand

    @staticmethod
    def generate_2d_velocities(t):
        """
        This function is used to generate data for the turbulence model in the form of white noise
        :param t: a 1D array that represents time
        :return: a 3D array that represents the two scalar components of velocity at a given time
                and the corresponding absolute velocity
        """
        v_rand = np.zeros((3, t.shape[0]))
        for i in range(2):
            v_rand[i, :] = Turbulence.generate_1d_velocities(t)
        v_rand[2, :] = np.sqrt(v_rand[0, :] ** 2 + v_rand[1, :] ** 2)

        return v_rand

    @staticmethod
    def generate_1d_turbulence(t, x_domain):
        """
        This function is used to generate 1D spacial data for turbulence over a given number of
        time samples
        :param t: a 1D array that represents time
        :param x_domain: the size of the x domain generating the 1D data
        :return: a 2D array containing locations and velocities over time
        """

        v_turb = np.zeros((x_domain, t.shape[0]))
        for i in range(x_domain):
            v_turb[i, :] = Turbulence.generate_1d_velocities(t)
        return v_turb

    @staticmethod
    def generate_2d_turbulence(t, x_domain, y_domain):
        """
        This function is used to generate 2D spacial data for turbulence over as given number of
        time samples
        :param t: a 1D array that represents time
        :param x_domain: the size of the domain in the x direction
        :param y_domain: the size of the domain in the y direction
        :return: a 4D array containing the locations, scalar and absolute components of
                velocities over time
        """
        pass
        v_turb = np.zeros((x_domain, y_domain, 3, t.shape[0]))
        for i in range(x_domain):
            for j in range(y_domain):
                v_turb[i, j, :, :] = Turbulence.generate_2d_velocities(t)
        return v_turb
