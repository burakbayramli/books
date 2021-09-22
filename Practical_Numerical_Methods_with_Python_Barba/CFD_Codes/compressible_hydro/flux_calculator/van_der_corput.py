"""
Author: Rohan
Date: 01/12/16

This file contains a class to generate a van der corput random number sequence
"""

import numpy as np
import math


class VanDerCorput(object):
    def __init__(self):
        pass

    @staticmethod
    def get_coefficients(n, k_1):
        """
        Generate the coefficients of number n in the sequence
        :param n: the number of the coefficient in the sequence
        :param k_1: coefficient of the van der corput sequence
        :return: array of coefficients a
        """
        assert isinstance(n, int)
        assert isinstance(k_1, int)

        m = np.floor(math.log(n, k_1))

        a = np.zeros(m + 1)
        i = m
        remainder = n
        while i >= 0:
            a[i] = int(remainder / (k_1 ** i))
            remainder = np.mod(remainder, k_1 ** i)
            i -= 1

        return a

    @staticmethod
    def alter_coefficients(a, k_1, k_2):
        """
        Convert coefficients a to A
        """
        assert isinstance(k_1, int)
        assert isinstance(k_2, int)

        i = 0
        while i < a.shape[0]:
            a[i] = np.mod(k_2 * a[i], k_1)
            i += 1
        return a

    @staticmethod
    def calculate_theta(n, k_1, k_2):
        """
        See Toro Chapter 6 for full explanation of pseudo random number generator
        :param n: sequence of number
        :param k_1: coefficient of algorithm
        :param k_2: coefficient of algorithm
        :return: random number between 0 and 1
        """
        assert isinstance(n, int)
        assert isinstance(k_1, int)
        assert isinstance(k_2, int)

        m = np.floor(math.log(n, k_1))

        # Generate coefficients
        a = np.zeros(m + 1)
        i = m
        remainder = n
        theta = 0
        while i >= 0:
            a[i] = int(remainder / (k_1 ** i))
            a[i] = np.mod(k_2 * a[i], k_1)
            remainder = np.mod(remainder, k_1 ** i)
            theta += a[i] * k_1 ** (-(i + 1))
            i -= 1

        return theta


def example():
    for i in range(1, 8):
        theta = VanDerCorput.calculate_theta(i, 2, 1)
        print(theta)

if __name__ == '__main__':
    example()