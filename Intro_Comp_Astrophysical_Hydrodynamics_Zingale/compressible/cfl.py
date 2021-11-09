#!/usr/bin/env python3

# a simple script to brute-force test stability of a multidimensional
# discretization scheme

import numpy as np


def stability(C, theta, phi):
    return 1.0 - 4.0*C + 6.0*C**2 + 2.0*(1.0 - 2.0*C)*C*(np.cos(theta) + np.cos(phi)) + 2.0*C**2 * np.cos(theta-phi)

def stabilityc(C, theta, phi):
    return (1.0 - C*(1.0 - np.exp(-1j * theta)) - C*(1.0 - np.exp(-1j*phi))) * \
           (1.0 - C*(1.0 - np.exp(+1j * theta)) - C*(1.0 - np.exp(+1j*phi))) 


# we setup a grid of phi and theta (the phases of the Fourier modes in
# the different coordinate directions) and for each proposed CFL
# number (C), we find the maximum magnitude of the amplitude
phi = np.linspace(0, np.pi, 181, endpoint=True)
theta = np.linspace(0, np.pi, 181, endpoint=True)

phi2d, theta2d = np.meshgrid(phi, theta, indexing="ij")

for C in np.linspace(0.1, 1.0, 37, endpoint=True):
    print(C, np.max(stabilityc(C, theta2d, phi2d)))

    
