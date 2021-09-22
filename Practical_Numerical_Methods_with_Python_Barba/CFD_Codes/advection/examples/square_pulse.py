"""
Author: Rohan
Date: 25/06/17

This file contains a linear advection example of a square pulse profile
"""

import numpy as np
from matplotlib import pyplot as plt

from CFD_Projects.advection.advection_sim import LinearAdvectionSim
from CFD_Projects.advection.flux_calculator import LaxFriedrichsFluxCalculator, LaxWendroffFluxCalculator


def example():
    num_pts = 100
    x = np.linspace(0.0, 2.0, num_pts)
    u = np.zeros(num_pts)
    for i, x_loc in enumerate(x):
        if x_loc <= 0.3:
            u[i] = 0.0
        elif x_loc <= 0.7:
            u[i] = 1.0
        else:
            u[i] = 0.0
    x_init = np.copy(x)
    u_init = np.copy(u)

    sim = LinearAdvectionSim(x, u, LaxFriedrichsFluxCalculator, 1.0, a=1.0)
    # sim = LinearAdvectionSim(x, u, LaxWendroffFluxCalculator, 1.0, a=1.0)
    x_res, u_res = sim.run_simulation()

    plt.figure()
    plt.plot(x_init, u_init, label='Initial')
    plt.plot(x_res, u_res)
    plt.scatter(x_res, u_res, label='Final')
    plt.xlim([0.0, 2.0])
    plt.show()


if __name__ == '__main__':
    example()