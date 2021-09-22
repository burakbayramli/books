"""
Author: Rohan
Date: 25/06/17

This file contains a linear advection example of a sinusoidal profile
"""

import numpy as np
from matplotlib import pyplot as plt

from CFD_Projects.advection.advection_sim import LinearAdvectionSim
from CFD_Projects.advection.flux_calculator import LaxFriedrichsFluxCalculator, LaxWendroffFluxCalculator


def example(a=None):
    num_pts = 100
    x = np.linspace(0, 2.0, num_pts)
    u = np.sin(np.pi * x)
    x_init = np.copy(x)
    u_init = np.copy(u)

    sim = LinearAdvectionSim(x, u, LaxFriedrichsFluxCalculator, 1.0, a=a)
    # For LaxWendroff, If final_time > 0.4, system becomes unstable at discontinuity
    # sim = LinearAdvectionSim(x, u, LaxWendroffFluxCalculator, 0.3, a=a)
    x_res, u_res = sim.run_simulation()

    plt.figure()
    plt.plot(x_init, u_init, label='Initial')
    plt.plot(x_res, u_res)
    plt.scatter(x_res, u_res, label='Final')
    plt.xlim([0.0, 2.0])
    plt.legend()
    plt.show()


if __name__ == '__main__':
    example()     # Linear
    example(1.0)  # Non-Linear

