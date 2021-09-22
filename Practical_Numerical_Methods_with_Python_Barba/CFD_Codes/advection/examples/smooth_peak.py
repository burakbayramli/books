"""
Author: Rohan
Date: 25/06/17

This file contains a linear advection example of a sinusoidal profile
"""

import numpy as np
from matplotlib import pyplot as plt

from CFD_Projects.advection.advection_sim import LinearAdvectionSim
from CFD_Projects.advection.flux_calculator import LaxFriedrichsFluxCalculator, LaxWendroffFluxCalculator


def example():
    num_pts = 100
    x = np.linspace(-1, 2, num_pts)
    u = np.exp(-8 * x ** 2)
    x_analytic = np.linspace(0.0, 2.0, num_pts)
    u_analytic = np.exp(-8 * (x_analytic - 1.0) ** 2)

    sim = LinearAdvectionSim(x, u, LaxFriedrichsFluxCalculator, 1.0, a=1.0)
    # sim = LinearAdvectionSim(x, u, LaxWendroffFluxCalculator, 1.0, a=1.0)
    x_res, u_res = sim.run_simulation()

    plt.figure()
    plt.plot(x_analytic, u_analytic, label="Analytic")
    plt.plot(x_res, u_res)
    plt.scatter(x_res, u_res, label="Final")
    plt.xlim([0.0, 2.0])
    plt.legend()
    plt.show()


if __name__ == '__main__':
    example()