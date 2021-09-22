"""
Author: Rohan
Date: 29/06/16

This file contains a class used to generate the 1D analytic solution to the shock tube problems using a Riemann
solution. This problem type is not part of the simulation hierarchy because it is analytic.
"""

import numpy as np
from matplotlib import pyplot as plt

from CFD_Projects.compressible_hydro.eos.thermodynamic_state import ThermodynamicState1D
from CFD_Projects.compressible_hydro.flux_calculator.riemann_solver import IterativeRiemannSolver


class AnalyticShockTube(object):
    def __init__(self, left_state, right_state, membrane_location, num_pts):
        assert isinstance(left_state, ThermodynamicState1D)
        assert isinstance(right_state, ThermodynamicState1D)
        assert left_state.gamma == right_state.gamma

        self.left_state = left_state
        self.right_state = right_state
        self.solver = IterativeRiemannSolver()
        self.membrane_location = membrane_location
        self.x = np.linspace(0, 1, num_pts)
        self.rho = np.zeros(num_pts)
        self.u = np.zeros(num_pts)
        self.p = np.zeros(num_pts)
        self.e_int = np.zeros(num_pts)
        self.e_kin = np.zeros(num_pts)

    def get_solution(self, time, membrane_loc):
        """
        Function used to reconstruct solution to the Riemann problem at a particular point in time using the sample
        function
        """
        assert isinstance(time, float)
        assert isinstance(membrane_loc, float)

        p_star, u_star = self.solver.get_star_states(self.left_state, self.right_state)

        for i, x_pos in enumerate(self.x):
            x_over_t = (x_pos - membrane_loc) / time

            p, u, rho, is_left = self.solver.sample(x_over_t, self.left_state, self.right_state, p_star, u_star)

            gamma = self.left_state.gamma if is_left else self.right_state.gamma
            self.rho[i] = rho
            self.u[i] = u
            self.p[i] = p
            self.e_int[i] = p / (rho * (gamma - 1))
            self.e_kin[i] = 0.5 * u ** 2

        return self.x, self.rho, self.u, self.p, self.e_int, self.e_kin


def test_sod_problems():
    """
    This function runs through the five shock tube problems outlined in Toro - Chapter 4. The results for contact
    velocity, pressure, and number of iterations should match those on p130-131.
    """
    gamma = 1.4
    p_left = [1.0, 0.4, 1000.0, 0.01, 460.894, 1.0]
    rho_left = [1.0, 1.0, 1.0, 1.0, 5.99924, 1.0]
    u_left = [0.0, -2.0, 0.0, 0.0, 19.5975, 0.75]
    p_right = [0.1, 0.4, 0.01, 100.0, 46.0950, 0.1]
    rho_right = [0.125, 1.0, 1.0, 1.0, 5.99242, 0.125]
    u_right = [0.0, 2.0, 0.0, 0.0, -6.19633, 0.0]
    t = [0.25, 0.15, 0.012, 0.035, 0.035, 0.2]
    x = [0.5, 0.5, 0.5, 0.5, 0.5, 0.3]
    for i in range(0, 6):
        left_state = ThermodynamicState1D(p_left[i], rho_left[i], u_left[i], gamma)
        right_state = ThermodynamicState1D(p_right[i], rho_right[i], u_right[i], gamma)

        sod_test = AnalyticShockTube(left_state, right_state, x[i], 1000)

        x_sol, rho_sol, u_sol, p_sol, e_int_sol, _ = sod_test.get_solution(t[i], x[i])

        title = "Sod Test: {}".format(i + 1)
        num_plts_x = 2
        num_plts_y = 2
        plt.figure(figsize=(20, 10))
        plt.suptitle(title)
        plt.subplot(num_plts_x, num_plts_y, 1)
        plt.title("Density")
        plt.plot(x_sol, rho_sol)
        plt.subplot(num_plts_x, num_plts_y, 2)
        plt.title("Velocity")
        plt.plot(x_sol, u_sol)
        plt.subplot(num_plts_x, num_plts_y, 3)
        plt.title("Pressure")
        plt.plot(x_sol, p_sol)
        plt.subplot(num_plts_x, num_plts_y, 4)
        plt.title("Energy")
        plt.plot(x_sol, e_int_sol)
        plt.show()


if __name__ == '__main__':
    test_sod_problems()