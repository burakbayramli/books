"""
Author: Rohan
Date: 26/09/16

This file contains tests for an exact riemann solver
"""

import unittest

from CFD_Projects.compressible_hydro.flux_calculator.riemann_solver import IterativeRiemannSolver
from CFD_Projects.compressible_hydro.eos.thermodynamic_state import ThermodynamicState1D


class RiemannSolverTest(unittest.TestCase):
    def sod_test(self, gamma, left, right, exact_p_star, exact_u_star):
        assert left.gamma == right.gamma
        solver = IterativeRiemannSolver(gamma)

        p_star, u_star = solver.get_star_states(left, right)

        self.assertAlmostEqual(exact_p_star, p_star, 1)
        self.assertAlmostEqual(exact_u_star, u_star, 1)

    def test_sod_problems(self):

        gamma = 1.4
        p_left = [1.0, 0.4, 1000.0, 0.01, 460.894]
        rho_left = [1.0, 1.0, 1.0, 1.0, 5.99924]
        u_left = [0.0, -2.0, 0.0, 0.0, 19.5975]
        p_right = [0.1, 0.4, 0.01, 100.0, 46.0950]
        rho_right = [0.125, 1.0, 1.0, 1.0, 5.99242]
        u_right = [0.0, 2.0, 0.0, 0.0, -6.19633]

        exact_p_star = [0.30313, 0.00189, 460.894, 46.0950, 1691.64]
        exact_u_star = [0.92745, 0.0000, 19.5975, -6.19633, 8.68975]

        for i in range(0, 5):
            left = ThermodynamicState1D(p_left[i], rho_left[i], u_left[i], gamma)
            right = ThermodynamicState1D(p_right[i], rho_right[i], u_right[i], gamma)

            self.sod_test(gamma, left, right, exact_p_star[i], exact_u_star[i])


if __name__ == '__main__':
    unittest.main()