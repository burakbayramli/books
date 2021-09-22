"""
Author: Rohan
Date: 19/12/16

This file contains a simple model to consider ideal turbines with annular wake rotation
"""

import unittest

from wind_theory.pysrc.applets.ideal_wake_rotation_model import get_induction_factor
from wind_theory.pysrc.applets.ideal_wake_rotation_model import calculate_C_p


class RiemannSolverTest(unittest.TestCase):
    def test_induction_factor(self):
        """
        Values for this test are taken from Wind Energy Explained by J.F. Manwell: p94
        """
        tip_speed_ratios = [0.5, 1.0, 1.5, 2.0, 2.5, 5.0, 7.5, 10.0]
        a_exact = [0.2983, 0.3170, 0.3245, 0.3279, 0.3297, 0.3324, 0.3329, 0.3330]

        for i, tsr in enumerate(tip_speed_ratios):
            a_approx = get_induction_factor(tsr)

            self.assertAlmostEqual(a_exact[i], a_approx, 3)

    def test_calculate_power_coefficients(self):
        """
        Values for this test are taken from Wind Energy Explained by J.F. Manwell: p94
        """
        tip_speed_ratios = [0.5, 1.0, 1.5, 2.0, 2.5, 5.0, 7.5, 10.0]
        cp_exact = [0.289, 0.416, 0.477, 0.511, 0.533, 0.570, 0.581, 0.585]

        for i, tsr in enumerate(tip_speed_ratios):
            cp_approx = calculate_C_p(tsr)

            self.assertAlmostEqual(cp_exact[i], cp_approx, 2)


if __name__ == '__main__':
    unittest.main()