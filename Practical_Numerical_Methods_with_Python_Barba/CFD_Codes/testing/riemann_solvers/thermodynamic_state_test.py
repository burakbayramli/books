"""
Author: Rohan
Date: 26/09/16

This file contains tests for thermodynamic state
"""

import unittest

from CFD_Projects.compressible_hydro.eos.thermodynamic_state import ThermodynamicState1D


class TestThermodynamicState(unittest.TestCase):

    def test_sound_speed(self):
        pressure = 1e5
        rho = 1.225
        gamma = 1.4

        air_state = ThermodynamicState1D(pressure, rho, 0.0, gamma)

        self.assertTrue(333 < air_state.sound_speed() < 340)


if __name__ == '__main__':
    unittest.main()