"""
Author: Rohan
Date: 03/12/16

This file contains the base simulation class for running problems with Riemann solvers.
"""


class BaseSimulationND(object):
    def __init__(self):
        self.x = 0
        self.densities = 0
        self.pressures = 0
        self.vel_x = 0
        self.internal_energies = 0
        self.dx = 0
        self.gamma = 0

        self.final_time = 0
        self.CFL = 0

        self.number_of_fluids = 0
        self.molar_masses = 0
        self.mass_ratios = 0
        self.specific_heats = 0

        self.flux_calculator = 0
        self.boundary_functions = 0

        self.is_initialised = False


class BaseSimulation1D(BaseSimulationND):
    def __init__(self):
        super(BaseSimulation1D, self).__init__()


class BaseSimulation2D(BaseSimulationND):
    def __init__(self):
        self.y = 0
        self.vel_y = 0.0
        self.dy = 0.0

        super(BaseSimulation2D, self).__init__()