"""
Author: Rohan
Date: 04/12/16

This file contains the implementation of boundary conditions for a Riemann solver based solution to a CFD problem
"""

from CFD_Projects.compressible_hydro.eos.thermodynamic_state import ThermodynamicState1D
from CFD_Projects.compressible_hydro.eos.thermodynamic_state import ThermodynamicState2D


class BoundaryConditionND(object):
    X_LOW = 0
    X_HIGH = 1
    Y_LOW = 2
    Y_HIGH = 3


class BoundaryCondition1D(BoundaryConditionND):
    @staticmethod
    def transmissive_boundary_condition(state):
        assert isinstance(state, ThermodynamicState1D)

        return state

    @staticmethod
    def reflecting_boundary_condition(state):
        assert isinstance(state, ThermodynamicState1D)

        return ThermodynamicState1D(state.p, state.rho, -state.u, state.gamma, state.mass_ratios)


class BoundaryCondition2D(BoundaryConditionND):
    @staticmethod
    def transmissive_boundary_condition(state, boundary):
        assert isinstance(state, ThermodynamicState2D)
        assert isinstance(boundary, int)

        return state

    @staticmethod
    def reflecting_boundary_condition(state, boundary):
        assert isinstance(state, ThermodynamicState2D)
        assert isinstance(boundary, int)

        if boundary == BoundaryCondition2D.X_LOW or boundary == BoundaryCondition2D.X_HIGH:
            return ThermodynamicState2D(state.p, state.rho, -state.u, state.v, state.gamma)
        elif boundary == BoundaryCondition2D.Y_LOW or boundary == BoundaryCondition2D.Y_HIGH:
            return ThermodynamicState2D(state.p, state.rho, state.u, -state.v, state.gamma)
        else:
            raise NotImplementedError("Boundary does not exist in 2D sim!")