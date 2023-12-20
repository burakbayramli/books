#
# Copyright (c) 2019-2023 steelpy
#
# Python stdlib imports
from __future__ import annotations
#from array import array
#from collections.abc import Mapping
from dataclasses import dataclass
from math import factorial
from typing import NamedTuple
#import re
#
# package imports
from .table2B import BeamLoad
from .table2C import BeamBendingSupports
#
#
#
# ---------------------------------------------------------------
#
#
#
#
#
# ---------------------------------------------------------------
#    Pilkey 2nd ed
#    TABLE 11-2
#    PART A: SIMPLE BEAMS WITH ARBITRARY LOADINGS: 
#    GENERAL RESPONSE EXPRESSIONS
# ---------------------------------------------------------------
#
#
#
@dataclass
class BendingGE:
    """
    Pilkey 2nd ed
    TABLE 11-2
    PART A: SIMPLE BEAMS WITH ARBITRARY LOADINGS: GENERAL RESPONSE EXPRESSIONS
    """

    __slots__ = ['FV', 'FM', 'Ftheta', 'Fw',
                 'V0', 'M0', 'theta0', 'w0',
                 'E', 'I']

    def __init__(self, E: float, I: float) -> None:
        """
        """
        self.E = E
        self.I = I
    #
    def load(self, FV: float, FM: float,
             Ftheta: float, Fw: float) -> None:
        """
        Load @ x
        """
        self.FV = FV
        self.FM = FM
        self.Ftheta = Ftheta
        self.Fw = Fw

    #
    def R0(self, V0: float, M0: float,
           theta0: float, w0: float) -> None:
        """ 
        Initial Parameters
        """
        self.V0 = V0
        self.M0 = M0
        self.theta0 = theta0
        self.w0 = w0

    #
    def V(self, x: float) -> float:
        """ Shear force"""
        return self.V0 + self.FV

    #
    def M(self, x: float) -> float:
        """ Bending moment"""
        return self.M0 + self.V0 * x + self.FM

    #
    def theta(self, x: float,  I: float) -> float:
        """ Slope"""
        return (self.theta0 + self.V0 * x**2 / (2 * self.E * I)
                + self.M0 * x / (self.E * I) + self.Ftheta)

    #
    def w(self, x: float, I: float) -> float:
        """ Deflection"""
        return (self.w0 - self.theta0 * x
                - self.V0 * x**3 / (factorial(3) * self.E * I)
                - self.M0 * x**2 / (2 * self.E * I) + self.Fw)
    #
    def response(self, x:float) -> list[float]:
        """
        x : distance from end 1

        results:
        [V, M, theta, w]
        """ 
        return [self.V(x), self.M(x),
                self.theta(x, I=self.I),
                self.w(x, I=self.I)]
#
#
