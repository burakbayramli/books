# 
# Copyright (c) 2019-2023 steelpy
# 

# Python stdlib imports
from __future__ import annotations
from bisect import bisect_right
from dataclasses import dataclass
from math import factorial, cosh, sinh
import re

# package imports
from steelpy.process.math.vector import Vector
from .table3 import TableBasic

#
#
# ---------------------------------------------------------------
# Pilkey 2nd ed
# TABLE 11-3
# PART B: BEAMS WITH AXIAL FORCES AND ELASTIC FOUNDATIONS: 
# LOADING FUNCTIONS
# ---------------------------------------------------------------
#
#
@dataclass
class ArbitraryLoading(TableBasic):
    __slots__ = ['L', 'L1']

    def __init__(self, L: float, L1: float) -> None:
        """
        L  : length of beam
        L1 : Distance to load from the left end
        """
        self.L: float = L
        self.L1: float = L1

    #
    #
    def V(self, x: float) -> float:
        """ Shear Force"""
        return 0

    #
    def M(self, x: float) -> float:
        """ Bending Moment"""
        return 0

    #
    def T(self, x: float) -> float:
        """ Torque"""
        return 0

    #
    def theta(self, x: float, E: float, G: float, I: float) -> float:
        """ Slope = EIy' """
        return 0

    #
    def w(self, x: float, E: float, G: float, I: float) -> float:
        """ Deflection = EIy"""
        return 0

    #
    def function_n(self, step: float, n: int) -> float:
        """ <x-L>^n """
        if n < 0:
            return 0
        elif step < 0:
            return 0
        elif n == 0:
            return 1
        else:
            return step ** n

    #
    def max_steps(self):
        """ """
        return [self.L1]
    

    #
    def __call__(self, x: float, E: float, G: float, I: float):
        """ 
        Formulas positive (+) is downwards and therefore sign is changed to maintain compatibility
        return: [V, M, theta, w]
        """
        #
        return [self.V(x),
                self.M(x),
                self.theta(x, E, G, I),
                self.w(x, E, G, I)]
#
#
#
@dataclass
class Trapezoidal(ArbitraryLoading):
    __slots__ = ['q1', 'q2', 'L', 'L1', 'L2',
                 'L3', 'slope', 'P']

    def __init__(self, q1: float, q2: float,
                 L: float, a1: float, a2: float,
                 P: float = 0) -> None:
        """
        q1 : line load end 1
        q2 : line load end 2
        a1  : Distant to q1 from the left end
        a2  : Distant to q1 from the rigth end
        L  : Length of beam
        P  : axial force (zero by befault)
        """
        super().__init__(L=L, L1=a1)
        self.q1 = q1
        self.q2 = q2
        self.L2 = a2
        self.P = P
        #
        self.L3 = self.L - self.L2
        self.slope = (self.q2 - self.q1) / (self.L3 - self.L1)

    #
    #
    def V(self, x: float, I: float) -> float:
        """ Shear Force"""
        step1 = x - self.L1
        step2 = x - self._L3
        ef = self.ei(x=x, I=I, k=0)
        #
        func1 = (-1 * self.slope
                 * (ef.e3 * self.function_n(step1, 1) + ef.Zeta * ef.e5 * self.function_n(step1, 1)
                    - ef.e3 * self.function_n(step2, 1) - ef.Zeta * ef.e5 * self.function_n(step2, 1)))
        
        func2 = (self.q2 * (ef.e2 * self.function_n(step2, 1) + ef.Zeta * ef.e4 * self.function_n(step2, 1))
                 - self.q1 * (ef.e2 * self.function_n(step1, 1) - ef.Zeta * ef.e4 * self.function_n(step1, 1)
                              + 2 * ef.Zeta * ef.e4 * self.function_n(step2, 1)))
        
        return func1 + func2

    #
    def M(self, x: float, I: float) -> float:
        """ Bending Moment"""
        step1 = x - self.L1
        step2 = x - self._L3
        ef = self.ei(x=x, I=I, k=0)
        #
        func1 = (-1 * self.slope *
                 (ef.e4 * self.function_n(step1, 1) - ef.e4 * self.function_n(step2, 1)))
        func2 = (self.q2 * ef.e3 * self.function_n(step2, 1) -
                 self.q1 * ef.e3 * self.function_n(step1, 1))
        return func1 + func2

    #
    def theta(self, x: float, E: float, G: float, I: float) -> float:
        """ Slope = EIy' """
        EI = E * I
        step1 = x - self.L1
        step2 = x - self.L3
        ef = self.ei(x=x, I=I, k=0)
        #
        func1 = (-1 * self.slope
                 * (ef.e5 * self.function_n(step1, 1) - ef.e5 * self.function_n(step2, 1)) / EI)
        func2 = (self.q2 * ef.e4 * self.function_n(step2, 1) - self.q1 * ef.e4 * self.function_n(step1, 1))
        return func1 + func2

    #
    def w(self, x: float, E: float, G: float, I: float) -> float:
        """ Deflection = EIy"""
        EI = E * I
        step1 = x - self.L1
        step2 = x - self.L3
        ef = self.ei(x=x, I=I, k=0)
        #
        func1 = (self.slope
                 * (ef.e6 * self.function_n(step1, 1) - ef.e6 * self.function_n(step2, 1)) / EI
                 - (ef.e4 * self.function_n(step1, 1)
                    + ef.Zeta * ef.e6 * self.function_n(step1, 1)
                    - ef.e4 * self.function_n(step2, 1)
                    - ef.Zeta * ef.e6 * self.function_n(step2, 1)) / (G * As))
        #
        func2 = ((self.q1 * ef.e5 * self.function_n(step1, 1) + self.q2 * ef.e5 * self.function_n(step2, 1)) / EI
                 - (self.q1 * (ef.e3 * self.function_n(step1, 1) + ef.Zeta * ef.e5 * self.function_n(step1, 1))
                    -self.q2 * (ef.e3 * self.function_n(step2, 1) + ef.Zeta * ef.e5 * self.function_n(step2, 1))) / (G * As))
        
        return func1 + func2

    #
    def max_steps(self):
        """ """
        wl = self.L - self.L1 - self.L2
        try:
            1 / self.q1  # end 1
            try:
                1 / self.q2  # end 2
                if self.q1 == self.q2:  # uniform
                    a = self.L1 + wl / 2.0
                    b = self.L2 + wl / 2.0
                    maxM = (a + wl * (b - a) / (2 * self.L)) / self.L
                else:  # trapezoidal
                    qrad = [0.2, 0.4, 0.6, 0.8, 1.0]
                    xrad = [0.555, 0.536, 0.520, 0.508, 0.50]
                    interp = Interpolate(qrad, xrad)
                    #
                    if abs(self.q1) <= abs(self.q2):
                        rad = interp(self.q1 / self.q2)
                    else:
                        rad = interp(self.q2 / self.q1)
                        rad = 1 - rad
                    maxM = (self.L1 / self.L) + rad
            except ZeroDivisionError:  # triangular
                maxM = (self.L1 / self.L) + (1 - 0.5774)
        except ZeroDivisionError:  # triangular
            maxM = (self.L1 / self.L) + 0.5774
            #
        x_steps = [0, 1 / 4, 3 / 8, 2 / 4, 5 / 8, 3 / 4, 1, maxM]
        x_steps = sorted(list(set(x_steps)))
        x_steps = [item for item in x_steps if item <= 1]
        return x_steps
    #


#
@dataclass
class Point(ArbitraryLoading):
    __slots__ = ['W', 'L', 'L1', 'P']

    def __init__(self, W: float, L: float, a: float,
                 P: float = 0):
        """
        W : Point load
        a : Distant to W from the left end
        L : Length of beam
        P : axial force (zero by befault)
        """
        super().__init__(L=L, L1=a)
        self.W = W
        self.P = P

    #
    #def q(self, x: float) -> float:
    #    """ Loading Function"""
    #    step = x - self.L1
    #    return self.function_n(step, -1) * -self.P

    #
    def V(self, x: float, I: float) -> float:
        """ Shear Force"""
        step = x - self.L1
        ef = self.ei(x=x, I=I, k=0)
        return (-1 * self.W
                * (ef.e1 * self.function_n(step, 1)
                   + ef.Zeta * ef.e3 * self.function_n(step, 1)))
        #

    def M(self, x: float, I: float) -> float:
        """ Bending Moment"""
        step = x - self.L1
        ef = self.ei(x=x, I=I, k=0)
        return -1 * self.W * ef.e2 * self.function_n(step, 1)

    #
    def theta(self, x: float, E: float, G: float, I: float) -> float:
        """ Slope = EIy' """
        step = x - self.L1
        EI = E * I
        ef = self.ei(x=x, I=I, k=0)
        return -self.W * ef.e3 * self.function_n(step, 1) / EI

    #
    def w(self, x: float, E: float, G: float,
          I: float, As: float) -> float:
        """ Deflection = EIy"""
        step = x - self.L1
        EI = E * I
        ef = self.ei(x=x, I=I, k=0)
        return (self.W
                * (ef.e4 * self.function_n(step, 1) / EI
                   - (ef.e2 * self.function_n(step, 1)
                      - ef.Zeta * ef.e4 * self.function_n(step, 1)) / (G * As)))

#
#
@dataclass
class Moment(ArbitraryLoading):
    __slots__ = ['C', 'L', 'L1', 'P']

    def __init__(self, M: float, L: float, a: float,
                 P: float = 0) -> None:
        """
        M : Moment load
        a : Distant to M from the left end
        L : Length of beam
        P : axial force (zero by befault)
        """
        super().__init__(L=L, L1=a)
        self.C = M
        self.P = P
    #
    def V(self, x: float, I: float) -> float:
        """ Shear Force"""
        step = x - self.L1
        ef = self.ei(x=x, I=I, k=0)
        return self.C * ef.Lambda * ef.e4 * self.function_n(step, 1)
    #
    def M(self, x: float, I: float) -> float:
        """ Bending Moment"""
        step = x - self.L1
        ef = self.ei(x=x, I=I, k=0)
        return (-1 * self.C
                * (ef.e1 * self.function_n(step, 1)
                   - ef.Eta * ef.e3 * self.function_n(step, 1)))

    #
    def theta(self, x: float, E: float, G: float, I: float) -> float:
        """ Slope = EIy' """
        step = x - self.L1
        EI = E * I
        ef = self.ei(x=x, I=I, k=0)
        return (-1 * self.C
                * (ef.e3 * self.function_n(step, 1)
                   - ef.Eta * ef.e4 * self.function_n(step, 1))
                / EI)

    #
    def w(self, x: float, E: float, G: float, I: float) -> float:
        """ Deflection = EIy"""
        step = x - self.L1
        EI = E * I
        ef = self.ei(x=x, I=I, k=0)
        return self.C * ef.e3 * self.function_n(step, 1) / EI


#
#

