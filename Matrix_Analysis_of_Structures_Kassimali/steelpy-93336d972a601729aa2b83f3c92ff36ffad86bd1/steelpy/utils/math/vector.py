# Copyright (c) 2022 steelpy
# -----------------------------------------------------------------------
# vector.py
# -----------------------------------------------------------------------
from __future__ import annotations
# Python stdlib imports
from array import array
from dataclasses import dataclass
from math import fsum
from typing import ClassVar

# -----------------------------------------------------------------------
#


@dataclass
class Vector:
    """
    A simple vector class.
    Instances of the Vec class can be constructed from numbers
    >>> xCoords = [1.0, 2.0, 3.0, 4.0]
    >>> yCoords = [5.0, 2.0, 4.0, 1.0]
    >>> x = Vector(xCoords)
    >>> y = Vector(yCoords)
    >>> print(x + y)
    Vec[6.0, 4.0, 7.0, 5.0]
    >>> print(10 * x)
    Vec[10.0, 20.0, 30.0, 40.0]
    >>> print(abs(x))
    5.477225575051661
    >>> print(x.dot(y))
    25.0
    >>> print(abs(x - y))
    5.0990195135927845
    >>> print(x.direction())
    Vec[0.18257418274879456, 0.3651483654975891, 0.547722578048706, 0.7302967309951782]
    """
    __slots__ = ['_coords']

    def __init__(self, a: list[float]) -> None:
        """Construct a new Vector object with numeric Cartesian coordinates
           given in array a."""
        # Make a defensive copy to ensure immutability.
        self._coords: ClassVar = array('f', a[:])   # Cartesian coordinates
        # self._n:ClassVar = len(a) # Dimension.

    def __getitem__(self, i) -> float:
        """Return the ith Cartesian coordinate of self."""
        return self._coords[i]

    def __add__(self, other) -> list[float]:
        """Return the sum of self and Vector object other."""
        if not isinstance(other, Vector):
            raise TypeError("must be a vector class")
        result = [x + y for x, y in zip(self._coords, other)]
        return Vector(result)
    
    def __iadd__(self, other) -> list[float]:
        """ """
        self._coords = self.__add__(other)
        return self

    def __sub__(self, other) -> list[float]:
        """Return the difference of self and Vector object other."""
        result = [x - y for x, y in zip(self._coords, other)]
        return Vector(result)

    def dot(self, other) -> float:
        """Return the inner product of self and Vector object other."""
        if not isinstance(other, Vector):
            raise TypeError("must be a vector class")
        result = fsum([x * y for x, y in zip(self._coords, other)])
        return result

    def __abs__(self) -> float:
        """Return the magnitude, that is, the Euclidean norm, of self."""
        return (self.dot(self))**0.50

    def direction(self) -> list[float]:
        """Return the unit vector of self."""
        return self.__mul__(1.0 / abs(self))

    # Return a string representation of self.
    # def __str__(self) -> str:
    #    return str(self._coords)

    def __repr__(self):
        """Return a string representation of self."""
        args = ', '.join(repr(x) for x in self._coords)
        return 'Vec[{}]'.format(args)

    def __len__(self) -> int:
        """Return the dimension of self."""
        return len(self._coords)

    def __mul__(self, scalar) -> list[float]:
        """Return the product of self and numeric object alpha."""
        if not isinstance(scalar, (float, int)):
            raise TypeError("must be a scalar")
        result = [x * scalar for x in self._coords]
        return Vector(result)
    
    def __truediv__(self, scalar) -> list[float]:
        """Return the product of self and numeric object alpha."""
        if not isinstance(scalar, (float, int)):
            raise TypeError("must be a scalar")
        result = [x / scalar for x in self._coords]
        return Vector(result)
    
    __rmul__ = __mul__
#
# -----------------------------------------------------------------------
# For testing.
# Create and use some Vector objects.


def test():
    import doctest
    doctest.testmod()
#
#
#test()
#
# def main():
#
#    xCoords = [1.0, 2.0, 3.0, 4.0]
#    yCoords = [5.0, 2.0, 4.0, 1.0]
#
#    x = Vector(xCoords)
#    y = Vector(yCoords)
#
#    print('x        = ' + str(x))
#    print('y        = ' + str(y))
#    print('x + y    = ' + str(x + y))
#    print('10x      = ' + str(10 * x))
#    print('|x|      = ' + str(abs(x)))
#    print('<x, y>   = ' + str(x.dot(y)))
#    print('|x - y|  = ' + str(abs(x - y)))
#
# if __name__ == '__main__':
#    main()
#
# -----------------------------------------------------------------------
#
# python vector.py
# x        = [1.0, 2.0, 3.0, 4.0]
# y        = [5.0, 2.0, 4.0, 1.0]
# x + y    = [6.0, 4.0, 7.0, 5.0]
# 10x      = [10.0, 20.0, 30.0, 40.0]
# |x|      = 5.477225575051661
# <x, y>   = 25.0
# |x - y|  = 5.0990195135927845
#
#import numpy as np
#import numpy.linalg as la
##
#xCoords        = np.array([1.0, -2.0, 3.0, 4.0])
#yCoords        = np.array([5.0, 2.0, 4.0, 1.0])
##
#x = Vector(xCoords)
#print(abs(x))
#print(la.norm(xCoords))
##
#print(xc)
#print(Vector([abs(item) for item in x])/ abs(x))
#print(np.abs(xCoords)/la.norm(xCoords))
#print('---')