#
# Copyright (c) 2009-2021 steelpy
#

# Python stdlib imports
from array import array
from bisect import bisect_right
from copy import copy
from dataclasses import dataclass
# from math import fsum
from typing import NamedTuple, Tuple, Union, List, Dict


# package imports


class Interpolation:
    __slots__ = [ 'x_list', 'y_list']

    def __init__(self, x_list: List[ float ], y_list: List[ float ]):
        """ """
        #text = [y - x <= 0 for x, y in zip( x_list, x_list[ 1:])]
        if any( y - x <= 0 for x, y in zip( x_list, x_list[ 1: ] ) ):
            raise ValueError ("x_list must be in strictly ascending order!" )
        # input
        self.x_list = x_list
        self.y_list = y_list
    #
    def linear(self, x: float):
        """ """
        intervals = zip( self.x_list, self.x_list[ 1: ], self.y_list, self.y_list[ 1: ] )
        slopes = [ (y2 - y1) / (x2 - x1) for x1, x2, y1, y2 in intervals ]
        #
        if not (self.x_list[ 0 ] <= x <= self.x_list[ -1 ]):
            raise ValueError( "x out of bounds!" )
        
        if x == self.x_list[ -1 ]:
            return self.y_list[ -1 ]
        
        i = bisect_right( self.x_list, x ) - 1
        return self.y_list[ i ] + slopes[ i ] * (x - self.x_list[ i ])
    #
    def rational(self, x: float):
        """"" p = rational(xData,yData,x)
        Evaluates the diagonal rational function interpolant p(x)
        that passes through the data points
        """""
        m = len(self.x_list)
        r = copy(self.y_list)
        rOld = zeros( m )
        for k in range( m - 1 ):
            for i in range( m - k - 1 ):
                if abs( x - self.x_list[ i + k + 1 ] ) < 1.0e-9:
                    return self.y_list[ i + k + 1 ]
                else:
                    c1 = r[ i + 1 ] - r[ i ]
                    c2 = r[ i + 1 ] - rOld[ i + 1 ]
                    c3 = (x - self.x_list[ i ]) / (x - self.x_list[ i + k + 1 ])
                    r[ i ] = r[ i + 1 ] + c1 / (c3 * (1.0 - c1 / c2) - 1.0)
                    rOld[ i + 1 ] = r[ i + 1 ]
        return r[ 0 ]
    #
    #
    def evalSpline(self, x: float):
        """"y = evalSpline(x).
        Evaluates cubic spline at x. The curvatures k can be
        computed with the function curvatures"""
        k = self.curvatures()

        i = self.findSegment( self.x_list, x )
        h = self.x_list[ i ] - self.x_list[ i + 1 ]
        y = (((x - self.x_list[ i + 1 ]) ** 3 / h - (x - self.x_list[ i + 1 ]) * h) * k[ i ] / 6.0
            - ((x - self.x_list[ i ]) ** 3 / h - (x - self.x_list[ i ]) * h) * k[ i + 1 ] / 6.0
            + (self.y_list[ i ] * (x - self.x_list[ i + 1 ]) - self.y_list[ i + 1 ] * (x - self.x_list[ i ])) / h)
        return y
    #
    def curvatures(self) -> List:
        """" k = curvatures(xData,yData).
        Returns the curvatures of cubic spline at its knots.
        """""
        n = len( self.x_list ) - 1
        c = [self.x_list[ x ] - self.x_list[ x+1 ] for x in range(n)]
        #c[ 0:n - 1 ] = self.x_list[ 0:n - 1 ] - self.x_list[ 1:n ]
        d = [2.0 * (self.x_list[x] - self.x_list[x+2]) for x in range(n-1)]
        d.insert(0,1)
        d.append(1)
        #d[ 1:n ] = 2.0 * (self.x_list[ 0:n - 1 ] - self.x_list[ 2:n + 1 ])
        #
        e = [(self.x_list[x+1] - self.x_list[x+2]) for x in range(n-1)]
        e.insert(0,0)
        #e[ 1:n ] = self.x_list[ 1:n ] - self.x_list[ 2:n + 1 ]
        #
        k = [(6.0 * (self.y_list[x] - self.y_list[ x+1])
                    / (self.x_list[x] - self.x_list[ x+1]) - 6.0 * (self.y_list[ x+1] - self.y_list[ x+2])
                    / (self.x_list[ x+1] - self.x_list[ x+2])) for x in range(n-1)]
        k.insert(0,0)
        k.append(0)
        #k[ 1:n ] = (6.0 * (self.y_list[ 0:n - 1 ] - self.y_list[ 1:n ])
        #            / (self.x_list[ 0:n - 1 ] - self.x_list[ 1:n ]) - 6.0 * (self.y_list[ 1:n ] - self.y_list[ 2:n + 1 ])
        #            / (self.x_list[ 1:n ] - self.x_list[ 2:n + 1 ]))
        LUdecomp3( c, d, e )
        LUsolve3( c, d, e, k )
        return k
    #
    def findSegment(self, xData, x):
        iLeft = 0
        iRight = len( xData ) - 1
        while 1:
            if (iRight - iLeft) <= 1: 
                return iLeft
            i = (iLeft + iRight) // 2
            if x < xData[ i ]:
                iRight = i
            else:
                iLeft = i
#
#
#
@dataclass
class LinearInterpolate:
    __slots__ = [ 'x_list', 'y_list', 'slopes' ]

    def __init__(self, x_list: List[ float ], y_list: List[ float ]):
        if any ( y - x <= 0 for x, y in zip ( x_list, x_list[ 1: ] ) ):
            raise ValueError ( "x_list must be in strictly ascending order!" )
        self.x_list = x_list
        self.y_list = y_list
        intervals = zip ( x_list, x_list[ 1: ], y_list, y_list[ 1: ] )
        self.slopes = [ (y2 - y1) / (x2 - x1) for x1, x2, y1, y2 in intervals ]

    def __call__(self, x: float):
        if not (self.x_list[ 0 ] <= x <= self.x_list[ -1 ]):
            raise ValueError ( "x out of bounds!" )
        if x == self.x_list[ -1 ]:
            return self.y_list[ -1 ]
        i = bisect_right ( self.x_list, x ) - 1
        return self.y_list[ i ] + self.slopes[ i ] * (x - self.x_list[ i ])
#
#
def curvatures(xData: List[ float ], yData: List[ float ]) -> List:
    """" k = curvatures(xData,yData).
    Returns the curvatures of cubic spline at its knots.
    """""
    n = len( xData ) - 1
    c = zeros( n )
    d = ones( n + 1 )
    e = zeros( n )
    k = zeros( n + 1 )
    c[ 0:n - 1 ] = xData[ 0:n - 1 ] - xData[ 1:n ]
    d[ 1:n ] = 2.0 * (xData[ 0:n - 1 ] - xData[ 2:n + 1 ])
    e[ 1:n ] = xData[ 1:n ] - xData[ 2:n + 1 ]
    k[ 1:n ] = (6.0 * (yData[ 0:n - 1 ] - yData[ 1:n ])
                / (xData[ 0:n - 1 ] - xData[ 1:n ]) - 6.0 * (yData[ 1:n ] - yData[ 2:n + 1 ])
                / (xData[ 1:n ] - xData[ 2:n + 1 ]))
    LUdecomp3( c, d, e )
    LUsolve3( c, d, e, k )
    return k
#
#
def evalSpline(xData: List[ float ], yData: List[ float ], x: float):
    """"y = evalSpline(xData,yData,k,x).
    Evaluates cubic spline at x. The curvatures k can be
    computed with the function curvatures"""
    k = curvatures ( xData, yData )

    def findSegment(xData, x):
        iLeft = 0
        iRight = len ( xData ) - 1
        while 1:
            if (iRight - iLeft) <= 1: return iLeft
            i = (iLeft + iRight) / 2
            if x < xData[ i ]:
                iRight = i
            else:
                iLeft = i

    i = findSegment ( xData, x )
    h = xData[ i ] - xData[ i + 1 ]
    y = (((x - xData[ i + 1 ]) ** 3 / h - (x - xData[ i + 1 ]) * h) * k[ i ] / 6.0
         - ((x - xData[ i ]) ** 3 / h - (x - xData[ i ]) * h) * k[ i + 1 ] / 6.0
         + (yData[ i ] * (x - xData[ i + 1 ]) - yData[ i + 1 ] * (x - xData[ i ])) / h)
    return y
#
#
#
def LUdecomp3(c, d, e):
    """"" c,d,e = LUdecomp3(c,d,e).
    LU decomposition of tridiagonal matrix [c\d\e]. On output
    {c},{d} and {e} are the diagonals of the decomposed matrix.
    """""
    n = len( d )
    for k in range ( 1, n ):
        lam = c[ k - 1 ] / d[ k - 1 ]
        d[ k ] -=  lam * e[ k - 1 ]
        c[ k - 1 ] = lam
    return c, d, e
#
#
def LUsolve3(c, d, e, b):
    """"x = LUsolve(c,d,e,b).
    Solves [c\d\e]{x} = {b}, where {c}, {d} and {e} are the
    vectors returned from LUdecomp3.
    """""
    n = len( d )
    for k in range( 1, n ):
        b[ k ] -=  c[ k - 1 ] * b[ k - 1 ]
    #
    b[ n - 1 ] /=  d[ n - 1 ]
    for k in range( n - 2, -1, -1 ):
        b[ k ] = (b[ k ] - e[ k ] * b[ k + 1 ]) / d[ k ]
    return b
#
#
#
def zeros(m, n=None, code: str = 'd') -> List:
    """
    Create zero matrix
    """
    if n:
        new_matrix = [ array ( code, [ 0 for row in range ( n ) ] )
                       for col in range ( m ) ]
    else:
        new_matrix = array ( code, [ 0 for row in range ( m ) ] )
    return new_matrix
#
#
def ones(m, n=None, code: str = 'd') -> List:
    """
    Create zero matrix
    """
    if n:
        new_matrix = [ array ( code, [ 1 for row in range ( n ) ] )
                       for col in range ( m ) ]
    else:
        new_matrix = array ( code, [ 1 for row in range ( m ) ] )
    return new_matrix
