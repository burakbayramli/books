# 
# Copyright (c) 2009-2023 steelpy
#
# Python stdlib imports
from __future__ import annotations
# from math import sqrt
from dataclasses import dataclass
# from typing import NamedTuple, Tuple, Union, List, Dict, Iterator

# package imports
from steelpy.f2uModel.mesh.process.nodes import CoordCartesian as Point

# from point import Point
# from euclid import Point3 as Point

"""
Reference 'The Shortest Line Between Two Lines in 3D' - Paul Bourke   
"""


@dataclass
class LineLineIntersect3D:
    """Determine information about the intersection of two line segments in 3D space"""
    __slots__ = ["p1", "p2", "p3", "p4", "uv1", "uv2",
                 "cp12", "_cp12_", "Pmem1", "Pmem2",
                 "inters_dist", "on_segment1", "position",
                 "left_dist", "right_dist", "on_segment2",
                 "uv"]

    def __init__(self, p1: list[float], p2: list[float],
                 p3: list[float], p4: list[float]):
        """                                                                                                                       <-->     <-->
            Calculate the points in 3D space Pa and Pb that define the line segment which is the shortest route between two lines p1p2 and p3p4.
            Each point occurs at the apparent intersection of the 3D lines.
            The apparent intersection is defined here as the location where the two lines 'appear' to intersect when viewed along the line segment PaPb.
            Equation for each line:
            Pa = p1 + ma(p2-p1)
            Pb = p3 + mb(p4-p3)
            
            Pa lies on the line connecting p1p2.
            Pb lies on the line connecting p3p4.

            The shortest line segment is perpendicular to both lines. Therefore:
            (Pa-Pb).(p2-p1) = 0
            (Pa-Pb).(p4-p3) = 0

            Where:            
            '.' indicates the dot product            

            A = p1-p3
            B = p2-p1
            C = p4-p3

            Substituting:
            (A + ma(B) - mb(C)).B = 0       &       (A + ma(B) - mb(C)).C = 0
            -----------------------------------------------------------------
            A.B + ma(B.B) - mb(C.B) = 0
            A.B + ma(B.B) - (ma(C.B)-A.C)/C.C)(C.B) = 0
            ma(B.B)(C.C) - ma(C.B)(C.B) = (A.C)(C.B)-(A.B)(C.C)
            ma = ((A.C)(C.B)-(A.B)(C.C))/((B.B)(C.C) - (C.B)(C.B))
            mb = (A.B + ma(B.B))/(C.B)

            If the cross product magnitude of the two lines is equal to 0.0, the lines are parallel.          
                                                                                                                                                 <-->
            A line extends forever in both directions. The name of a line passing through two different points p1 and p2 would be "line p1p2" or p1p2.                                           
            The two-headed arrow over p1p2 signifies a line passing through points p1 and p2.

            Two lines which have no actual intersection but are not parallel are called 'skew' or 'agonic' lines. Skew lines can only exist in
            three or more dimensions.

            Determine whether the apparent intersection point lies between the line segment end points or beyond one of the line segment end points.
            This information is to be used to evaluate the framing condition of mem1 (p1p2).
            Convention for members:
                p1p2 - mem1.left.location, mem1.right.location
                p3p4 - mem2.left.location, mem2.right.location
                
            Set a keyword indicating the apparent intersection point position with respect to the line segment end points p1 and p2 as follows:
                'LE' indicates the apparent intersection point occurs at p1 (within fudge_factor distance)
                'RE' indicates the apparent intersection point occurs at p2 (within fudge_factor distance)
                'Beyond LE' indicates the apparent intersection point occurs beyond p1
                'Beyond RE' indicates the apparent intersection point occurs beyond p2
                'Not Beyond LE' indicates the apparent intersection point occurs in between p1 and p2 and is closer to p1
                'Not Beyond RE' indicates the apparent intersection point occurs in between p1 and p2 and is closer to p2
            Calculate the magnitude and direction (beam member 'X' distance) the apparent intersection point occurs from line segment p1p2 end points.
        """
        self.p1 = Point(*p1, name="p1", number=1)
        self.p2 = Point(*p2, name="p2", number=2)
        self.p3 = Point(*p3, name="p2", number=3)
        self.p4 = Point(*p4, name="p3", number=4)

    #
    def cross_product(self, p1, p2):
        return Point(p1.y * p2.z - p1.z * p2.y,
                     p1.z * p2.x - p1.x * p2.z,
                     p1.x * p2.y - p1.y * p2.x,
                     name="p1xp2", number=7)

    def dot_product(self, p1, p2):
        return (p1.x * p2.x + p1.y * p2.y + p1.z * p2.z)

    def mag(self, p):
        return (p.x ** 2 + p.y ** 2 + p.z ** 2) ** 0.50

    def normalise(self, p1, p2):
        p = p2 - p1
        m = self.mag(p)
        if m == 0:
            return Point(0.0, 0.0, 0.0, name="norm", number=8)
        else:
            return Point(p.x / m, p.y / m, p.z / m)

    def ptFactor(p, f):
        return Point(p.x * f, p.y * f, p.z * f, name="factored", number=9)

    #
    def _process(self):
        """ """
        A = self.p1 - self.p3
        B = self.p2 - self.p1
        C = self.p4 - self.p3

        # Line p1p2 and p3p4 unit vectors
        self.uv1 = self.normalise(self.p1, self.p2)
        self.uv2 = self.normalise(self.p3, self.p4)

        # Check for parallel lines
        self.cp12 = self.cross_product(self.uv1, self.uv2)
        self._cp12_ = self.mag(self.cp12)

        if round(self._cp12_, 6) != 0.0:
            ma = ((self.dot_product(A, C) * self.dot_product(C, B)
                   - self.dot_product(A, B) * self.dot_product(C, C))
                  / (self.dot_product(B, B) * self.dot_product(C, C)
                     - self.dot_product(C, B) * self.dot_product(C, B)))

            mb = ((ma * self.dot_product(C, B) + self.dot_product(A, C))
                  / self.dot_product(C, C))

            # Calculate the point on line 1 that is the closest point to line 2
            _Pa = self.p1 + self.ptFactor(B, ma)
            Pa = Point(_Pa.x, _Pa.y, _Pa.z, name="Pa", number=5)
            self.Pmem1 = Pa

            # Calculate the point on line 2 that is the closest point to line 1
            _Pb = self.p3 + self.ptFactor(C, mb)
            Pb = Point(_Pb.x, _Pb.y, _Pb.z, name="Pb", number=6)
            self.Pmem2 = Pb

            # Distance between lines            
            self.inters_dist = Pa.distance(Pb)

            if round(ma, 3) >= 0.0 and round(ma, 3) <= 1.0:
                self.on_segment1 = 1
                xl_dir = 1
                xr_dir = -1
                if round(ma, 2) == 0.0:
                    self.position = "LE"  # apparent intersection is at p1
                elif round(ma, 2) == 1.0:
                    self.position = "RE"  # apparent intersection is at p2
                    xr_dir = 1
                    xl_dir = 1
                elif ma <= 0.5:
                    self.position = "Not Beyond LE"  # apparent intersection is closer to p1
                elif ma > 0.5:
                    self.position = "Not Beyond RE"  # apparent intersection is closer to p2
                else:
                    Warning('self.position calculation error, self.on_segment = 1')
                    raise ValueError
            else:
                self.on_segment1 = 0
                if ma < 0.0:
                    self.position = "Beyond LE"  # apparent intersection is beyond p1
                    xl_dir = -1
                    xr_dir = -1
                elif ma > 0.0:
                    self.position = "Beyond RE"  # apparent intersection is beyond p2
                    xl_dir = 1
                    xr_dir = 1
                else:
                    Warning('self.position calculation error, self.on_segment = 0')
                    raise ValueError

            # Set the member 'X' direction with respect to p1 and p2 - either '+' or '-'
            self.left_dist = round(Pa.distance(self.p1) * xl_dir, 8)
            self.right_dist = round(Pa.distance(self.p2) * xr_dir, 8)

            if round(mb, 3) >= 0.0 and round(mb, 3) <= 1.0:
                self.on_segment2 = 1
            else:
                self.on_segment2 = 0

            # Calculate the unit vector of PaPb
            if round(self.inters_dist, 4) > 0.0:
                self.uv = self.normalise(Pb, Pa)
            else:
                self.uv = Point(0.0, 0.0, 0.0)
        else:  # Lines are parallel
            self.Pmem1 = None
            self.Pmem2 = None
            self.inters_dist = None
            self.left_dist = None
            self.right_dist = None
            self.uv = None

    # Return False if lines are parallel, and return True if lines are not parallel        
    def not_parallel(self):
        if round(self._cp12_, 5) != 0.0:
            return True
        else:
            return False
        #


#
@dataclass
class DistancePointLine3D:
    """Determine information about the relationship between 
    a line segment and a point in 3D space"""
    __slots__ = ["p1", "p2", "Pa", "Pb", "on_segment",
                 "dist", "position", "uv",  # "Pmem1",
                 "left_dist", "right_dist"]

    def __init__(self, p1: list[float], p2: list[float]) -> None:
        """
        Points p1 and p2 are the end points of member #1.
        Point Pb is a point that lies on member #2.
        Calculate the minimum distance in 3D space between point Pb and line p1p2.
        
        Point Pb is closest to line p1p2 at an intersecting perpendicular 
        line PaPb. Pa lies on line p1p2.
        
        The dot product of two vectors, A and B, will equal the cosine of the 
        angle between the vectors, times the length of each vector.
        
        A dot B = A.x * B.x + A.y * B.y + A.z * B.z
        
        If the vectors are unit vectors, the dot product is equal to the cosine 
        of the angle between the vectors.
        
        Since the angle between lines p1p2 and PaPb is 90 degrees, the dot product 
        of vector p1p2 and vector PaPb is 0 (cos(90 deg)=0).
        
        Determine location of point Pa and the scalar distance from point Pb.

        If the calculation result for 'u' is between 0 and 1, Pa lies on line segment p1p2.

        Determine whether point Pa lies between the line segment end points or beyond
        one of the line segment end points.
        Set a keyword indicating point Pa position with respect to the line segment end
        points as follows:
            'LE' indicates Pa occurs at p1 (within fudge_factor distance)
            'RE' indicates Pa occurs at p2 (within fudge_factor distance)
            'Beyond LE' indicates Pa occurs beyond p1
            'Beyond RE' indicates Pa occurs beyond p2
            'Not Beyond LE' indicates Pa occurs in between p1 and p2 and is closer to p1
            'Not Beyond RE' indicates Pa occurs in between p1 and p2 and is closer to p2
        Calculate the scalar distance and direction (beam member 'X' distance) Pa occurs
        from each line segment end point.
        """
        self.p1 = Point(*p1, name="p1", number=1, index=0)
        self.p2 = Point(*p2, name="p2", number=2, index=1)

    #
    def point(self, Pb: list[float]) -> None:
        """ """
        self.Pb = Point(*Pb, name="Pb", number=3, index=2)
        # return self.dist

    #
    def _process(self, tol: float = 0.05) -> None:
        """ """
        u = (((self.Pb.x - self.p1.x) * (self.p2.x - self.p1.x)
              + (self.Pb.y - self.p1.y) * (self.p2.y - self.p1.y)
              + (self.Pb.z - self.p1.z) * (self.p2.z - self.p1.z))
             / ((self.p2.x - self.p1.x) ** 2
                + (self.p2.y - self.p1.y) ** 2
                + (self.p2.z - self.p1.z) ** 2))
        #
        self.Pa = Point(self.p1.x + u * (self.p2.x - self.p1.x),
                        self.p1.y + u * (self.p2.y - self.p1.y),
                        self.p1.z + u * (self.p2.z - self.p1.z),
                        name="Pa", number=4, index=3)
        #
        # self.Pmem1 = self.Pa
        self.dist = round(self.Pa.distance(self.Pb), 8)
        # check distance from Pa to Pb < tol
        if self.dist > tol:
            raise Warning('Point Pb({:1.3f}m) is outside tolerance({:1.3f}m)'
                          .format(self.dist, tol))
        # main module
        if round(u, 3) >= 0.0 and round(u, 3) <= 1.0:
            self.on_segment = 1
            xl_dir = 1
            xr_dir = -1
            if round(u, 3) == 0.0:
                self.position = "LE"  # apparent intersection is at p1
            elif round(u, 3) == 1.0:
                self.position = "RE"  # apparent intersection is at p2
                xr_dir = 1
                xl_dir = 1
            elif u <= 0.5:
                self.position = "Not Beyond LE"  # apparent intersection is closer to p1
            elif u > 0.5:
                self.position = "Not Beyond RE"  # apparent intersection is closer to p2
            else:
                raise Warning('position calculation error, on_segment = 1')
        else:
            self.on_segment = 0
            if u < 0.0:
                self.position = "Beyond LE"  # apparent intersection is beyond p1
                xl_dir = -1
                xr_dir = -1
            elif u > 1.0:
                self.position = "Beyond RE"  # apparent intersection is beyond p2
                xl_dir = 1
                xr_dir = 1
            else:
                raise Warning('position calculation error, on_segment = 0')

        # Set the member 'X' direction with respect to p1 and p2 - either '+' or '-'
        self.left_dist = round(self.Pa.distance(self.p1) * xl_dir, 8)
        self.right_dist = round(self.Pa.distance(self.p2) * xr_dir, 8)

        # Calculate the unit vector of PaPb
        if self.dist > 0.0:
            self.uv = Point((self.Pa.x - self.Pb.x) / self.dist,
                            (self.Pa.y - self.Pb.y) / self.dist,
                            (self.Pa.z - self.Pb.z) / self.dist,
                            name="uv", number=5)
        else:
            self.uv = Point(0.0, 0.0, 0.0, name="uv", number=5, index=4)

    #
    def is_on_segment(self, Pb: list[float], tol: float = 0.05) -> bool:
        """ """
        self.point(Pb)
        self._process(tol)
        try:
            1 / self.on_segment
            return True
        except ZeroDivisionError:
            return False

#
#
