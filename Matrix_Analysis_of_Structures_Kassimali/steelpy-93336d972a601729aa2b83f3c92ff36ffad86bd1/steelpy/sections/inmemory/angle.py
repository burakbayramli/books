# 
# Copyright (c) 2019-2023 steelpy
#

# Python stdlib imports
from __future__ import annotations
from array import array
from collections import namedtuple
from dataclasses import dataclass
import math
#

# package imports
from steelpy.sections.process.operations import SectionProperty, ShapeProperty
from steelpy.sections.process.stress import BeamStress
from .operations import SectionBasic, ShapeBasic

#
#
points = namedtuple('Points', ['y', 'z'])
#
# ----------------------------------------
#      Standard Sections Profiles
# ----------------------------------------
#
class Angle(SectionBasic):
    __slots__ = ['_labels', '_number', '_title', 
                 '_d', '_tw', '_b', '_r']
    
    def __init__(self):
        """ """
        super().__init__()
        self._d: array = array('f', [])
        self._tw: array = array('f', [])        
        self._b: array = array('f', [])
        self._r: array = array('f', [])  
    #
    #
    def __setitem__(self, shape_name: int|str, parameters: list) -> None:
        """
        parameters = [node1, node2, material, section, roll_angle]
        """
        try:
            self._labels.index(shape_name)
            raise Exception('element {:} already exist'.format(shape_name))
        except ValueError:
            self._labels.append(shape_name)
            self._title.append('NULL')
            mnumber = next(self.get_number())
            self._number.append(mnumber)
            #
            self._d.append(parameters[0])
            self._tw.append(parameters[1])
            self._b.append(parameters[2])
            try:
                self._r.append(parameters[3]*0.50)
            except ValueError:
                self._r.append(0.50 * (self._tw[-1] / math.sqrt(2.0)))
    #
    def __getitem__(self, shape_name: str | int):
        """
        """
        try:
            index = self._labels.index(shape_name)
        except ValueError:
            raise Exception(f" section name {shape_name} not found")
        #
        return AngleBasic(name=self._labels[index], 
                          d=self._d[index], tw=self._tw[index],
                          b=self._b[index], r=self._r[index])
#
#
@dataclass
class AngleBasic(ShapeBasic):
    """
    Calculate the section properties of an angle section

         +   +
             |         
        d    |         Z
             |         ^
         +   +-----+   + > Y
             *  b  *

    Parameters
    ----------
    d  : Section Heigh
    tw : Web/flange thickness
    b  : Base
    r  : Root Radious

    Returns
    ----------
    area: Section area
    Zc  : Elastic neutral centre
    Yc  : Elastic neutral centre
    Iy  : Second moment of area about mayor axis
    Zey : Elastic modulus about mayor axis
    Zpy : Plastic modulus about mayor axis
    SFy : Shape factor mayor axis
    ry  : Radius of gyration about mayor Axis
    Iz  : Second moment of area about minor axis
    Zez : Elastic modulus about minor axis
    Zpz : Plastic modulus about minor axis
    SFz : Shape factor minor axis
    rz  : Radius of gyration about minor Axis
    SC  : Shear centre
    Cw  : Warping constant

    Notes
    ----------
    Uses formulas from:
    1.- Full plastic capacity of equal angle sections under biaxial
        bending and normal force [A.E. Charalampakis]
    2.- Formulas for stress, strain and strucutral matrices [W.D. Pilkey]
    3.- Roark's formulas for stress and strain [7th Edition]
    4.- Wikipedia

    Examples
    ----------

    """
    #name:str|int
    d:float
    tw:float
    b:float
    r:float
    type:str = 'Angle'
    #
    #    
    def _properties(self):
        # -------------------------------------------------
        #
        # Simmetric angle
        if math.isclose(self.d, self.b):

            # -------------------------------------------------
            #   Cross-Sectional Area (A.1)
            area = ((2 * self.d * self.tw)
                    - self.tw ** 2 + (2 - math.pi / 2.0) * self.r ** 2)

            # Distance v1 of elastic centroid from heel (A.2)
            _v1 = (((6 * math.sqrt(2)) * (((50 / 3.0 - 5 * math.pi) * self.r ** 3)
                                          - ((2 - math.pi / 2.0) * (self.d - 3 * self.tw) * self.r ** 2)
                                          + ((self.d ** 2 + self.d * self.tw - self.tw * 2) * self.tw))) /
                   (((24 - 6 * math.pi) * self.r ** 2) + (24 * self.d * self.tw) - (12 * self.tw ** 2)))

            #  Distance v2 (A.3)
            _v2 = ((self.d / math.sqrt(2)) + (1 - math.sqrt(2)) +
                   (self.tw / math.sqrt(2)) - _v1)

            # Distance v3 (A.4)
            _v3 = self.d / math.sqrt(2)

            # Moment of inertia around u-u (A.5)
            _Iu = (((70 - 21 * math.pi) * self.r ** 4 / 24.0) +
                   ((self.d - self.tw) ** 2 * (math.pi - 4) * self.r ** 2 / 4.0) -
                   (self.tw ** 4 / 12.0) + (self.d ** 3 * self.tw / 3.0) + (self.d * self.tw ** 3 / 3.0) -
                   (self.d ** 2 * self.tw ** 2 / 2.0))

            # Elastic section modulus around u-u (A.6)
            _Wu = _Iu / _v3

            # Moment of inertia around v-v (A.7)
            _Iv = ((1.0 / ((288 - 72 * math.pi) * self.r ** 2 + 288 * (self.d - (self.tw / 2.0)) * self.tw)) *
                   ((((7926 * math.pi) - (1233 * math.pi ** 2) - 12776) * self.r ** 6)
                    + (432 * (math.pi - (10.0 / 3.0)) * (self.d - self.tw) * (math.pi - 4) * self.r ** 5)
                    + (((1422 * math.pi) - (36 * math.pi ** 2) - 4188) * self.tw ** 2)
                    + (((72 * ((-79 * math.pi / 2.0) + math.pi ** 2 + (349 / 3.0)) * self.d * self.tw)
                        - (36 * self.d ** 2 * (math.pi - 4) ** 2) * self.r ** 4))
                    + (432 * (math.pi - (10.0 / 3.0)) * (4 * self.tw ** 2 / 3.0 + self.d ** 2 -
                                                         4 * self.d * self.tw) * self.tw * self.r ** 3)
                    - (24 * (self.d ** 3 - (9 * self.tw * self.d ** 2) + (13 * self.tw ** 2 * self.d) - (
                                       13 * self.tw ** 3 / 4.0)) *
                       ((math.pi - 4) * self.tw * self.r ** 2))
                    + (-(72 * self.d * self.tw ** 5) + (12 * self.tw ** 6) + (24 * self.tw ** 2 * self.d ** 4) -
                       (48 * self.tw ** 3 * self.d ** 3) + (96 * self.tw ** 4 * self.d ** 2))))

            # Elastic section modulus around v-v (A.8)
            _Wv = _Iv / _v1

            # Distance e (A.9)
            _e = _v1 / math.sqrt(2)
            Zc = _e
            Yc = _e

            # Moment of inertia around y-y or z-z (A.10)
            Iy = ((1.0 / (((288 - 72 * math.pi) * self.r ** 2)
                          + (288 * (self.d - self.tw / 2.0) * self.tw)))
                  * (((3732 * math.pi - 5968 - 585 * math.pi ** 2) * self.r ** 6)
                     + ((216 * (math.pi - 4) * (math.pi - 10.0 / 3.0) * (self.d - self.tw) * self.r ** 5)
                        + (60 * self.d ** 4 * self.tw ** 2))
                     + (-(120 * self.d ** 3 * self.tw ** 3) + (132 * self.d ** 2 * self.tw ** 4)
                        - (72 * self.d * self.tw ** 5) +
                        (12 * self.tw ** 6) + (((216 * (math.pi - 10.0 / 3.0))) *
                                               ((self.d ** 2 - 4 * self.d * self.tw + 4 * self.tw ** 2 / 3.0)
                                                * self.r ** 3 * self.tw)))
                     + (((-27 * self.d ** 2 * (math.pi - 4) ** 2)
                         + (54 * (272 / 3.0 - 94 * math.pi / 3.0 + math.pi ** 2) * self.d * self.tw)
                         + ((846 * math.pi - 2448 - 27 * math.pi ** 2) * self.tw ** 2)) * self.r ** 4)
                     + (12 * (math.pi - 4) * (self.d ** 3 + 3 * self.d ** 2 * self.tw
                                              - 8 * self.d * self.tw ** 2 + 2 * self.tw ** 3) * self.r ** 2 * self.tw)))

            Iz = Iy

            # Elastic section modulus araund y-y or z-z (A.11)
            Zey = Iy / (self.d - _e)
            Zez = Zey

            # Radii of gyration around any axis (A.12)
            ry = (Iy / area) ** 0.50
            rz = ry

            # Plastic Properties
            # Distance v1p of the plastic centroid from heel (A.13)
            _v1p = ((3 * (math.pi - 4) * self.r ** 2
                     + 4 * self.d * self.tw + 6 * self.tw ** 2)
                    / (8 * math.sqrt(2) * self.tw))

            # Major plastic section modulus around u'-u' (A.14)
            Zpy = ((((48 * self.r ** 3
                      + 3 * (math.pi - 4) * (self.d - self.tw) * self.r ** 2
                      - 6 * self.d * self.tw ** 2
                      + 6 * self.d ** 2 * self.tw
                      + 2 * self.tw ** 3) * math.sqrt(2))
                    / 12.0) - (16 * self.r ** 3 / 3.0))

            # Minor Plastic section modulus around v'-v' (A.15)
            Zpz = ((math.sqrt(2) / (192 * self.tw))
                   * ((-27 * (math.pi - 4) ** 2 * self.r ** 4)
                      + (96 * (3 * math.pi - 10) * self.r ** 3 * self.tw)
                      - ((12 * (math.pi - 4) * self.r ** 2) * ((2 * self.d - 11 * self.tw) * self.tw))
                      + (4 * self.tw ** 2 * (12 * self.d ** 2
                                             - 12 * self.d * self.tw + 13 * self.tw ** 2))))

            _phi = 1.0

            # -------------------------------------------------
            #   Plastic Modulus about Minor Axis
            #   error, needs fix

            # _Zpy = 0
            # _Zpz = 0
            #
        #
        else:
            _b1 = self.b - self.tw
            _h1 = self.d - self.tw

            # -------------------------------------------------
            #   Cross-Sectional Area
            area = (self.d + _b1) * self.tw

            # -------------------------------------------------
            #   Elastic Neutral Centre
            Zc = (self.d ** 2 + _b1 * self.tw) / (2 * (self.d + _b1))
            Yc = (self.b ** 2 + _h1 * self.tw) / (2 * (self.b + _h1))

            # -------------------------------------------------
            #   Shear Centre 
            _SCz = self.tw / 2.0
            _SCy = self.tw / 2.0

            # -------------------------------------------------
            #               Section Properties
            # -------------------------------------------------

            #   Second Moment of Area about Mayor Axis

            Iy = ((self.tw * (self.d - Zc) ** 3 + self.b * Zc ** 3 -
                   _b1 * (Zc - self.tw) ** 3) / 3.0)

            Zey = Iy / (self.d - Zc)
            _Zpy = 0

            #   Radius of gyration about Mayor Axis
            self.ry = (Iy / area) ** 0.50
            _SFy = 0

            # -------------------------------------------------
            #   Second Moment of Area about Minor Axis

            Iz = ((self.tw * (self.b - Yc) ** 3 + self.d * Yc ** 3 -
                   _h1 * (Yc - self.tw) ** 3) / 3.0)

            Zez = Iz / (self.b - Yc)
            _Zpz = 0

            #   Radius of gyration about Minor Axis 
            rz = (Iz / area) ** 0.50
            _SFz = 0

            # -------------------------------------------------
            #   Product of inertia
            _Izy = (self.b * _b1 * self.d * _h1 * self.tw) / (4 * (self.b + _h1))

            _Iu = (0.50 * (Iz + Iy) +
                   0.50 * ((Iz - Iy) ** 2 + 4 * _Izy ** 2) ** 0.50)

            _Iv = (0.50 * (Iz + Iy) -
                   0.50 * ((Iz - Iy) ** 2 + 4 * _Izy ** 2) ** 0.50)

            _phi = (math.atan(2 * _Izy / (Iy - Iz))) / 2.0

            _Wu = 0
            _Wv = 0
            _Wup = 0
            _Wvp = 0
            _v1p = 0
        #
        _b1 = self.d - 0.50 * self.tw
        _b2 = self.b - 0.50 * self.tw
        _c1 = _b1 - 0.50 * self.tw
        _c2 = _b2 - 0.50 * self.tw
        #
        # -------------------------------------------------
        #   Warping Constant Cw (Bleich 1952, Picard and Beaulie 1991)
        Cw = (((_b1 ** 3 + _b2 ** 3) * (self.tw ** 3)) / 36.0)

        # -------------------------------------------------
        #   Torsional Constant (fillets neglected)
        J = (((_b1 + _b2) * (self.tw ** 3)) / 3.0)

        # -------------------------------------------------
        #   Product of inertia
        Iyz = (self.tw * (_b1 * Yc * (_b1 - 2 * Zc)) +
               self.tw * (_b2 * Zc * (_b2 - 2 * Yc)))

        Jx = Iy + Iz
        rp = (Jx / area) ** 0.50
        #
        return ShapeProperty(area=area, Zc=Zc, Yc=Yc,
                           Iy=Iy, Zey=Zey, Zpy=Zpy, ry=ry,
                           Iz=Iz, Zez=Zez, Zpz=Zpz, rz=rz,
                           J=J, Cw=Cw)

    #
    #
    def _stress(self, actions, stress=None):
        """
        """
        #
        # get section's coordinates
        coord =  self.section_coordinates()
        prop = self.properties()
        #
        # ----------------------------------------------
        # FIXME: torsion
        tau_x = [actions.Mx * 0
                 for item in coord.y]
        #
        # In Plane
        tau_y = [actions.Fy / prop.area
                 for item in coord.y]
        # Out Plane
        tau_z = [actions.Fz / prop.area
                 for item in coord.z]
        #
        # get bending stress
        sigma_x = [actions.Fx / prop.area for item in coord.y]
        sigma_y = [actions.My * item / prop.Iy for item in coord.z]
        sigma_z = [actions.Mz * item / prop.Iz for item in coord.y]
        #
        stress_out = BeamStress(sigma_x, sigma_y, sigma_z, 
                                tau_x, tau_y, tau_z, coord)
        #
        if stress:
            stress_out = self.add_stress(stress=stress, other=stress_out)
        #
        return stress_out 

    #
    #
    def print_file(self, file_name):

        check_out = print_header()

        check_out.append("{:23s} {:>19} {:1.4E} {:1.4E} {:1.4E} {:1.4E}\n"
                         .format(self.type, "", self.d, self.tw, self.b, self.tw))

        check_out.extend(print_properties(self))

        # file_checkout = split_file_name(file_name)
        # file_checkout = str(file_checkout[0]) +'_check_me.txt'
        file_checkout = str(file_name) + '.txt'
        add_out = open(file_checkout, 'w')
        add_out.write("".join(check_out))
        add_out.close()
        print('ok')
    #
    def _dimension(self) -> str:
        """ Print section dimensions"""
        return ("{:23s} {:>19} {:1.4E} {:1.4E} {:1.4E} {:1.4E}\n"
                .format(self.type, "", self.d, self.tw, self.b, self.tw))

    #
    #
    @property
    def CoG(self):
        """ """
        # Simmetric angle
        if math.isclose(self.d, self.b):
            # Distance v1 of elastic centroid from heel (A.2)
            v1 = (((6 * math.sqrt(2)) * (((50 / 3.0 - 5 * math.pi) * self.r ** 3)
                                          - ((2 - math.pi / 2.0) * (self.d - 3 * self.tw) * self.r ** 2)
                                          + ((self.d ** 2 + self.d * self.tw - self.tw * 2) * self.tw))) /
                   (((24 - 6 * math.pi) * self.r ** 2) + (24 * self.d * self.tw) - (12 * self.tw ** 2)))            
            # Distance e (A.9)
            Yc = v1 / math.sqrt(2)
            Zc = Yc
        else:
            _b1 = self.b - self.tw
            _h1 = self.d - self.tw            
            # -------------------------------------------------
            #   Elastic Neutral Centre
            Zc = (self.d ** 2 + _b1 * self.tw) / (2 * (self.d + _b1))
            Yc = (self.b ** 2 + _h1 * self.tw) / (2 * (self.b + _h1))
        #
        #
        return Yc, Zc        
    #
    def section_coordinates(self):
        """
                     1 
                   +
                   |       ^ z
                   + 2     |
                   |       |
                   + 3     +--> y
                   |
                   + 4
                   | 
        +-+----+-+-+      
        5 6    7 8 9
        """
        # horizontal
        
        CoG = self.CoG
        #
        hzt1 = CoG[0] - self.tw * 0.50
        hzt2 = -1 * (self.b - CoG[0])
        hzt3 = CoG[0] - self.tw
        coord_y = [hzt1, hzt1, hzt1, hzt1, # 1,2,3,4
                   hzt2, hzt2 + self.tw, 0, hzt3, hzt1]    # 5,6,7,8
        # vertical
        top1 = CoG[1]
        top2 = CoG[1] - self.tw
        top3 = -1 * (self.d - CoG[1] - self.tw)
        top4 = -1 * (self.d - CoG[1] - self.tw * 0.50)
        coord_z = [top1, top2, 0, top3, #1-4
                   top4, top4, top4, top4, top4]  # 5-9
        
        return points(coord_y, coord_z)      
    #
