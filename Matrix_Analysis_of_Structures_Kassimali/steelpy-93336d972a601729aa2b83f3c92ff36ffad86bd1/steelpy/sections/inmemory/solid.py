# 
# Copyright (c) 2019-2023 steelpy
#

# Python stdlib imports
from __future__ import annotations
from array import array
from collections import namedtuple
from dataclasses import dataclass
import math
import re
#from typing import NamedTuple

# package imports
from steelpy.sections.process.operations import ShapeProperty
from ..process.stress import BeamStress
from .operations import SectionBasic, ShapeBasic

# ----------------------------------------
#      Basic Solid Shapes
# ----------------------------------------
#
points = namedtuple('Points', ['y', 'z'])
#
#
@dataclass
class SolidSection(SectionBasic):
    """ """
    #
    def __init__(self):
        """ """
        super().__init__()
        self._d: array = array('f', [])
        self._wb: array = array('f', [])
        self._wt: array = array('f', [])
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
            shape_type = parameters.pop(0)
            self._labels.append(shape_name)
            self._title.append('NULL')
            mnumber = next(self.get_number())
            self._number.append(mnumber)
            #
            self._d.append(parameters[0])
            if re.match(r"\b((solid|bar(\_)?)?circular|round)\b", shape_type, re.IGNORECASE):
                self._type.append('round')
                self._wb.append(0)
                self._wt.append(0)

            elif re.match(r"\b((solid|bar(\_)?)?rectangle)\b", shape_type, re.IGNORECASE):
                self._type.append('rectangle')
                self._wb.append(parameters[1])
                self._wt.append(parameters[1])

            elif re.match(r"\b((solid|bar(\_)?)?trapeziod)\b", shape_type, re.IGNORECASE):
                self._type.append('trapeziod')
                self._wb.append(parameters[1])
                self._wt.append(parameters[2])

            else:
                raise Exception(f" section type {shape_type} not recognized")
    #
    def __getitem__(self, shape_name: str | int):
        """
        """
        try:
            index = self._labels.index(shape_name)
        except ValueError:
            raise Exception(f" section name {shape_name} not found")


        shape_type = self._type[index]

        if re.match(r"\b((solid|bar(\_)?)?circular|round)\b", shape_type, re.IGNORECASE):
            return CircleBasic(name=shape_name, d=self._d[index], type=shape_type)

        elif re.match(r"\b((solid|bar(\_)?)?rectangle)\b", shape_type, re.IGNORECASE):
            return RectangleBasic(name=shape_name, depth=self._d[index], width=self._wb[index],
                                  type=shape_type)

        elif re.match(r"\b((solid|bar(\_)?)?trapeziod)\b", shape_type, re.IGNORECASE):
            c = abs(self._wt[index] - self._wb[index]) / 2.0
            return Trapeziod(name=shape_name, depth=self._d[index], width=self._wb[index],
                             a=self._wt[index], c=c, type=shape_type)

        else:
            raise Exception(f" section type {shape_type} not recognized")
    #
    #
    #def set_default(self):
    #    """ """
    #    self.cls._default = self.name
    #
    #@property
    #def height(self):
    #    """
    #    d : height of rectangular bar
    #    """
    #    return self.depth
    ##
    #@property
    #def d(self):
    #    """
    #    d : Section height of rectangular bar
    #    """
    #    return self.depth
    #@d.setter
    #def d(self, value):
    #    """
    #    d : Section height of rectangular bar
    #    """
    #    self.depth = value
    ##
    #@property
    #def w(self):
    #    """
    #    w : width of rectangular bar
    #    """
    #    return self.width
    #@w.setter
    #def w(self, value):
    #    """
    #    w : width of rectangular bar
    #    """
    #    self.width = value
    ##
    #@property
    #def wb(self):
    #    """
    #    wb : width bottom of rectangular bar
    #    """
    #    return self.width
    #
    #@wb.setter
    #def wb(self, value):
    #    """
    #    wb : width bottom of rectangular bar
    #    """
    #    self.width = value
    ##
    #@property
    #def wt(self):
    #    """
    #    wt : width top of rectangular bar
    #    """
    #    return self.a
    #
    #@wt.setter
    #def wt(self, value):
    #    """
    #    wt : width top of rectangular bar
    #    """
    #    self.a = value
    #
#
#
@dataclass
class RectangleBasic(ShapeBasic):
    """
    Calculate the section properties of a rectangular solid section\n

+   +-----+
    |     |
d   |     |   Z
    |     |   ^
+   +-----+   + > Y
    *  w  *

    Parameters
    ----------
    d : Height
    w : Width

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
    Cw  : Warping constant = None

    Notes
    ----------
    Uses formulas from:
    1.- Formulas for stress, strain and strucutral matrices [W.D. Pilkey]
    2.- Roark's formulas for stress and strain [7th Edition]
    3.- Wikipedia

    Examples
    ----------

    """
    #name: str | int
    depth:float
    width:float
    type: str
    #
    #
    #
    def _properties(self):
        """
        """
        #-------------------------------------------------
        #   Cross-Sectional Area
        area = self.width * self.depth
        #-------------------------------------------------
        #   Elastic Neutral Centre 
        Zc = self.depth / 2.0
        Yc = 0 * self.depth
        #-------------------------------------------------
        #   Plastic Neutral Centre 
        _Zp = 0
        _Yp = 0
        #-------------------------------------------------
        #   Shear Centre 
        SCz = 0 * self.depth
        SCy = 0 * self.width
        #-------------------------------------------------
        #   Warping Constant Cw
        Cw = 0
        #-------------------------------------------------
        #               Section Properties
        #-------------------------------------------------
        #   Second Moment of Area about Mayor Axis
        Iy = self.width*self.depth**3 / 12.0
        #   Elastic Modulus about Mayor Axis
        Zey = self.width*self.depth**2 / 6.0
        #   Plastic Modulus about Mayor Axis
        Zpy = self.width*self.depth**2 / 4.0
        #   Shape Factor
        SFy = 1.50
        #   Radius of gyration about Mayor Axis
        ry = self.depth / 12**0.50
        #-------------------------------------------------
        #   Second Moment of Area about Minor Axis
        Iz = self.width**3 *self.depth / 12.0
        #   Elastic Modulus about Minor Axis
        Zez = self.width**2 *self.depth / 6.0
        #   Plastic Modulus about Minor Axis
        Zpz = self.width**2 *self.depth / 4.0
        #   Shape Factor
        SFz = 1.50
        #   Radius of gyration about Minor Axis 
        rz = self.width / 12**0.50
        #-------------------------------------------------
        #   Torsional Constant
        if self.depth == self.width:
            J = 0.1406 * self.depth**4
            # Polar area section module
            Zej = 0.208 * self.depth**3
        else:
            J = ((self.depth**3 * self.width / 3.0) 
                 * (1 - (0.630 *  self.depth / self.width) 
                    + (0.052 * self.depth**5 / self.width**5)))
            # Polar area section module
            Zej = (self.depth**2 * self.width
                   / (3 + 1.8 * self.depth / self.width))
            if self.depth > self.width:
                J = ((self.depth * self.width**3 / 3.0) 
                     * (1 - (0.630 * self.width / self.depth) 
                        + (0.052 * self.width**5 / self.depth**5)))
                # Polar area section module
                Zej = (self.depth * self.width**2
                       / (3 + 1.8 * self.width / self.depth))
        #
        #   Product of inertia
        _Iyz = 0.0
        Jx = self.width*self.depth*(self.width**2 + self.depth**2) / 12.0
        rp = ((self.width**2 + self.depth**2) / 12.0)**0.50
        #
        #-------------------------------------------------
        #self._get_section_coordinates()        
        #
        #-------------------------------------------------
        #
        return ShapeProperty(area=area, Zc=Zc, Yc=Yc,
                             Iy=Iy, Zey=Zey, Zpy=Zpy, ry=ry,
                             Iz=Iz, Zez=Zez, Zpz=Zpz, rz=rz,
                             J=J, Cw=Cw)
    #
    def shear_stress(self, Vz: float=1.0, Vy: float=1.0,
                     stress_type: str='average'):
        """
        """
        #-------------------------------------------------        
        #            Shear Stress Calculation
        #
        # get section's coordinates
        coord =  self.section_coordinates()
        #coord_y = self.section_coordinates.y # lateral
        #coord_z = self.section_coordinates.z # vertical
        prop = self.properties()
        # Area of Web
        #
        tau_z = Vz / prop.area
        tau_y = Vy / prop.area
        #
        #
        if stress_type != 'average':
            # Shape factor (section 8.10 roakrs 7ed)
            #_alpha = 3.0 / 2.0  
            #tau_z *= _alpha
            #tau_y *= _alpha
            #
            qz = [0.50 * (self.depth**2 / 4 - _z**2) * self.width 
                  for _z in coord.z]
            qy = [0.50 * (self.width**2 / 4 - _y**2) * self.depth 
                  for _y in coord.y]
            #
            tau_y = [_qy * Vy / (prop.Iz * self.depth) for _qy in qy]
            tau_z = [_qz * Vz / (prop.Iy * self.width) for _qz in qz]
            #
        else:
            tau_y = [tau_y for _ in coord.y]
            tau_z = [tau_z for _ in coord.z]

        return tau_y, tau_z
    #
    def torsional_stress(self, T):
        """
        Roark Torsion chapter
        """
        coord =  self.section_coordinates()
        d =  self.depth
        w =  self.width
        a = w / 2.0
        b = d / 2.0
        if a == b:
            K = 2.25 * a**4
            tau_max = 0.601 * T / a**3
        elif a > b :
            K = (a * b**3 * (16/3.0 - 3.36 * b / a 
                               * (1.0 - b**4 / (12.0 * a**4))))
            
            tau_max = ((3 * T * (1.0 + 0.6095 * b / a 
                                 + 0.8865 * (b / a)**3
                                 + 0.91 * (b / a)**4))
                       / (8 * a * b**2))
        else:
            raise ValueError(' section not applicable')
        
        tau_x = [tau_max for item in coord.y]
        return tau_x
    #
    #
    def curved(self, R):
        """
        ---------
        R = Radio
        """
        # shear area
        #warea = self.area
        #
        d = self.depth
        b = self.width
        # extreme fibre distances c
        c = d/2.0
        # 
        c1 = d - c
        #self.c1 = c1
        #
        # centroidal radius
        #_R = R
        #_R = R - c1
        #self.R = R
        # Shift of neutral axis from neutral axis
        e = c * (R/c - 2.0 / math.log((R/c + 1.0) / (R/c - 1.0)))
        #self.e = e
        # where
        #
        # Correction factors
        # stress factors Ki
        ki = (1.0 / (3.0*e / c) 
              * ((1.0 - (e / c)) / ((R / c) - 1.0)))
        # stress factors Ko
        ko = (1.0 / (3.0*e / c) 
              * ((1.0 + (e / c)) / ((R / c) + 1.0)))
    
        # Modulus of rigidity factor (section 8.10)
        F = 3/2
        
        #self.tau_y, self.tau_z = self.shear_stress(stress_type='true')
        return c, c1, e, ki, ko, F
    #
    def print_file(self, file_name):
        
        check_out = print_header()
        
        check_out.append("{:23s} {:>19} {:1.4E} {:>9} {:1.4E}\n"
                         .format(self.type, "", self.depth, "", self.width))
        
        check_out.extend(print_properties(self))
        
        #file_checkout = split_file_name(file_name)
        #file_checkout = str(file_checkout[0]) +'_check_me.txt'
        file_checkout = str(file_name) + '.txt'
        add_out = open(file_checkout,'w')
        add_out.write("".join(check_out))
        add_out.close()        
        print('ok')
    #
    #
    def _shape(self):
        """
        """
        _section = []
        _section.append("+   +-----+{:35s}{:1.3E} {:1.3E}\n"
                        .format("", self.d.convert('millimetre').value, 
                                self.w.convert('millimetre').value))
        _section.append("    |     |\n")
        _section.append("d   |     |   Z\n")
        _section.append("    |     |   ^\n")
        _section.append("+   +-----+   + > Y\n")
        _section.append("    +  w  +\n")
        return _section
    #
    # 
    def _stress(self, actions, stress=None,
                stress_type:str = 'average'):
        """
        """
        # get section's coordinates
        coord =  self.section_coordinates()
        prop = self.properties()
        #
        tau_y, tau_z = self.shear_stress(actions.Fz, actions.Fy, 
                                           stress_type=stress_type)
        #
        #a =  self.width
        #b =  self.depth
        #if a < b:
        #    b =  self.width
        #    a =  self.depth            
        #tau_x = [3 * actions.Mx / (a * b**2 * (1 - 0.63 * b / a + 0.25 * b**2 / a**2))
        #         for _ in coord.y]
        tau_x = self.torsional_stress(T=actions.Mx)
        #
        sigma_x = [actions.Fx / prop.area for _ in coord.y]
        sigma_y = [actions.My * _coord / prop.Iy for _coord in coord.z]
        sigma_z = [actions.Mz * _coord / prop.Iz for _coord in coord.y]
        #
        #
        stress_out = BeamStress(sigma_x, sigma_y, sigma_z, 
                                tau_x, tau_y, tau_z, coord)        
        #
        if stress:
            stress_out = self.add_stress(stress=stress, other=stress_out)
        #
        return stress_out
    #
    def section_coordinates(self):
        """
        1    2     3
        +----+-----+
        |    :     |       ^ z
        |    :     |       |
      4 +    + 5   + 6     +--> y
        |    :     |
        |    :     | 
        +----+-----+      
        7    8     9
        """
        # horizontal
        _width = self.width * 0.50
        coord_y = [-1 * _width, 0 * _width, _width, 
                   -1 * _width, 0 * _width, _width, 
                   -1 * _width, 0 * _width, _width]
        # vertical
        _h = self.depth * 0.50
        coord_z = [_h , _h , _h, 
                   0 * _h, 0 * _h, 0 * _h, 
                   -1 * _h, -1 * _h, -1 * _h]
        
        return points(coord_y, coord_z)
        #print('ok')
    #
    def _dimension(self) -> str:
        """ """
        return  ("{:32s}{:1.4E} {:1.4E} {:1.4E}\n"
                 .format(self.type, self.depth, self.width, self.width))
    #
    #
    #
#
@dataclass
class CircleBasic(ShapeBasic):
    """
    Calculate the section properties of a circular solid section\n
    
    Parameters
    ----------
    d : Diameter

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
    """
    #name: str | int
    d:float
    type: str = 'Circular'
    #
    #
    def _properties(self):
        """
        """
        #
        #self.depth *= factors[0]
        R = 0.50 * self.d
        #-------------------------------------------------
        #   Cross-Sectional Area
        area = math.pi * R**2
        #-------------------------------------------------
        #   Elastic Neutral Centre 
        Zc = self.d / 2.0
        Yc = 0
        #-------------------------------------------------
        #   Shear Centre 
        _SCz = 0
        _SCy = 0
        #-------------------------------------------------
        #   Warping Constant Cw
        Cw = 0
        #-------------------------------------------------
        #               Section Properties
        #-------------------------------------------------
        #   Second Moment of Area about Mayor Axis
        Iy = math.pi * R**4 / 4.0
        #   Elastic Modulus about Mayor Axis
        Zey = math.pi * R**3 / 4.0
        #   Plastic Modulus about Mayor Axis
        Zpy = 4 * math.pi * R**3 / 3.0
        #   Shape Factor
        SFy = 1.698
        #   Radius of gyration about Mayor Axis
        ry = R / 2.0
        #-------------------------------------------------
        #   Second Moment of Area about Minor Axis
        Iz = math.pi * R**4 / 4.0
        #   Elastic Modulus about Minor Axis
        Zez = math.pi * R**3 / 4.0
        #   Plastic Modulus about Minor Axis
        Zpz = 4 * math.pi * R**3 / 3.0
        #   Shape Factor
        SFz = 1.698
        #   Radius of gyration about Minor Axis 
        rz = R / 2.0
        #-------------------------------------------------
        #   Torsional Constant
        J = math.pi * self.d**4 / 32.0
        #   Product of inertia
        _Iyz = 0.0
        Jx = Iy + Iz
        rp = self.d / math.sqrt(8.0)
        #
        return ShapeProperty(area=area, Zc=Zc, Yc=Yc,
                           Iy=Iy, Zey=Zey, Zpy=Zpy, ry=ry,
                           Iz=Iz, Zez=Zez, Zpz=Zpz, rz=rz,
                           J=J, Cw=Cw)
    #
    def shear_stress(self, Vy, Vz,
                     stress_type:str ='average'):
        """
        """
        #-------------------------------------------------        
        #            Shear Stress Calculation
        #
        prop = self.properties()
        coord =  self.section_coordinates()
        #
        tau_z = [Vz / prop.area for item in coord.z]
        #
        tau_y = [Vy / prop.area for item in coord.y]
        #
        if stress_type != 'average':
            # Shape factor (section 8.10 roakrs 7ed)
            _alpha = 4.0 / 3.0            
            tau_z = tau_z * _alpha
            tau_y = tau_y * _alpha
        #
        return tau_y, tau_z
    #
    def torsional_stress(self, T):
        """
        """
        coord =  self.section_coordinates()
        r = self.d / 2.0
        K = math.pi * r**4
        tau_max = 2 * T / (math.pi * r**3)
        
        tau_x = [tau_max for item in coord.y]
        return tau_x
    #
    def curved(self, R):
        """
        ---------
        R = Radio
        """
        # shear area
        warea = self.area
        D = self.depth
    
        # extreme fibre distances c
        c = D/2.0
    
        c1 = D - c
    
        # centroidal radius
        #_R = R
        #_R = R - c1
    
        # Shift of neutral axis from neutral axis
        e = c*(((R/c) - math.sqrt((R/c)**2 - 1.0))/2.0)
    
        # where
        _Ic = self.Iy
    
        # stress factors Ki
        self.ki = ((1.0 / (4.0*e / c)) * 
                   ((1.0 - (e / c)) / ((R / c) - 1.0)))
    
        # stress factors Ko
        self.ko = ((1.0 / (4.0*e / c)) * 
                   ((1.0 + (e / c)) / ((R / c) + 1.0)))
    
        # Modulus of rigidity factor (section 8.10)
        self.F = 4/3
    #
    def _stress(self, actions, stress=None, stress_type: str='average'):
        """
        """
        # get section's coordinates
        coord =  self.section_coordinates()
        prop = self.properties()
        #
        tau_y, tau_z = self.shear_stress(Vz=actions.Fz, Vy=actions.Fy, 
                                         stress_type=stress_type)
        #           
        tau_x = self.torsional_stress(T=actions.Mx)
        #
        sigma_x = [actions.Fx / prop.area for _ in coord.y]
        sigma_y = [actions.My * _coord / prop.Iy for _coord in coord.z]
        sigma_z = [actions.Mz * _coord / prop.Iz for _coord in coord.y]
        #
        stress_out = BeamStress(sigma_x, sigma_y, sigma_z, 
                                tau_x, tau_y, tau_z, coord)        
        #
        if stress:
            stress_out = self.add_stress(stress=stress, other=stress_out)
        #
        return stress_out
    #
    def section_coordinates(self, theta: float = 90, steps: int = 2):
        """
        """
        # horizontal
        radius = self.d * 0.50
        arc_length = radius * math.tau * theta / 360
        sinc = arc_length / steps
        r_theta = 360 * sinc / (radius * math.tau)
        coord_1 = []        
        #
        for i in range(steps + 1):
            rad = math.radians(i * r_theta)
            _x, _z = self._circunference_line(x=rad, r=radius)
            coord_1.append(_x)
            #coord_2.append(_z)
        #
        coordx = list(reversed(coord_1))
        coordx.extend([-item for item in coord_1[1:]])
        coordx.extend(list(reversed(coordx[1:-1])))
        #
        coordz = coord_1.copy()
        coordz.extend(list(reversed(coord_1[:steps])))
        coordz.extend([-item for item in coordz[1:-1]])
        return points(coordx, coordz)   
    #
    def _circunference_line(self, x: float, r: float, xp1: float = 0, yp1: float = 0):
        """
        Calculating the coordinates of a point on a circles
        circumference from the radius, an origin and the
        arc between the points
        """
        xp2 = xp1 + r * math.sin(x)
        yp2 = yp1 - r * (1 - math.cos(x))
        return xp2, yp2    
    #
    def _dimension(self) -> str:
        """ """
        return  ("{:9s} {:1.4E}\n"
                 .format(self.type, self.d))
    #
    #
    #
    #
#
@dataclass
class Trapeziod(ShapeBasic):
    """
    Calculate the section properties of a trapezoidal solid section\n  
    
        | c |  wt  |
    +   +   +------+
           *        *
    d     *          *     Z
         *            *    ^
    +   +--------------+   + > Y
        |      wb      |
    
    Parameters
    ----------
    d  : Section height
    wb : Width bottom
    wt : Width top (default Wb)
    c  : (default [wb-wt]/2)

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
    """
    #name: str | int
    depth:float
    width:float
    a:float
    c:float
    type: str
    #
    #
    def _properties(self):
        """
        :return:
        area : Geometric Area (m^2)
        Zc : Z Distance to Centroid (m)
        Yc : Y Distance to Centroid (m)
        Iy : Second moment of area (m^4)
        Zey : Elastic Section Modulus (m^3)
        Zpy : Plastic Section Modulus (m^3)
        ry : Radius of gyration (m)
        Iz : Second moment of area (m^4)
        Zez : Elastic Section Modulus (m^3)
        Zpz : Plastic Section Modulus (m^3)
        rz : Radius of gyration (m)
        J : Torsional Constant (m^4)
        Cw : Warping Constant ()
        """
        # get geometry
        try:
            depth = self.depth.value
            width = self.width.value
            a = self.a.value
            c = self.c.value
            try:
                a = self.a.value
            except AttributeError:
                a = width
            try:
                c = self.c.value
            except AttributeError:
                c = abs(a - width) / 2.0            
        except AttributeError:
            depth = self.depth
            width = self.width
            try:
                a = self.a
            except AttributeError:
                a = width
            try:
                c = self.c
            except AttributeError:
                c = abs(a - width) / 2.0
        #-------------------------------------------------
        #   Cross-Sectional Area
        area = depth * (a + width) / 2.0
        #-------------------------------------------------
        #   Elastic Neutral Centre 
        Zc = (depth / 3.0 * ((2 * a + width) / (a + width)))

        Yc = ((2*width**2 + 2*a*width - c*width
               - 2*c*a - a**2)/(3*(a + width)))
        #
        #self.Yc = ((2*width**2 + 2*a*width - c*width
        #            - 2*c*a - a**2)/(3*(a + width)))
        #-------------------------------------------------
        #   Plastic Neutral Centre 
        #_Zp = 'N/A'
        #_Yp = 'N/A'
        
        #-------------------------------------------------
        #   Shear Centre 
        #_SCz = 'N/A'
        #_SCy = 'N/A'
        
        #-------------------------------------------------
        #   Warping Constant Cw
        Cw = 0 * width
        #-------------------------------------------------
        #               Section Properties
        #-------------------------------------------------
        #   Second Moment of Area about Mayor Axis
        #
        Iy = (depth**3 / (36.0 * (a + width))
              * (a**2 + 4 * a * width + width**2))
        #   Elastic Modulus about Mayor Axis
        Zey = Iy / Zc
        #   Plastic Modulus about Mayor Axis
        #Zpy = (depth**2/(12*(a + width))
        #       * (a**2 + 4* a*width + width**2))
        factor = 2
        if c != 0:
            factor = 1
        Zpy = (depth**2/(48*factor * (a + width))
               * (11* a**2 + 26*a * width + 11*width**2))
        #   Shape Factor
        #_SFy = 'N/A'
        #   Radius of gyration about Mayor Axis
        ry = (Iy / area)**0.50
        #-------------------------------------------------
        #   Second Moment of Area about Minor Axis
        Iz = (depth/(36*(a + width))
              * (4*a*width*c**2 + 3*a**2 *width*c
                 - 3*a*width**2 *c + a**4 + width**4
                 + 2*a**3 *width + a**2 * c**2 + a**3 * c
                 + 2*a*width**3 - c*width**3 + width**2 * c**2))
       #
       #self.Iz = (depth / (36 * (a + width))
       #            * (width**4 + a**4 + 2*width*a * (width**2 + a**2))
       #            - c * (width**3 + 3*width**2 * a
       #                        - 3*width*a**2 - a**3)
       #            + c**2 * (width**2 + 4*width*a + a**2))
       #
        #   Elastic Modulus about Minor Axis
        Zez = Iz / Yc
        #   Plastic Modulus about Minor Axis
        Zpz = (depth/12 * (2*a**2 - a*width + 2*width**2))
        #   Shape Factor
        #_SFz = 'N/A'
        #   Radius of gyration about Minor Axis 
        rz = (Iz / area)**0.50
        #
        #-------------------------------------------------
        #   Torsional Constant
        if c == 0:
            if depth == width:
                J = 0.1406 * depth ** 4
                # Polar area section module
                Zej = 0.208 * depth ** 3
            else:
                J = ((depth ** 3 * width / 3.0) *
                     (1 - (0.630 * depth / width) +
                      (0.052 * depth ** 5 / width ** 5)))
                # Polar area section module
                Zej = (depth ** 2 * width
                       / (3 + 1.8 * depth / width))
                if depth > width:
                    J = ((depth * width ** 3 / 3.0) *
                         (1 - (0.630 * width / depth) +
                          (0.052 * width ** 5 / depth ** 5)))
                    # Polar area section module
                    Zej = (depth * width ** 2
                           / (3 + 1.8 * width / depth))
        else:
            s = abs(width-a) / depth
            Vl = 0.10504 - 0.10 * s + 0.0848 * s**2 - 0.06746 * s**3 + 0.0515 * s**4
            Vs = 0.10504 + 0.10 * s + 0.0848 * s**2 + 0.06746 * s**3 + 0.0515 * s**4
            J = (width/12.0 * (a + width) * (a**2 + width**2)
                 - Vl * width**4 - Vs * a**4)
        #
        #-------------------------------------------------
        #   Product of inertia
        Jx = Iy + Iz
        rp = (Jx / area)**0.50
        #
        # reset Yc to the centre of the section
        #self.Yc =  Yc # - max(a, width) * 0.50
        #self.Zc =  Zc
        #
        #
        return ShapeProperty(area=area, Zc=Zc, Yc=Yc,
                           Iy=Iy, Zey=Zey, Zpy=Zpy, ry=ry,
                           Iz=Iz, Zez=Zez, Zpz=Zpz, rz=rz,
                           J=J, Cw=Cw)
    #
    def shear_stress(self, Vz=1.0, Vy=1.0, stress_type='average'):
        """
        """
        #-------------------------------------------------
        #            Shear Stress Calculation
        #
        # get section's coordinates
        coord_y = self.section_coordinates.y # lateral
        coord_z = self.section_coordinates.z # vertical
        # Area of Web
        # The overall depth times the web thickness
        #self.Aw = self.area
        #
        # Area of Flange
        #self.Af = self.area
        #
        tau_z = Vz / self.area
        tau_y = Vy / self.area
        #
        #
        if stress_type != 'average':
            # Shape factor (section 8.10 roakrs 7ed)
            #_alpha = 3.0 / 2.0
            #tau_z *= _alpha
            #tau_y *= _alpha
            #
            qz = [0.50 * (self.depth**2 / 4 - _z**2) * self.width
                  for _z in coord_z]
            qy = [0.50 * (self.width**2 / 4 - _y**2) * self.depth
                  for _y in coord_y]
            #
            tau_y = [_qy * Vy / (self.Iz * self.depth) for _qy in qy]
            tau_z = [_qz * Vz / (self.Iy * self.width) for _qz in qz]
            #
        else:
            tau_y = [tau_y for _ in coord_y]
            tau_z = [tau_z for _ in coord_z]

        return tau_y, tau_z
    #
    def curved(self, R:Union[Units, float]):
        """
        ---------
        R: radius of curvature measured to centroid of section
        c : distance from centroidal axis to extreme fiber on concave side of beam

        :return:
        e: distance from centroidal axis to neutral axis measured toward center of curvature
        ki: sigma_i/sigma
        k0: sigma_0/sigma
        sigma_i = actual stress in extreme fiber on concave side
        sigma_0 = actual stress in extreme fiber on convex side
        sigma = fictitious unit stress in corresponding fiber as computed by ordinary flexure formula for a straight beam
        """
        b1 = self.a
        b = self.width
        d = self.depth
        # Distance from centroidal axis to extre fiber on concave side of beam
        c = d / (3*(1+b1/b)/(1+2*b1/b))
        # distance from centroidal axis to extreme fiber on convex side of beam
        c1 = c * (d/c - 1.0)
        # Conditions
        # if R/d >= 8 then the beam should be considered thin
        # if R/d < 8 then the beam should be considered thick
        thin = 1
        thick = 0
        if R / d < 8:
            thin = 0
            thick = 1.0
        #
        area = d/2 * (b + b1)
        Ic = d**3/36 * (b**2 + 4*b*b1 + b1**2)/ (b+b1)
        #
        # distance from centroidal axis to neutral axis measured towards centre of curvature
        h = ((R - c * (0.50*(1+b1/b)*(d/c)**2)
                 /((R/c + c1/c - b1/b*(R/c - 1.0)))*math.log((R/c + c1/c)/(R/c - 1.0))
                 - (1.0 - b1/b)*d/c) * thick + Ic/(R*area) * thin)
        # stress factors Ki
        ki = (1.0/(2*h/c) * (1.0 - h/c)/(R/c - 1.0)
              * (1.0 + 4*b1/b + (b1/b)**2)/(1.0+b1/b)**2)
        # stress factors Ko
        k0 = ((c1/c)/(2*h/c) * (c1/c + h/c)/(R/c + c1/c)
              * (1 + 4*b1/b + (b1/b)**2)/(2 + b1/b)**2)
        #
        # Modulus of rigidity factor (section 8.1)
        F = 3/2
        return ki, k0

    #
    #
    #
    #
    def _get_section_table(self) -> tuple:
        """
        """
        project = (self.name, None, self.type,
                   None, None,
                   self.height.value, None,
                   self.width.value, None,
                   self.a.value, None,)
        return project
    #
    #def __str__(self) -> str:
    #    """ """
    #    output = "{:<12s} {:>19} {:1.4E}\n".format("Bar", "", self.depth)
    #    output += "{:12s}{:<12s}{:40s} {:1.4E}\n".format("", self.type.capitalize(), "", self.a)
    #    output += "{:>64} {:1.4E}\n".format("", self.width)       
    #    #return  ("{:23s} {:1.4E} {:1.4E}"
    #    #         .format(self.type, self.d, self.t))
    #    return output
    #
    def _dimension(self) -> str:
        """ Print section dimensions"""
        output = "{:<8s}{:>24}{:1.4e} {:1.4e} {:1.4e}\n"\
            .format("BarTrapz", " ", self.depth, self.a, self.width)
        #output += "{:15s}{:<8s}\n".format("", "Trapzd")
        #output += "{:67} {:1.4e}\n".format("", self.width)
        return output
    #
    #def _properties(self) -> str:
    #    """ Print section properties"""
    #    output = "{:<12s} {:>19} {:1.4E}\n".format("Bar", "", self.depth)
    #    output += "{:12s}{:<12s}{:40s} {:1.4E}\n".format("", self.type.capitalize(), "", self.a)
    #    output += "{:>64} {:1.4E}\n".format("", self.width)
    #    return output
    #
    #

#
#
#
#