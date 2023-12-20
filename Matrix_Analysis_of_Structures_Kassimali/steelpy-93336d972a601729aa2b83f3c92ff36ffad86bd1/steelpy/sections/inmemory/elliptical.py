# 
# Copyright (c) 2019-2023 steelpy
#

# Python stdlib imports
from __future__ import annotations
import math
#

# package imports
from .operations import SectionBasic


# ----------------------------------------
#      Elliptical Sections Profiles
# ----------------------------------------
#
class HollowSemiellipse(SectionBasic):
    """
    Calculate the section properties of a Hollow Semiellipse
    with constant wall thickness Tw.
    The midthickness perimeter is an ellipse 0.2 < a/b < 0.50

    Parameters
    ----------
    d  : Section Heigh
    b  : Base
    tw : Wall thickness

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
    1.- Formulas for stress, strain and strucutral matrices [W.D. Pilkey]
    2.- Roark's formulas for stress and strain [7th Edition]
    3.- Wikipedia

    Examples
    ----------

    """
    #
    def __init__(self):
        """
        """
        super().__init__()
    #
    def geometry(self, **kwargs):
        
        for key, value in kwargs.items():
            _dim = find_section_dimensions(key)
            get_dimension(self, _dim, value)
        self.type = 'Hollow Semiellipse'
    #
    # 
    def _property(self):
        """ """
        #
        
        #self.d *= factors[0]
        #self.tw *= factors[0]
        
        #self.a *= factors[0]
        #self.ta *= factors[0]
        
        #self.b *= factors[0]
        #self.tb *= factors[0]
        #
        _a = self.d - 0.50 * self.tw
        _b = self.b / 2.0 - 0.50 * self.tw
        
        # Note : there is a limit on the maximum 
        # wall thickness allowed in this case.
        # Cusps will form in the perimeter at
        # the ends of the mayor axis if this 
        # maximum is exceeded.
        if _a/_b < 1.0 :
            _tmax = 2 * _a**2 / _b
        
        else: _tmax = 2 * _b**2 / _a
        
        if self.tw > _tmax :
            sys.exit('error : t > tmax')
             
        #-------------------------------------------------
        #   Cross-Sectional Area
        _C = (_a - _b) / (_a + _b)
        _K1 = 0.2464 + 0.002222 * ((_a / _b) + (_b / _a))
        _K2 = 1 - 0.3314 * _C + 0.0136 * _C**2 + 0.1097 * _C**3
        _K3 = 1 + 0.9929 * _C - 0.2287 * _C**2 - 0.2193 * _C**3
        
        self.area = ((self.tw * math.pi / 2.0) * 
                     (_a + _b) * (1.0 + _K1 * ((_a - _b) / (_a + _b))**2))
        
        # Centroid
        self.Zc = ((2.0 * _a * _K2 / math.pi) 
                   + (self.tw**2 * _K3 / (6.0 * math.pi * _a)))
        
        _Zc1 = _a + self.tw / 2.0 - self.Zc
        
        self.Yc = 0
        _Yc1 = _b + self.tw / 2.0
        
        #-------------------------------------------------
        #               Section Properties
        #-------------------------------------------------
        
        #   Second Moment of Area about Mayor Axis
        #   --------------------------------------
        _K4 = 0.1349 + 0.1279 * (_a / _b) - 0.01284 * (_a / _b)**2
        _K5 = 0.1349 + 0.1279 * (_a / _b) - 0.01284 * (_b / _a)**2
        
        _Iy = ((((self.tw * _a**2 * math.pi / 8.0) * (_a + 3 * _b)) 
                * (1 + _K4 * ((_a - _b) / (_a + _b))**2))
               + (((self.tw**3 * math.pi / 32.0) * (3 * _a + _b)) 
                * (1 + _K5 * ((_a - _b) / (_a + _b))**2)))
        
        self.Iy = _Iy - self.area * self.Zc**2
        
        
        _K2 = 0.1349 + 0.1279 * (_b / _a) - 0.01284 * (_b / _a)**2
        _K3 = 0.1349 + 0.1279 * (_a / _b) - 0.01284 * (_a / _b)**2
        
        self.Iz = 0.50 * ((((self.tw * _b**2 * math.pi / 4.0) * (_b + 3 * _a)) 
                           * (1 + _K2 * ((_b - _a) / (_b + _a))**2))
                          + (((self.tw**3 * math.pi / 16.0) * (3 * _b + _a)) 
                           * (1 + _K3 * ((_b - _a) / (_b + _a))**2)))
        
        
        #   Elastic Modulus about Mayor Axis
        #   --------------------------------------
        self.Zey = self.Iy / _Zc1
        
        # 
        self.Zez = self.Iz / _Yc1               
        
        #   Plastic Modulus about Mayor Axis
        #   --------------------------------------
        # Let Zp be the vertical distance  from  the bottom
        # to the plastic neutral axis
        _DD = self.tw / _tmax
        _DD = max(_DD , 0.20)
        _DD = min(_DD , 1.0)
        
        if _a / _b > 0.25 and _a / _b < 1.0:
            _C1 = 0.5067 - 0.5588 * _DD + 1.3820 * _DD**2
            _C2 = 0.3731 + 0.1938 * _DD - 1.4078 * _DD**2
            _C3 = -0.140 + 0.0179 * _DD + 0.4885 * _DD**2
            _C4 = 0.0170 - 0.0079 * _DD - 0.0565 * _DD**2
            #
            _C5 = -0.0292 + 0.3749 * math.sqrt(_DD) + 0.0578 * _DD
            _C6 = 0.36740 - 0.8531 * math.sqrt(_DD) + 0.3882 * _DD
            _C7 = -0.1218 + 0.3563 * math.sqrt(_DD) - 0.1803 * _DD
            _C8 = 0.01540 - 0.0448 * math.sqrt(_DD) + 0.0233 * _DD
        #
        elif _a / _b >= 1.0 and _a / _b < 4.0:
            _C1 = 0.4829 + 0.0725 * _DD - 0.1815 * _DD**2
            _C2 = 0.1957 - 0.6608 * _DD + 1.4222 * _DD**2
            _C3 = 0.0203 + 1.8999 * _DD - 3.4356 * _DD**2
            _C4 = 0.0578 - 1.6666 * _DD + 2.6012 * _DD**2
            #
            _C5 = 0.22410 - 0.3922 * math.sqrt(_DD) + 0.2960 * _DD
            _C6 = -0.6637 + 2.7357 * math.sqrt(_DD) - 2.0482 * _DD
            _C7 = 1.52110 - 5.3864 * math.sqrt(_DD) + 3.9286 * _DD
            _C8 = -0.8498 + 2.8763 * math.sqrt(_DD) - 1.8874 * _DD
        #
        else : 
            sys.exit('error a/b > 4 or a/b < 0.25')
        
        # Plastic neutral axis
        _Zp = (_a * (_C1 + _C2 / (_a / _b) + _C3 / (_a / _b)**2 
                     + _C4 / (_a / _b)**3))
        
        _Yp = 0
        
        # Plastic section moduli mayor axis
        self.Zpy = (4.0 * _a**2 * self.tw * (_C5 + _C6 / (_a / _b) 
                                             + _C7 / (_a / _b)**2 
                                             + _C8 / (_a / _b)**3))
        
        # Plastic section moduli minor axis
        _K4 = 0.1835 + 0.895 * (_b / _a) - 0.00978 * (_b / _a)**2
        
        self.Zpz = (0.50 * (((1.3333 * self.tw * _b * (_b + 2 * _a))
                             * (1 + _K4 * ((_b - _a) / (_a + _b))**2))
                             + (self.tw**3 / 3.0)))
        
        #-------------------------------------------------
        #   Radius of gyration
        self.ry = math.sqrt(self.Iy / self.area)
        self.rz = math.sqrt(self.Iz / self.area)
        #
        
        #return self.area, _Zc, _Yc, _Iy, _Zey, _Zpy, _ry, _Iz, _Zez, _Zpz, _rz, _Zp
    #
    def print_file(self, file_name):

        check_out = print_header()       

        check_out.append("{:23s} {:1.4E} {:1.4E}"
                         .format(self.type, self.d, self.tw))

        check_out.extend(print_properties(self))

        #file_checkout = split_file_name(file_name)
        #file_checkout = str(file_checkout[0]) +'_check_me.txt'
        file_checkout = str(file_name) + '.txt'
        add_out = open(file_checkout,'w')
        add_out.write("".join(check_out))
        add_out.close()
        print('ok')     
#
class EllipticalSegment(SectionBasic):
    """
    Calculate the circular and elliptical segments
    cross section properties
    
    Parameters
    ----------
    a      : Mayor Axis
    b      : Minor Axis
    thetaG : Angle (degrees)
    

    Returns
    ----------
    area: Section area
    Zc  : Elastic neutral centre
    Yc  : Elastic neutral centre
    
    Iy  : Second moment of area about mayor axis
    Zey : Elastic modulus about mayor axis
    ry  : Radius of gyration about mayor Axis
    
    Iz  : Second moment of area about minor axis
    Zez : Elastic modulus about minor axis
    rz  : Radius of gyration about minor Axis
    
    Notes
    ----------
    Uses formulas from:
    1.- Geometric properties for the design of unusual member
        cross-sections in bending [A.J. Sadowski]

    Examples
    ----------    
    """
    def __init__(self):
        """""
        """""
        super().__init__()

    #    
    def geometry(self, a, b, thetaG):
        #
        
        self.a = float(a)
        self.b = float(b)
        self.theta = float(thetaG)
        self.p = 0
        self.q = 0
        self.type = 'Elliptical Segment'
    
    def _property(self):
        """ """
        #
        _thetaG = math.radians(self.theta)
        _thetaG = min(abs(_thetaG), 0.50 * math.pi)
        
        # Area
        self.area = (0.50 * self.a * self.b 
                     * (2 * _thetaG - math.sin( 2 * _thetaG)))
        
        # Centroid
        self.Zc = ((4.0 * self.b * math.sin(_thetaG)**3) 
                  / (3.0 * (2 * _thetaG - math.sin(2 * _thetaG))))
        
        self.Yc = 0
        
        # Second Moment of Area about x
        self.Iy = ((self.a * self.b**3 / 16.0) 
                   * (4 * _thetaG - math.sin(4 * _thetaG)))
        
        # Second Moment of Area about y
        self.Iz = ((self.a**3 * self.b / 24.0) 
                   * (6.0 * _thetaG - math.sin(2 * _thetaG) 
                      * (3.0 + 2.0 * math.sin(_thetaG)**2)))
        
        # Second Moment of Area about the horizontal centroidal C
        self.Ic = self.Iy - self.area * self.Zc**2
        
        # The distances from the centroid to the extreme fibres
        _y1 = self.a * math.sin(_thetaG)
        _z1 = self.b - self.Zc
        _z2 = self.Zc - self.b * math.cos(_thetaG)
        
        # elastic section moduli
        self.Zey = min(self.Ic / _z1, self.Ic / _z2)
        self.Zez = self.Iz / _y1
        
        # plastic section moduli
        _Zpy = 0
        _Zpz = 0
        
        # radii of gyration
        self.ry = math.sqrt(self.Ic / self.area)
        self.rz = math.sqrt(self.Iz / self.area)
        #
        
        #return _Area, _Zc, _Yc, _Iy, _Zey, self.Ic, _ry, _Iz, _Zez, _Zpz, _rz
    # 
    def print_file(self, file_name):

        check_out = print_header_ellipse()       

        check_out.append("{:23s} {:1.4E}  {:1.4E}  {:1.4E}"
                         .format(self.type, self.a, self.b, self.theta))

        check_out.extend(print_properties(self))

        #file_checkout = split_file_name(file_name)
        #file_checkout = str(file_checkout[0]) +'_check_me.txt'
        file_checkout = str(file_name) + '.txt'
        add_out = open(file_checkout,'w')
        add_out.write("".join(check_out))
        add_out.close()
        print('ok')     
#
class EllipticalSector(SectionBasic):
    """
    Calculate the circular and elliptical sectors
    cross section properties
    
    Parameters
    ----------
    a      : Mayor Axis
    b      : Minor Axis
    thetaG : Angle (degrees)
    
    Returns
    ----------
    area: Section area
    Zc  : Elastic neutral centre
    Yc  : Elastic neutral centre
    
    Iy  : Second moment of area about mayor axis
    Zey : Elastic modulus about mayor axis
    ry  : Radius of gyration about mayor Axis
    
    Iz  : Second moment of area about minor axis
    Zez : Elastic modulus about minor axis
    rz  : Radius of gyration about minor Axis
    
    Notes
    ----------
    Uses formulas from:
    1.- Geometric properties for the design of unusual member
        cross-sections in bending [A.J. Sadowski]

    Examples
    ----------    
    """
    def __init__(self):
        """
        """
        super().__init__()
    #
    def geometry(self, a, b, thetaG):
        #
        self.a = float(a)
        self.b = float(b)
        self.theta = float(thetaG)
        self.p = 0
        self.q = 0
        self.type = 'Elliptical Sector'
    #
    def _property(self):
        """
        """
        _thetaG = math.radians(self.theta)
        _thetaG = min(_thetaG, 0.50 * math.pi)
        
        # Area
        self.area = self.a * self.b * _thetaG
        
        # Centroid
        self.Zc = (2 * self.b * math.sin(_thetaG)) / (3 * _thetaG)
        self.Yc = 0
        
        # Second Moment of Area about x
        self.Iy = ((self.a * self.b**3 / 8.0) 
                   * (2 * _thetaG + math.sin(2 * _thetaG)))
        
        # Second Moment of Area about y
        self.Iz = ((self.a**3 * self.b / 8.0) 
                   * (2 * _thetaG - math.sin(2 * _thetaG)))
        
        # Second Moment of Area about the horizontal centroidal C
        self.Ic = self.Iy - self.area * self.Zc**2
        
        # The distances from the centroid to the extreme fibres
        _y1 = self.a * math.sin(_thetaG)
        _z1 = self.b - self.Zc
        _z2 = self.Zc - self.b * math.cos(_thetaG)
        
        # elastic section moduli
        self.Zey = min(self.Ic / _z1, self.Ic / _z2)
        self.Zez = self.Iz / _y1
        
        # plastic section moduli
        _Zpy = 0
        _Zpz = 0
        
        # radii of gyration
        self.ry = math.sqrt(self.Ic / self.area)
        self.rz = math.sqrt(self.Iz / self.area)
        #
        #
        #return self.area, self.Zc, _Yc, self.Ic, _Zey, _Zpy, _ry, self.Iz, _Zez, _Zpz, _rz
    #
    def print_file(self, file_name):

        check_out = print_header_ellipse()       

        check_out.append("{:23s} {:1.4E}  {:1.4E}  {:1.4E}"
                         .format(self.type, self.a, self.b, self.theta))

        check_out.extend(print_properties(self))

        #file_checkout = split_file_name(file_name)
        #file_checkout = str(file_checkout[0]) +'_check_me.txt'
        file_checkout = str(file_name) + '.txt'
        add_out = open(file_checkout,'w')
        add_out.write("".join(check_out))
        add_out.close()
        print('ok')     
#
class SuperEllipse(SectionBasic):
    """
    Calculate the superellipse cross section properties
    Superellipses as a function of the powers p and q
    
    Parameters
    ----------
    a : Mayor Axis
    b : Minor Axis
    p : 
    q : 

    Returns
    ----------
    area: Section area
    Zc  : Elastic neutral centre
    Yc  : Elastic neutral centre
    
    Iy  : Second moment of area about mayor axis
    Zey : Elastic modulus about mayor axis
    ry  : Radius of gyration about mayor Axis
    
    Iz  : Second moment of area about minor axis
    Zez : Elastic modulus about minor axis
    rz  : Radius of gyration about minor Axis
    
    Notes
    ----------
    Uses formulas from:
    1.- Geometric properties for the design of unusual member
        cross-sections in bending [A.J. Sadowski]

    Examples
    ----------    
    """
    def __init__(self):
        """
        """
        super().__init__()
    #
    def geometry(self, a, b, p=2.0, q=2.0):
        # 
        
        self.a = float(a)
        self.b = float(b)
        self.theta = 90
        self.p = float(p)
        self.q = float(q)
        self.type = 'Super Ellipse'
    #
    def _property(self):
        """
        :return:
        """
        
        if self.p <= 0 or self.q <= 0:
            sys.exit("error p & q > 0")
        
        # Area
        self.area = ((2.0 * self.a * self.b / self.q) *
                     ((math.gamma(1.0 / self.q) * math.gamma((1.0 + self.p) / self.p))
                      / (math.gamma((self.p + self.p * self.q + self.q) / (self.p * self.q)))))
        
        # Centroid
        self.Zc = ((math.pow(4, 1.0 / self.q) * self.b / (2 * math.sqrt(math.pi))) 
                   * ((math.gamma((2.0 + self.q) / (2 * self.q)) 
                     * math.gamma((self.p + self.p * self.q + self.q) / (self.p * self.q)))
                    / (math.gamma((2 * self.p + self.p * self.q + self.q) / (self.p * self.q)))))
        self.Yc = 0
        
        # Second Moment of Area about x
        self.Iy = ((2.0 * self.a * self.b**3 / self.q) *
                   ((math.gamma(3.0 / self.q) * math.gamma((1.0 + self.p) / self.p))
                    / (math.gamma((3 * self.p + self.p * self.q + self.q) / (self.p * self.q)))))
        
        # Second Moment of Area about y
        self.Iz = ((2.0 * self.a**3 * self.b / self.p) *
                   ((math.gamma(3.0 / self.p) * math.gamma((1.0 + self.q) / self.q))
                    / (math.gamma((self.p + self.p * self.q + 3 * self.q) / (self.p * self.q)))))
        #print('Jy',_Iz / 10**4)
        
        # Second Moment of Area about the horizontal centroidal C
        self.Ic = self.Iy - self.area * self.Zc**2
        #print('Jx',self.Ic / 10**4)
        
        # The distances from the centroid to the extreme fibres
        _y1 = self.a 
        _z1 = self.b - self.Zc
        _z2 = self.Zc 
        
        # elastic section moduli
        self.Zey = min(self.Ic / _z1, self.Ic / _z2)
        self.Zez = self.Iz / _y1
        
        # plastic section moduli
        _Zpy = 0
        _Zpz = 0
        
        # radii of gyration
        self.ry = math.sqrt(self.Ic / self.area)
        self.rz = math.sqrt(self.Iz / self.area)
        #
        
        #return _Area, _Zc, _Yc, self.Ic, _Zey, _Zpy, _ry, _Iz, _Zez, _Zpz, _rz
    #   
    #
    def print_file(self, file_name):

        check_out = print_header_ellipse()       

        check_out.append("{:23s} {:1.4E}  {:1.4E}  {:1.4E}       {:1.4E}  {:1.4E}"
                         .format(self.type, self.a, self.b, self.theta, self.p, self.q))

        check_out.extend(print_properties(self))

        #file_checkout = split_file_name(file_name)
        #file_checkout = str(file_checkout[0]) +'_check_me.txt'
        file_checkout = str(file_name) + '.txt'
        add_out = open(file_checkout,'w')
        add_out.write("".join(check_out))
        add_out.close()
        print('ok')     
#
def quarterCircle(r):
    """
    Calculate a quarter of a circle
    
    Parameters
    ----------
    r : radius

    Returns
    ----------
    area: Section area
    Zc  : Elastic neutral centre
    Yc  : Elastic neutral centre
    
    Iy  : Second moment of area about mayor axis
    Zey : Elastic modulus about mayor axis
    ry  : Radius of gyration about mayor Axis
    
    Iz  : Second moment of area about minor axis
    Zez : Elastic modulus about minor axis
    rz  : Radius of gyration about minor Axis
    
    Notes
    ----------
    Uses formulas from:
    1.- Structural Engineering Formulas
        Ilya Mikelson

    Examples
    ----------    
    """
    
    # Area
    _Area = math.pi * r**2 / 4.0
    #
    # Centroid
    _Zc = 4 * r / (3 * math.pi)
    _Yc = _Zc
    
    # Second Moment of Area about x
    _Iy = 0.07135 * r**4
    _Iy1 = 0.05489 * r**4
    _Iy2 = math.pi * r**4 / 16.0
    
    # Second Moment of Area about y
    _Iz = 0.03843 * r**4
    _Iz1 = _Iy1
    _Iz2  = _Iy2
    
    return _Area, _Zc, _Yc, _Iy, _Iy1, _Iy2, _Iz, _Iz1, _Iz2
#
def closed_cross_section(a, b1, A1, Yc1, Ic1, Iy1, 
                      b2 = 0, A2 = 0, Yc2 = 0, 
                      Ic2 = 0, Iy2 = 0):
    """
    Elliptical Sections Profiles Extension
    Open cross-sections which are extended to half of the circumference
    (thetaG = 1/2pi) may be combined together to make a hollow
    closed cross-section with finite thickness t, e.g. a tube, hollow  
    rod, pipe or cylindrical shell,   
    """
    # check if section is symmetrical
    if b2 == 0:
        b2  = b1
        A2  = A1
        Yc2 = Yc1
        Ic2 = Ic1
        Iy2 = Iy1
    
    _d = b1 + b2
    
    # Total cross area
    _A = A1 + A2
    
    # Centroidal C-axis of full section
    _Yc = (A1 * (Yc1 + b2) + A2 * (b2 - Yc2)) / _A
    
    # Second moment of full area
    _Ixx = ((Ic1 + A1 * (Yc1 + b2 - _Yc)**2)
            + (Ic2 + A2 * (_Yc - b2 + Yc2)**2))
    
    _Iyy = Iy1 + Iy2
    
    # Extreme fibre distances
    _x1 = a
    _y1 = _d - _Yc
    _y2 = _Yc
    
    # Elastic section moduli
    _Sy = min(_Iyy / _y1, _Iyy / _y2)
    _Sx = _Ixx / _x1
    
    # radii of gyration
    _ry = math.sqrt(_Iyy / _A)
    _rx = math.sqrt(_Ixx / _A)
    
    #
    return _A, _Yc, _x1, _Ixx, _Sx, _rx, _Iyy, _Sy, _ry 
    #
#
def hollow_ellipse(a, b, t):
    """
    a
    b
    t
    """
    
    # Area
    K1 = 0.2464 + 0.002222 * (a/b + b/a)
    
    Area = math.pi * t * (a + b) * (1 + K1 * ((a-b)/(a+b))**2)
    
    # Centroid
    Zc = a + t / 2.0
    Yc = b + t / 2.0
    
    #   Second Moment of Area about Mayor Axis
    #   --------------------------------------
    K2 = 0.1349 + 0.1279 * a/b - 0.01284 * (a/b)**2
    K3 = 0.1349 + 0.1279 * b/a - 0.01284 * (b/a)**2
    
    Iy = (math.pi * t * a**2 / 4.0 * (a + 3*b) * (1 + K2 * ((a-b)/(a+b))**2)
          + math.pi * t**3 / 16.0 * (3*a + b) * (1 + K3 * ((a-b)/(a+b))**2))

    #   Second Moment of Area about Minor Axis
    #   --------------------------------------
    K2 = 0.1349 + 0.1279 * b/a - 0.01284 * (b/a)**2
    K3 = 0.1349 + 0.1279 * a/b - 0.01284 * (a/b)**2
    
    Iz = (math.pi * t * b**2 / 4.0 * (b + 3*a) * (1 + K2 * ((b-a)/(b+a))**2)
          + math.pi * t**3 / 16.0 * (3*b + a) * (1 + K3 * ((b-a)/(b+a))**2))
    
    #   Elastic Modulus about Mayor Axis
    #   --------------------------------------    
    K4 = 0.1835 + 0.895 * a/b - 0.00978 * (a/b)**2
    
    Zey = 1.3333 * t * a * (a + 2*b) * (1 + K4 * ((a-b)/(a+b))**2) + t**3 / 3.0
    
    #   Elastic Modulus about Minor Axis
    #   --------------------------------------
    
    K4 = 0.1835 + 0.895 * b/a - 0.00978 * (b/a)**2
    
    Zez = 1.3333 * t * b * (b + 2*a) * (1 + K4 * ((b-a)/(b+a))**2) + t**3 / 3.0
    
    return Area, Zc, Yc, Iy, Zey, Iz, Zez