# 
# Copyright (c) 2019-2023 steelpy
#

# Python stdlib imports
from __future__ import annotations
from array import array
from dataclasses import dataclass
from collections import namedtuple
import math
#import re
#

# package imports
from steelpy.sections.process.stress import BeamStress
from steelpy.sections.process.operations import ShapeProperty
from .operations import SectionBasic, ShapeBasic
#import numpy as np
#
#
#
points = namedtuple('Points', ['y', 'z'])
#
def radial_shear_factor(D: float, Tw: float, Tft: float, 
                        Tfb: float, c: float, c1: float, 
                        R: float, e: float) -> float:
    '''
    Radial/horizontal web shear stress factor calculation
    Ref Roark 7 ed chapter 9 : Shear stress due to the radial 
                               shear force V
    
    tr : thickness of the section normal to the plane of curvature
         at the radial position r
    
    '''  
    # total area of the web 
    Aw = (D - Tft - Tfb) * Tw  
    #
    # Average Shear
    tau_average = 1.0/Aw
    print(' ')
    print('shear average : {: 1.4E}'.format(tau_average))
    #
    # --------------------------------------------
    # Shear stress due to the radial shear force V
    rn = R -e             # radial position to neautral axis
    b = R - c - Tft      # radial position to inner extreme fiber
    tr = Tw   # width of cross section where stresses are calculated
    # fraction of the web dep
    dwo = (c1 - Tft - e) / 3.0 
    dwi = (-c + Tfb + e) / 3.0
    CoorZ = [(c1 - Tft), 2 * dwo + e, dwo + e, e, 0,
              -e, dwi -e, 2*dwi -e, (-c + Tfb)]
    #
    tau_radial = []
    for i in range(len(CoorZ)):
        r = R + CoorZ[i]      # radial position to point of interest
        _cr =  (r - b) / 2.0  # distance from centroide to extreme fibre
        _r1 = _cr + b           # point of cross section from radial centre 
        _Ar = (tr * math.log(r /b) )* r  # area of part portion below r
        #_Ar = (tr * math.log((_r1 / _cr + 1) / (_r1 / _cr - 1))) * r
        _Qr = _Ar * _r1
        # roark Ed 7 equ (9.1-4)
        tau_radial.append( (rn / (tr*Aw*e*r**2)) * (R*_Ar - _Qr))
    #
    #print(' ')
    #for i in range(len(tau_radial)):
    #    print('tau : {: 1.4E} {: 1.4E}'
    #          .format(tau_radial[i], CoorZ[i]))
    #print(' ')
    print('shear radial  : {: 1.4E} '.format(max(tau_radial)))
    
    tau_y = max(tau_average, max(tau_radial))
    #print(' ')
    print('-----------------------------')
    print('Max Shear (tau) : {: 1.4E}'. format(tau_y))
    return tau_y
#
#-------------------------------------------------
#
#-------------------------------------------------
#
class Ibeam(SectionBasic):
    __slots__ = ['_labels', '_number', '_title', '_type', '_default', 
                 '_d', '_tw', '_bf', '_tf', '_bfb', '_tfb',
                 '_r']
    def __init__(self):
        """ """
        super().__init__()
        self._d: array = array('f', [])
        self._tw: array = array('f', [])
        self._bf: array = array('f', [])
        self._tf: array = array('f', [])
        self._bfb: array = array('f', [])
        self._tfb: array = array('f', [])
        self._r: array = array('f', [])   # fillet radius
    #
    #
    def __setitem__(self, shape_name: int|str, parameters: list) -> None:
        """
        parameters = [node1, node2, material, section, roll_angle]
        """
        try:
            self._labels.index(shape_name)
            raise Exception('Section {:} already exist'.format(shape_name))
        except ValueError:
            self._labels.append(shape_name)
            mnumber = next(self.get_number())
            self._number.append(mnumber)
            #
            self._d.append(parameters[0])
            self._tw.append(parameters[1])
            self._bf.append(parameters[2])
            self._tf.append(parameters[3])
            self._bfb.append(parameters[4])
            self._tfb.append(parameters[5])
            self._r.append(parameters[6])
            self._title.append(parameters[7])
            #            
    #
    def __getitem__(self, shape_name: str | int):
        """
        """
        try:
            index = self._labels.index(shape_name)
        except ValueError:
            raise Exception(f" section name {shape_name} not found")
        #
        return IbeamBasic(name=self._labels[index], 
                          d=self._d[index], tw=self._tw[index],
                          bft=self._bf[index], tft=self._tf[index],
                          bfb=self._bfb[index], tfb=self._tfb[index], 
                          root_radius=self._r[index])
#
#
@dataclass
class IbeamBasic(ShapeBasic):
    """
    ============================================  
    Calculate the section properties of a I beam   
    ============================================   
         
         *  a  *
    +    +-----+  
            |  
    d       |         Z  
            |         ^  
    +  +---------+    + > Y  
       *    b    *  

    Parameters
    ----------
    d  : Section Height   
    tw : Web thickness   
    bft: Top compression flange base   
    tft: Top flange thickness   
    bfb: Bottom tension flange base [dft]
    tfb: Bottom flange thickness    [tft]
    r  : root radious
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
    Sxc : Elastic section modulus referred to compression flange
    Sxt : Elastic section modulus referred to tension flange
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
    #name:str | int
    #build: str = 'welded'
    d: float
    tw: float
    bft: float
    tft: float
    bfb: float
    tfb: float
    root_radius:float = 0
    type:str = 'I section'
    #
    def shear_stress(self, Vy, Vz,
                     stress_type:str ='average'):
        """
        Vy : horizontal force
        Vz : vertical force
        stress_type: average/true
        -------------------------
        
        Return:
        tau_y : stress horizontal
        tau_z : stress vertical
        """
        #
        #-------------------------------------------------        
        #            Shear Stress Calculation
        # vertical section coordinates
        coord =  self.section_coordinates()
        prop = self.properties()
        #        
        if 'average' in stress_type.lower():
            # Area of Web
            # The overall depth times the web thickness
            Aw = self.d * self.tw
            index = [0, 2, 6, 8]
            tau_z = [0 if z in index else 1
                     for z, item in enumerate(coord.z)]
            tau_z = [item * Vz / Aw for item in tau_z] # vertical
            #
            # Area of Flange
            Af = (self.bfb * self.tfb + self.bft * self.tft)
            index = [4]
            tau_y = [0 if y in index else 1
                     for y, item in enumerate(coord.y)]
            tau_y = [item * Vy / Af for item in tau_y] # horizontal
        
        else:
            # True Shear Stress
            Zcb = self.d - prop.Zc
            # Centroid of top half of web from Neutral-Axis times area of top half of web
            q5a = (prop.Zc - self.tft) / 2.0 * self.tw * (prop.Zc - self.tft)
            # Centroid of bottom half of web from Neutral-Axis times area of bottom half of web
            q5b = (Zcb - self.tfb) / 2.0 * self.tw * (Zcb - self.tfb)
            #
            # Section points 2 and 8 are on the extreme edges of the flange
            # where there is no shear stress
            q = [0 , 0 , 0 ,  
                 (prop.Zc - self.tft / 2.0) * self.tft * self.bft,
                 0,
                 (Zcb - self.tfb / 2.0) * self.tfb * self.bfb,
                 0 , 0 , 0]
            #---------------------------------------------------------------------
            #
            q[4] = max(q5a + q[3], q5b + q[5])
            q = [_q * Vz / prop.Iy for _q in q]
            # vertical
            #---------------------------------------------------------------------
            tau_z = [q[0]/self.bft,
                     q[1]/self.bft, 
                     q[2]/self.bft,
                     q[3]/self.tw,
                     q[4]/self.tw, 
                     q[5]/self.tw,
                     q[6]/self.bfb,
                     q[7]/self.bfb,
                     q[8]/self.bfb]
            #---------------------------------------------------------------------
            # get load proportion
            Vtop = Vy * Zcb / self.d
            Vbot = Vy * prop.Zc / self.d
            # get area flange
            bft = 2 * self.bft * self.tft
            bfb = 2 * self.bfb * self.tfb
            # horizontal
            tau_y = [0 * Vtop / bft,
                     3.0 * Vtop / bft, 
                     0 * Vtop / bft, 
                     3.0 * Vtop / bft, 
                     0 * Vtop / bfb, 
                     3.0 * Vbot / bfb,
                     0 * Vtop / bfb, 
                     3.0 * Vbot / bfb, 
                     0 * Vtop / bfb]
        # 
        return tau_y, tau_z
    #
    def _torsional_stress2(self, theta, E: float, G: float):
        """
        Roark's Torsion chapter
        """
        #if not G:
        #G = self._material.E / (2 * (1.0 + self._material.poisson))
        #
        #if not theta:
        #    theta = self._get_rotation(To, E, G, l)
        #
        _hw = (self.d - self.tft - self.tfb) # - 2 * self.root_radius
        _ho = (self.d - 0.5 * self.tft - 0.5 * self.tfb)        
        #
        if 'symmetrical' in self.type.lower():
            tau_1 = self.tft * G * theta[0]
            tau_2 = - _ho * self.bft**2 * E * theta[2] / 16.0
            # bending
            sigma_y = _ho * self.bft * E * theta[1] / 4.0
        else:
            # shear
            tau_1 = max(self.tw, self.tft, self.tfb) * G * theta[0]
            #
            if self.tfb * self.bfb > self.tft * self.bft:
                tau_2 = ((_ho * self.tfb * self.bfb**3 * self.bft**2)
                         / (8 * self.tft * self.bft**3 + self.tfb * self.bfb**3))
            else:
                tau_2 = ((_ho * self.tft * self.bft**3 * self.bfb**2)
                         / (8 * self.tft * self.bft**3 + self.tfb * self.bfb**3))                
            #
            tau_2 *= E * theta[2]
            #
            # bending 
            #
            e = (self.tft * self.bft**3 * _ho
                 / (self.tft * self.bft**3 + self.tfb * self.bfb**3))
            #
            if self.tfb * self.bfb**2 > self.tft * self.bft**2:
                sigma_y = ((_ho * self.bft / 2.0) 
                           * self.tfb * self.bfb**3 
                           / (self.tft * self.bft**3 
                              + self.tfb * self.bfb**3))
            else:
                sigma_y = ((_ho * self.bfb / 2.0) 
                           * self.tft * self.bft**3 
                           / (self.tft * self.bft**3 
                              + self.tfb * self.bfb**3))
            #
            sigma_y *= E * theta[1]
        #
        # bending stress
        sigma_y = [_sigma + math.copysign(sigma_y, _sigma)
                        if _sigma != 0 else 0
                        for _sigma in sigma_y]
        #
        # shear stress
        tau_z = [_tau + math.copysign(tau_1, _tau) - math.copysign(tau_2, _tau)
                      if _tau != 0 else 0
                      for _tau in tau_z]
        #
        #print('ok')
        return sigma_y, tau_z
    #
    def torsional_stress(self, Tw, B):
        """
        Pilkey Formulas
        Chapter 14.
        """
        #coord =  self.section_coordinates()
        #ho = (self.d - 0.5 * self.tft - 0.5 * self.tfb)
        #
        Cw, J, K, w, Qw = self.warping_properties()
        # Normal warping stress
        #index = [0, 2, 6, 8]
        #sigma_w = [0 if y in index else 1
        #           for y, item in enumerate(coord.y)]
        #sigma_w = [item for item in sigma_w]
        sigma_wt = w[0] / Cw
        sigma_wb = w[1] / Cw
        sigma_w = [0, 0, 0, # 1,2,3
                   sigma_wt,      #4
                   0,              #5
                   -1 * sigma_wb, #6
                   0, 0, 0] # 7,8,9
        sigma_w =  [item * B for item in sigma_w]
        # Shear warping stress
        tau_wt = Qw[0] / (Cw * self.tft)
        tau_wb = Qw[1] / (Cw * self.tfb)
        tau_w = [tau_wt, 0, -1 * tau_wt, # 1,2,3
                 0, 0, 0,                #4,5,6
                 tau_wb, 0, -1 * tau_wb] # 7,8,9
        tau_w = [item * Tw for item in tau_w]
        #1 / 0
        return sigma_w,  tau_w
        
    #
    def curved(self, R:float):
        """
        ---------
        R = Radio
        """
        if 'symmetrical' in self.type.lower():
            b = self.bfb
            b1 = self.tw
            t = self.tfb
            d = self.d
            ry = self.ry
        
            # shear area
            warea = self.area
        
            # extreme fibre distances c
            c = d/2.0
            self.c = c
        
            c1 = d - c
            self.c1 = c1
        
            # centroidal radius
            #R = R
            # R = orad - c1
            self.R = R
        
            # Shift of neutral axis from neutral axis
            e = (c *((R/c)- ((2.0*(t/c + (1 - t/c)*(b1/b))) 
                                 / ((math.log(((R/c)**2 + (R/c + 1)*(t/c) - 1.0) 
                                              / ((R/c)**2 - (R/c - 1.0)*(t/c) - 1.0))) 
                                    + ((b1/b)*math.log((R/c - t/c + 1.0) 
                                                         /(R/c + t/c - 1.0)))))))
            self.e = e
            # where
            Ic = self.Iy
        
            # stress factors Ki
            self.ki = ((Ic / (warea * c**2 * (R/c - 1.0))) 
                       * ((1.0 - e / c) / (e / c)))
        
            # stress factors Ko
            self.ko = ((Ic / (warea * c**2 * (R/c + 1.0))) 
                       * ((1.0 + e / c) / (e / c)))
        
            # Modulus of rigidity factor (section 8.10)
            nai = c - e    # neautral axis inner fiber
            nao = c1 + e   # neautral axis outer fiber
        
            D1 = nai - t
            D2 = nai 
            t1 = b1
            t2 = b
            r = ry
        
            self.F = ((1 + (((3*(D2**2 - D1**2)*D1)/(2.0*D2**3)) * (t2/t1 - 1.0)))
                      * (4*D2**2 / (10*r**2)))
            #
            # Shear factor (section 8.1 equ 8.1-13)
            self.tau_y = radial_shear_factor(d, b1, t, t, c, c1, R, e)
        
        else:
            b = self.bfb
            t = self.tfb
            b1 = self.bft
            t1 = self.tft
            d = self.d
            b2 = self.tw
        
            # shear area
            warea = d * b2
        
            # extreme inner fibre distance c
            c = (d * (((b1/b - b2/b)*(2.0 - t1/d)*(t1/d) 
                         + (1.0 - b2/b)*(t/d)**2 + (b2/b))
                        / (2*self.area / (b*d))))
            self.c = c
            # extreme outer fibre distance c
            c1 = d - c
            self.c1 = c1
            # centroidal radius
            #_R = R
            #R = R - c1
            self.R = R
        
            # Shift of neutral axis from neutral axis
            e = (c * ((R/c)-(((self.area/(b*d))*(d/c)) 
                                 / (math.log((R/c + t/c - 1)/(R/c - 1)) 
                                    + ((b2/b)*math.log((R/c + c1/c - t1/c )
                                                         / (R/c + t/c - 1)))
                                    + ((b1/b)*math.log((R/c + c1/c)
                                                         / (R/c + c1/c - t1/c)))))))
            self.e = e
            # where
            Ic = self.Iy
        
            # stress factors Ki
            self.ki = ((Ic / (self.area * c**2 * (R/c - 1.0))) 
                       * ((1.0 - e / c) / (e / c)))
        
            # stress factors Ko
            self.ko = ((Ic / (self.area * c**2 * (e/c ))) 
                       * ((d/c + e/c - 1.0) / (R/c  + d/c - 1.0))
                       * (1.0  / (d / c - 1.0)))
        
            # Modulus of rigidity factor (section 8.10)
            nai = c - e    # neautral axis inner fiber
            nao = c1 + e   # neautral axis outer fiber
        
            if nai <= nao:
                D1 = nai - t1
                D2 = nai 
                t1 = b2
                t2 = b1
                r = self.ry
                print ('inner fiber ', nai)
            else:
                D1 = nao - t
                D2 = nao 
                t1 = b2
                t2 = b
                r = self.ry
                print ('outer fiber ', nao)
            
            
            self.F = ((1 + (((3*(D2**2 - D1**2)*D1)/(2.0*D2**3)) * (t2/t1 - 1.0)))
                      * (4*D2**2 / (10*r**2)))
            
            #
            # Shear factor (section 8.1 equ 8.1-13)
            #_shearFactor = shear_factor(c, c1, R, e)
            #
            #
            #R, _F, e, c, c1, _ki, _ko, _shearFactor
            self.tau_y = radial_shear_factor(d, b1, t, t1, c, c1, R, e)
        #
        #
    #
    #
    def get_compactness(self, material: str):
        """
        """
        _class_B41a, _class_B41b = open_section_compactness(self, material)
    
        return _class_B41a, _class_B41b
    #
    #
    def _properties(self):
        """
        """
        #
        self.type = 'Symmetrical I section'
        #
        try:
            if self.bft != self.bfb:
                self.type = 'Asymmetrical I section'
        except AttributeError:
            try:
                self.bfb = self.bft
            except AttributeError:
                self.bft = self.bfb
        #
        try:
            if self.tft != self.tfb:
                self.type = 'Asymmetrical I section'
        except AttributeError:
            try:
                self.tfb = self.tft
            except AttributeError:
                self.tft = self.tfb        
        #
        #
        #-------------------------------------------------   
        #
        _hw = (self.d - self.tft - self.tfb) # - 2 * self.root_radius
        _ho = (self.d - 0.5 * self.tft - 0.5 * self.tfb)
        #-------------------------------------------------
        #   Cross-Sectional Area
        area = (self.bft*self.tft
                + self.bfb*self.tfb 
                + _hw*self.tw)
        #-------------------------------------------------
        #   Elastic Neutral Centre 
        Zc = ((self.bft * self.tft**2 / 2.0
               + self.bfb * self.tfb 
               * (_hw + self.tft + self.tfb / 2.0) 
               + _hw * self.tw * (_hw / 2.0 + self.tft)) 
              / (self.bft * self.tft 
               + _hw*self.tw 
               + self.bfb * self.tfb))
        Yc = 0 * Zc
        #   Plastic Neutral Centre 
        if (self.bfb * self.tfb > (self.bft * self.tft + _hw * self.tw)):
            Zp = (self.d
                  - (0.5 * area / self.bfb)
                  - self.tft)
        elif (self.bft * self.tft > (self.bfb * self.tfb + _hw * self.tw)):
            Zp = (self.d -
                  (self.tfb + _hw 
                  + ((0.5 * area - self.bfb
                  * self.tfb - _hw * self.tw)
                  / self.bft)) - self.tft) 
        else:
            Zp = (self.d -
                  ((0.5 * area - self.bfb
                  * self.tfb)
                  / self.tw + self.tfb) 
                  - self.tft)
        # Warping Constant Cw
        # Picard and Beaulieu 1991
        d = self.d - (self.tft + self.tfb) / 2.0
        
        _alpha = (1.0 / (1 + (self.bft / self.bfb)**3 
                         * (self.tft / self.tfb)))
        
        Cw = (d**2 * self.bft**3 * self.tft * _alpha) / 12.0
        #-------------------------------------------------
        #   Torsional Constant
        if self.bft == self.bfb and self.tft == self.tfb :
            J = ((2 * self.bft * self.tft**3 / 3.0)
                      + (self.d * self.tw**3 / 3.0))
            #
            K = ((2 * self.tft**3 * self.bft +
                  self.tw**3 * _ho) / 3.0)
        else:
            J = ((self.bft * self.tft**3
                  + self.bfb * self.tfb**3 
                  + d * self.tw**3) / 3.0)
            #
            K = (self.tft**3 * self.bft + self.tfb**3 * self.bfb
                 + self.tw**3 * _ho)            
        #
        #-------------------------------------------------
        #   Shear Centre
        SCz = (((self.d - self.tft / 2.0 - self.tfb / 2.0)
                * self.tft * self.bft**3) 
               / (self.tft * self.bft**3 
                  + self.tfb * self.bfb**3))
        SCy = 0 * SCz
        #-------------------------------------------------
        #               Section Properties
        #-------------------------------------------------
        #   Second Moment of Area about Mayor Axis
        Iy = (self.bft * self.tft**3 / 12.0
              + (self.bft * self.tft 
                 * (Zc - self.tft / 2.0)**2 )
              + self.bfb * self.tfb**3 / 12.0 
              + (self.bfb * self.tfb 
                 * (_hw + self.tfb / 2.0 + self.tft - Zc)**2)
              + self.tw * _hw**3 / 12.0 
              + self.tw * _hw * (_hw / 2.0 + self.tft - Zc)**2)
        #   Second Moment of Area of Compression Flange about Mayor Axis
        _Iy_ft = (self.bft * self.tft**3 / 12.0 + 
                  (self.bft * self.tft
                   * (Zc - self.tft / 2.0)**2 ))
        #   Elastic Modulus about Mayor Axis
        if Zc >= (self.d - Zc):
            Zey = Iy / Zc
        else:
            Zey = Iy / (self.d - Zc)
        #   Plastic Modulus about Mayor Axis
        Zpy = ((self.tw * _hw**2 / 4.0)
               + (self.bft * self.tft 
                  * (Zc - self.tft / 2.0))
               + (self.bfb * self.tfb 
                  * (self.d - Zc - self.tfb / 2.0)))
        #   Radius of gyration about Mayor Axis
        ry = (Iy / area)**0.5
        SFy = Zpy / Zey
        # Elastic modulus of compression flange about major axis
        Sxc = (self.bft * self.tft**3 / 12.0
               + (self.bft * self.tft 
                  * (Zc - self.tft / 2.0)**2 )) / Zc
        # Elastic modulus of tension flange about major axis
        Sxt = (self.bfb * self.tfb**3 / 12.0
               + (self.bfb * self.tfb 
                  * ((self.d-Zc) - self.tfb / 2.0)**2 )) / (self.d-Zc)
        #
        #-------------------------------------------------
        #   Second Moment of Area about Minor Axis
        Iz = (self.bft**3 * self.tft / 12.0 +
              self.bfb**3 * self.tfb / 12.0 +
              self.tw**3 * _hw / 12.0)
        #   Elastic Modulus about Minor Axis
        if self.bft >= self.bfb:
            Zez = 2 * Iz / self.bft
        else:
            Zez = 2 * Iz / self.bfb
        #   Plastic Modulus about Minor Axis  
        Zpz = ((self.tft * self.bft**2
                + self.tfb * self.bfb**2 
                + _hw * self.tw**2) / 4.0)
        #   Radius of gyration about Minor Axis  
        rz = (Iz / area)**0.5
        
        SFz = Zpz / Zez
        #-------------------------------------------------
        #   Product of inertia
        _Iyz = 0
        Jx = Iy + Iz
        rp = (Jx / area)**0.50
        
        # warping statical moment at point s on cross-section
        Sws = _ho * self.bft**2 * self.tft / 16.0
        # normalized warping function at point s on cross-section
        Wns = _ho * self.bft / 4.0
        #
        #-------------------------------------------------
        #
        return ShapeProperty(area=area, Zc=Zc, Yc=Yc,
                             Iy=Iy, Zey=Zey, Zpy=Zpy, ry=ry,
                             Iz=Iz, Zez=Zez, Zpz=Zpz, rz=rz,
                             J=J, Cw=Cw )
    #
    def warping_properties(self):
        """
        Returns:
        Cw, J, K, w, Qw
        """
        #hw = (self.d - self.tft - self.tfb) # - 2 * self.root_radius
        ho = (self.d - 0.5 * self.tft - 0.5 * self.tfb)
        #
        # Warping Constant Cw
        # Picard and Beaulieu 1991
        d = self.d - (self.tft + self.tfb) / 2.0
        
        _alpha = (1.0 / (1 + (self.bft / self.bfb)**3 
                         * (self.tft / self.tfb)))
        
        Cw = (d**2 * self.bft**3 * self.tft * _alpha) / 12.0        
        #-------------------------------------------------
        #   J : Torsional Constant         (lenght^4)
        #   K : Torsional stiffness factor (lenght^4)
        if self.bft == self.bfb and self.tft == self.tfb :
            J = ((2 * self.bft * self.tft**3 / 3.0)
                 + (self.d * self.tw**3 / 3.0))
            #
            K = ((2 * self.tft**3 * self.bft +
                  self.tw**3 * ho) / 3.0)
        else:
            J = ((self.bft * self.tft**3
                  + self.bfb * self.tfb**3 
                  + d * self.tw**3) / 3.0)
            #
            K = (self.tft**3 * self.bft + self.tfb**3 * self.bfb
                 + self.tw**3 * ho)
        #
        # warping statical moment at point s on cross-section
        Sws = ho * self.bft**2 * self.tft / 16.0
        # normalized warping function at point s on cross-section
        Wns = ho * self.bft / 4.0
        #
        #-------------------------------------------------
        # omega : Sectorial coordinate
        #
        w1 = (self.bft * ho
              / (2.0 * (1.0 + (self.bft / self.bfb)**3
                        * (self.tft / self.tfb))))
        
        w2 = (self.bfb * ho
              / (2.0 * (1.0 + (self.bfb / self.bft)**3
                        * (self.tfb / self.tft))))        
        #
        Qw1 = self.bft * self.tft * w1 / 4.0
        Qw2 = self.bfb * self.tfb * w2 / 4.0
        #
        return Cw, J, K, (w1, w2), (Qw1, Qw2)
    #
    @property
    def CoG(self):
        """ """
        _hw = (self.d - self.tft - self.tfb)
        #-------------------------------------------------
        Yc = 0
        #   Elastic Neutral Centre 
        Zc = ((self.bft * self.tft**2 / 2.0
                    + self.bfb * self.tfb 
                    * (_hw + self.tft + self.tfb / 2.0) 
                    + _hw * self.tw * (_hw / 2.0 + self.tft)) 
                   / (self.bft * self.tft 
                    + _hw*self.tw 
                    + self.bfb * self.tfb))
        #
        return Yc, Zc
    #
    #
    def _stress(self, actions, stress=None,
                stress_type: str='average'):
        """
        stress points = [1 2 3 4 5 6 7 8 9]
        """
        prop = self.properties()
        # get section's coordinates
        coord =  self.section_coordinates()
        # get shear stress
        tau_y, tau_z = self.shear_stress(actions.Fy, actions.Fz, 
                                         stress_type=stress_type)
        # FIXME: don't know what to do here
        sigma_w, tau_w = self.torsional_stress(Tw=actions.Tw, B=actions.B)
        tau_x = [actions.Mx * 0 for _ in coord.z]
        #
        #
        # get bending stress
        sigma_x = [actions.Fx / prop.area for item in coord.y]
        sigma_y = [actions.My * item / prop.Iy for item in coord.z]
        sigma_z = [actions.Mz * item / prop.Iz for item in coord.y]
        #
        # bending + torsion
        #try:
        tau_y = [tau_w[x] + item
                 for x, item in enumerate(tau_y)]
        #
        sigma_z = [sigma_w[x] + item
                   for x, item in enumerate(sigma_z)]
        #
        if stress:
            if isinstance(stress.tau_x, list):
                # assuming section stress already calculated
                stress.tau_x = self._combine_stress(tau_x, stress.tau_x)
                stress.tau_y = self._combine_stress(tau_y, stress.tau_y)
                stress.tau_z = self._combine_stress(tau_z, stress.tau_z)
                #
                stress.sigma_x = self._combine_stress(sigma_x, stress.sigma_x)
                stress.sigma_y = self._combine_stress(sigma_y, stress.sigma_y)
                stress.sigma_z = self._combine_stress(sigma_z, stress.sigma_z)
            else:
                # Assuming global stress
                stress_tau_x = [stress.tau_x for x in range(9)]
                stress.tau_x = self._combine_stress(tau_x, stress_tau_x)
                #
                stress_tau_y = [stress.tau_y for x in range(9)]
                _index = [4] # 3, 4, 5
                for x in _index:
                    stress_tau_y[x] *= 0
                stress.tau_y = self._combine_stress(tau_y, stress_tau_y)
                #
                stress_tau_z = [stress.tau_z for x in range(9)]
                _index = [0, 2, 6, 8]
                for x in _index:
                    stress_tau_z[x] *= 0
                stress.tau_z = self._combine_stress(tau_z, stress_tau_z)
                #
                stress_sigma_x = [stress.sigma_x for _ in coord.y]
                stress.sigma_x = self._combine_stress(sigma_x, stress_sigma_x)
                #
                _factor_z = [1, 1, 1, 1, 0, -1, -1, -1, -1]
                stress_sigma_y = [stress.sigma_y * _coord for _coord in _factor_z]
                stress.sigma_y = self._combine_stress(sigma_y, stress_sigma_y)
                #
                _factor_y = [1, 0, -1, 0, 0, 0, 1, 0, -1]
                stress_sigma_z = [stress.sigma_z * _coord  for _coord in _factor_y]
                stress.sigma_z = self._combine_stress(sigma_z, stress_sigma_z)
        
        else:
            stress = BeamStress(sigma_x, sigma_y, sigma_z, 
                                tau_x, tau_y, tau_z, coord)
        #
        return stress
    #
    #
    def section_coordinates(self):
        """
        1    2     3
        +----+-----+
        |____+_____| 4    ^ z
             |            |
             + 5          +--> y
             |
         ____+_____  6
        |          |
        +----+-----+      
        7    8     9
        """
        CoG = self.CoG
        # horizontal
        coord_y = [-1 * self.bft * 0.50, 0, self.bft * 0.50, 
                   0, 0, 0, 
                   -1 * self.bfb * 0.50, 0, self.bfb * 0.50]
        # vertical
        Zc = CoG[0]
        _Zcb = Zc - self.d
        coord_z = [Zc, Zc, Zc, 
                   Zc - self.tft, 0 * _Zcb , _Zcb + self.tfb,
                   _Zcb, _Zcb, _Zcb]
        #
        return points(coord_y, coord_z)
    #
    #def set_default(self):
    #    """ """
    #    self.cls._default = self.name
    #def push_property(self):
    #    """ """
    #    self.properties
    #
    # --------------------------------------------
    #
    def _dimension(self) -> str:
        """ """
        out = "{:32s}{:1.4E} {:1.4E} {:1.4E}\n".format(self.type, self.d, self.bft, self.bfb)
        out += "{:48s}{:1.4E} {:1.4E} {:1.4E}\n".format("", self.tw, self.tft, self.tfb)
        out += "{:70s}{:1.4E}\n".format("", self.r)
        return  out
    #    
    #
    def _shape(self):
        """
        """
        _section = []
        if 'symmetrical' in self.type.lower():
            _section.append("{:2s}+   bf    +{:33s}{:1.3E} {:1.3E}  {:1.3E} {:1.3E}\n"
                            .format("", "", 
                                    self.d, 
                                    self.tw, 
                                    self.bft, 
                                    self.tft))
            _section.append("+ +====+====+ tf\n")
            _section.append("{:}| tw\n".format(7*" ")) 
            _section.append("d{:6s}|{:6s}Z\n".format("", ""))
            _section.append("{:7s}|{:6s}^\n".format("", ""))
            _section.append("+ +====+====+ + > Y\n")
            _section.append("{:2s}+   bf    +\n".format(""))            
        else:
            _section.append("{:4s}+ bft +{:34s}{:1.3E} {:1.3E}  {:1.3E} {:1.3E}\n"
                            .format("","", 
                                    self.d, 
                                    self.tw, 
                                    self.bft, 
                                    self.tft))
            _section.append("+   +==+==+{:55s}{:1.3E} {:1.3E}\n"
                            .format("", 
                                    self.bfb, 
                                    self.tfb))
            _section.append("{:7s}|\n".format("")) 
            _section.append("d{:6s}|{:6s}Z\n".format("", ""))
            _section.append("{:7s}|{:6s}^\n".format("", ""))
            _section.append("+ +====+====+ + > Y\n")
            _section.append("{:2s}+   bfb   +\n".format(""))
        
        return _section
    #
    #
    def print_file(self, file_name):
        """
        """
        check_out = print_header()       

        check_out.append("{:23s} {:>19} {:1.4E} {:1.4E} {:1.4E} {:1.4E}\n"
                         .format(self.type, "", self.d, self.tw, self.bft, self.tft))
        
        check_out.append("{:>65} {:1.4E} {:1.4E}\n"
                         .format("", self.bfb, self.tfb))        

        check_out.extend(print_properties(self))

        #file_checkout = split_file_name(file_name)
        #file_checkout = str(file_checkout[0]) +'_check_me.txt'
        file_checkout = str(file_name) + '.txt'
        add_out = open(file_checkout,'w')
        add_out.write("".join(check_out))
        add_out.close()
        print('ok')    
    #
    # --------------------------------------------
    #
    @property
    def r(self):
        """ """
        return self.root_radius
    
    @r.setter
    def r(self, value):
        """ """
        self.root_radius = value
    #
    #
    #def __getattr__(self, attr):
    #    """
    #    Getter for myattr
    #    :param attr:
    #    :return:
    #    """
    #    # section top flange width
    #    if re.search(r"\bbf(t)?\b", attr, re.IGNORECASE):
    #        return self.bf
    #    # section top flange thickness
    #    elif re.search(r"\btf(t)?\b", attr, re.IGNORECASE):
    #        return self.tf
    #    # fillet radious
    #    elif re.search(r"\br\b", attr, re.IGNORECASE):
    #        return self.root_radius
    #    else:
    #        try:
    #            return self.__dict__[attr]
    #        except KeyError:
    #            raise AttributeError(f"Variable {attr} not found")

    #
    #def __setattr__(self, attr, value):
    #    """
    #    Setter for myattr
    #    :param attr:
    #    :return:
    #    """
    #
    #    # section top flange width
    #    if re.search(r"\bbf(t)?\b", attr, re.IGNORECASE):
    #        #self.bf = value
    #        super().__setattr__(attr, value)
    #    # section top flange thickness
    #    elif re.search(r"\btf(t)?\b", attr, re.IGNORECASE):
    #        #self.tf = value
    #        super().__setattr__(attr, value)
    #    # fillet radious
    #    elif re.search(r"\br\b", attr, re.IGNORECASE):
    #        self.root_radius = value
    #    else:
    #        try:
    #            # super().__setattr__(attr, value)
    #            self.__dict__[attr] = value
    #        except KeyError:
    #            raise AttributeError(f"Variable {attr} not found")
    #
    #
#
#

