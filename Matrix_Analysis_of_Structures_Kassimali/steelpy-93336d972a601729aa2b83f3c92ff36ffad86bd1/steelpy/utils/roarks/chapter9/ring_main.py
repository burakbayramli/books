# Copyright (c) 2009-2022 steelpy
#

# Python stdlib imports
#from array import array
from collections.abc import Mapping
#from dataclasses import dataclass
#import math
import re
from typing import Dict, List, Tuple, Union, NamedTuple

# package imports
from steelpy.material.main import Materials
from steelpy.utils.units.main import Units
from steelpy.sections.main import Sections
from .circular_ring import circular_ring, stress2, RadialForces

#
#
#
#
#
class Cases(Mapping):
    __slots__ = ['_labels', '_cases', '_istep']
    
    def __init__(self, istep:int=360):
        """
        """
        self._istep = istep
        self._labels : List = []
        self._cases : List = []    
    # ---------------------------------
    #
    def __setitem__(self, load_name: int,
                    case_data: Union[List[float], Dict[str, float]]) -> None:
        """
        case_data = [case, load, phase, theta, phi]
        case : Roark ring cases (1,2,..) 
        load : load value (could be applied force W, applied moment Mo and applied distributed force w)
        theta: load position angles.
        phi  : load position angles (case 18).
        rho  : liquid weight per unit volume.
        phase: the rotation angle of the axis of symmetry of the loading pattern.
        """
        try:
            self._labels.index(load_name)
            raise Exception('    *** warning load {:} already exist'
                            .format(load_name))
        except ValueError:
            self._labels.append(load_name)
            #
            if isinstance(case_data, (list, tuple)):
                data = get_data_list(data=case_data, steps=6)
            elif isinstance(case_data, dict):
                data = get_data_dic(data=case_data)
            else:
                raise IOError('input data not valid')
            #
            data.append(self._istep)
            self._cases.append(circular_ring(*data))
    #
    def __getitem__(self, load_name: Union[int, str]) -> Tuple:
        """
        load_name : node number
        """
        try:
            index = self._labels.index(load_name)
            return self._cases[index]
        except ValueError:
            raise IndexError('   *** load {:} does not exist'.format(load_name))
    #
    def __len__(self):
        return len(self._labels)

    def __iter__(self):
        """
        """
        return iter(self._labels)

    def __contains__(self, value):
        return value in self._labels
    #
    #
    def radial_forces(self,  R, k1, k2):
        """ """
        rforces = {}
        for load_name in self._labels:
            ring = self.__getitem__(load_name)
            rforces[load_name] = ring.get_load(R, k1, k2)
        return rforces
    #
    def force_combined(self, R, k1, k2):
        """ """
        rforces = self.radial_forces(R, k1, k2)
        for item in rforces.values():
            try:
                xm += item['M']
                xh += item['N']
                xs += item['V']
            except UnboundLocalError:
                xm = item['M']
                xh = item['N']
                xs = item['V']
                xloc = item['x']
        #return RadialForces(xloc, xm, xh, xs)
        return RadialForces({'x':xloc, 'M':xm, 'N':xh, 'V':xs})
    #
    def print(self):
        """ """
        # SUPERPOSITION OF ALL LOAD CASES
        print ("")
        print ('                                                              SUPERPOSITION OF ALL LOAD CASES')
        print ('          -------------Forces-------------    -----------------------Stresses (n/mm^2)---------------------------')
        print ('Location  Moment     Thrust        Shear     Bending     Bending      Hoop        Shear       Bend.in.   Bend.out',)
        print ('Degrees    N.mm        N             N        Inner       Outer                               +Hoop       +Hoop',)
        for i in range(len(_xloc)):
            print ("{:>5.1f}  {: 1.4e} {: 1.4e} {: 1.4e} {: 1.4e} {: 1.4e} {: 1.4e} {: 1.4e} {: 1.4e} {: 1.4e}"
                   .format(_xloc[i], self.xm[i], self.xh[i], self.xs[i], self.bstri[i],
                           self.bstro[i], self.hstr[i],
                           self.sstr[i], self.cstri[i], self.cstro[i]))
        
        print ("")
        print ('max. combined stress    max. shear stress')
        print ('      N/mm**2                N/mm**2')
        print ("   {: 1.3f}            {: 1.3f}".format(self.cstmax, self.shmax))
        
        lo = min(self.xs)
        hi = max(self.xs)
        Vy = max(abs(lo), abs(hi))
        print ('')
        print  ("results written to specified filename")
    #    
#
#
#
#
def get_data_list(data:List[Units], steps:int)->List[float]:
    """
    [case, load, theta, phi, rho, phase]
    """
    new_data = [0] * steps
    phase = 0
    if data[0] in [1, 15, 16]:
        if len(data) > 2:
            phase = data.pop()
            #phase = phase.value
            new_data[-1] = phase.value
    elif data[0] == 18:
        if len(data) > 4:
            phase = data.pop()
            new_data[-1] = phase.value
    #
    #elif data[0] in [16, 17]:
    #    if len(data) > 4:
    #        phase = data.pop()
    #        phase = phase.value
    #    #print('--')
    else:
        if len(data) > 3:
            phase = data.pop()
            new_data[-1] = phase.value
    #new_data = []
    #for x in range(steps):
    x = 2
    for val in data:
        #try:
        try:
            try: # Force
                item = val.convert('newton')
                #new_data.append(item)
                new_data[1] = item.value
            except RuntimeError: # Moment
                try:
                    item = val.convert('newton*metre').value
                    #new_data.append(item)
                    new_data[1] = item.value
                except RuntimeError: 
                    try: # density
                        item = val.convert('kilogram/metre^3')
                        #new_data.append(item)
                        new_data[-2] = item.value
                    except RuntimeError: # degree unit
                        #new_data.append(data[x].value)
                        new_data[x] = val.value
                        x += 1
        except AttributeError:
            #new_data.append(data[x])
            new_data[0] = val
        #except IndexError:
        #    new_data.append(0.0)
    #
    #new_data[-1] = phase
    return new_data
#
def get_data_dic(data:Dict)->List[float]:
    """[case, load, theta, phi, rho, phase]"""
    new_data = [0,0,0,0,0,0]
    for key, item in data.items():
        if re.match(r"\b(case|number)\b", key, re.IGNORECASE):
            new_data[0] = item
        elif re.match(r"\b(w|m(o|0)?|load)\b", key, re.IGNORECASE):
            new_data[1] = item.convert('newton').value
        elif re.match(r"\b(theta)\b", key, re.IGNORECASE):
            new_data[2] = item.value
        elif re.match(r"\b(phi)\b", key, re.IGNORECASE):
            new_data[3] = item.value
        elif re.match(r"\b(rho)\b", key, re.IGNORECASE):
            new_data[4] = item.value        
        elif re.match(r"\b(phase)\b", key, re.IGNORECASE):
            new_data[5] = item.value        
    return new_data
#
#
#
class  Ring:
    __slots__ = ['_material', '_geometry', '_load_case',
                 '_units', '_stiffener', '_sections', 
                 '_R', '_d', '_b']
    
    def __init__(self, istep:int = 360):
        """
        The following are assumptions applicable to the cases ring program supported from section
        8.3, Roark’s Formulas for Stress and Strain, 7th edition.  
        1.- The ring is of uniform section.
        2.- It is such large radius in comparison with its radial thickness that the deflection theory for
        straight beams is applicable;
        3.- It is nowhere stressed beyond elastic limit.
        4.- It is not so severely deformed as to lose its essentially circular shape.
        5.- Its deflection is due primarily to bending, but it is desired, the deflections due to
        deformations caused by axial tension or compression the ring and/or by transverse shear
        stresses in the ring may be included.
        """
        self._units = Units()
        # Number of points around ring
        self._load_case = Cases(istep)
        #
        #self.LocalAxis = 'x y z'
        ##
        ##self._chord = API_design()
        self._sections = Sections()
        #self._stiffness_flag = False
    #
    #     
    @property
    def material(self):
        """
        """
        return self._material
    
    @material.setter
    def material(self, value:Materials):
        """
        """
        self._material = value
    #
    @property
    def geometry(self):
        """
        """
        return self._geometry
    
    @geometry.setter
    def geometry(self, value:Sections):
        """
        """
        self._geometry = value  
    #
    #
    #@property
    #def LE(self):
    #    """Effective Lenght"""
    #    return self._LE
    #
    #@LE.setter
    #def LE(self, L:Units):
    #    """Effective Lenght"""
    #    self._LE = L
    #
    #
    #@property
    #def chord(self):
    #    """
    #    """
    #    return self._chord
    #
    #
    @property
    def load(self):
        """ """
        return self._load_case
    #
    #    
    @property
    def stiffener(self):
        """
        """
        return self._stiffener
        #return self._section
    #
    #@property
    def radial_forces(self):
        """ """
        k1, k2, alpha, beta = self.k_factors()
        R = self._R
        return self._load_case.radial_forces(R, k1, k2)    
    #
    #@property
    def force_combined(self):
        """ """
        k1, k2, alpha, beta = self.k_factors()
        R = self._R
        #
        #try:
        return self._load_case.force_combined(R, k1, k2)
        #except NameError:
        #    section = self._section()
        #    properties = section.properties
        #    Iy = properties.Iy
        #    area = properties.area
        #    Kt = 1 + Iy / (area*R**2)
        #    #self._load_case.properties(beta, area, Iy)
        #    return self._load_case.radial_force(R, k1, k2, beta, area, Iy)
    #
    #@property
    def radial_stress(self):
        """ """
        # get cross section of ring
        section = self._section()
        properties = section.properties
        d = self._d
        R = self._R
        c, c1, e, ki, ko, F = section.curved(R)
        #
        Iy = properties.Iy
        area = properties.area 
        #
        # Shear factor
        if R/d <= 0.70:
            F = max(F, 2.04)
        elif R/d <= 1.5:
            F = max(F, 1.56)
        else:
            F = max(F, 1.50)
        #
        rforce = self.force_combined()
        #
        return stress2(Iy, area, c, c1, ki, ko, F, rforce)
                       #rforce['x'], rforce['M'], rforce['N'], rforce['V'])
    #
    def _section(self):
        """ """
        try:
            return self._sections['cross_section']
        except Exception:
            # get cross section of ring
            d = self._geometry.t
            #
            # FIXME: what b should be set?
            b = 1* self._units.m
            BE = 1.1*(self._geometry.t * self._geometry.d)**0.50
            #
            self._sections['cross_section'] = ['Rectangle', d, BE]
            #self._sections['cross_section'] = ['Rectangle', d, b.value]
            properties = self._sections['cross_section'].properties()
            R = self._geometry.d * 0.50 - properties.Zc            
        #
        self._R = R
        self._d = d
        #self._b = b
        return self._sections['cross_section']
    #
    def k_factors(self):
        """     
        The Hoop-stress deformation factor : 
        alpha = I/AR^2
        
        The Transverse (radial) shear deformation factor : 
            beta = FEI/GAR^2  [thin rings]
            beta = 2F(1+v)e/R [thick rings]
                
        G is the shear modulus of elasticity
        F is the shape factor for the cross section [see Sec 8.10 (7ed)]
            
        Note that these constants are unity if not correction for hoop
        stress or shear stress is necessary or desired for use with thin
        rings
        """
        section = self._section()
        properties = section.properties()
        d = self._d
        R = self._R
        Iy = properties.Iy 
        area = properties.area
        c, c1, e, ki, ko, F = section.curved(R)
        #
        material = self.material
        E = material.E
        G = material.G
        v = material.poisson
        #
        #
        if R/d < 0.60:
            raise RuntimeError('R/d [{1.3f:}] < 0.6 not applicable'.format(R/d))        
        
        elif R/d > 8.0: # thin
            # Hoop-stress deformation factor
            alpha = Iy / (area*R**2)     
            # Transverse (radial) shear deformation factor
            beta = F*E*Iy / (G*area*R**2)
            ring_status = 'thin ring'
        
        else: # thick ring
            # Hoop-stress deformation factor
            alpha = e/R 
            # Transverse (radial) shear deformation factor
            beta = 2*F*(1+v)*e/R  
            ring_status = 'thick ring'
        #
        k1 = 1 - alpha + beta
        k2 = 1 - alpha
        #
        print('========= 0 ============')
        print('k Factors')
        print(f'R/d = [{R/d:1.3f}] {ring_status}')
        print(f'k1 = {k1:1.3f} k2 = {k2:1.3f}')
        print('')
        #
        return k1, k2, alpha, beta
    #
    #
    def _shear_factor(self, _c, _c1, _R, _e):
        '''
        Radial/horizontal web shear stress factor calculation
        Ref Roark 7 ed chapter 9 : Shear stress due to the radial 
                                   shear force V
        
        tr : thickness of the section normal to the plane of curvature
             at the radial position r
        
        '''
        #
        #global _D, _Tw, _Bft, _Tft, _Bfb, _Tfb
        #
        #  
        # total area of the web 
        _Aw = (_D - _Tft - _Tfb) * _Tw       
        # Area of Flange
        _Af = (_Bft*_Tft) + (_Bfb*_Tfb)
        
        # Average Shear
        _tau_average = 1.0/_Aw
        print(' ')
        print('shear average : {: 1.4E}'.format(_tau_average))
        
        #
        # --------------------------------------------
        # Shear stress due to the radial shear force V
        _rn = _R -_e             # radial position to neautral axis
        _b = _R - _c - _Tft      # radial position to inner extreme fiber
        _tr = _Tw   # width of cross section where stresses are calculated
        
        # fraction of the web dep
        _dwo = (_c1 - _Tft - _e) / 3.0 
        _dwi = (-_c + _Tfb + _e) / 3.0
        _CoorZ = [(_c1 - _Tft), 2 * _dwo + _e, _dwo + _e, _e, 0,
                  -_e, _dwi -_e, 2*_dwi -_e, (-_c + _Tfb)]
        
        _tau_radial = []
        for i in range(len(_CoorZ)):
            _r = _R + _CoorZ[i]      # radial position to point of interest
            _cr =  (_r - _b) / 2.0  # distance from centroide to extreme fibre
            _r1 = _cr + _b           # point of cross section from radial centre 
            _Ar = (_tr * math.log(_r /_b) )* _r  # area of part portion below r
            #_Ar = (_tr * math.log((_r1 / _cr + 1) / (_r1 / _cr - 1))) * _r
            _Qr = _Ar * _r1
            # roark Ed 7 equ (9.1-4)
            _tau_radial.append( (_rn / (_tr*_Aw*_e*_r**2)) * (_R*_Ar - _Qr))
        #
        #print(' ')
        #for i in range(len(_tau_radial)):
        #    print('tau : {: 1.4E} {: 1.4E}'
        #          .format(_tau_radial[i], _CoorZ[i]))
        #print(' ')
        print('shear radial  : {: 1.4E} '.format(max(_tau_radial)))
        
        _tau_y = max(_tau_average, max(_tau_radial))
        #print(' ')
        print('-----------------------------')
        print('Max Shear (tau) : {: 1.4E}'. format(_tau_y))
        return _tau_y
    #    
    #
    def print_results(self, PrintOption = 'MONITOR'):
        """
        """
        #
        #---------------------
        for i in range(self.istep):
            self.xm.append(0)
            self.xh.append(0)
            self.xs.append(0)
            self.bstri.append(0)
            self.bstro.append(0)
            self.hstr.append(0)
            self.sstr.append(0)
            self.cstri.append(0)
            self.cstro.append(0)
        #
        # Open report.txt For Append As #1
        #
        print (" ")
        print ("        STRESSES IN CIRCULAR RINGS 7th edition")
        print ("                R.J.ROARK & W.C.YOUNG")
        print ("******************************************************")
        print (" ")
        print ('RINGS DATA ECHO')
        print ('number of load cases: {:}'.format(self.iload))
        print ('number of points around ring: {:}'.format(self.istep))
        print ("Cross Section")
        print ('outside radius of ring: {:}'.format(self.orad))
        print ('outside flange: {:} x {:} thk'.format(self.fow,self.fot))
        print ('inside flange:  {:} x {:} thk'.format(self.fiw,self.fit))
        print ('web             {:} x {:} thk'.format(self.wlh,self.wth))
        print ("")
        print ('CALCULATED SECTION PROPERTIES AND FACTORS')
        # sectional properties of curved beam are calculated
        # and output to file
        if self._stiffness_flag :
            # _section.height = 1.1 * (self.geometry.D * self.geometry.t)**0.50
            #_D = _D + _Tft + _Tfb
            self._section.curved(self.orad)
            #(self.section.area, self.section.R, self.section.Iy, self.section.F, 
            # self.section.e, _c, _c1, _ki, _ko, _shearFactor) = ring.cibeam2(self.SectionType, self.orad,
            #                                                     _D, _Tw, _Bft, _Tft, _Bfb, _Tfb)
            #
            print ('section area :',self._section.area,'mm**2')
            print ('second moment of area @ centroid :',_section.Iy,'mm**4')
            print ('centroidal radius ',self._section.R)
            #print ('extreme fibre distances   ci: ',_c,',  co: ', _c1)
            print ('stress factors            ki: ', self._section.ki,',  ko: ',self._section.ko)
            print(' ')
            #
            # constant factors calculated and written to file
            
            _k1, _k2 = ring.factors2(self.E, self.G, self.nu, 
                                     _section.area, _section.R, 
                                     _section.Iy, _section.F, _section.e,
                                     _section.d)
        else:
            R = self.geometry.D
            _k1, _k2 = 1, 1
            _section = Rectangle()
            _section.d = 1.0 * self.units.m
            _section.t = self.geometry.t
            _section._get_properties()
            _section.curved(R)
        #
        # start of main subroutineme loop - each load case is called 
        # in turn control is passed to roark routines 
        #  insert extra roark cases here
        #
        #print('-->')
        for _ring in self.RingCase.values():
            _xloc, _xm, _xh, _xs = ring.circular_ring(_ring.case, _ring.ring_load.value,
                                                      _ring.theta.value, _ring.phi.value, 
                                                      _ring.phase.value, self.istep,
                                                      R.value, _k1, _k2,)
            #
            print ('LOCATION    MOMENT      THRUST      SHEAR')
            print ('             N.mm         N           N')
            for i in range (len(_xloc)):
                print("{:>5.1f}    {: 1.4e} {: 1.4e} {: 1.4e}"
                      .format(_xloc[i], _xm[i], _xh[i], _xs[i]))
                #
                self.xm[i] += _xm[i]
                self.xh[i] += _xh[i]
                self.xs[i] += _xs[i]
            print ('')
        #
        _tau_y = max([abs(_item.value) for _item in _section.tau_y])
        # stresses are calculated for superposition of 
        (self.cstmax, self.shmax, self.bstri, self.bstro,
         self.hstr, self.sstr, self.cstri, self.cstro) = ring.stress2(_section.Iy.value, _section.area.value, 
                                                                      _section.c.value, _section.c1.value, 
                                                                      _section.ki.value, _section.ko.value, 
                                                                      _tau_y, self.istep, 
                                                                      _xloc, self.xm, self.xh, self.xs)
        # SUPERPOSITION OF ALL LOAD CASES
        print ("")
        print ('                                                              SUPERPOSITION OF ALL LOAD CASES')
        print ('          -------------Forces-------------    -----------------------Stresses (n/mm^2)---------------------------')
        print ('Location  Moment     Thrust        Shear     Bending     Bending      Hoop        Shear       Bend.in.   Bend.out',)
        print ('Degrees    N.mm        N             N        Inner       Outer                               +Hoop       +Hoop',)
        for i in range(len(_xloc)):
            print ("{:>5.1f}  {: 1.4e} {: 1.4e} {: 1.4e} {: 1.4e} {: 1.4e} {: 1.4e} {: 1.4e} {: 1.4e} {: 1.4e}"
                   .format(_xloc[i], self.xm[i], self.xh[i], self.xs[i], self.bstri[i],
                           self.bstro[i], self.hstr[i],
                           self.sstr[i], self.cstri[i], self.cstro[i]))
        
        print ("")
        print ('max. combined stress    max. shear stress')
        print ('      N/mm**2                N/mm**2')
        print ("   {: 1.3f}            {: 1.3f}".format(self.cstmax, self.shmax))
        
        lo = min(self.xs)
        hi = max(self.xs)
        Vy = max(abs(lo), abs(hi))
        print ('')
        print  ("results written to specified filename")
#
#
#
#