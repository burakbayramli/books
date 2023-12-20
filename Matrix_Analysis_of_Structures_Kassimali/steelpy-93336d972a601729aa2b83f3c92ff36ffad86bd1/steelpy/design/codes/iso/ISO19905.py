#!/usr/bin/env python
#
#   ISO19905 Jackup Design Module 26/12/2011
#
# *******************************************
#                 Bug History
# 
#
# *******************************************
#
#
from __future__ import print_function
import math
import sys
import datetime
from buckingham import Number, FindLength, FindForce, FindMass
from SectProp import *
#
#
# ----------------------------------------
#      Sections Profiles Names
# ----------------------------------------
#
def SecFinder(SecID):
    """
    """
    _SecID = str(SecID).lower()
    _SecID = _SecID.replace('section','')
    _SecID = _SecID.replace('shape','')
    _SecID = _SecID.replace('type','')
    _SecID = _SecID.replace('member','')
    _SecID = _SecID.replace('chord','')
    _SecID = _SecID.replace('rack','')
    _SecID = _SecID.replace('opposed','')
    _SecID = _SecID.replace(' ','')
    _SecID = _SecID.replace('-','')
    _SecID = _SecID.replace('_','')
    _SecID = _SecID.strip()
    
    _TubularSplit = ['splittubular', 'tubularsplit']
    _Tubular = ['hssround','pipe','tubular','chs','circular']
    _Triangular = ['triangular']
    
    if _SecID in _Tubular:
        _FinalSec = 'tubular'
    
    elif _SecID in _TubularSplit:
        _FinalSec = 'rack_split_tubular'
    
    elif _SecID in _Triangular:
        _FinalSec = 'triangular'
    
    else:
        print('error section {} no available'.format(_SecID))
        sys.exit('error section {} no available'.format(_SecID))
    
    #
    return _FinalSec
    #
#
def CompTypeFinder(compType):
    """
    """
    _compType = str(compType).lower()
    _compType = _compType.replace('component','')
    _compType = _compType.replace('chord','')
    _compType = _compType.replace('plate','')
    _compType = _compType.replace('type','')
    _compType = _compType.replace('tubular','')
    _compType = _compType.replace('section','')
    #_compType = _compType.replace('flange','')
    _compType = _compType.replace(' ','')
    _compType = _compType.replace('-','')
    _compType = _compType.replace('_','')
    _compType = _compType.strip()
    
    #_Internal = ['internal', 'int']
    #_Outstand = ['outstand', 'out']
    _rackPlate = ['1', 'one','rack']
    _splitTubular = ['2', 'two','split']
    _sidePlate = ['3', 'three', 'side']
    _backPlate = ['4', 'four', 'back']
    #back_plate
    
    #if _compType in _Internal:
    #    _finalCompType = 'internal'
    
    #elif _compType in _Outstand:
    #    _finalCompType = 'outstand'
    
    if _compType in _rackPlate:
        _finalCompType = 'rack_plate'
    
    elif _compType in _splitTubular:
        _finalCompType = 'split_tubular'
    
    elif _compType in _sidePlate:
        _finalCompType = 'side_plate'
    
    elif _compType in _backPlate:
        _finalCompType = 'back_plate'
    
    else:
        comp_error(_compType)
    
    #
    return _finalCompType
    
#
def analysisFinder(analysisType):
    """
    """
    _analysisType = str(analysisType).lower()
    _analysisType = _analysisType.replace('analysis','')
    _analysisType = _analysisType.replace(' ','')
    _analysisType = _analysisType.replace('-','')
    _analysisType = _analysisType.replace('_','')
    _analysisType = _analysisType.strip()
    
    _linear = ['linear', 'elastic', 'firstorder',
               'firstorderlinearelastic']
    _nonlinear = ['nonlinear', 'plastic', 'secondorder',
                  'pdelta']
    
    if _analysisType in _linear:
        _aTypeOut = 'linear'
    
    elif _analysisType in _nonlinear:
            _aTypeOut = 'nonlinear'
    
    else:
        comp_error(_analysisType)
    #
    return _aTypeOut
    #
#
#
def unitFactor(UnitsType, UnitIn, UnitOut):
    """
    Finds factor for units change
    
    UnitsType : string
          force/length/mass
    UnitIn    : string
    UnitOut   : string
    """
    #
    if UnitOut == '':
        UnitOut = UnitIn
    #
    #
    if UnitsType == 'force':
        _UnitIn = FindForce(UnitIn)
        _UnitOut = FindForce(UnitOut)
    #
    elif UnitsType == 'length':
        _UnitIn = FindLength(UnitIn)
        _UnitOut = FindLength(UnitOut)
    #
    elif UnitsType == 'mass':
        _UnitIn = FindMass(UnitIn)
        _UnitOut = FindMass(UnitOut)
    #
    else:
        print("error units {} not recognized".format(UnitsType))
        sys.exit("error units {} not recognized".format(UnitsType))
    
    #
    #print ('Unit Length Out =' , UnitLengthOut )
    #
    _Unit = Number(1, dims = str(_UnitIn))
    _UnitFactor = _Unit.convert(str(_UnitOut)).value
    #print ('Final ==>',_UnitLengthFactor)
    #
    #return UnitLengthOut, _UnitLengthFactor
    return _UnitFactor
    #
#
# ----------------------------------------
#      Sections Componet Profiles 
# ----------------------------------------
#
#
def rackSplitTubSec(self):
    """
    """
    print("")
    print("---- Rack Split Tubular Section Properties ---")
    print("")
    #
    _Case = ['', '']
    _Area = [0, 0]
    _Y = [0, 0]
    _Z = [0, 0]
    _Iy = [0, 0]
    _Iz = [0, 0]
    _Zz = [0, 0]
    _App = [0, 0]
    _diy = [0, 0]
    _lambdap = [0, 0]
    _lambdar = [0, 0]
    _lambdah = [0, 0]
    _Ip = [0, 0]
    _Fy = []
    _E = []
    _Av = [0, 0]
    _hrp = 0
    
    for comp in self.component.itervalues():
        #
        if comp.name == 'rack_plate':
            _LengthFactor = unitFactor('length', comp.unitLength, self.UnitLength)
            _ForceFactor = unitFactor('force', comp.unitForce, self.UnitForce)
            #print(comp.Type, _unitLengthFactor)
            #
            #if comp.Class < 4:
            # call rectagular section [ + area 1]
            _a = 0.50 * comp.t * _LengthFactor
            _hrp = _a
            _b = comp.D * _LengthFactor
            _tcomp = comp.t * _LengthFactor
            _Case[0] = 'add'
            
            # check Applicability
            _Fact_d = unitFactor('length', comp.unitLength, 'meter')
            _d = comp.d * _Fact_d
            A12_6_1(_b, _tcomp , comp.Fy, comp.E, _d)
            
            #
            (_Area[0], _Y[0], _Z[0], _Iy[0], _Sy, _Zy, _ry, 
             _Iz[0], _Sz, _Zz[0], _rz) = BarRactangular(_a, _b)
            
            # elastic centre
            _Y[0] = _Y[0] + _a / 2.0
            # plastic centre
            _diy[0] = _a / 2.0
            #
            _App[0] = comp.Fy * _Area[0]
            
            # for rectangular rolled or welded web or flange components
            # supported along both edges:
            _lambdap[0] = 1.17 * math.sqrt(comp.E / comp.Fy)
            _lambdar[0] = 1.44 * math.sqrt(comp.E / comp.Fy)
            _lambdah[0] = _b / _tcomp
            #
            #
            _Fy.append(comp.Fy * _ForceFactor / _LengthFactor**2)
            _E.append(comp.E * _ForceFactor / _LengthFactor**2)
            #
            #
            _Av.append(_a * _b)
            #
            _Ip[0] = _a * _b * (_b**2 + _a**2) / 12.0
            #
        #
        elif comp.name == 'split_tubular':
            
            _LengthFactor = unitFactor('length', comp.unitLength, self.UnitLength)
            _ForceFactor = unitFactor('force', comp.unitForce, self.UnitForce)
            
            #
            # call elliptical Segment [ + area 2]
            _Diam = comp.D * _LengthFactor
            _rad = _Diam / 2.0
            _b = comp.b * _LengthFactor
            _tw = comp.t * _LengthFactor
            _tmax = comp.tmax * _LengthFactor
            _Case[1] = 'add'
            #
            
            # check Applicability
            _Fact_Fy_L = unitFactor('length', comp.unitLength, 'mm')
            _Fact_Fy_F = unitFactor('force', comp.unitForce, 'N')
            _Fymod = comp.Fy * _Fact_Fy_F / _Fact_Fy_L**2
            A12_5_1(_Diam, _tw, comp.Fy, comp.d)
            
            # find properties
            # circle
            if _rad == _b:
                (_Area[1], _Y[1], _Z[1], _Iy[1], _Sy, _Zy, _ry, 
                 _Iz[1], _Sz, _Zz[1], _rz, _diy[1]) = HollowSemicircle(_rad, _tw)
                #
                (_Area3, _Yc3, _Zc3, _Iy3, _Sy3, _Zy3, _ry3, 
                 _Iz3, _Sz3, _Zz3, _rz3, _Yp3) = HollowSemicircle(_rad, _tmax)
                
                # perimer of circle
                _perimeter = math.pi * _Diam
            
            # ellipse
            else:
                (_Area[1], _Y[1], _Z[1], _Iy[1], _Sy, _Zy, _ry,  
                 _Iz[1], _Sz, _Zz[1], _rz, _diy[1]) = HollowSemiellipse(_rad, _b, _tw)
                #
                (_Area3, _Yc3, _Zc3, _Iy3, _Sy3, _Zy3, _ry3, 
                 _Iz3, _Sz3, _Zz3, _rz3, _Yp3) = HollowSemiellipse(_rad, _b, _tmax)
                
                # perimer of ellipse [after Ramanujan's formula]
                _ab1 = 3 * (_rad + _b)
                _ab2 = (3 * _rad + _b)
                _ab3 = (_rad + 3 * _b)
                _perimeter = math.pi * ( _ab1 - math.sqrt(_ab2 * _ab3))
            
            #
            _App[1]  = comp.Fy * _Area[1]
            
            # for components derived from tubulars (with reference to Table A.12.2-1)
            _lambdap[1] = 0.077 * comp.E / comp.Fy
            _lambdar[1] = 0.102 * comp.E / comp.Fy
            #_lambdah[0] = _Diam  / _tw
            _lambdah[1] = _Diam  / _tw
            #
            #
            _Fy.append(comp.Fy * _ForceFactor / _LengthFactor**2)
            _E.append(comp.E * _ForceFactor / _LengthFactor**2)
            #
            #
            _Av.append(table_A12_6_1('tubular', _Area[1]))
            #
            _Ip[1] = (math.pi/32.0) * (_Diam**4 - (_Diam - 2 * _tmax)**4)
            #
        #    
        else:
            _LengthFactor = unitFactor('length', comp.unitLength, self.UnitLength)
            _ForceFactor = unitFactor('force', comp.unitForce, self.UnitForce)
            #
            #
            _Fy.append(comp.Fy * _ForceFactor / _LengthFactor**2)
            _E.append(comp.E * _ForceFactor / _LengthFactor**2)
            #
            if 'rack_plate' in comp.name:
                _b = comp.b * _LengthFactor
                _t = comp.t * _LengthFactor
                _Av.append(_b * _t)
                #
                #
            #
    #
    #
    # From parallel axis theorem combine sections (half)
    _Y[1] = _Y[1] + _hrp
    _diy[1] = _diy[1] + _hrp
    _A, _y, _z, _Iyy, _Izz = parallel_axis_theorem(_Case, _Area, _Y, _Iy, _Z, _Iz)
    #print("")
    #print ("A = {: 1.4e} \nYc = {: 1.4e} \nXc = {: 1.4e} \nIy = {: 1.4e}  \nIz = {: 1.4e}"
    #       .format(_A, _y, _z, _Iyy, _Izz))
    #
    print("-----------------")
    _unitStress = str(self.UnitForce) + '/' + str(self.UnitLength) + '^2'
    _m2  = str(self.UnitLength) + '^2'
    _m3 = str(self.UnitLength) + '^3'
    _m4 = str(self.UnitLength) + '^4'    
    
    # Fymin is the minimum yield strength of the Fyi of all
    # components in the cross-section of a prismatic member
    self.Fymin = Number(min(_Fy), dims = _unitStress)
    print('Fymin = {:1.4e}'.format(self.Fymin.convert(_unitStress).value))
    
    # Fylbt
    # yield strength, Fy of the material that first yields
    # when bending about the minor axis.
    # Conservatively, Fy may be taken as the maximum yield
    # strength of all the components in a prismatic cross
    # section
    self.Fylbt = Number(max(_Fy), dims = _unitStress)
    print('Fylbt = {:1.4e}'.format(self.Fylbt.convert(_unitStress).value))
    
    self.E = Number(min(_E), dims = _unitStress)
    print('E min = {:1.4e}'.format(self.E.convert(_unitStress).value))
    
    # Av is the effective shear area in the direction being
    # considered, see Table A.12.6-1;
    self.Av = Number(2 * sum(_Av), dims = _m2)
    print('Av    = {:1.4e}'.format(self.Av.value))
    
    # calculate full cross section (2 half)
    a = _rad 
    b = _rad + _hrp
    if a == 0: a = b
    
    (_Af, _Yc, _Zc, _Iy, _Sy, _ry, 
     _Iz, _Sz, _rz ) = closed_cross_section(a, b, _A, _y, _Iyy, _Izz)
    
    _Yc = max(_Yc, _Diam - _Yc)
    self.Yc = Number(_Yc, dims = str(self.UnitLength))
    self.Zc = Number(_Zc, dims = str(self.UnitLength))
    #
    self.Iy = Number(_Iy, dims = _m4)
    self.Sy = Number(_Sy, dims = _m3)
    self.ry = Number(_ry, dims = str(self.UnitLength))
    #
    self.Iz = Number(_Iz, dims = _m4)
    self.Sz = Number(_Sz, dims = _m3)
    self.rz = Number(_rz, dims = str(self.UnitLength))
    
    # Ao the area enclosed by the median line of the perimeter
    # material of the section
    _Ao = 2 * (_Area[0] + _Area3)
    
    # Sigma_bt
    # the sum of the width and thickness, respectively, 
    # of each component (wall of the section) forming
    # the closed perimeter
    _Sigma_bt = ((_perimeter / _tmax)
                 + (_Diam / _tcomp))     
    
    # torsion constant
    self.J = Number((4.0 * _Ao**2) / _Sigma_bt , dims = _m4)
    
    # Ip = polar moment of inertia
    self.Ip = Number(2 * sum(_Ip), dims = _m4)

    #if self.SectionClass < 3:
    #
    #  A.12.3.2.1 Axial properties Class 1 and class 2 sections
    
    # Ptp is the representative axial tensile strength of a non-circular
    # prismatic member and is the representative local axial compressive
    # strength of a non-circular prismatic member
    self.Ptp = Number(2 * (_App[0] + _App[1]) , dims = str(self.UnitForce))
    
    # For axial tension and compression, the fully plastic effective
    # cross-sectional area for use in a strength assessment, Ap is:
    self.Ap = Number(self.Ptp.convert(self.UnitForce).value  
                     / self.Fymin.convert(_unitStress).value , dims = _m2)
    
    #
    # A.12.3.2.2 Flexural properties Class 1 and class 2 sections
    #
    # The fully plastic effective section modulus Zp is: 
    # in plane
    _summApdi = 2 * (_App[0] * _diy[0] + _App[1] * _diy[1])
    self.Zpy = Number(_summApdi / self.Fymin.convert(_unitStress).value,
                      dims = _m3)
    # out of plane
    self.Zpz = Number(2 * sum(_Zz), dims = _m3)
    
    # is the eccentricity between the axis used for structural 
    # analysis and that used for structural strength checks, 
    # taking due account of the sign in combination with the 
    # sign convention for Pu.
    self.ey = Number(0, dims = str(self.UnitLength))
    self.ez = Number(0, dims = str(self.UnitLength))
    #
    #
    print("-----------------")
    print("Ap  = {:1.4e} ".format(self.Ap.convert(_m2).value))
    print("Y   = {:1.4e} ".format(self.Yc.convert(self.UnitLength).value))
    print("Iyy = {:1.4e} ".format(self.Iy.convert(_m4).value))
    print("Zpy = {:1.4e} ".format(self.Zpy.convert(_m3).value))
    print("ry  = {:1.4e} ".format(self.ry.convert(self.UnitLength).value))
    print("Izz = {:1.4e} ".format(self.Iz.convert(_m4).value))
    print("Zpz = {:1.4e} ".format(self.Zpz.convert(_m3).value))
    print("rz  = {:1.4e} ".format(self.rz.convert(self.UnitLength).value))
    print("-----------------")
    #print("Ao  = {:1.4e} ".format(_Ao))
    #print('Sigmabt = {: 1.4e}'.format( _Sigma_bt ))
    print("J   = {:1.4e} ".format(self.J.convert(_m4).value))
    print("-----------------")
    print("Ip  = {:1.4e} ".format(self.Ip.convert(_m4).value))
    print("ex  = {:1.4e}".format(self.ey.convert(self.UnitLength).value))
    print("ey  = {:1.4e}".format(self.ez.convert(self.UnitLength).value))
    print("Pp  = {:1.4e} ".format(self.Ptp.convert(self.UnitForce).value))
    print("-----------------")
    #
    #return _Ap,  _Zpy, _Zpz
    #
    #
    #
    #
#
#
# ----------------------------------------
#      ISO19905 Section 1 & 2
# ----------------------------------------
#
# -----------------------------------
# A.12.2.3 Classification definitions
#
def A12_2_Classification(CompType, Build, D, b, tw, Fy, E,
                         sigma1 = 0.5, sigma2 = 0.5, alpha = 1.0):
    """
    A.12.2 Classification definitions
    """
    #
    print("")
    print ("A.12.2.3 Classification definitions")
    #
    # A.12.2.3.1 Tubular member classification
    if CompType == 'tubular':
        # A cross-section of a tubular member is a class 1 section when:
        _class_comp = 2
        _ratio = D / tw
        _limit = 0.0517 * E / Fy
        
        if _ratio <= _limit:
            _class_comp = 1
        
        _class_bend = _class_comp
        _case = 'A.12.2.3.1'
        _case_ratio = 'D/t'
        
        # NOTE Compliance with class 1 classification is only relevant
        # when undertaking earthquake, accidental or alternative
        # strength analyses (see 10.7, 10.8 and 10.9). In all other 
        # cases the distinction between class 1 (plastic) and class 2
        # (compact) is irrelevant to the assessment.
    
    #
    # A.12.2.3.2 Non-circular prismatic member classification
    # Non-circular prismatic members that contain curved or tubular
    # components should have the curved components classified based on 
    # the values given in Table A.12.2-1 and their flat components 
    # classified based on Tables A.12.2-2 to A.12.2-4. 
    # The limits given in Table A.12.2-1 tend to be conservative as in
    # most cases there is additional support for the curved component 
    # by the flat components (e.g. the rack in a split tube chord
    # reinforces the split tube and helps to prevent local buckling).
    # When the limits given in Table A.12.2-1 are considered to be too
    # onerous, it could be possible to justify the use of alternative
    # limits through rational analysis.
    
    elif CompType == 'curved':
        (_class_comp, _class_bend,
        _lambdap_c, _lambdar_c, 
        _lambdap_b, _lambdar_b ) = table_A12_2_1(D, tw, Fy, E)
        #print("class =" , _class )
    
    #
    elif CompType == 'internal':
        # Flange internal components
        _b = b
        (_class_comp_flange, _class_bend_flange, 
         _lambdap_flange, _lambdar_flange) = table_A12_2_2(_b, tw, Fy, E)
        #
        (_class_comp_web, _class_bend_web, _class_comb_web,
         _lambdap_web, _lambdar_web) = table_A12_2_4(_b, tw, Fy, E)
        
        _class_comp = max(_class_comp_flange, _class_comp_web)
        _class_bend = max(_class_bend_flange, _class_bend_web, _class_comb_web )
    #
    elif CompType == 'outstand':
        # Outstand components
        _b = b - D
        if _b > 0:
            (_class_comp, _class_bend,
             _lambdap, _lambdar) = table_A12_2_3(Build, _b, tw, Fy, E)
        
        else:
            print("no outstand components to be checked")
    #
    else:
        comp_error(CompType)
    #
    #
    if max(_class_comp, _class_bend) == 3:
        print("section class 3 ==> no yet implemented")
        sys.exit("section class 3 ==> no yet implemented")
    #
    elif max(_class_comp, _class_bend) == 4:
        print("section class 4 ==> no yet implemented")
        sys.exit("section class 4 ==> no yet implemented")
    #
    else:
        return _class_comp, _class_bend
    #
#
# Table A.12.2-1  Classification limits for non-circular prismatic members
# containing curved components
def table_A12_2_1(d, tw, Fy, E):
    """
    Table A.12.2-1
    Classification limits for non-circular prismatic members
    containing curved components    
    """
    # Section in bending
    #
    _ratio = d / tw
    _class_comp = 1
    _class_bend = 1
    
    # for sections type 3
    # for components derived from tubulars 
    _lambdap_c = 0.077 * E / Fy   # (A.12.6-10)
    # (with reference to from AISC)
    _lambdar_c = 0.102 * E / Fy   # (A.12.6-14)
    
    # for components derived from circular tubes and
    # subject to pure bending
    _lambdap_b = 0.103 * E / Fy   # (A.12.6-29)
    #
    _lambdar_b = 0.220 * E / Fy   # (A.12.6-35)
    
    #
    #
    if _ratio <= 0.052 * E / Fy:
        _class_bend = 1
    
    elif _ratio <= 0.103 * E / Fy:
        _class_bend = 2
    
    elif _ratio <= 0.220 * E / Fy:
            _class_bend = 3    
    
    else: _class_bend = 4
    
    # Section in compression
    
    if _ratio <= 0.052 * E / Fy:
        _class_comp = 1
    
    elif _ratio <= 0.077 * E / Fy:
        _class_comp = 2
    
    elif _ratio <= 0.102 * E / Fy:
            _class_comp = 3    
    
    else: _class_comp = 4
    #
    #
    print("")
    print("Table A.12.2-1 d/t / (E/Fy): {: 1.4e}".format(_ratio / (E / Fy)))    
    return _class_comp, _class_bend, _lambdap_c, _lambdar_c, _lambdap_b, _lambdar_b
    #
#
# Table A.12.2-2  Cross-section classification - Flange internal components
def table_A12_2_2(b, tw, Fy, E):
    """
    Table A.12.2-2
    Cross-section classification Flange internal components
    Limiting width-to-thickness ratios for compressed internal
    components
    """
    # Section in bending & compression
    #
    _ratio = b / tw
    _class_comp = 1
    _class_bend = 1
    #
    # for rectangular rolled or welded web or flange components
    # supported along both edges
    _lambdap = 1.17 * math.sqrt(E / Fy)  # (A.12.6-7)
    # 
    _lambdar = 1.44 * math.sqrt(E / Fy)  # (A.12.6-11)
    
    #
    #
    if _ratio <= 1.03 * math.sqrt(E / Fy):
        _class_bend = 1
        _class_comp = 1
    
    elif _ratio <= 1.17 * math.sqrt(E / Fy):
        _class_bend = 2
        _class_comp = 2
    
    elif _ratio <= 1.44 * math.sqrt(E / Fy):
            _class_bend = 3
            _class_comp = 3
    
    else: 
        _class_bend = 4
        _class_comp = 4
    
    #
    print("")
    print("Table A.12.2-2 b/t / (E/Fy)^0.50: {: 1.4e}".format(_ratio / math.sqrt(E / Fy))) 
    return _class_comp, _class_bend, _lambdap, _lambdar
    #
#
# Table A.12.2-3  Cross-section classification - outstand components
def table_A12_2_3(Build, b, tf, Fy, E, alpha = 1.0,
                  sigma1 = 0.5, sigma2 = 0.5):
    """
    Table A.12.2-3
    Cross-section classification Outstand components
    Limiting width-to-thickness ratios for outstand components
    
    psi : is the ratio of compressive stress to bending stress
    sigma1 : is the compressive stress if sigma2 is tensile or the
             larger compressive stress if sigma2 is also compressive
    sigma2 : is the tensile stress if sigma2 is tensile or the 
             smaller compressive stress if sigma 2 is compressive
    """
    # NOTES
    # In the figures relating to stress distributions, the dimension
    # b is illustrated only in the case of rolled sections. For welded
    # sections, b should be assigned as shown in the diagrams at the 
    # top of the table.
    #
    # When determining alpha for Class 1 & 2 members, the loads should 
    # be scaled to give a fully plastic stress distribution. For all
    # classes it is conservative to use the relevant compression case
    #
    _ratio = b / tf
    _class_comp = 1
    _class_comb = 1
    _psi = sigma2 / sigma1
    _build = str(Build).lower
    
    # tip in compression
    if _psi >= -1.0 and _psi <= 1.0:
        _Ksigma_c = 0.57 - 0.21 * _psi + 0.07 * _psi**2
    
    else:
        print("error ksigma compression")
        sys.exit("error ksigma")
        
    # tip in tension
    if _psi <= 1.0 and _psi >= 0.0:
        _Ksigma_t = 0.578 / (_psi + 0.34)
    
    elif _psi < 0.0 and _psi >= -1.0:
        _Ksigma_t = 1.70 - 5.0 *_psi + 17.1 * _psi**2
    
    else:
        print("error ksigma tension")
        sys.exit("error ksigma")
    
    # Rolled Section
    if _build == 'rolled':
        #
        # for rectangular rolled flange or web components 
        # supported along one edge and subject to combinations
        # of compression and bending:
        _lambdap = 0.37 * math.sqrt(E / Fy)  # (A.12.6-8) & (A.12.6-25)
        # 
        _lambdar = 0.55 * math.sqrt(E / Fy)  # (A.12.6-12) & (A.12.6-31)
        
        #
        # Section in compression
        
        if _ratio <= 0.33 * math.sqrt(E / Fy):
            _class_comp = 1
        
        elif _ratio <= 0.37 * math.sqrt(E / Fy):
            _class_comp = 2
        
        elif _ratio <= 0.55 * math.sqrt(E / Fy):
            _class_comp = 3        
        
        else: _class_comp = 4
        
        # Section in bending & compression
        # check tip in compression & tension
        if (_ratio <= 0.33 / alpha * math.sqrt(E / Fy) 
            and _ratio <= (0.33 / (alpha * math.sqrt(alpha)) 
                          * math.sqrt(E / Fy))):
            _class_comb = 1
        
        elif (_ratio <= 0.37 / alpha * math.sqrt(E / Fy) 
              and _ratio <= (0.37 / (alpha * math.sqrt(alpha)) 
                            * math.sqrt(E / Fy))):
            _class_comb = 2
        
        elif (_ratio <= 0.84 * math.sqrt(_Ksigma_c * E / Fy) 
              and _ratio <= 0.84 * math.sqrt(_Ksigma_t * E / Fy)):
            _class_comb = 3    
        
        else: _class_comb = 4
    
    # Welded Section
    else:
        #
        # for rectangular welded flange or web components
        # supported along one edge and subject to
        # combinations of compression and bending:
        _lambdap = 0.33 * math.sqrt(E / Fy)  # (A.12.6-9) & (A.12.6-26)
        # 
        _lambdar = 0.50 * math.sqrt(E / Fy)  # (A.12.6-13) & (A.12.6-32)
        
        # Section in compression
        if _ratio <= 0.30 * math.sqrt(E / Fy):
            _class_comp = 1
        
        elif _ratio <= 0.33 * math.sqrt(E / Fy):
            _class_comp = 2
        
        elif _ratio <= 0.50 * math.sqrt(E / Fy):
            _class_comp = 3   
        
        else: _class_comp = 4
        
        # Section in bending & compression
        # check tip in compression & tension
        if (_ratio <= 0.3 / alpha * math.sqrt(E / Fy) 
            and _ratio <= (0.3 / (alpha * math.sqrt(alpha)) 
                          * math.sqrt(E / Fy))):
            _class_comb = 1
        
        elif (_ratio <= 0.33 / alpha * math.sqrt(E / Fy) 
              and _ratio <= (0.33 / (alpha * math.sqrt(alpha)) 
                            * math.sqrt(E / Fy))):
            _class_comb = 2
        
        elif (_ratio <= 0.76 * math.sqrt(_Ksigma_c * E / Fy) 
              and _ratio <= 0.76 * math.sqrt(_Ksigma_t * E / Fy)):
            _class_comb = 3    
        
        else: _class_comb = 4
    #
    print("")
    print("alpha : {: 1.4e}".format(alpha))
    print("Table A.12.2-3 b/t / (E/Fy)^0.50: {: 1.4e}".format(_ratio / math.sqrt(E / Fy)))
    return _class_comp, _class_comb, _lambdap, _lambdar
    #
#
# Table A.12.2-4  Cross-section classification - web internal components
def table_A12_2_4(d, tw, Fy, E, alpha = 1.0,
                  sigma1 = 0.5, sigma2 = 0.5):
    """
    Table A.12.2-4
    Cross-section classification Web internal components
    Limiting width-to-thickness ratios for web internal components
    """
    # NOTES
    # When determining alpha for Class 1 & 2 members, the loads 
    # should be scaled to give a fully plastic stress distribution. For
    # all classes it is conservative to use the relevant compression case
    #
    _ratio = d / tw
    _class_comp = 1
    _class_bend = 1
    _class_comb = 1
    _psi = sigma2 / sigma1
    
    #
    # for rectangular rolled or welded web components supported
    # along both edges and subject to combinations of compression
    # and bending:
    if alpha > 0.5:
        _lambdap = ((4.82 * math.sqrt(E/Fy)) 
                    / (5.12 * alpha - 1))              # (A.12.6-27)
    # 
    else:
        _lambdap = 1.55 * math.sqrt(E/Fy) / alpha      # (A.12.6-28)
    # 
    if _psi > -1.0:
        _lambdar = ((1.44 * math.sqrt(E/Fy)) 
                    / (0.674 + 0.327 * _psi))          # (A.12.6-11)
    #
    else:
        _lambdar = (2.07 * (1 -_psi) 
                    * math.sqrt(abs(_psi)) 
                    * math.sqrt(E/Fy))                 # (A.12.6-34)
    
    #
    # Web subject to bending
    if alpha == 0.50:
        if _ratio <= 2.56 * math.sqrt(E/Fy):
            _class_bend = 1
        
        elif _ratio <= 3.09 * math.sqrt(E/Fy):
            _class_bend = 2
        
        elif _ratio <= 4.14 * math.sqrt(E/Fy):
            _class_bend = 3
        
        else: _class_bend = 4
    
    # Web subject to compression
    elif alpha == 1.0:
        if _ratio <= 1.03 * math.sqrt(E/Fy):
            _class_comp = 1
        
        elif _ratio <= 1.17 * math.sqrt(E/Fy):
            _class_comp = 2
        
        elif _ratio <= 1.44 * math.sqrt(E/Fy):
            _class_comp = 3
        
        else: _class_comp = 4        
    
    # Web subject to bending and compression
    else:
        if alpha > 0.50:
            if _ratio <= (5.18 * math.sqrt(E/Fy)) / (6.043 * alpha - 1):
                _class_comb = 1
            
            elif _ratio <= (4.82 * math.sqrt(E/Fy)) / (5.12 * alpha - 1):
                _class_comb = 2
        
        # d/tw < 0.5
        else:
            if _ratio <= 1.28 * math.sqrt(E/Fy) / alpha:
                _class_comb = 1
            
            elif _ratio <= 1.55 * math.sqrt(E/Fy) / alpha:
                _class_comb = 2
            
            else:
                if _psi > -1.0:
                    if _ratio <= ((1.44 * math.sqrt(E/Fy)) 
                                  / (0.674 + 0.327 * _psi)):
                        _class_comb = 3
                    
                    else: _class_comb = 4
                
                else:
                    if _ratio <= (2.07 * (1 -_psi) 
                                  * math.sqrt(abs(_psi)) * math.sqrt(E/Fy)):
                        _class_comb = 3
                    
                    else: _class_comb = 4
    #
    print("")
    print("alpha : {: 1.4e}".format(alpha))
    print("Table A.12.2-4 d/t / (E/Fy)^0.50: {: 1.4e}".format(_ratio / math.sqrt(E / Fy))) 
    return _class_comp, _class_bend, _class_comb, _lambdap, _lambdar
    #
#
# Lateral Torsional Buckling
def LateralTorsionalBuckling(Section, Lb, rltb, Fyltb, E, A, I1, I2, Zp, J, Fymin):
    """
    Lateral Torsional Buckling
    
    Parameters
    ----------
    Lb : float
    rltb : float
       Is the radius of gyration about the minor axis as 
       defined in Equation (A.12.3-6)
    E  : float
    Fyltb : float
       Is the yield strength, Fy of the material that first
       yields when bending about the minor axis
    A  : float
       Is the gross cross-sectional area;
    I1 : float
       Is the major axis second moment of area of the gross
       cross-section
    I2 : float
       Is the minor axis second moment of area of the gross
       cross-section
    J  : float
       Is the torsion constant
    
    """
    #
    # Prismatic members which do not satisfy the following simplified
    # lateral torsional buckling criteria should be assessed further 
    # to determine a reduced member bending strength Mb using the 
    # guidance given in A.12.6.2.6
    
    # Singly symmetric open sections, from F2-5 of AISC [A.12.5-1]:
    if Section == 'open':
        _Lbmax = rltb * (1.760 * math.sqrt(E / Fyltb))         # (A.12.2-2)
    
    # The LTB criterion for any closed section is derived from 
    # BS 5400-3 [A.12.5-2]
    else:
        _Lbmax = (rltb * ((0.36 * I2 * E / (Zp * Fymin)) 
                          * (math.sqrt(A * J / ((I1 - I2) 
                                                * (I1 - J / 2.6))))))  # (A.12.2-3)
    #
    print("")
    print("A.12.6.2.6 Lateral torsional buckling strength check")
    #
    if Lb > _Lbmax:
        print("Lb [{: 1.4e}] > Lbmax [{: 1.4e}] fail".format(Lb / rltb , _Lbmax / rltb ))
        sys.exit()
    
    else:
        print("Lb [{: 1.4e}] < Lbmax [{: 1.4e}] pass".format(Lb / rltb , _Lbmax / rltb ))
    #
    return _Lbmax
    #
#
# 
def BS_5400():
    pass
#
# A.12.2.3.3 Reinforced components
def A12_2_3_3():
    #
    # When a reinforcing component is used, there should be
    # four independent checks of the cross section classification 
    # in accordance with Tables A.12.2-2 to A.12.2-4:
    #
    # 1) the reinforcing plate (using t2) over the width b2, 
    #    using increased buckling coefficient (see below)
    # 2) the combined plate using tcheck over width b1
    # 3) the base plate (using t1) over the width b2 using 
    #    increased buckling coefficient (see below)
    # 4) the base plate (using t1) over the dimension of the 
    #    unreinforced widths (conservatively taken as b1-b2)
 
    # If the cross section is found to be slender (class 4), then 
    # the effective width of each of the base plate, reinforcing 
    # plate, and the combined plate should be determined from 
    # Table A.12.3-2.
    pass
#
# -----------------------------------------------------------
# A.12.3 Section properties of non-circular prismatic members
#
def A12_3():
    """
    Section properties of non-circular prismatic members
    """
    # Cross-sectional properties appropriate for the strength assessment
    # of non-circular prismatic members of all classes should be determined 
    # as described in A.12.3.2 to A.12.3.4; the nomenclature and definition
    # of variables is summarized in A.12.3.5. The properties appropriate for
    # the stiffness assessment of prismatic members should be based on elastic
    # considerations.
    
    # Where elastic section properties are determined for class 1 and 2 
    # sections in place of plastic section properties (e.g. for Euler 
    # amplification calculations or structural analysis input of stiffness 
    # parameters), these should be determined in accordance with A.12.3.3.
    
    # Cross-sectional properties are normally required in respect of both 
    # major and minor axes of a non-circular prismatic member.
    
    # Cross-sectional properties for tubular members are specified in A.12.5.
    # The cross-sectional properties used in the stiffness model (e.g. when
    # determining structural deflections and natural periods) can differ from
    # those used when assessing member strengths. For example, leg chord
    # properties may include approximately 10 % of the maximum rack tooth area
    # when determining the leg stiffness. This additional material should not be
    # included when calculating the section properties for strength assessment, 
    # except it may be used when determining the column buckling strength 
    # (A.12.6.2.4) and moment amplification (A.12.4).    
    pass
#
# Table A.12.3-1  Section properties  Effective widths for components
# in slender sections
def tableA12_3_1(SecName, CompType, b, t, Fy, E, sigma1 = 0.5, sigma2 = 0.5):
    """
    Table A.12.3-1  
    Section properties  Effective widths for components in slender sections
    
    psi is the ratio of compressive stress to bending stress;
    sigma 1 is the compressive stress if sigma 2 is tensile or 
            the larger compressive stress if sigma 2 is also 
            compressive;
    sigma 2 is the tensile stress if sigma 2 is tensile or the 
            smaller compressive stress if sigma 2 is compressive;
    k is the buckling coefficient;
    rho is the reduction coefficient;
    lambda p is the plate slenderness parameter;
    lambda plim is the limiting plate slenderness ratio;
    lambda po is the plate slenderness ratio coefficient;
    """
    # Effective sections should be based on actual plating thicknesses 
    # combined with plating effective widths. The effective widths of 
    # compression flange internal or outstand components should be 
    # determined in accordance with the formulae presented in Table 
    # A.12.3-1 a) or b), respectively. The effective widths of web internal
    # components subject to compression and/or bending should be determined
    # as shown in Table A.12.3-1 c) for which the following definitions 
    # apply, compression is taken as positive and tension as negative)
    
    d = b
    _psi = sigma2 / float(sigma1)
    print("")
    print("psi : {}".format(_psi))
    
    # find the buckling coefficient K
    if _psi <= 1 and _psi > 0:
        _K = 8.2 / (1.05 + _psi)
    
    elif _psi <= 0 and _psi > -1:
        _K = 7.81 - 6.29 * _psi + 9.78 * _psi**2
    
    elif _psi <= -1 and _psi > -3:
        _K = 5.98 * (1 - _psi)**2
    
    else:
        print("error psi ({}) not within the range [table A.12.3-1]".format(_psi))
        sys.exit("error psi ({}) not within the range [table A.12.3-1]".format(_psi))
    
    print("k : {}".format(_K))
    
    # the plate slenderness parameter lambda P
    _lambdaP = 1.04 * (d / t) * math.sqrt(Fy / (E * _K))
    
    # find the reduction coefficient rho
    if _lambdaP > 0.75 and (3 + _lambdaP) >= 0:
        _rho = (_lambdaP - 0.047 * (3 + _lambdaP)) / _lambdaP**2
    
    elif _lambdaP <= 0.75:
        _rho = 1.0
    
    else:
        print("error lambda p ({}) not within the range [table A.12.3-1]".format(_lambdaP))
        sys.exit("error lambda p ({}) not within the range [table A.12.3-1]".format(_lambdaP))        
    
    print("rho : {}".format(_rho))
    #
    # c) Internal components under compression and/or bending
    if _psi <= 0:
        _dc = d / (1 - _psi)
        _deff = _rho * _dc
        _de1 = 0.40 * _deff
        _de2 = 0.60 * _deff 
    #
    elif _psi <= 1:
        _deff = _rho * d
        _de1 = 2 * _deff / (5 - _psi)
        _de2 = _deff - _de1
    
    else:
        print("error psi ({}) >> 1".format(_psi))
        sys.exit("error psi ({}) >> 1".format(_psi))
    
    print("deff : {}, de1 : {}, de2 : {}".format(_deff, _de1, _de2))
    
    # 
    #
    if 'rack_plate' in SecName :
        #  b) Outstand components under compression and/or bending
        if CompType == 'outstand':
            _beffec_flange = (0.50 * t * math.sqrt(E / Fy))
            _beffec_web = (0.75 * t * math.sqrt(_K * E / Fy))
            _beffec = min(_beffec_flange, _beffec_web)
        
        else:
            # a) Compression internal components
            _beffec = 2 * (0.72 * t * math.sqrt(E / Fy))
    
    #
    elif 'side_plate' in SecName :
        # a) Compression internal components
        _beffec = (0.72 * t * math.sqrt(E / Fy))
    
    #
    elif 'back_plate' in SecName:
        #  b) Outstand components under compression and/or bending
        if CompType == 'outstand':
            _beffec_flange = (0.50 * t * math.sqrt(E / Fy))
            _beffec_web = (0.75 * t * math.sqrt(_K * E / Fy))
            _beffec = min(_beffec_flange, _beffec_web)
        
        # a) Compression internal components
        else:
            _beffec_flange = 2 * (0.72 * t * math.sqrt(E / Fy))
            _beffec_web = _beffec_flange
            _beffec = min(_beffec_flange, _beffec_web)
    
    #
    elif 'split_tubular' in SecName:
        return 0, 0
    
    #
    else:
        print('error section {} no supported'.format(SecName))
        sys.exit('error section {} no supported'.format(SecName))
    #
    #
    return _deff, _beffec
    #
#
# Plastic and compact sections
def A12_3_2():
    """
    A.12.3.2.1 Axial properties Class 1 and class 2 sections
    """
    # For class 1 plastic and class 2 compact sections, 
    # section properties should be determined assuming 
    # that fully plastic behaviour can occur.
    
    # The properties required for a strength assessment 
    # should be determined taking into account the physical 
    # distribution of components comprising the cross-section
    # and their yield strengths.
    
    # For simplicity, the following approximations can be 
    # used to determine the relevant properties
    
    # For axial tension and compression, the fully plastic
    # effective cross-sectional area for use in a strength
    # assessment,
    pass
#
# A.12.4 Effects of axial force on bending moment
def A12_4(AnalysisType, FAxial, Pu, Mu, Ac, r, e, L, K, Cm, E):
    """
    A.12.4 Effects of axial force on bending moment
    
    Pu is the axial force in the member due to factored actions 
        determined in an analysis that includes global P-delta effects;
    
    Mu is the moment in a member due to factored actions determined
       in an analysis that includes global P-delta effects;
    
    e  is the eccentricity between the axis used for structural analysis
       and that used for structural strength checks, taking due account 
       of the sign in combination with the sign convention for Pu
    
    L  is the unbraced length of member for the plane of flexural buckling 
       normally taken as:
         1.- face to face length for braces,
         2.- braced point to braced point length for chords,
         3.- longer segment length of X-braces (one pair is in tension,
             if not braced out-of-plane)
    
    I  is the second moment of area for the plane of bending as defined 
       in A.12.3.5.3;
    
    Ac is defined in A.12.3.5.2
    """
    # Euler moment amplification (p-delta) applies to all members 
    # in axial compression.
    # For class 1, 2, and 3 cross-sections the eccentricity between
    # the elastic and plastic centroids induces an additional moment.
    # This affects members in both tension and compression.
    # For class 4 members, in addition to the Euler moment amplification,
    # there is an eccentricity between the full cross-section area 
    # normally used in the structural analysis and the effective neutral
    # axis used in the member strength check. This can affect members
    # in both tension and compression.
    
    # A.12.4.2 Member moment correction due to eccentricity of axial force
    #
    # The plastic centroid or "centre of squash" is defined as the location
    # at which the axial force produces no moment on the fully plastic section. 
    # For chords with material asymmetry (e.g. when the section includes
    # components of differing yield strengths) the centre of squash can be 
    # offset from the elastic centroid. Before a section is checked, the moments 
    # should be corrected by the moment due to the axial force times the
    # eccentricity between the elastic centroid (used in the structural analysis)
    # and the "centre of squash" in accordance with Equation (A.12.4-1). 
    # There is no eccentricity for tubular members or for non-circular prismatic
    # members with material symmetry.
    
    # The corrected effective moment, Mue, should be calculated for each axis
    # of bending from:
    Mue = Mu + e * Pu             # (A.12.4-1)
    #
    #
    # A.12.4.3 Member moment amplification and effective lengths
    PE = 0
    #
    # B is the member moment amplification factor for the axis under 
    # consideration:
    
    if AnalysisType == 'nonlinear':
        
        # (i) for members in tension or (ii) members in compression where
        # the individual member forces are determined from a second order 
        # analysis, i.e. the equilibrium conditions are formulated on
        # the elastically deformed structure so that local P-Delta effects
        # are already included in Mu:
        B = 1.0
        #
        #
    
    # linear
    else:
        
        # (i) members in tension
        if FAxial == 'tension':
            B = 1.0
        #
        else:
            # PE and is to be calculated for the plane of bending.
            PE = (math.pi**2 * Ac * E) / (K * L / r)**2
            
            # for members in compression where the local member forces are 
            # determined from a first-order linear elastic analysis, i.e. 
            # the equilibrium conditions are formulated on the undeformed 
            # structure and therefore Mu does not include the local member
            # P-Delta effects:
            B = Cm / (1.0 - Pu / PE)
            
            # as above, but where the interaction surface approach of 
            # A.12.6.3.3 is to be applied
            B_2 = max(B, 1.0)
            #
            #  
    #
    Mua = B * Mue
    #
    print("")
    print("A.12.4 Effects of axial force on bending moment")
    print("Mue = {: 1.4e}".format(Mue))
    print("B   = {: 1.4e}".format(B))
    print("Mua = {: 1.4e}".format(Mua))
    print("PE  = {: 1.4e}".format(PE))
    #
    return Mue, Mua, PE
    #
#
#
# ---------------------------------------------------------------
# A.12.5 Strength of tubular members
#
# A.12.5.1 Applicability
def A12_5_1(D, t, Fy, d):
    """
    A.12.5.1 Applicability
    """
    # The strength of unstiffened tubular members that satisfy 
    # the following condition should be assessed in accordance
    # with this clause.
    # Any tubular with:
    if D/t > 120:
        # Tubulars that do not satisfy this condition should be
        # assessed using alternative methods that result in levels of
        # reliability comparable to those implicit in this document,
        # such as References [A.12.5-3] and [A.12.5-4].
        print("error D/t [{}] > 120 [A.12.5.1]".format(D/t))
        sys.exit("error D/t [{}] > 120 [A.12.5.1]".format(D/t))
    #
    # The formulations are considered applicable for steels with yield
    # strength of up to 700 N/mm2. The yield strength used in this clause
    # should be as specified in A.12.2.2.
    if Fy > 700:
        print("error Fy > 700 N/mm2")
        sys.exit("error Fy > 700 N/mm2")
    #
    # NOTE The formulations for tubular members are based on ISO 19902 
    # Clause 13. However, for use in this document, the ISO 19902 
    # formulations have been converted to a force base rather than 
    # a stress base.
    
    # The provisions given in this clause ignore the effect of 
    # hydrostatic pressure. The condition under which hydrostatic
    # pressure may be ignored for a specific member is given by:
    #
    if d > 0 and D/t > 211.0 / pow(d, 0.335):
        print("error D/t > d")
        sys.exit("error D/t > d")
    #
    print("-----------------")
    print("A.12.5.1 Applicability of tubular / split tubular members")
    print("D/t [{:1.4e}] < 120         ==> Ok".format(D / t))
    print("D/t [{:1.4e}] < 211/d^0.335 ==> hydrostatic press can be ignored".format(D / t))
    print("Fy  [{:1.4e}] < 700 N/mm^2  ==> Ok".format(Fy))
    #
    #
#
# A.12.5.2.1 Axial tensile strength check
def A12_5_2_1(A, Fy, GammaRTt):
    """
    Fy = yield stress as defined in A.12.2.2
    A = total cross-sectional area
    GammaRTt = partial resistance factor for axial tension, 1,05
    """
    # Tubular members subjected to axial tensile forces, Put, 
    # should satisfy:
    Put = A * Fy / GammaRTt
    #
    return Put
    #
#
# A.12.5.2.2 Axial compressive strength check
def A12_5_2_2(D, t, Fy, GammaRTc = 1.10, Cx = 0.3):
    """
    t =
    D =
    Fy = yield stress as defined in A.12.2.2
    A = total cross-sectional area
    Cx = critical elastic buckling coefficient
    E = as defined in A.12.1.1
    K = effective length factor in y or z direction, see A.12.4.3
    L = unbraced length in y or z direction measured between centre-lines
    I = second moment of area of the tubular.
    GammaRTc = partial resistance factor for axial compressive strength
    """
    #
    # # A.12.5.2.3 Local buckling strength
    
    # Cx = critical elastic buckling coefficient
    # The theoretical value of Cx for an ideal tubular is 0,6. 
    # However, a reduced value of Cx = 0,3 is recommended 
    # for use in the determination of Pxe to account for the 
    # effect of initial geometric imperfections. 
    # A reduced value of Cx = 0,3 is also implicit in the 
    # limits for A Fy/Pxe given in Equations 12.5-5.
    #
    # The representative elastic local buckling strength
    _Pxe =   2 * Cx * E * A * (t / D)
    # The representative local buckling strength, Pyc, 
    # should be determined from:
    if A * Fy / Pxe <= 0.170:
        _Pyc = A * Fy
    #
    elif A * Fy / _Pxe  > 0.170 and A * Fy / _Pxe  <= 200 * Fy / E:
        _Pyc = (1.047 - 0.274 * A * Fy / _Pxe) * A * Fy
    #
    else:
        print("error")
        sys.exit("error")
    
    #
    # A.12.5.2.4 Column buckling strength
    #
    # PE = smaller of the Euler buckling strengths about the
    #      y or z direction
    _PE = math.pi**2 * E * I / (K * L)**2
    #
    # Lambda = column slenderness parameter
    _Lambda = (_Pyc / _PE)**0.5                  # (A.12.5-7)
    #
    # The representative axial compressive strength of tubular
    # members, Pa, should be determined from:
    if _Lambda > 1.34:
        _Pa = 0.90 * _Pyc / _Lambda**2           # (A.12.5-6b)
    #
    else:
        _Pa = (1.0 - 0.278 * _Lambda**2) * Pyc   # (A.12.5-6a)
    #
    #
    # Tubular members subjected to axial compressive forces, Puc, 
    # should satisfy:
    #
    Puc = _Pa / GammaRTc
    #
    return Puc
    #


#
# A.12.5.2.5 Bending strength check
def A12_5_2_5(Mu, GammaRTb = 1.05):
    """
    Mu = Muy or Muz the bending moment about member 
         y- and z-axes respectively due to factored
         actions
    GammaRTb = partial resistance factor for bending
    """
    # Mp = plastic moment strength
    _Mp = Fy * [D**3 - (D - 2 * t)**3] / 6.0
    
    # Mb = representative bending moment strength, 
    # determined from:
    if (Fy * D) / (E * t) <= 0.0517:
        _Mb = _Mp                                       # (A.12.5-9a)
    #
    elif ((Fy * D)/(E * t) > 0.0517 
          and (Fy * D)/(E * t) <= 0.1034):
        _Mb = (1.13 - 2.58 * (Fy * D)/(E * t)) * _Mp    # (A.12.5-9b)
    #
    elif ((Fy * D)/(E * t) > 0.1034 
          and (Fy * D)/(E * t) <= 120 * (Fy / E)):
        _Mb = (0.94 - 0.76 * (Fy * D)/(E * t)) * _Mp    # (A.12.5-9c)
    #
    # Tubular members subjected to bending moments, Mu, 
    # should satisfy:
    _Mu = _Mb / GammaRTb                                # (A.12.5-8)
    #
    return _Mu
    #
# 
# A.12.5.3 Tubular member combined strength checks
#
# A.12.5.3.1 Axial tension and bending strength check
def A12_5_3_1():
    """
    Put = axial tensile force due to factored actions
    A   = total cross-sectional area
    Fy  = yield stress as defined in A.12.2.2
    Muy, Muz = bending moments about member y- and z-axes
        respectively due to factored actions determined in
        an analysis which includes global P-D effects
    Mb = representative moment strength, as defined in 
        equations A.12.5-9
    
    GammaRTt = partial resistance factor for axial tension, 1,05
    GammaRTb = partial resistance factor for bending, 1,05
    """
    # Tubular members subjected to combined axial tension 
    # and bending forces should satisfy the following
    # condition along their length:
    _UR = (GammaRTt * Put / (A * Fy) 
           + GammaRTb * math.sqrt(Muy**2 + Muz**2) / Mb)   # (A.12.5-10)
    #
    return _UR
    #
#
# A.12.5.3.2 Axial compression and bending strength check
def A12_5_3_2():
    """
    Puc = axial compressive force due to factored actions
    Pyc = the representative local buckling strength in A.12.5.2.3,
    Pa = as defined in A.12.5.2.4
    Muy = corrected effective bending moment about member y-axis
          due to factored actions determined in an analysis which
          includes global P-D effects
    Muz = corrected effective bending moment about member z-axis
          due to factored actions determined in an analysis which
          includes global P-D effects
    Muay = amplified bending moment about member y-axis due to 
           factored actions from A.12.4.3
    Muaz = amplified bending moment about member z-axis due to 
           factored actions from A.12.4.3
    Mb = representative bending strength, as defined in equations A.12.5-9
    
    GammaRTb = partial resistance factor for bending, 1,05
    GammaRTc = partial resistance factor for axial compressive strength, 1,15
    """
    # Tubular members subjected to combined axial compression
    # and bending forces should satisfy the following
    # conditions at all cross sections along their length:
    
    # beam-column check:
    UR_1 = ((GammaRTc * Puc / Pa) 
            + (GammaRTb / Mb) * math.sqrt(Muay**2 + Muaz**2))  # (A.12.5-11)
    #
    # and local strength check:
    UR_2 = ((GammaRTc * Puc / Pyc) 
            + (GammaRTb / Mb) * math.sqrt(Muy**2 + Muz**2))    # (A.12.5-12)
    #
    return UR_1, UR_2
    #
#
# A.12.5.3.3 Beam shear strength check
def A12_5_3_3(Pv, A, Fy, GammaRTv = 1.05):
    """
    A.12.5.3.3 Beam shear strength check
    
    Pv = representative shear strength
    A  = total cross-sectional area
    GammaRTv = partial resistance factor for beam
               shear strength, 1,05
    """
    # Pv = representative shear strength
    _Pv = A * Fy / (2 * math.sqrt(3))
    # Tubular members subjected to beam shear forces 
    # should satisfy:
    _V = _Pv / GammaRTv      # (A.12.5-13)
    #
    return _V
    #
#
# A.12.5.3.4 Torsional shear strength check
def A12_5_3_4(D, t, Fy, GammaRTv = 1.05):
    """
    A.12.5.3.4 Torsional shear strength check
    
    D  = 
    t  =
    Fy =
    GammaRTv = partial resistance factor for beam
               shear strength, 1,05
    """
    # Ip = polar moment of inertia
    _Ip = (math.pi / 32.0) * (D**4 - (D - 2 * t)**4)
    
    # Tv = representative torsional strength
    _Tv = 2 * Ip * Fy / (D * math.sqrt(3))
    
    # Tubular members subjected to torsional 
    # shear forces should satisfy:
    _Tu = _Tv / GammaRTv            # (A.12.5-14)
    #
    return _Tu
    #
#
#
# -------------------------------------------------
# A.12.6 Strength of prismatic members
#
# A.12.6.1 General
def A12_6_1(b, t , Fy, E, dw):
    """
    A.12.6 Strength of prismatic members
    
    b width of base plate
    t thickness of base plate
    Fy
    E
    dw effective head of water in metres for which additional analysis
       is not required, depth below the water surface 
       (including penetration into the seabed where applicable)
    """
    # The structural strength provisions for rolled and 
    # welded prismatic members are generally based on AISC 2005
    # Specification for Structural Steel Buildings, Reference
    # [A.12.5-1]. The AISC 2005 specification for LRFD was
    # interpreted and, in some cases, modified for use in the
    # assessment of mobile jack-up structures. The formulations
    # for column buckling for lower strength steels in A.12.6.2.4
    # were modified for consistency with the approach used for 
    # higher strength steels which was taken from Galambos[A.12.6-5].
    # Interpretation of the specifications was necessary to enable a
    # straight-forward method to be presented for the assessment of
    # beam-columns with components of varying yield strength and/or 
    # with cross sections having only a single axis of symmetry. 
    # Development of the specifications was necessary to provide:
    
    # a) A method to deal with member cross-sections comprising 
    # components constructed of steels with different yield strengths.
    
    # b) A method for the assessment of beam-columns under biaxial
    # bending to overcome a conservatism which has been identified 
    # in the standard AISC interaction equations.
    
    # The yield strength used in this subclause should be as specified
    # in A.12.2.2.
    
    # The resistance factors used in the AISC 2005 LRFD specification 
    # have been adopted.
    
    # The effects of hydrostatic loading on prismatic members should be
    # considered. The critical condition for hydrostatic loading on 
    # prismatic chord members is likely to occur when high spudcan 
    # fixity results in high chord axial loads in deepwater.
    
    _beta = (b / t) * math.sqrt(Fy / E)
    
    if _beta > 2.0:
        print("beta > 2 ==> rational analysis should be used to assess the effects of hydrostatic pressure")
        sys.exit("beta > 2 ==> N/A")
    #
    _dw = (298 * _beta**4 - 2092 * _beta**3 - 5542 * _beta**2 
           - 6603 * _beta + 3025)
    #
    print("-----------------")
    print("A.12.6 Strength of prismatic members")
    print("Hydrostatic pressure effects on flat plate components")
    print("beta = {: 1.4e}".format(_beta))
    #
    if dw > _dw :
        print("dw [{: 1.4e}] > limiting dw [{: 1.4e}] ==> Fail".format(dw, _dw))
    #
    else:
        print("dw [{: 1.4e}] < limiting dw [{: 1.4e}] ==> Ok".format(dw, _dw))
    #
    #
#
# A.12.6.2 Non-circular prismatic members subjected to tension, 
#          compression, bending or shear
#
# A.12.6.2.1 General
def A12_6_2(self):
    """
    A.12.6.2 Non-circular prismatic members subjected to tension, 
             compression, bending or shear    
    """
    #
    # Non-circular prismatic members subjected to axial tension, 
    # axial compression, bending or shear should satisfy the 
    # applicable strength and stability checks specified in 
    # A.12.6.2.2 to A.12.6.2.7.
    
    _E  = self.E.convert('newton/millimeter^2').value
    _Ap = self.Ap.convert('millimeter^2').value
    _ry = self.ry.convert('millimeter').value
    _rz = self.rz.convert('millimeter').value
    
    _Pu  = self.Pu.convert('newton').value
    _Muy = self.Muy.convert('newton*millimeter').value
    _Muz = self.Muz.convert('newton*millimeter').value
    
    # A.12.4.2 Member moment correction due to eccentricity of axial force
    
    # Main Axis y
    _Ly = self.Ly.convert('millimeter').value
    _ey  = self.ey.convert('millimeter').value
    _Pu = self.Pu.convert('newton').value
    _Muy = self.Muy.convert('newton*millimeter').value
    
    _Muey, _Muay, _PEy = A12_4(self.AnalysisType, self.FAxial,
                               _Pu, _Muy, _Ap, _ry,
                               _ey, _Ly, self.Ky, self.Cmy, _E)
    
    self.Muey = Number(_Muey, dims = 'newton*millimeter')
    self.Muay = Number(_Muay, dims = 'newton*millimeter')
    self.PEy  = Number(_PEy, dims = 'newton')
    
    # Weak axis z
    _Lz = self.Lz.convert('millimeter').value
    _ez  = self.ez.convert('millimeter').value
    _Muz = self.Muz.convert('newton*millimeter').value
    
    _Muez, _Muaz, _PEz = A12_4(self.AnalysisType, self.FAxial,
                               _Pu, _Muz, _Ap, _rz,
                               _ez, _Lz, self.Kz, self.Cmz, _E)
    
    self.Muez = Number(_Muez, dims = 'newton*millimeter')
    self.Muaz = Number(_Muaz, dims = 'newton*millimeter')
    self.PEz  = Number(_PEz, dims = 'newton')
    
    # tension check
    if self.FAxial == 'tension':
        _Pt = self.Ptp.convert('newton').value
        _Pp = A12_6_2_2(_Pt, self.GammaRPt)
        self.Pp = Number(_Pp, dims = 'newton')
        
        _URt = _Pu / _Pp
        print("Pu  = {: 1.4e}".format(_Pu))
        print("URt = {: 1.4f}".format(_URt))
        print("-----------------")
    
    # compression check
    else:    
        # A.12.6.2.3 Axial compressive local strength check
        _Ppl = self.Ptp.convert('newton').value
        _Fy = self.Fymin.convert('newton/millimeter^2').value
        
        _Pp = A12_6_2_3(_Ppl, self.GammaRPcl)
        
        self.Pp = Number(_Pp, dims = 'newton')
        
        _URc = _Pu / _Pp
        print("Pu   = {: 1.4e}".format(_Pu))
        print("URc  = {: 1.4f}".format(_URc))
        print("-----------------")
        
        # A.12.6.2.4 Axial compressive column buckling strength
        _PE = min(_PEy, _PEz)
        _Pn = A12_6_2_4(_Ppl, _PE, _Fy)
        self.Pn = Number(_Pn, dims = 'newton')
        
        _URc = _Pu / _Pn
        print("Pu   = {: 1.4e}".format(_Pu))
        print("URcn = {: 1.4f}".format(_URc))
        print("-----------------")
    
    # bending check
    
    # A.12.6.2.5 Bending moment strength
    _Fymin  = self.Fymin.convert('newton/millimeter^2').value
    _Zpy = self.Zpy.convert('millimeter^3').value
    _Mby = A12_6_2_5(self.SectionClass,
                    _Zpy, _Fymin, self.GammaRPb)
    self.Mby = Number(_Mby, dims = 'newton*millimeter')
    #
    _Zpz = self.Zpz.convert('millimeter^3').value
    _Mbz = A12_6_2_5(self.SectionClass,
                    _Zpz, _Fymin, self.GammaRPb)
    self.Mbz = Number(_Mbz, dims = 'newton*millimeter')
    
    # A.12.6.2.6 Bending moment strength affected by lateral torsional
    # buckling    
    _Lb = self.Lb.convert('millimeter').value
    _rz = min(_ry, _rz)
    _Fylbt  = self.Fylbt.convert('newton/millimeter^2').value
    _Iy = self.Iy.convert('millimeter^4').value
    _Iz = self.Iz.convert('millimeter^4').value
    _J = self.J.convert('millimeter^4').value
    
    _Lbmax = A12_6_2_6(self.SectionCase, _Lb, _rz, _Fylbt,
                       _E, _Ap, _Iy, _Iz, _Zpy, _J, _Fymin)
    
    # A.12.6.2.7 Bending strength check
    _Mumaxy = A12_6_2_7(_Mby, self.GammaRPb)
    _URmy = _Muy / _Mumaxy
    print("Muy   = {: 1.4e}".format(_Muy))
    print("URm-y = {: 1.4f}".format(_URmy))
    print("-----------------")
    #
    _Mumaxz = A12_6_2_7(_Mbz, self.GammaRPb)
    _URmz = _Muz / _Mumaxz
    print("Muz   = {: 1.4e}".format(_Muz))
    print("URm-z = {: 1.4f}".format(_URmz))
    print("-----------------")
    #
    #print("ok")
    #
#
# A.12.6.2.2 Axial tensile strength check
def A12_6_2_2(Pt, gammaRPt = 1.05):
    """
    A.12.6.2.2 Axial tensile strength check
    
    Prismatic members subjected to axial tensile forces, Put, should
    satisfy:
              Put < = Pt / GammaRPt
    Where:
    Pt is the representative axial tensile strength of a non-circular 
       prismatic member
    gammaRPt is the partial resistance factor for axial tensile strength
    """
    #
    Put_max = Pt / gammaRPt
    
    print("")
    print("A.12.6.2.2 Axial tensile strength check")
    print("Pt = {: 1.4e}".format(Pt))
    print("Limiting Pu = {: 1.4e}".format(Put_max))
    #
    return Put_max
    #
#
# A.12.6.2.3 Axial compressive local strength check
def A12_6_2_3(Ppl, GammaRPcl = 1.10):
    """
    A.12.6.2.3 Axial compressive local strength check
    
    Non-circular prismatic members subjected to axial compressive 
    forces, Puc, due to factored actions should satisfy:
    Puc <= Ppl / GammaRPcl
    Where:
    Ppl is the representative local axial compressive strength of 
        a non-circular prismatic member;
    GammaRPcl is the partial resistance factor for local axial 
              compressive strength;
    """
    #
    print("")
    print("A.12.6.2.3 Axial compressive local strength check")
    Puc = Ppl / GammaRPcl
    print("Puc  = {: 1.4e}".format(Puc))
    #
    return Puc
    #
#
# A.12.6.2.4 Axial compressive column buckling strength
def A12_6_2_4(Ppl, PE, Fy):
    """
    A.12.6.2.4 Axial compressive column buckling strength
    
    PE is the minimum Euler buckling load for any plane of bending, 
       as defined in A.12.4.3 (including rack teeth of chords, 
       see A.12.3.1).    
    """
    #
    # There is no axial compressive column buckling strength
    # check because it is inherent in the combined strength
    # check for compression in A.12.6.3. However, the representative
    # axial compressive strength of all member classifications 
    # subjected to flexural buckling should be determined from the 
    # following Equations (A.12.6-16) to (A.12.6-19):
    #
    # where, in addition to the definitions in A.12.6.2.3,
    _lambdac = math.sqrt(Ppl / PE)                         # (A.12.6-20)
    # [derived from AISC[A.12.5-1], Ch. E3, see also F.1]
    
    # b) alternatively, for high strength steels (Fy > 450 MPa), 
    # the following may be used (see F.1):
    if Fy > 450:
        if _lambdac > 1.20:
            Pn = 0.8608 / pow(_lambdac, 1.854) * Ppl       # (A.12.6-19)
        #
        else:
            Pn = pow(0.7625 , pow(_lambdac, 3.22)) * Ppl   # (A.12.6-18)
    
    # a) for all grades of steel (conservative for high strength steel)
    else:
        # for Lambdac > 1,5 [derived from AISC[A.12.5-1], Equation E3-3]
        if _lambdac > 1.50:
            Pn = 0.877 / pow(_lambdac, 2) * Ppl           # (A.12.6-17)
        
        # for Lambda c <= 1,5 [derived from AISC[A.12.5-1], Equation E3-2]
        else:
            Pn = pow(0.658 , pow(_lambdac, 2)) * Ppl      # (A.12.6-16)
    #
    # When section contains un-reinforced cut-outs, the slenderness parameter,
    # Lambda c, should be based on the minimum section unless otherwise 
    # determined by analysis.
    
    print("")
    print("A.12.6.2.4 Axial compressive column buckling strength")
    print("lambdac = {: 1.4e}".format(_lambdac))
    print("Pn   = {: 1.4e}".format(Pn))
    #
    return Pn
    #
#
# A.12.6.2.5 Bending moment strength
def A12_6_2_5(SectionClass, Zp, Fymin, GammaRPb):
    """
    A.12.6.2.5 Bending moment strength
    """
    # The classification of member cross-sections in A.12.2
    # is used to identify the potential for local buckling. The
    # slender section properties determined in A.12.3.4 account 
    # for the local buckling of class 4 cross-sections.
    # The bending moment strength of typical closed section jack-up
    # chord members used in truss legs is not normally limited by 
    # lateral torsional buckling. However, this should be checked as
    # described in A.12.2.3.2.
    
    # A.12.6.2.5.4 Class 4 slender section bending moment strength
    if SectionClass == 4:
        pass
    
    # A.12.6.2.5.3 Class 3 semi-compact section bending moment strength
    if SectionClass == 3:
        pass
    
    # A.12.6.2.5.2 Class 1 plastic and class 2 compact section 
    # bending moment strength
    else:
        # The representative bending moment strength, Mb, is given by 
        # the plastic bending moment of the entire section:
        # Mb is the representative bending moment strength;
        # Zp is the fully plastic effective section modulus, 
        #    determined from Equation (A.12.3-2);
        # Fymin is the minimum yield strength of all components
        #       in the cross-section of a prismatic member, in 
        #       stress units, as defined in A.12.2.2.
        
        _Mb = Zp * Fymin     # (A.12.6-21)
        
        # NOTE Hybrid sections built up from components of different 
        # yield strengths are addressed by the methodology described 
        # in A.12.3.2.
    #
    print("")
    print("A.12.6.2.5 Bending moment strength")
    print("Mb = {: 1.4e}".format(_Mb))
    return _Mb
    #
#   
# A.12.6.2.6 Bending moment strength affected by lateral torsional
# buckling
def A12_6_2_6(Section, Lb, rz, Fylbt, E, Ap, Iy, Iz, Zpy, J, Fymin):
    #
    # The reduced representative bending moment strength Mb due to LTB
    # should be calculated for all members that do not meet the screening 
    # checks of either Equation (A.12.2-2) or (A.12.2-3) for open and 
    # closed sections respectively, regardless of the class of section.
    # When the representative bending moment strength is reduced due to 
    # LTB compared to the strength calculated in A.12.6.2.5, the reduced
    # bending moment strength should be used in the strength checks.
    # 
    LateralTorsionalBuckling(Section, Lb, rz, Fylbt , 
                             E, Ap, Iy, Iz, Zpy, J, Fymin)
    #
    #
#
# A.12.6.2.7 Bending strength check
def A12_6_2_7(Mb, GammaRPb):
    #
    # Non-circular prismatic members subjected to bending moments, Mu,
    # should satisfy:
    #
    print("")
    print("A.12.6.2.7 Bending strength check")     
    _Mumax = Mb / GammaRPb
    print("Mumax = {: 1.4e}".format(_Mumax))
    #
    #
    return _Mumax
    #
#
#
# ---------------------------------------------------------------
# A.12.6.3 Non-circular prismatic member combined strength checks
#
def A12_6_3(self):
    """
    A.12.6.3 Non-circular prismatic member combined strength checks
    """
    #
    # A.12.6.3.1 General
    
    # GammaRPa is the partial resistance factor for axial strength;
    #    GammaRPa = GammaRPt for axial tensile strength, 
    #    GammaRPa = 1,05 in Equations (A.12.6-38, A.12.6-39 and A.12.6-40),
    #    GammaRPa = GammaRPcl for axial compressive strength, 
    #    GammaRPa = 1,1 in Equation (A.12.6-38),
    if self.FAxial == 'tension':
        GammaRPa = self.GammaRPt
        _Pp = self.Pp.convert('newton').value
    
    # compression
    else: 
        GammaRPa = self.GammaRPcl
        _Pp = self.Pn.convert('newton').value
    
    # There are two different assessment approaches for the strength 
    # of non-circular prismatic members subjected to combined axial 
    # forces and bending moments:
    
    # a) the interaction equation approach (see A.12.6.3.2), which 
    #    is applicable to all member classifications;
    _Ppls = self.Ptp.convert('newton').value
    _Mby  = self.Mby.convert('newton*millimeter').value
    _Mbz  = self.Mbz.convert('newton*millimeter').value
    
    _Pu  = self.Pu.convert('newton').value
    _Muy = self.Muy.convert('newton*millimeter').value
    _Muz = self.Muz.convert('newton*millimeter').value
    
    _Muey = self.Muey.convert('newton*millimeter').value
    _Muez = self.Muez.convert('newton*millimeter').value  
    _Muay = self.Muay.convert('newton*millimeter').value
    _Muaz = self.Muaz.convert('newton*millimeter').value     
    
    self.URat, self.URac = A12_6_3_2(self.FAxial, self.eta, _Pp, 
                                     _Pu, _Muey , _Muez, _Muay, _Muaz, 
                                     _Ppls, _Mby, _Mbz,
                                     GammaRPa, self.GammaRPb)
    # 
    
    # b) the plastic interaction surface approach (see A.12.6.3.3), 
    #    which is applicable to members in class 1 and 2.    
    if self.SectionClass == 1 or self.SectionClass == 2:
        self.URb = A12_6_3_3(self.SectionType, 
                             _Pu, _Muy, _Muz, _Ppls, _Mby, _Mbz,
                            GammaRPa, self.GammaRPb)
    #
#
# A.12.6.3.2 Interaction equation approach
def A12_6_3_2(FAxial, eta, Pp, Pu, Muey , Muez, Muay, Muaz,
              Ppls, Mby, Mbz, GammaRPa, GammaRPb):
    """
    A.12.6.3.2 Interaction equation approach
    where :
    Pu is the applied axial force in a member due to factored actions, 
       determined in an analysis that includes Pu effects (see A.12.4);
    Ppls is the representative local axial strength of a non-circular 
        prismatic member,
    Pp is the representative axial strength of a non-circular prismatic
       member,
    Muey is the corrected bending moment due to factored actions about
       the member y-axis from A.12.4;
    Muez is the corrected bending moment due to factored actions about
      the member z-axis from A.12.4;
    Muay is the amplified bending moment due to factored actions about
      the member y-axis from A.12.4;
    Muaz is the amplified bending moment due to factored actions about
      the member z-axis from A.12.4;
    Mby is the representative bending moment strength about the member
      y-axis, as defined in A.12.6.2.5 or A.12.6.2.6.
    
    """
    # Each non-circular prismatic structural member should satisfy
    # the following conditions in Equations (A.12.6-38] to [A.12.6-40] 
    # at all cross-sections along its length. When the shear due to 
    # factored actions is greater than 60 percent of the shear strength, 
    # the bending moment strength should be reduced parabolically to zero 
    # when the shear equals the shear strength (Pv in A.12.6.3.4).
    #
    # Local strength check (for all members):
    # (A.12.6-38)
    _UR1 = ((GammaRPa * Pu / Ppls) 
            + pow((pow((GammaRPb * Muey / Mby),eta) 
                   + pow((GammaRPb * Muez / Mbz),eta)), 1.0 / eta))
    
    print("")
    print("A.12.6.3.2 Interaction equation approach")
    print("Uint [Local strength check ] = {: 1.4f}".format(_UR1))
    
    _UR2 = 0
    if FAxial == 'compression':
        # and beam-column check (for members subject to axial compression):
        if GammaRPa * Pu / Pp > 0.20:
            # after AISC[A.12.5-1], Equation H1-1a (A.12.6-39)
            _UR2 = ((GammaRPa * Pu / Pp) 
                + (8.0 / 9.0) * pow((pow((GammaRPb * Muay / Mby),eta) 
                                     + pow((GammaRPb * Muaz / Mbz),eta)), 1.0 / eta))
        #
        else:
            # after AISC[A.12.5-1], Equation H1-1b (A.12.6-40)
            _UR2 = ((GammaRPa * Pu / (2.0 * Pp))
                + pow((pow((GammaRPb * Muay / Mby),eta) 
                       + pow((GammaRPb * Muaz / Mbz),eta)), 1.0/eta))
        
        print("Uint [beam-column check ]    = {: 1.4f}".format(_UR2))
        print("-----------------")
    #
    #
    #
    return _UR1, _UR2
    #
#
# A.12.6.3.3 Interaction surface approach
def A12_6_3_3(SectType, P, My, Mz, Pp, Mby, Mbz, GammaRPa, GammaRpb = 1.10):
    """
    A.12.6.3.3 Interaction surface approach
    
    P  = chord member axial force
    My and Mz = local y and z axis bending moments
    
    Pp = the representative axial strength of a non-circular
         prismatic member, as defined in A.12.6.3.2;
    Mby = the representative bending moment strength, 
          as defined in A.12.6.3.2;
    Mbz = the representative bending moment strength, 
          as defined in A.12.6.3.2;
    
    GammaRPa = the partial resistance factor for axial strength,
             = GammaRPt for tensile strength (1.05)
             = GammaRPc for axial compressive strength (1.10)
             = GammaRPcl for local strength (1.10)
    GammaRpb = the partial resistance factor for bending strength
    
    **Note**
    For the strength check, the applied member forces (P, My, Mz)
    should be Pu, Muey, Muez as defined in A.12.6.3.2.
    
    For the beam-column check, the applied member forces (P, My, Mz)
    should be Pu, Muay, Muaz as defined in A.12.6.3.2.    
    """
    #
    # IMPORTANT - The assessor should note that the sign of the
    # moment is crucially important for sections without material
    # or geometric symmetry. The sign convention should therefore 
    # be observed with care.
    P = abs(P)
    
    # Py = chord member axial strength
    Py = Pp / GammaRPa         # (A.12.6-41)
    
    # Mpy and Mpz = local y and z axis bending strengths
    Mpy = Mby / GammaRpb       # (A.12.6-42)
    Mpz = Mbz / GammaRpb       # (A.12.6-43)
    
    # ANNEX F.3 Guidance on A.12.6.3.3: Interaction surface approach
    
    # Annex F provides by way of example conservative interaction
    # equations and curves for generic families of chord cross-sections
    # based on plastic strengths Py, Mpy, and Mpz. The resistance 
    # factors should be introduced by the assessor.
    
    if P/Py > 1.0:
        print("Fail ==> P/Py > 1")
        sys.exit()
    
    # M'py and M'pz = adjusted local y and z axis bending 
    # strengths used in simplified interaction equations
    
    if SectType == 'rack_split_tubular':
        #
        _xi = 2.0
        K = 0.0
        #
        M_py = Mpy * (1.0 - pow(P / Py, 1.85))
        M_pz = Mpz * (1.0 - pow(P / Py, 2.25))
        #
    
    #
    elif SectType == 'triangular':
        pass
    #
    else:
        pass
    
    #
    # xi = interaction equation exponent for biaxial bending
    # used in simplified interaction equations
    
    # (F.3-1)
    _M_K = (Mz / M_pz - K) / (M_pz / Mpz - K)
    Uint_rel = ((P / Py) 
                + ((1.0 - P / Py) 
                   * pow((pow(My / M_py, _xi) 
                          + pow(_M_K, _xi)), 1/_xi)))
    #
    #
    print("")
    print("A.12.6.3.3 Interaction surface approach")
    print("Uintrel = {: 1.4f}".format(Uint_rel))
    print("-----------------")
    return Uint_rel
    #
#
#
# A.12.6.3.4 Beam shear
def A12_6_3_4(Av, Fymin, GammaRPv = 1.10):
    """
    A.12.6.3.4 Beam shear
    
    Vy, Vz is the beam shear due to factored actions in the local
        y- and z-directions
    Av is the effective shear area in the direction being considered,
       see Table A.12.6-1
    GammaRPv is the partial resistance factor for torsional and beam 
       shear strengths
    """
    #
    # Pvy, Pvz is the representative shear strength in the local 
    # y- and z-directions:
    _Pv = Av * Fymin / math.sqrt(3.0)      # (A.12.6-46)
    #
    # Non-circular prismatic members subjected to beam shear forces
    # due to factored actions should satisfy:
    _Vmax = _Pv / GammaRPv
    #
    print("")
    print("A.12.6.3.4 Beam shear")
    print("Pv    = {: 1.4e}".format(_Pv))
    print("Vmax  = {: 1.4e}".format(_Vmax))
    #
    return _Vmax
    #
#
# Table A.12.6-1 - Effective shear area for various cross-sections
def table_A12_6_1(SecType, A, D = 0, t = 0):
    """
    Table A.12.6-1 - Effective shear area for various cross-sections
    
    A is the area of cross-section. A[oi] is the area of rectilinear
      component i.
    Ds is the overall depth of cross-section. d is the web depth; for 
       rolled sections measured with respect to root radii, for welded
       sections measured between inside faces of flanges.
    t is the web thickness.
    T is the flange thickness of a welded T-section.
    Bs is the overall breadth of cross-section.
    theta is the angle between the shear force direction being considered and 
          the larger dimension of the cross-section of component i. 
    """
    #
    if SecType == 'tubular':
        _Av = 0.50 * A
    #
    elif SecType == 'plate':
        _Av = 0.90 * A
    #
    else:
        print("no yet implemented")
    #
    #
    #print("")
    #print("Av = {: 1.4e}".format(_Av))
    return _Av
    #
#
#
# A.12.6.3.5 Torsional shear
def A12_6_3_5(Ipp, rt, Fymin, GammaRPv = 1.10):
    """
    A.12.6.3.5 Torsional shear
    
    Tu is the torsional moment due to factored actions;
    Ipp is the polar moment of inertia of the non-circular prismatic member;
    rt is the maximum distance from centroid to an extreme fibre;
    GammaRPv is the partial resistance factor for torsional and beam shear strengths
    """
    #
    # Tvp is the representative torsional strength of the non-circular prismatic member
    _Tvp = Ipp * Fymin / (rt * math.sqrt(3.0))
    #
    # Closed-section non-circular prismatic members subjected to torsional shear forces
    # due to factored actions should satisfy:
    _Tumax = _Tvp / GammaRPv
    #
    # Open-section non-circular prismatic members subjected to torsional shear forces 
    # should be checked as appropriate.
    #
    print("")
    print("A.12.6.3.5 Torsional shear")
    print("Tvp   = {: 1.4e}".format(_Tvp))
    print("Tumax = {: 1.4e}".format(_Tumax))
    return _Tumax
    #
    #
#
#
#
#
class Component(object):
    
    __slots__ = ('name','Type', 'number', 'build', 'Class',
                 'unitLength', 'unitForce', 'unitMass','unitStress', 'unitDensity',
                 'D', 'b', 't', 'tmax', 'theta', 'd',
                 'MaterialType', 'Fy', 'E', 'UTS', 'poisson', 'G', 'rho')
    
    #
    def __init__(self, Name, Number, Type = 'N/A', Build = 'welded', Class = 4,
                 ForceUnit = '', LengthUnit = '', massUnit = '',
                 D = 0, b = 0 , tw = 0, tmax = 0, Theta = 0, d = 0,
                 Yield = 0, E = 0, Fu = 0, Nu = 0.30, G = 0, rho = 0):
        
        # Section Data
        self.name = Name
        self.Type = Type
        self.number = Number
        self.build = Build
        self.Class = Class
        # Units
        self.unitLength = LengthUnit
        self.unitForce = ForceUnit
        self.unitMass = massUnit
        # section
        self.D = D
        self.b = b
        self.t = tw
        self.tmax = tmax
        self.theta = Theta
        # material
        self.MaterialType = 'steel'
        self.Fy = Yield              # Minimum Yield Stress [N/mm2]
        self.E = E                   # Youngs Modulus [N/mm2]
        self.UTS = Fu
        self.poisson = Nu
        self.G = G
        self.rho = rho
        # effective head of water applicable to the tubular in question;
        self.d = d
        #
    #
#
#
# material properties
def material(component, CompID, Fy , 
             E = 0, Fu = 0, Nu = 0.30, G = 0, rho = 0):
    """
    """
    #
    _unitForce = component[CompID].unitForce
    _unitLength = component[CompID].unitLength
    _unitMass = component[CompID].unitMass
    _unitStress = str(_unitForce) + '/' + str(_unitLength) + '^2'
    _unitDensity = str(_unitMass) + '/' + str(_unitLength) + '^3'    
    #
    # ------- Material Section ------- 
    #
    # Minimum Yield Stress [N/mm2]
    component[CompID].Fy  = float(Fy)
    
    # Youngs Modulus [N/mm2]
    if E == 0:
        _conv = Number(205000., dims = 'newton/millimeter^2')
        E = _conv.convert(_unitStress).value
    component[CompID].E  = float(E)
    
    # Minimum Tensile Strenth [N/mm2]
    if Fu == 0:
        Fu = component[CompID].Fy / 0.90
    component[CompID].UTS  = float(Fu)
    
    # Poisson
    component[CompID].poisson = float(Nu)
    
    # Shear Modulus [N/mm2]
    if G == 0:
        _conv = Number(77200., dims = 'newton/millimeter^2')
        G = _conv.convert(_unitStress).value
    component[CompID].G  = float(G)
    
    # Material Density [kg/m3]
    if rho == 0:
        _conv = Number(7850., dims = 'kilogram/meter^3')
        rho = _conv.convert(_unitDensity).value
    component[CompID].rho  = float(rho)
    #
    return component
    #
#
# error message
def comp_error(comp):
    #
    print("**error no {} component was found".format(comp))
    sys.exit("**error no {} component was found".format(comp))
    #
#
# units modification
def swapUnits(unitIn, unitOut, comp):
    #
    _unitIn = str(unitIn).lower()
    _unitOut = str(unitOut).lower()
    #
    _UnitsType = 'length'
    _unitLenIn = comp[_unitIn].unitLength
    _unitLenOut = comp[_unitOut].unitLength
    _unitLengthFactor = unitFactor(_UnitsType, _unitLenIn, _unitLenOut)
    #
    return _unitLengthFactor
    #
#
#
#
class ISO19905_1():
    #
    def __init__(self, AnalysisType, SectionType, SectionID = ""):
        #
        #
        self.AnalysisType = analysisFinder(AnalysisType)
        self.SectionType = SecFinder(SectionType)
        self.SectionName = str(SectionID).upper()
        #
        self.component = {}
        #
        self.UnitLength = 'meter'
        self.UnitForce = 'newton'
        self.UnitMass = 'kilogram'
        self.LocalAxis = 'x y z'
        self.MaterialType = 'steel'
        #
        self.compNo = 0
        #
        self.GammaRPt = 1.05
        self.GammaRPcl = 1.10
        self.GammaRPb = 1.10
        self.GammaRPv = 1.05
        #
        self.eta = 1.0
        #
    
    #
    #-------------------------------------------------
    #                   General Data
    #-------------------------------------------------   
    #
    def LocalAxisSystem(self, MainAxis = 'z y x'):
        #
        self.LocalAxis = str(MainAxis).lower()
        # 
    #
    #
    def Units(self, Force = 'newton', Length = 'meter', Mass = 'kilogram'):
        #
        self.UnitLength = FindLength(Length)
        self.UnitForce  = FindForce(Force)
        self.UnitMass = FindMass(Mass)
        #     
    #
    #
    #-------------------------------------------------
    #           Section and Material Data
    #-------------------------------------------------
    #     
    # 1 rack plate component
    def RackPlateSection(self, b, Tf, Fy, Build = 'welded',
                         E = 0, Fu = 0, Nu = 0.30, G = 0, rho = 0):
        """
        """
        #
        CompID = "rack_plate"       
        
        try:
            self.component[CompID].name = CompID
        
        except KeyError:
            Type = 'internal'
            #self.compNo += 1
            self.component[CompID] = Component(CompID, 0, Type)
            #self.component[CompID].Type = Type
        
        # Section Data
        self.component[CompID].build = Build
        self.component[CompID].unitLength = self.UnitLength
        self.component[CompID].unitForce = self.UnitForce
        self.component[CompID].unitMass = self.UnitMass
        
        # Geometry Section
        self.component[CompID].b  = float(b)
        self.component[CompID].t = float(Tf)
        
        # ------- Material Section ------- 
        self.component = material(self.component, CompID, Fy , E, Fu, Nu, G, rho)
        #
        #print("ok")
        #
    
    #
    # 2 split tubular component
    def SplitTubularSection(self, D, Tav, Fy, Tmax = 0, b = 0, Build = 'welded',
                            E = 0, Fu = 0, Nu = 0.30, G = 0, rho = 0):
        """
        
        Parameters
        ----------        
        D  : float 
           Outside diameter
        t  : float 
           Wall thickness
        Fy : float
           Yield strength (stress units)
        
        Optional Parameters [default]
        ----------
        b     : float
              Ellipse height [D]
        Build : string (welded/rolled)
              Section type [welded]
        E     : float 
              Elastic Modulus [205000 newton/millimeter^2]
        UTS   : float
        
        poisson : float
                [0.30]
        G       : float 
                Shear modulus []
        rho     : float 
                Material density []
        
        Examples
        ----------        
        """
        #
        CompID   = "split_tubular"
        
        try:
            self.component[CompID].name = CompID
        
        except KeyError:
            #self.compNo += 1
            Type = 'curved'
            self.component[CompID] = Component(CompID, 0, Type)
            #self.component[CompID].Type = 'curved'
        
        # Section Data
        self.component[CompID].build = Build
        self.component[CompID].unitLength = self.UnitLength
        self.component[CompID].unitForce = self.UnitForce
        self.component[CompID].unitMass = self.UnitMass
        
        # Geometry Section
        self.component[CompID].D  = float(D)
        self.component[CompID].t = float(Tav)
        if Tmax == 0:
            Tmax = float(Tav)
        self.component[CompID].tmax = float(Tmax)
        
        if b == 0:
            b = D / 2.0
        self.component[CompID].b = float(b)
        
        # ------- Material Section ------- 
        self.component = material(self.component, CompID, Fy , E, Fu, Nu, G, rho)
        #
        #print("ok")
    #
    # 3 side plate component
    def SidePlateSection(self, b, tf, Fy, theta = 45., Build = 'welded',
                         E = 0, Fu = 0, Nu = 0.30, G = 0, rho = 0):
        """
        """
        #
        CompID = "side_plate"
        self.b_spc =   Number(float(b),   dims = self.UnitLength)
        self.tf_spc =  Number(float(tf),  dims = self.UnitLength)
        self.theta_spc = float(theta)
        #
        # ------- Material Section ------- 
        self.component = material(self.component, CompID, Fy , E, Fu, Nu, G, rho)
        #
    #
    # 4 back plate component
    def BackPlateSection(self, b, tf, Fy, Build = 'welded',
                         E = 0, Fu = 0, Nu = 0.30, G = 0, rho = 0):
        """
        """
        #
        CompID = "back_plate_"
        self.b_bpc =   Number(float(b),   dims = self.UnitLength)
        self.tf_bpc =  Number(float(tf),  dims = self.UnitLength)
        #
        # ------- Material Section ------- 
        self.component = material(self.component, CompID, Fy , E, Fu, Nu, G, rho)
        #
    #
    # Outstand components
    def OutstandSection(self, compType, b, t, Fy, Build = 'welded',
                        E = 0, Fu = 0, Nu = 0.30, G = 0, rho = 0):
        """
        """
        #
        _compType = CompTypeFinder(compType)
        
        #try:
        #    self.component[CompID].name = CompID
        
        #except KeyError:
        self.compNo += 1
        CompID = str(_compType) +"_" + str(self.compNo)
        Type = "outstand"
        self.component[CompID] = Component(CompID, self.compNo, Type)
        
        # Section Data
        self.component[CompID].build = Build
        self.component[CompID].unitLength = self.UnitLength
        self.component[CompID].unitForce = self.UnitForce
        self.component[CompID].unitMass = self.UnitMass
        
        # Geometry Section
        self.component[CompID].b  = float(b)
        self.component[CompID].t = float(t)
        #
        # ------- Material Section ------- 
        self.component = material(self.component, CompID, Fy , E, Fu, Nu, G, rho)
        #        
    #
    #-------------------------------------------------
    #                Length of Beam
    #-------------------------------------------------    
    #
    def EffectiveLength(self, Ly, Lz = 0):
        """
        Beam unbraced length Ly & Lz
        
        Parameters
        ----------
        Ly : float
           Beam unbraced length in the majoy axis y
        Lz : float
           Beam unbraced length in the minor axis z
        """
        #
        self.Ly = Number(float(Ly), dims = self.UnitLength)  
        self.Lz = Number(float(Lz), dims = self.UnitLength)
        #
    #
    #
    def UnbracedLength(self, Lb):
        """
        Unbraced Length
        
        Parameters
        ----------
        Lb : float
           Is the effective length of a beam-column between supports,
           i.e. the length between points that are either braced against
           lateral displacement of the compression flange, or braced 
           against twist of the cross-section, in addition to lateral 
           support;        
        """
        #
        self.Lb = Number(float(Lb), dims = self.UnitLength)
        #
    #    
    #
    def StabilityFactors(self, Ky, Kz = 0):
        """
        Stability Factors
        
        Parameters
        ----------
        Ky : float
           effective length factor in majoy axis (y-direction)
        Kz : float
           effective length factor in minor axis (z-direction)
        """
        #
        self.Ky = float(Ky)
        self.Kz = float(Kz)
        #
        if self.Kz == 0:
            self.Kz = self.Ky
    #
    #-------------------------------------------------
    #                 Acting Forces
    #-------------------------------------------------
    #
    def FactoredLoads(self, Pu, Vy, Vz, Tu, Muy, Muz):
        """
        Factored Loads
        
        Parameters
        ----------
        Pu  : float
            applied axial force due to factored actions
        Vy  : float
            beam shear due to factored actions in the local y direction
        Vz  : float
            beam shear due to factored actions in the local z direction
        Tu  : float
            torsional moment due to factored actions;
        Muy : float
            bending moment about member y due to factored actions
        Muz : float
            bending moment about member z due to factored actions
        """
        #
        _unitMoment = str(self.UnitForce) + '*' + str(self.UnitLength)
        
        # Axial force
        self.FAxial = 'tension'  # Flag Axial Force (COMPRESSION/TENSION)
        if Pu < 0: self.FAxial = 'compression'
        self.Pu = Number(abs(float(Pu)), dims = self.UnitForce)
        # Shear force
        self.Vy = Number(abs(float(Vy)), dims = self.UnitForce)
        self.Vz = Number(abs(float(Vz)), dims = self.UnitForce)
        # Torsion
        self.Tu = Number(abs(float(Tu)), dims = _unitMoment)
        # Bending
        self.Muy = Number(float(Muy), dims = _unitMoment)
        self.Muz = Number(float(Muz), dims = _unitMoment)   
        #
        #     
        #
    #
    #
    def MomentModifiers(self, Cmy, Cmz = 0):
        """
        Moment reduction factor Cm
        
        Parameters
        ----------
        Cmy : float
            Moment reduction factor in majoy axis (y-direction)
        Cmz : float
            Moment reduction factor in minor axis (z-direction)
        """
        self.Cmy = Cmy
        self.Cmz = Cmz
        #
        if self.Cmz == 0:
            self.Cmz = self.Cmy
    #    
    #-------------------------------------------------
    #                 User Defined
    #-------------------------------------------------   
    #
    def PartialResistanceFactors(self, GRt= 1.05, GRc = 1.10, GRb = 1.10, GRv = 1.10):
        #      Factors
        # Tension
        self.GammaRPt = GRt
        # Compression
        self.GammaRPcl = GRc
        # Bending
        self.GammaRPb = GRb
        # Shear
        self.GammaRPv = GRv
        #    
    #
    #
    def HydrostaticPressureData(self, Hw, T, d, z, WaveLength = 0, Rhow = 0.0000010250, g = 9.810):
        #
        self.Hw = Hw      # Hw is the wave height;
        self.Tw = T       # Tw wave period
        self.dw = d       # dw is the still water depth to the sea floor;
        # zw is the depth of the member relative to still water level 
        # (measured positive upwards);
        self.zw = z
        #
        self.WaveLength = WaveLength  # Wave length
        self.Rhow = Rhow  # is the acceleration due to gravity (m/s2)
        self.g = g        # is the acceleration due to gravity (m/s2);
        # Hydrostatic check on
        self.HydroCheck = 'ON'
        #
    #
    #-------------------------------------------------
    #                 Print Results
    #-------------------------------------------------    
    #
    def PrintResult(self):
        """
        """
        #
        print("")
        print("----------------------------------------------")
        print("           ISO 19905 Code Check Tool")
        print("                 Alpha Version")
        print("                   21/01/12")
        print("")
        print("-------------- PRINTING RESULTS --------------")
        #
        #
        # Define section properties
        
        if self.SectionType == 'rack_split_tubular':
            
            # the exponent for biaxial bending (??)
            self.eta = 2.0
            
            #
            self.SectionCase = "closed"
            
            # check if split tubular section was given
            if 'split_tubular' not in self.component:
                comp_error('split_tubular')
                
            # check if rack plate section was given
            if 'rack_plate' not in self.component:
                comp_error('rack_plate')
                
            # find factor from rack_plate to split_tubular units
            _LengthFactor = swapUnits('split_tubular', 'rack_plate',
                                          self.component)
            # find rack plate D for section geometry purpose
            self.component['rack_plate'].D  = (self.component["split_tubular"].D 
                                               * _LengthFactor)
            # check that rack plate b <= D
            self.component['rack_plate'].b = min(self.component['rack_plate'].b 
                                                 ,self.component['rack_plate'].D)
            
            # iterate components to find class
            _class = []
            for comp in self.component.itervalues():
                #
                _Fact_L = unitFactor('length', comp.unitLength, 'mm')
                _Fact_F = unitFactor('force', comp.unitForce, 'N')
                
                _D = comp.D * _Fact_L
                _b = comp.b * _Fact_L
                _t = comp.t * _Fact_L
                _Fy = comp.Fy * _Fact_F / _Fact_L**2
                _E  = comp.E  * _Fact_F / _Fact_L**2
                #
                _class_comp, _class_bend = A12_2_Classification(comp.Type, comp.build,
                                                                _D, _b, _t, _Fy, _E)
                print (comp.Type, _class_comp, _class_bend )
                comp.Class = max(_class_comp, _class_bend)
                
                if comp.Type != "outstand": _class.append(_class_comp)
                #
            #
            self.SectionClass = max(_class)
            print("")
            print("Section Class : {}".format(self.SectionClass))
            print("-----------------")
            
            # A.12.3.4 Slender sections
            if self.SectionClass == 4:
                for comp in self.component.itervalues():
                    print("slender")
                    _deff, _de1 = tableA12_3_1(comp.name, comp.Type, comp.b, comp.t,
                                               comp.Fy, comp.E)
                    print("try slender ", _deff, _de1)
                    print("")
            
            # Plastic, compact & semi-compact sections
            else:
                # calculate split tubular composite section property
                rackSplitTubSec(self)
                #
            #
        
        #
        elif self.SectionType == 'triangular':
            #
            self.SectionCase = "closed"
            #
        
        #
        elif self.SectionType == 'tubular':
            pass
        
        #
        else:
            print('error component {} no currently supported'.format(self.SectionType))
            sys.exit('error component {} no currently supported'.format(self.SectionType))
        
        
        #
        # start code check calculations
        
        # tubular sections
        if self.SectionType == 'tubular':
            pass
        
        # Prismatic member
        else:
            # strength check
            A12_6_2(self)
            
            # combined strength checks
            A12_6_3(self)
            
            # Shear check
            _Fymin  = self.Fymin.convert('newton/millimeter^2').value
            _Av = self.Av.convert('millimeter^2').value
            _Vmax = A12_6_3_4(_Av, _Fymin, self.GammaRPv)
            
            _Vy  = self.Vy.convert('newton').value
            _URvy = _Vy / _Vmax
            print("")
            print("Vy    = {: 1.4e}".format(_Vy))
            print("URv-y = {: 1.4f}".format(_URvy))
            print("-----------------")
            
            _Vz  = self.Vz.convert('newton').value
            _URvz = _Vz / _Vmax
            print("")
            print("Vz    = {: 1.4e}".format(_Vz))
            print("URv-z = {: 1.4f}".format(_URvz))  
            print("-----------------")
            
            # torsion check
            _Ip = self.Ip.convert('millimeter^4').value
            _Yc = self.Yc.convert('millimeter').value
            _Tumax = A12_6_3_5(_Ip, _Yc, _Fymin, self.GammaRPv)
            
            _Tu  = self.Tu.convert('newton*millimeter').value
            _URt = _Tu / _Tumax
            print("Tu    = {: 1.4e}".format(_Tu))
            print("URt   = {: 1.4f}".format(_URt))
            print("-----------------")
            #
        #
        print("")
        print("----------- FINISH PRINTING RESULTS ----------")
        #
    #
    #
