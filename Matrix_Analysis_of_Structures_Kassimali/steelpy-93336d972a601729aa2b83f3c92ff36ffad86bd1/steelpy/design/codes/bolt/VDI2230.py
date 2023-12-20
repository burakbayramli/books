#!/usr/bin/env python
#coding:utf-8
# Author:  Salvador Vargas-Ortega --<svortega@gmail.com>
# Purpose: DVI bolt Design Tool
# Created: 06/16/12

import sys
import unittest
import math
import datetime
import sys
from buckingham import Number, FindLength, FindForce, FindMass
#
# -------------------
#
def analysisFinder(analysis):
    """
    Analysis type:
    Initial bolt size
    Torque analysis
    Thread stripping analysis
    Joint analysis
    """
    _analysis = str(analysis).lower()
    _analysis = _analysis.replace('analysis','')
    _analysis = _analysis.replace('initial','')
    _analysis = _analysis.replace('stripping','')
    _analysis = _analysis.replace('size','')
    _analysis = _analysis.replace(' ','')
    _analysis = _analysis.replace('_','')
    _analysis = _analysis.replace('-','')
    _analysis = _analysis.strip()
    #
    _bolt   = ['bolt']
    _torque = ['torque']
    _thread = ['thread']
    _joint  = ['joint']
    #
    if _analysis in _bolt: _type = 'bolt'
    
    elif _analysis in _torque: _type = 'torque'
    
    elif _analysis in _thread: _type = 'thread'
    
    elif _analysis in _joint: _type = 'joint'
    
    else:
        print('error analysis type {} no available'.format(analysis))
        sys.exit('error analysis type {} no available'.format(analysis))
    #
    return _type
    #
#
def loadingFinder(load):
    """
    """
    _load = str(load).lower()
    _load = _load.replace(' ','')
    _load = _load.replace('-','')
    _load = _load.replace('_','')
    _load = _load.strip()
    #
    _static = ['static']
    _dyamic = ['dynamic']
    #
    if _load in _static : _type = 'static'
    
    elif _load in _dyamic : _type = 'dyamic'
    
    else:
        print('error loading type {} no available'.format(load))
        sys.exit('error loading type {} no available'.format(load))
    #
    return _type
    #
#
def tigthtFinder(tigtht):
    """
    Bolt tigtht method : screwdriver, wrench, controled   
    """
    #
    _tigtht = str(tigtht).lower()
    _tigtht = _tigtht.replace('_','')
    _tigtht = _tigtht.replace('/','')
    _tigtht = _tigtht.replace('-','')
    _tigtht = _tigtht.replace('torque','')
    _tigtht = _tigtht.replace('with','')
    _tigtht = _tigtht.replace('or','')
    _tigtht = _tigtht.replace('by','')
    _tigtht = _tigtht.replace('yielding','')
    _tigtht = _tigtht.replace('yield','')
    _tigtht = _tigtht.replace('point','')
    _tigtht = _tigtht.replace('rotationangle','')
    _tigtht = _tigtht.replace('rotation','')
    _tigtht = _tigtht.replace('angle','')
    _tigtht = _tigtht.replace(' ','')
    _tigtht = _tigtht.strip()
    #
    _screwdriver = ['screwdriver', 'hand']
    _wrench = ['wrench']
    _controled = ['controled']
    #
    if _tigtht in _screwdriver : _method = 'screwdriver'
    
    elif _tigtht in _wrench : _method = 'wrench'
    
    elif _tigtht in _controled : _method = 'controled'
    
    else:
        print('error Bolt tigtht method {} no available'.format(tigtht))
        sys.exit('error Bolt tigtht method {} no available'.format(tigtht))
    #
    return _method   
    #
#
def jointFinder(joint):
    """
    ESV : Tapped thread joint
    DSV : Through bolted joint
    """
    #
    _joint = str(joint).lower()
    _joint = _joint.replace('joint','')
    _joint = _joint.replace(' ','')
    _joint = _joint.replace('_','')
    _joint = _joint.replace('-','')
    _joint = _joint.strip()
    #
    _ESV = ['esv','tappedthread', 'tapped' ,'thread','screw']
    _DSV = ['dsv','throughbolted', 'bolted', 'through']
    
    if _joint in _ESV: _type = 'ESV'
    
    elif _joint in _DSV:_type = 'DSV'
    
    else:
        print('error joint type {} no available'.format(joint))
        sys.exit('error joint type {} no available'.format(joint))       
    #
    return _type
    #
#
def boltFinder(bolt):
    """
    """
    #
    _bolt = str(bolt).lower()
    _bolt  = _bolt.replace('bolt','')
    _bolt  = _bolt.replace('type','')
    _bolt  = _bolt.replace(' ','')
    _bolt = _bolt.strip()
    #
    #_M1_6 = ['m1.6', 'm1,6']
    #_M2 = ['m2']
    #_M2_5 = ['m2.5', 'm2,5']
    _M3 = ['m3']
    _M4 = ['m4']
    _M5 = ['m5']
    _M6 = ['m6']
    _M8 = ['m8']
    _M10 = ['m10']
    _M12 = ['m12']
    _M14 = ['m14']
    _M16 = ['m16']
    _M18 = ['m18']
    _M20 = ['m20']
    _M22 = ['m22']
    _M24 = ['m24']
    _M27 = ['m27']
    _M30 = ['m30']
    #_M36 = ['m36']
    #_M42 = ['m42']
    #_M48 = ['m48']
    #_M56 = ['m56']
    #_M64 = ['m64']
    _user = ['user', 'userdefined']
    #
    if _bolt in _M3 : _type = 'M3'; _dh = 3.4
        
    elif _bolt in _M4 : _type = 'M4'; _dh = 4.5
    
    elif _bolt in _M5 : _type = 'M5'; _dh = 5.5
    
    elif _bolt in _M6 : _type = 'M6'; _dh = 6.6
    
    elif _bolt in _M8 : _type = 'M8'; _dh = 9.0
    
    elif _bolt in _M10 : _type = 'M10'; _dh = 11.0
    
    elif _bolt in _M12 : _type = 'M12'; _dh = 13.5
    
    elif _bolt in _M14 : _type = 'M14'; _dh = 15.5
    
    elif _bolt in _M16 : _type = 'M16'; _dh = 17.5
    
    elif _bolt in _M18 : _type = 'M18'; _dh = 20.0
    
    elif _bolt in _M20 : _type = 'M20'; _dh = 22.0
    
    elif _bolt in _M22 : _type = 'M22'; _dh = 24.0
    
    elif _bolt in _M24 : _type = 'M24'; _dh = 26.0
    
    elif _bolt in _M27 : _type = 'M27'; _dh = 30.0
    
    elif _bolt in _M30 : _type = 'M30'; _dh = 33.0
    
    #elif _bolt in _user : _type = 'user'; _dh = 0.0
    
    else:
        print('Warning bolt {} no available'.format(bolt))
        _type = 'user'; _dh = 0  
    #
    return _type, _dh
    #
#
def boltHeadFinder2(head):
    """
    """
    _head = str(head).lower()
    _head  = _head.replace('bolt','')
    _head  = _head.replace('screw','')
    _head  = _head.replace('head','')
    _head  = _head.replace(' ','')
    _head  = _head.replace('_','')
    _head  = _head.replace('-','')
    _head  = _head.strip()
    #
    _hexagonon = ['hexagonon', 'hex']
    _hexsocket = ['hexagononsocket', 'hexsocket']
    _hexsockedcsunk = ['hexagonsocketcountersunk']
    _slotcsunk = ['slottedcountersunk']
    _slotrised = ['slottedrisedcountersunk']
    _slotcheese = ['slottedcheese']
    _special = ['special']
    _hexflange = ['hexflange']
    _user = ['user', 'userdefined']
    #
    if _head in _hexagonon: _type = 'hex'
    
    elif _head in _hexsocket : _type = 'hexsocket'
    
    elif _head in _hexsockedcsunk : _type = 'hexsocketcs'
    
    elif _head in _slotcsunk : _type = 'slottedcs'
    
    elif _head in _slotrised : _type = 'slotrisedcs'
    
    elif _head in _slotrised : _type = 'slotrisedcs'
    
    elif _head in _slotcheese : _type = 'slotcheese'
    
    elif _head in _special : _type = 'special'
    
    elif _head in _hexflange : _type = 'hexflange'
    
    elif _head in _user : _type = 'user'
    
    else:
        print('error bolt head type {} no available'.format(head))
        sys.exit('error bolt head type {} no available'.format(head))         
    #
    return _type
    #
#
def boltHeadFinder(head):
    """
    """
    _head = str(head).lower()
    _head  = _head.replace('bolt','')
    _head  = _head.replace('screw','')
    _head  = _head.replace('head','')
    _head  = _head.replace(' ','')
    _head  = _head.replace('_','')
    _head  = _head.replace('-','')
    _head  = _head.strip()
    #
    _hexagonon = ['hexagonon', 'hex']
    _slotted = ['slotted','slottedhex', 'slottedhexagonon','hexslotted', 'hexagononslotted']
    _socket = ['socket', 'hexagonsocketcountersunk', 'hexagononsocket', 'hexsocket']
    _cheese = ['cheese', 'slottedcheese']
    _flat = ['flat']
    _pan = ['pan']
    _filser = ['filser']
    _oval = ['oval']
    _special = ['special']
    #
    if _head in _socket: _type = 'socket'
    else: _type = _head
    #
    return _type
    #
#
def matGradeFinder(grade):
    """
    Rp02min : Minimun 0.20% yield strength of the threads
    RmS : Ultimate tensile strength of the threads
    """
    _grade = str(grade).lower()
    _grade = _grade.replace('grade','')
    _grade = _grade.replace('strength','')
    _grade = _grade.replace('bolt','')
    _grade = _grade.replace(' ','')
    _grade = _grade.replace('_','')
    _grade = _grade.replace('-','')
    _grade = _grade.strip()
    #
    _46 = ['4.6', '4,6']
    _48 = ['4.8', '4,8']
    _58 = ['5.8', '5,8']
    _88 = ['8.8', '8,8']
    _98 = ['9.8', '9,8']
    _109 = ['10.9', '10.9']
    _129 = ['12.9', '12,9']
    _user = ['user', 'userdefined']
    #
    if _grade in _46 :
        _gradeOut = '4.6'
        Rp02min = 240.0
        RmS = 400.0
    
    elif _grade in _48 :
        _gradeOut = '4.8'
        Rp02min = 340.0
        RmS = 420.0
    
    elif _grade in _58 :
        _gradeOut = '5.8'
        Rp02min = 420.0
        RmS = 520.0
    
    elif _grade in _88 :
        _gradeOut = '8.8'
        Rp02min = 660.0
        RmS = 830.0
    
    elif _grade in _98 :
        _gradeOut = '9.8'
        Rp02min = 720.0
        RmS = 940.0
    
    elif _grade in _109 :
        _gradeOut = '10.9'
        Rp02min = 940.0
        RmS = 1040.0
    
    elif _grade in _129 :
        _gradeOut = '10.9'
        Rp02min = 1100.0
        RmS = 1220.0
    #
    elif _grade in _user :
        _gradeOut = 'user'
        Rp02min = 0.0
        RmS = 0.0
    
    else:
        print('error material grade type {} no available'.format(grade))
        sys.exit('error material grade type {} no available'.format(grade))
    #
    return _gradeOut, Rp02min, RmS 
    #
#
def matFinder(mat):
    """
    """
    _mat = str(mat).lower()
    _mat = _mat.replace('material','')
    _mat = _mat.replace('alloy','')
    _mat = _mat.replace('grey','')
    _mat = _mat.replace('cast','')
    _mat = _mat.replace('solution','')
    _mat = _mat.replace('treated','')
    _mat = _mat.replace('age','')
    _mat = _mat.replace('hardened','')
    _mat = _mat.replace('(','')
    _mat = _mat.replace(')','')
    _mat = _mat.replace('/','')
    _mat = _mat.replace('-','')
    _mat = _mat.replace('_','')
    _mat = _mat.replace(' ','')
    _mat =_mat.strip()
    #
    _steel = ['steel', 'annealing', 'steelannealing']
    _austHeat = ['austeniticheat', 'heat']
    _austF = ['austeniticf60', 'austeniticf90', 'f60', 'f90']
    _iron = ['iron', 'fe']
    _alum = ['aluminium', 'aluminum', 'al']
    _titan = ['titanium', 'ti']
    _user = ['user']
    #
    if _mat in _steel :
        _matOut = 'steel'; _shearR = 0.60
    
    elif _mat in _austHeat :
        _matOut = 'austeniticheat'; _shearR = 0.80
    
    elif _mat in _austF :
        _matOut = 'austeniticf'; _shearR = 0.65
    
    elif _mat in _iron :
        _matOut = 'iron'; _shearR = 0.90
    
    elif _mat in _alum :
        _matOut = 'aluminium'; _shearR = 0.70
    
    elif _mat in _titan :
        _matOut = 'titanium'; _shearR = 0.60
    
    elif _mat in _user :
            _matOut = 'user'; _shearR = 0.60
    
    else:
        print('error material type {} no available'.format(mat))
        sys.exit('error material type {} no available'.format(mat))       
    #
    return _matOut, _shearR
    #
#
def table541(Rz, load = 'axial'):
    """
    Rz : Average roughness heigth
    ---
    """
    #
    if Rz < 10 :
        if load == 'axial':
            Rzt = 3; Rzn = 2.5; Rzi = 1.5
        else:
            Rzt = 3; Rzn = 3; Rzi = 2
    
    elif Rz < 40 :
        if load == 'axial':
            Rzt = 3; Rzn = 3; Rzi = 2
        else:
            Rzt = 3; Rzn = 4.5; Rzi = 2.5
        
    elif Rz < 160 :
        if load == 'axial':
            Rzt = 3; Rzn = 4; Rzi = 3
        else:
            Rzt = 3; Rzn = 6.5; Rzi = 3.5
    
    else:
        print('error RZ height {} no available'.format(Rz))
        sys.exit('error RZ height {} no available'.format(Rz))
    #
    return Rzt, Rzn, Rzi
    #
#
def rollTreatFinder(rolled):
    """
    """
    _rolled = str(rolled).lower()
    _rolled = _rolled.replace('rolled','')
    _rolled = _rolled.replace('heat','')
    _rolled = _rolled.replace('treatment','')
    _rolled = _rolled.replace('thread','')
    _rolled = _rolled.replace('limit','')
    _rolled = _rolled.replace('endurance','')
    _rolled = _rolled.replace('the','')
    _rolled = _rolled.replace(' ','')
    _rolled = _rolled.replace('-','')
    _rolled = _rolled.replace('_','')
    _rolled = _rolled.strip()
    #
    _before = ['before', 'sv']
    _after = ['after', 'sg']
    #
    if _rolled in _before :
        _treat = 'SV'
    
    elif _rolled in _after :
        _treat = 'SG'
    
    else:
        print('error rolled heat treatment [{}] no available'.format(rolled))
        sys.exit('error rolled heat treatment [{}] no available'.format(rolled))         
    #
    return _treat
    #
#
#
# -------------------
#
def printUnits(_length, _force = ''):
    #
    # 
    if _length == 'millimeter':
        _unitOut = {'length' : 'mm', 'mass' :"kg/m"}
    elif _length == 'meter':                               
        _unitOut = {'length' : ' m', 'mass' :"kg/m"}
    elif _length == 'centimeter':                          
        _unitOut = {'length' : 'cm', 'mass' :"kg/m"}
    elif _length == 'kilometer':                           
        _unitOut = {'length' : 'km', 'mass' :"kg/m"}
    elif _length == 'inch':                                
        _unitOut = {'length' : 'in', 'mass' :"lb/f"}
    elif _length == 'foot':                                
        _unitOut = {'length' : 'ft', 'mass' :"lb/f"}
    elif _length == 'mile':                                
        _unitOut = {'length' : 'mi', 'mass' :"lb/f"}
    #
    else:
        _unitOut = {'length' : '--', 'mass' :"----"}
    #
    #
    if _force == 'newton' : _unitOut['force'] = '  N'
    
    elif _force == 'kilonewton' : _unitOut['force'] = ' kN'
    
    elif _force == 'meganewton' : _unitOut['force'] = ' MN'
    
    elif _force == 'pound' : _unitOut['force'] = ' lb'
    
    elif _force == 'pound/16.0' : _unitOut['force'] = ' oz'
    
    elif _force == '1000*pound' : _unitOut['force'] = 'kip'
        
    else: _unitOut['force'] = '---'
    #
    return _unitOut
    #
#
#
# -------------------
#
# 4 Calculation Steps
# -------------------
#
def R0(dw, hmin):
    """
    R0 Determining the nominal diameter d
       and checking the lomiting size G    
    """
    
    # DSV [Throught bolted joint]
    G = hmin + dw  # (R0/1)
    
    # ESV [Tapped thread joint]
    # G1 = (1.5,...,2)  dw
    return G
    #
#
#
def R1(FMmax, FMmin):
    """
    R1 Determining the tightening factor alphaA
       (Section 5.4.3)
    """
    #
    alphaA = FMmax / FMmin  # (R1/1)
    
    return alphaA
    #
#
#
def R2_a(FQmax, MYmax, qF, qM, ra, mu_Tmin):
    """
    R2 Determining the required minimum clamp load Fkerf 
       (Section 5.4.1)
       
    The required minimum clamp load Fkerf is determined
    while taking into account the following requirements:
    a) Friction grip to transmit a transverse load FQ
       and/or a torque about the bolts axis My
    """
    
    # a)
    FKQ = ((FQmax / (qF * mu_Tmin)) 
           + (MYmax / (qM * ra * mu_Tmin)))  # (R2/1)
    #
    return FKQ
    #
#
#
def R2_b(Pimax, AD):
    """
    R2 Determining the required minimum clamp load Fkerf 
       (Section 5.4.1)
       
    The required minimum clamp load Fkerf is determined
    while taking into account the following requirements:
    b) Sealing against s medium
    """
    
    # b)
    FKP = AD * Pimax   # (R2/2)
    #
    return FKP
    #
#
#
def R2_c(FAmax, MBmax, Ssym, a, u, AD, IBT):
    """
    R2 Determining the required minimum clamp load Fkerf 
       (Section 5.4.1)
       
    The required minimum clamp load Fkerf is determined
    while taking into account the following requirements:
    c) Prevention of opening (Sec 5.3.2)
    
    FAmax : Axial Load
    MBmax : Working moment (bending moment) at the bolting point
    AD    : Sealing area (at most interface area less the through
            hole for the bolt)
    a     : Distance of the substitutional line of action of the
            load FA from the axis of the imaginary laterally 
            symmetrical deformation solid
    u     : Edge distance of the opening point U from the axis of the
            imaginary laterally symmetrical deformation solid
    Ssym  : Distance of the bolt axis from the axis of the imaginary
            laterally symmetrical deformation solid
    IBT   : Moment of gyration of the interface area
    """
    
    # c)
    FKA = ((FAmax * (AD * (a * u - Ssym * u) / (IBT + Ssym * u * AD)))
           + MBmax * (u * AD / (IBT + Ssym * u * AD))) # (R2/3)
    #
    return FKA
    #
#
#
def R3():
    """
    R3 Dividing the working load into FSA and FPA,
       determining Phi, deltaS, deltaP and n
       (Sec 5.1, Sec 5.2.2 and Sec 5.3)
    """
    #
    Phi = FSA / FA  # (R3/1)
    #
    FPA = (1.0 - Phi) * FA  # (R3/2)
    #
    # a) Concentric clamping and loading
    if Ssym == 0 and a == 0 :
        Phin = n * (deltaP / (deltaS + deltaP))  # (R3/3)
    #
    # b) eccentric clamping
    else:
        Phicn = n * (deltaP / (deltaS + deltaP))  # (R3/4)
    #
    #
#
#
def R4(fZ, delta_P, delta_S):
    """
    R4 Preload changes Fz
       (Section 5.4.2)
    
    Fz : Loss of preload due to embedding
    """
    #
    Fz = fZ / (delta_P + delta_S)  # (R4/1)
    #
    return Fz
    #
#
#
def R4_T(delta_P, delta_S):
    """
    R4 Preload changes DeltaFVth
       (Section 5.4.2)
    
    DeltaFVth : Loss of preload due to thermal expansion
    """    
    #
    DeltaFVth = ((lK * (alpha_S * Delta_Ts - alpha_P * Delta_Tp))
                 / (delta_S * (ESRT / EST) + delta_P * (EPRT/EPT)))  # (R4/2)
    #
#
#
def R5(FZ, FAmax , FKerf, Phi_cn, Delta_FVth = 0):
    """
    R5 Determining the minimum assembly preload FMmin
       (Sec 5.4.3)
    """
    #
    if Delta_FVth < 0 : Delta_FVth = 0
    #
    FMmin = FKerf + (1.0 - Phi_cn) * FAmax + FZ + Delta_FVth  # (R5/1)
    #
    return FMmin
    #
#
#
def R6(FMmin, alpha_A):
    """
    R6 Determining the maximum assembly preload FMmax
       (Sec 5.4.3)
    ---
    FMmin   :
    alpha_A :
    """
    #
    FMmax = alpha_A * FMmin  # (R6/1)
    #
    return FMmax
    #
#
#
def R7(P, A0, d0, d2, Rp02min, mu_Gmin, v = 0.90):
    """
    R7 Determining the assembly stress sigmaredM and FMzul
       and checking bolt size
       (Sec 5.5.1)
    """
    #
    sigma_redM = v * Rp02min    # (R7/1)
    #
    # (R7/2)
    FMzul = A0 * ((v * Rp02min) 
                  / (math.sqrt(1 + 3 * ((3./2.) * (d2/d0) 
                                        * (P/(math.pi * d2) 
                                           + 1.155 * mu_Gmin))**2)))
    #
    # The bolt size roughly estimate in R0 can continue
    # to be used, the following apply:
    #if FMzul >= and FMTab >= FMmax : '??'  # (R7/3)
    #
    return sigma_redM, FMzul
    #
#
#
def R8(A0, P, FMzul, FAmax, Phi_cn, d0, d2, Rp02min,
       mu_Gmin = 0.08, ktau = 0.50, Delta_FVth = 0):
    """
    R8 Determine the working stress sigma_redB
       (Sec 5.5.2)
    """
    #
    if Delta_FVth < 0 : Delta_FVth = 0
    
    FSmax = FMzul + Phi_cn * FAmax - Delta_FVth  # (R8/1)
    
    # The maximum tensile stress
    sigma_zmax = FSmax / A0   # (R8/2)
    
    # The maximum torsional stress
    MG = (FMzul * (d2/2.0) 
          * (P / (math.pi * d2) + 1.155 * mu_Gmin)) # (R8/3)
    
    # with d0 = dS or d0 = dimin (d0 = dT for reduced-shank bolts)
    WP = (math.pi / 16.) * d0**3 
    
    tau_max = MG / WP     # (R8/3)
    
    # working stress
    sigma_redB = math.sqrt(sigma_zmax**2 
                           + 3 * (ktau * tau_max)**2) # (R8/4)
    
    # the following must apply:
    #sigmaredB = min(sigmaredB, Rp02min)  # (R8/5-1)
    
    # Or alternatively a safety margin agains esceeding
    # the yield point
    SFy = Rp02min / sigma_redB # (R8/5-2)
    
    #if SFy  > 1.0 : print('SFy > {} --> PASS'.format(SFy))
    #else: print('SFy < {} --> FAIL'.format(SFy))
    
    # For complete loss of the torsional stress and for
    # torsional-free tightening :
    # Rp02min * A0 >= FSmax # (R8/5-3)
    SFt = Rp02min / sigma_zmax
    
    #if SFy  > 1.0 : print('SFt > {} --> PASS'.format(SFt))
    #else: print('SFt < {} --> FAIL'.format(SFt))    
    #
    return sigma_zmax, sigma_redB, tau_max
    #
#
#
def R9_1(Phi_en, As, FAmax, FAmin, d, rolled = 'before',
       sigma_w = 0, Nz = 0, ND = 2*10**6):
    """
    R9 Determining the alternating stress sigma_a, sigma_ab
       (Sec 5.5.3)
    General
    """
    #
    # General
    FSAo = FAmax * Phi_en
    FSAu = FAmin * Phi_en
    
    sigma_a = (FSAo - FSAu) / (2.0 * As)      # (R9/1)
    
    # Reference values for the fatigue limit of
    # high-strength bolts relative to the stress
    # cross section As at numbers of alternating
    # cycles of: ND = 2*10**6
    
    # rolled after head treatment (SG)
    if rolled == 'SG':
        sigma_As = (2.0 - FSm / F02min) * sigma_ASV  # (R9/5-2)
    
    # rolled before heat treatment (SV)
    else:
        sigma_As = 0.85 * (150. / d + 45.0)  # (R9/5-1)
    
    # Alternative safety verification with
    SD = sigma_As / sigma_a # (R9/4)
    
    #if SD > 1.2 : print('SD > {} --> PASS'.format(SD))
    #else: print('SD < {} --> FAIL'.format(SD))
    
    # If only a few thousand alternating cycles (Nz > 10x10^4)
    # occur with stress amplitudes which are greater than
    # the fatigue stregth sigma_AS, the endurance limit of the
    # joint can then be established if the following dynamic
    # strength values are assumed:
    
    if sigma_w > sigma_As:
        
        if rolled == 'SG':
            sigma_AZS = sigma_As * (ND / NZ)**(1.0/6.0)
    
        else:
            sigma_AZS = sigma_As * (ND / NZ)**(1.0/3.0)
        
        return sigma_a, sigma_As, sigma_AZS
    #
    #
    #if sigma_a < sigma_As : print('sigma a < sigma A  [OK]')
    #else: print('sigma a > sigma A  [FAIL]')     
    #
    return sigma_a, sigma_As
    #
#
#
def R9_2(As, sigma_SAbo, sigma_SAbu, d, rolled = 'before',
       sigma_w = 0, Nz = 0, ND = 2*10**6):
    """
    R9 Determining the alternating stress sigma_a, sigma_ab
       (Sec 5.5.3)
    For eccentric clamping and/or loading, taking into account
    the bending load.
    """
    
    # Eccentric:
    sigma_ab = (sigma_SAbo - sigma_SAbu) / 2.0  # (R9/2)
    
    # sigma_a_ab <= sigmaAS  # (R9/3)  
    
    # Reference values for the fatigue limit of
    # high-strength bolts relative to the stress
    # cross section As at numbers of alternating
    # cycles of: ND = 2*10**6
    
    # rolled after head treatment (SG)
    if rolled == 'SG':
        sigma_As = (2.0 - FSm / F02min) * sigma_ASV  # (R9/5-2)
    
    # rolled before heat treatment (SV)
    else:
        sigma_As = 0.85 * (150. / d + 45.0)  # (R9/5-1)
    
    # Alternative safety verification with
    SD = sigma_As / sigma_ab # (R9/4)
    
    #if SD > 1.2 : print('SD > {} --> PASS'.format(SD))
    #else: print('SD < {} --> FAIL'.format(SD))
    
    # If only a few thousand alternating cycles (Nz > 10x10^4)
    # occur with stress amplitudes which are greater than
    # the fatigue stregth sigma_AS, the endurance limit of the
    # joint can then be established if the following dynamic
    # strength values are assumed:
    
    if sigma_w > sigma_As:
        
        if rolled == 'SG':
            sigma_AZS = sigma_As * (ND / NZ)**(1.0/6.0)
    
        else:
            sigma_AZS = sigma_As * (ND / NZ)**(1.0/3.0)
        
        return sigma_a, sigma_As, sigma_AZS
    #
    #
    #if sigma_ab < sigma_As : print('sigma a < sigma A  [OK]')
    #else: print('sigma a > sigma A  [FAIL]')     
    #
    return sigma_ab, sigma_As
    #
#
#
def R10_assembly(FMzul, Apmin, PG):
    """
    R10 Determining the surface pressure Pmax
        (Sec 5.5.4)
    Assembly state
    """
    
    # Assembly state
    PMmax = min(FMzul / Apmin, PG)  # (R10/1)
    
    # Alternative safety verification
    Sp = PG / PMmax     # (R10/4)
    
    #if Sp  > 1.0 : print('Sp > {} --> PASS'.format(Sp))   
    #else: print('Sp < {} --> FAIL'.format(Sp))    
    #
    return PMmax
    #
#
#
def R10_working(FVmax, FSAmax, Apmin, PG, delta_FVth = 0):
    """
    R10 Determining the surface pressure Pmax
        (Sec 5.5.4)
    Working state
    """
    
    # Working state
    if delta_FVth < 0 : delta_FVth = 0
    PBmax = (FVmax + FSAmax - delta_FVth) / Apmin
    PBmax = min(PBmax, PG)          # (R10/2)
    
    # Alternative safety verification
    Sp = PG / PBmax     # (R10/4)
    
    #if Sp  > 1.0 : print('Sp > {} --> PASS'.format(Sp))
    #else: print('Sp < {} --> FAIL'.format(Sp))    
    #
    return PBmax
    #
#
#
def R10_yield(FMTab, Apmin, PG):
    """
    R10 Determining the surface pressure Pmax
        (Sec 5.5.4)
    For the maximum surface pressure with yield
    or angle controlled tightening techniques.
    """
    
    #
    Pmax = 1.40 * (FMTab / Apmin)   # (R10/3)
    
    # Alternative safety verification
    Sp = PG / Pmax     # (R10/4)
    
    if Sp  > 1.0 : print('Sp > {} --> PASS'.format(Sp))
        
    else: print('Sp < {} --> FAIL'.format(Sp))    
    #
    return Pmax
    #
#
#
def R11(As, RmS, FmGM):
    """
    R11 Determining the minimum length of engagement
        meffmin
        (Sec 5.5.5)
    ---
    As  : Tensile stress area of the bolt
    RmS : Tensile stress of the bolt
    FmGM : 
    """
    # Tensile strength
    FmS = RmS * As
    #
    #if FmS > FmGM :
    #    print('FmS [{}] > FmGM [{}] --> Fail'.format(FmS, FmGM ))
    #else:
    #    print('FmS [{}] < FmGM [{}] --> Ok'.format(FmS, FmGM ))
    #
    return FmS
    #
#
#
def R12(FMzul, FAmax, FZ, alphaA, Phi_en, qF = 1, 
        muTmin = 1, qM = 1, ra = 1, Atau = 1, Rm = 1,
        tauB = 1, deltaFVth = 1, FQmax = 1, MYmax = 1):
    """
    R12 Determining the safety margin against slipping SG and
        the shearing stress tauQmax
        (Sec 5.5.6)
    ---
    FMzul
    FAmax
    Fz
    alphaA
    Phi_en
    ---
    deltaFVth
    ---
    """
    
    # The minimum residual clamp load
    if deltaFVth < 0 : deltaFVth = 0
    
    FKRmin = (FMzul / alphaA - (1.0 - Phi_en) * FAmax 
              - FZ - deltaFVth)   # (R12/1)
    
    # The clamp load required for transmiting tranverse loads
    FKQerf = ((FQmax / (qF * muTmin)) 
              + (MYmax / (qM * ra * muTmin)))  # (R12/2)
    
    # the following must apply:
    # FKRmin > FKQerf  # (R12/3)
    
    # Alternative safety verification against slipping:
    SG = FKRmin / FKQerf  # (R12/4)
    
    # For the shearing stress in the bolts cross section
    # Atau at the interface:
    tauQmax = FQmax / Atau   # (R12/5)
    
    # The aim is to avoid shearing of the bolt:
    # tauQmax < tauB  # (R12/6-1)
    
    # or
    FQmax = Atau * Rm * (tauB / Rm) # (R12/6-2)
    
    # Alternative safety verification against shearing:
    SA = (tauB * Atau / FQmax)  # > 1.1 (R12/7)
    #
    #
    #
#
#
def R13(FMzul, P, d2, DKm, mu_Gmin, mu_Kmin, 
        MU = 0, MKZu = 0):
    """
    R13 Determining the tightening torque MA
        (Sec 5.4.3)
    """
    
    # The tightening torque may be calculated :
    MA = (FMzul * (0.16 * P + 0.58 * d2 * mu_Gmin 
                   + mu_Kmin * (DKm / 2.0)))  # (R13/1)
    
    #
    if MU != 0:
        MAS = MA + MU + MKZu  # (R13/1)
        return MAS
    #
    return MA
    #
#
#
# 5 Calculation Quantities
# ------------------------
#
def VD_5111_i(di, li, Es):
    """
    5.1.1.1 Axial Resilience
    
    F  : axial load
    Ai : bolt cross section
    li : bolt lenth
    """
    #
    Ai = math.pi / 4.0 * di**2
    # Bolt's elastic resiliance in axial direction
    delta_i = li / (Es * Ai)   # (5.1/2)
    #
    return delta_i
    #
#
#
def VD_5111(jointType, boltHead, d, d3, lGew, Es, EBI):
    """
    5.1.1.1 Axial Resilience
    
    Joint type :
        ESV - Tapped blind hole joint
        DSV - Through bolted joint with nut
    Bolt head :
        socket
        other
    F  : axial load
    d  : bolt diameter
    d3 : bolt minor diameter of the thread
    lGew : lenght of the free loaded thread
    Es  : Bolt Young's modulus
    EBI : Young's modulus of the component with internal treatd
    """
    
    # Elastic resilience of the engaged thread
    lG = 0.50 * d                     # (5.1/6)
    Ad3 = (math.pi / 4.0) * d3**2     # (5.1/7)
    delta_G = lG / (Es * Ad3)         # (5.1/5)
    
    # The elastic resilience of the nut or tapped thread region
    # ESV Tapped blind hole joint
    if jointType == 'ESV':
        EM = EBI
        lM = 0.33 * d   # (5.1/11)
    # bolted and stud bolted joints
    # DSV Through bolted joint with nut
    else:
        EM = Es
        lM = 0.40 * d  # (5.1/10)
    
    AN = (math.pi / 4.0) * d**2      # (5.1/9)
    delta_M = lM / (EM * AN)         # (5.1/8)
    
    # Elastic resilience of the engaged thread and of the nut
    # or tapped thread region
    delta_GM = delta_G + delta_M   # (5.1/4)
    
    # The resilience of the unengageg loaded part of the thread
    delta_Gew = lGew / (Es * Ad3)      # (5.1/12)
    
    # The elastic resilience of the head of standardized
    # hexagon head bolts and hexagon socket screws
    if boltHead == 'socket': lSK = 0.40 * d    # (5.1/15)
    else : lSK = 0.50 * d                      # (5.1/14)
    delta_SK = lSK / (Es * AN)                 # (5.1/13)
    
    # Elastic resilience of the bolt
    # delta_S = deltaSK + deltai + deltaGew + deltaGM  # (5.1/3)
    #delta_S = deltaSK + deltaGew + deltaGM
    #
    return delta_SK, delta_Gew, delta_GM
    #
#
#
def VD_5112(jointType, boltHead, d, d3, lGew, Es, EBI):
    """
    5.1.1.2 Bending Resilience
    """
    #
    # Elastic resilience of the engaged thread
    lG = 0.50 * d                     # (5.1/6)
    beta_G = VD_5112_1(d3, lG, Es)     # (5.1/19)
    
    # The elastic resilience of the nut or tapped thread region
    # ESV Tapped blind hole joint
    if jointType == 'ESV':
        EM = EBI
        lM = 0.33 * d   # (5.1/11)
    # bolted and stud bolted joints
    # DSV Through bolted joint with nut
    else:
        EM = Es
        lM = 0.40 * d  # (5.1/10)
    
    beta_M = VD_5112_1(d, lM, EM)     # (5.1/19)
    
    # Elastic resilience of the engaged thread and of the nut
    # or tapped thread region
    beta_GM = beta_G + beta_M   # (5.1/4)
    
    # The resilience of the unengageg loaded part of the thread
    beta_Gew = VD_5112_1(d3, lGew, Es)     # (5.1/19)
    
    # The elastic resilience of the head of standardized
    # hexagon head bolts and hexagon socket screws
    if boltHead == 'socket': lSK = 0.40 * d    # (5.1/15)
    else : lSK = 0.50 * d                      # (5.1/14)
    beta_SK = VD_5112_1(d, lSK, Es)     # (5.1/19)
    #
    return beta_SK, beta_Gew, beta_GM
    #
#
#
def VD_5112_1(di, li, E):
    """
    5.1.1.2 Bending Resilience
    """
    # Bending angle
    #gamma = (MB * lK) / (E * I)  # (5.1/16)
    Ii = math.pi / 64.0 * di**4
    # The bending resilience
    beta_i = li / (E * Ii)       # (5.1/17)
    # 
    return beta_i
    #
#
#
def VD_5112_2(beta_S, d3, Es, MBgesS = 0):
    """
    5.1.1.2 Bending Resilience
    For the definition of a common substituinal bar
    of constant diam d3
    """
    #
    I3 = math.pi / 64.0 * d3**4 # (5.1/21)
    #
    lers = beta_S * Es * I3     # (5.1/19)
    #
    if MBgesS != 0 :
        gammaS = beta_S * MBgesS
        return lers, gammaS
    #
    return lers
    #
#
#
def VD_512(joint, dW, DAa, lK):
    """
    5.1.2 Resilience of superimposed clamped parts
    ---
    dW  : Outside diameter of the plane head bearing
          surface of the bolt
    DAa : Substituitional outside diameter of the basic solid
    lK  : Clamping length
    ---
    """
    #
    #
    betaL = lK / dW  # (5.1/28)
    y = DAa / dW     # (5.1/29)
    #print('betaL = {} , y  = {}'.format(betaL, y))
    #
    if joint == 'ESV':
        tan_phi = (0.348 + 0.013 * math.log(betaL) 
                   + 0.193 * math.log(y))          # (5.1/26)
    
    else:
        tan_phi = (0.362 + 0.032 * math.log(betaL/2.0) 
                   + 0.153 * math.log(y))          # (5.1/27)
        
    #print('phi = {}'.format(tan_phi))
    #
    # ESV
    if joint == 'ESV': w = 2
    # DSV
    else: w = 1
    #
    DAGr = dW + w * lK * tan_phi  # (5.1/23)
    #print('DAGr = {} , w  = {}'.format(DAGr, w))    
    #
    return tan_phi, DAGr, w
    #
#
#
def VD_5121_1(dh, dW, lK, DA, Ep, tan_phi, DAGr, w):
    """
    5.1.2.1 Resilience for a concentrically clamped
            single-bolted joint
    """
    #A0 = ((dW + dh) * (dW + w * lK * tan_phi - dh))
    #A1 = ((dW - dh) * (dW + w * lK * tan_phi + dh))
    #A2 = (2 * math.log(A0/A1))
    #A3 = (w * Ep * math.pi * dh * tan_phi)
    #print A2
    #print A3
    
    # (5.1/24)
    if DA >= DAGr :
        delta_p = ((2 * math.log(((dW + dh) * (dW + w * lK * tan_phi - dh)) 
                                 / ((dW - dh) * (dW + w * lK * tan_phi + dh)))) 
                   / (w * Ep * math.pi * dh * tan_phi))
    # (5.1/25)
    else:
        delta_p = (((2.0 / (w * dh * tan_phi)) 
                    * (math.log(((dW + dh)*(DA - dh)) / ((dW - dh)*(DA + dh))))
                    + ((4.0 / (DA**2 - dh**2)) * (lK - (DA - dW) / (w * tan_phi))))
                   / (Ep * math.pi))
    #
    return delta_p
    #
#
#
def VD_5121_2(dh, dW, lK, DA, Ep, tan_phi, w):
    """
    5.1.2.1 Resilience for a eccentrically clamped
    single-bolted joint
    If the resiliances of the substitutional deformation
    cone and deformation sleeve are to be calculated 
    separatately.
    """
    # The cone height is calculated as Fig 5.1/3
    lV = min((DA - dW) / (2 * tan_phi),
             w * lK / 2.0)     # (5.1/31)
    
    # For the sleeve height
    lH = lK - (2 * lV / w)     # (5.1/32)
    
    # (5.1/30)
    delta_Vp = ((math.log(((dW + dh) * (dW + 2 * lV * tan_phi - dh)) 
                          / ((dW - dh) * (dW + 2 * lV * tan_phi + dh)))) 
                / (Ep * dh * math.pi * tan_phi))
    
    # To be used for the resilience of the sleeve
    # (5.1/33)
    delta_Hp = 4 * lH / (Ep * math.pi * (DA**2 - dh**2))
    
    # For the resilience there follows
    # (5.1/34)
    delta_P = 2. / w * delta_Vp + delta_Hp
    
    #
    return delta_P, lV, lH
    #
#
#
def VD_5122_1(jointType, e, cT, dW, hmin, dh, t = 0, ts = 0):
    """
    5.1.2.2 Resilience for an eccentrically clamped single
            bolted joint
    ---
    
    Joint type :
            ESV - Tapped blind hole joint
            DSV - Through bolted joint with nut
    
    e  : Distance of the bolt axis from edge of the interface
         on the side at risk of opening
    
    cT : Measurement of the interface area perpendicular to 
         the width b
    
    dW : Outside diameter of the plane head bearing
         surface of the bolt
    
    hmin : The smallest plate thickness of two clamped
           plate
    
    dh   : Hole diameter of the clamped parts
    
    ---
    t  : Bolt spacing in a multi-bolted joint
    
    ts : Counterbore depth
    ---
    """
    #
    # (5.1/38)
    if jointType == 'DSV':
        G = dW + hmin
        DAmin = 1.6 * hmin + dW
    #
    else:
        # (5.1/39a) and (5.1/39c)
        G = 1.5 * dW + 1.2 * ts
        # (5.1/39b)
        #Gmax = 3 * dW
        #G = max(G_, Gmax)
        DAmin = 1.2 * dW
    
    #
    if e > G/2.0:
        print("error e [{:1.4e}] > G/2 [{:1.4e}]".format(e, G/2.0))
        sys.exit("error e [{:1.4e}] > G/2 [{:1.4e}]".format(e, G/2.0))
    
    if cT < G:
        print("")
        print("WARNING cT [{:1.4e}] > G [{:1.4e}]".format(cT, G))
        #sys.exit("error cT [{:1.4e}] > G [{:1.4e}]".format(cT, G))
    
    # (5.1/46)
    # Moment of gyration of the interface area
    if t == 0 : t = G
    bT = min(G, t)
    IBT = bT * cT**3 / 12.0
    
    # Sealing area (at most interface area less the
    # through-hole for the bolt)
    AD = (cT * t - (math.pi * dh**2 / 4.0))
    
    # (5.1/47)
    # Distance of the bolts axis from the axis of
    # the imaginary laterally symetrical deformation
    # solid
    #tan_phi, DAGr, w = VD_512(jointType, dW, DAI, lK, dWu)
    # VD_512(joint, dW, DAa, lK)
    tan_phi, DAGr, w = VD_512("DSV", dW, cT, hmin)
    
    # Average (substitional sleeve) diameter of inside
    DAR = 2 * (cT - e)
    DAM = (DAR + DAGr) / 2.0
    
    Ssym = (cT - e - DAM / 2.0)
    
    # Edge distance of the opening point U from the
    # axis of the imaginary laterally symmetrical
    # deformation solid
    u = e + Ssym
    #
    return DAmin, G, IBT, AD, Ssym, u
    #
#
#
def VD_5122_2(delta_P, DA, DAGr, dW, Ssym, b, cT, w, lV, lH, li, Ei):
    """
    5.1.2.2 Resilience for an eccentrically clamped single
            bolted joint
    """
    #
    
    # (5.1/41a)
    IVBers = ((0.147 * (DA - dW) * dW**3 * DA**3)
              /(DA**3 - dW**3))
    # (5.1/50a)
    #IBersi = [0.295 * (li[i] * tan_phi * dwi**3)]
    
    # (5.1/41b)
    IVeBers = IVBers + Ssym**2 * (math.pi / 4.0) * DA**2 
    
    # (5.1/42)
    if b <= DAGr : IHBers = b * cT**3 / 12.0
    
    # (5.1/43)
    IBersi = (li / ((2. / w) * (lV / IVeBers) 
                    + (lH / IHBers)))
    
    # (5.1/45)
    #IBT = bT * cT**3 / 12.0
    
    # (5.1/49)
    #Summ = sum(li[i] / (Ei[i] * IBersi[i]) for i in range(len(li)))
    Summ = li / (Ei * IBersi)
    
    delta_Pp = delta_P + Ssym**2 * Summ
    
    #
    return delta_Pp, IBersi
    #
#
#
def VD_5123(delta_P, a, Ssym, li, Ei, IBersi):
    """
    5.1.2.3 Resilience for eccentric application of an
            axial working load
    """
    #
    try:
        Summ = sum(li[i] / (Ei[i] * IBersi[i]) for i in range(len(li)))
    
    except: Summ = li / (Ei * IBersi)
        
    # 5.1/52)
    delta_Ppp = delta_P + a * Ssym * Summ
    #
    return delta_Ppp
    #
#
#
def VD_531(delta_P, delta_S, n, FA = 0):
    """
    5.3.1 Load factor and additional bolt load up to
          the opening limit
    ---
    delta_P  : Elastic resiliance of the clamped parts 
               for concentric loading
    delta_S  : Elastic resiliance of the bolt
    n        : Load introduction factor
    ---
    FA : Axial load in the axial axis of the bolt
    ---
    """
    
    # For the theorical case of the load induction in
    # the bolt heat and nut bearing areas
    Phi_K = delta_P / (delta_S + delta_P)   # (5.3/4)
    
    # Load factor in the case of of concentric loading
    # and clamping
    Phi_n = n * Phi_K                       # (5.3/6)
    
    # Axial additional bolt load
    FSA = Phi_n * FA                        # (5.3/7)
    #
    return Phi_K , Phi_n, FSA
    #
#
#
def VD_5311(delta_P, delta_P1, delta_S, n, FA = 0, FV = 0):
    """
    5.3.1.1 Concentric loading
    ---
    delta_P  : Elastic resiliance of the clamped parts 
               for concentric loading
    delta_P1 : Elastic resiliance of the clamped parts
               for eccentric clamping
    delta_S  : Elastic resiliance of the bolt
    n        : Load introduction factor
    ---
    FA : Axial load in the axial axis of the bolt
    FV : Preload
    ---
    """
    #
    # Load factor for eccentric clamping and concentric load
    # introduction via the clamped parts.
    Phi_n = n * delta_P / (delta_S + delta_P1)    # (5.3/10)
    
    # Axial additional bolt load
    FSA = Phi_n * FA   # (5.3/9)
    
    # The axial working load at the opening limit during
    # concentric load
    FAabZ = (1.0 / (1.0 - Phi_n)) * FV   # (5.3/8)    
    #
    return Phi_n, FAabZ, FSA
    #
#
#
def VD_5312(delta_P1, delta_P2, delta_S, n, FA = 0, FM = 0):
    """
    5.3.1.2 Eccentric loading
    ---
    delta_P1  : Elastic resiliance of the clamped parts 
               for eccentric clamping
    delta_P2 : Elastic resiliance of the clamped parts
               for eccentric clamping and loading by MB
    delta_S  : Elastic resiliance of the bolt
    n        : Load introduction factor
    ---
    FA : Axial load in the axial axis of the bolt
    FM : Assembly preload
    ---
    """
    # Load factor for eccentric clamping and eccentric load
    # introduction in planes passing through the bolt head
    # and nut bearing areas
    Phi_ek = delta_P2 / (delta_S + delta_P1)   # (5.3/13)
    
    # Load factor for eccentric clamping and eccentric load
    # introduction via the clamped parts.
    Phi_en = n * Phi_ek                        # (5.3/12)
    
    # The axial working load at the opening limit during
    # eccentric load
    FAab =  (1.0 / (1.0 - Phi_en)) * FM        # (5.3/14)
    
    # Axial additional bolt load
    FSA = Phi_en * FA         # (5.3/15)
    
    # Proportion of the axial load which changes the loading
    # of the clamped parts, additional plate load.
    FPA = (1 - Phi_en) * FA   # (5.3/16)
    #
    return Phi_ek, Phi_en, FAab, FSA, FPA
    #
#
#
def VD_532():
    pass
#
#
def VD_553(FA, As, IBers, Phi_en, Ssym, a, lK, lers, dS, d3, Es, Ep):
    """
    5.5.3 Alternating stress
    """
    #
    I_Bers = IBers - (math.pi / 64.) * d3**4
    
    # (5.5/37)
    sigma_SAb = ((1 + (1.0 / Phi_en - Ssym / a) * (lK / lers) * (Es / Ep) 
                  * (math.pi * a * dS**3 / (8.0 * I_Bers))) 
                 * (Phi_en * FA / As))
    # 
    #
    return sigma_SAb
    #
#
#
def VD_555(P, d, d2, s, D1, D2, As, tauBM, tauBS, Rm, angle = 30):
    """
    P : Pitch of the thread
    d : Bolt diameter
    s : Width across flats for nuts
    D1 : Nut thread minor diameter
    D2 : Nut thread pitch diameter
    tauBM : Shearing strength of the nut
    tauBS : Shearing strength of the bolt
    ---
    angle : 
    """
    #
    _angle = math.radians(angle)
    #
    # From the stregth ratio, from Eq (5.5/43) and 
    # (5.5/44e), at the same shearing stregth ratio:
    # (5.5/44g)
    Rs = (((d * (P/2.0 + (d - D2) * math.atan(_angle))) 
           / (D1 * (P/2.0 + (d2 - D1) * math.atan(_angle)))) 
          * (tauBM / tauBS))
    #
    
    # C1, internal thread dilation strength reduction factor
    if s/d < 1.4 :
        print('error s/d [{:1.4e}] < 1.4'.format(s/d))
        sys.exit('error s/d [{:1.4e}] < 1.4'.format(s/d))
    
    elif s/d > 1.9 :
        C1 = 1
    
    else:
        C1 = 3.8 * s/d - (s/d)**2 - 2.61  # (5.5/44a)
    #
    # C2 external thread stregth reduction factor
    if Rs > 2.2 :
        print('warning Rs [{:1.4e}] > 2.2 '.format(Rs))
        #sys.exit('error Rs [{:1.4e}] > 2.2 '.format(Rs))
    
    elif Rs <= 1 : C2 = 0.897
    
    else:
        C2 = (5.594 - 13682 * Rs + 14.107 * Rs**2 
              - 6.057 * Rs**3 + 0.9353 * Rs**4)
    #
    # C3 internal thread strength reduction factor
    if Rs < 0.40 :
        print('error Rs [{:1.4e}] < 0.4'.format(Rs))
        sys.exit('error Rs [{:1.4e}] < 0.4'.format(Rs))
    
    elif Rs >= 1 : C3 = 0.897    # (5.5/44b)

    else:
        # (55/44b)
        C3 = (0.728 + 1.769 * Rs - 2.896 * Rs**2 
              + 1.296 * Rs**3)
    
    # Design Rules
    # The basic design rules for axial threaded joint are:
    # 1.- Any thread-stripping failure should be avoided
    # 2.- The bolt shank should fail in thension prior to
    #     thread stripping
    # The two rules require that the ultimate shear strength 
    # values of the external and internal threads should exceed
    # the tensile strength of the bolted shank. And for
    # serviceability, it is preferable that stripping of nut
    # threads occurs prior to the failure of bolts threads
    # which implies:
    #               FmGS > FmGm
    # Therefore, the overall design criteria can be expresed
    # in the following form:
    #           FmGs > FmGM > FmS
    # This inequality equation results in the minimum length
    # of engagement. To account for the fact that a length of
    # engagement of about 0.8P in the nut threads remain 
    # unloaded, the minimum engagement is:
    # (5.5/48)
    meff = (((Rm * As * P) / (C1* C3 * tauBM 
                              * (P/2.0 + (d - D2) * math.tan(_angle)) 
                              * math.pi * d)) 
            + (0.8 * P))
    
    # (5.5/43)
    ASGM = math.pi * d * (meff/P) * (P/2.0 + (d - D2) * math.tan(_angle))
    
    # The stripping force of the nut thread
    FmGM = tauBM * ASGM * C1 * C3    # (5.5/42)
    #
    return Rs, C1, C2, C3, meff, ASGM , FmGM
    #
#
#
def VD_5562():
    """
    5.5.6.2 Load Distribution
    """
#
#
def VD_5563():
    """
    5.5.6.3  Static Stress
    """
#
#
def VD_5564():
    """
    5.5.6.4 Dynamic Stress
    """
#
#-------------------------------------------------
#
def checkInput(l, b, Lt, lK, shank, plate, Es):
    """
    l  : Bolt length up to head
    b  : Thread length
    Lt : Minimum thread engagement length
    shank :
    plate : 
    ---
    lS   : Total length og the bolt shank
    lGew : Length of the free loaded thread
    """
    if b > l :
        print('error Thread length b [{:1.4E}] > Bolt length l [{:1.4E}]'.format(b, l))
        sys.exit('error Thread length b [{:1.4E}] > Bolt length l [{:1.4E}]'.format(b, l))
    #
    _lS = l - b 
    
    lSi = 0
    beta_i = []
    delta_i = []
    for bolt in shank.itervalues():
        # print(bolt.name)
        _unitLength = bolt.unitLength
        _li = Number(float(bolt.li), dims = _unitLength)
        li = _li.convert('millimeter').value
        lSi += li
        _d = Number(float(bolt.di), dims = _unitLength)
        d = _d.convert('millimeter').value
        beta_i.append(VD_5112_1(d, li, Es))
        delta_i.append(VD_5111_i(d, li, Es))
    
    if lSi == 0:
        pass
    
    elif lSi != _lS:
        print('error Sum bolt shank [{:1.4E}] != lS [{:1.4E}]'.format(lSi, _lS))
        sys.exit('error Sum bolt shank [{:1.4E}] != lS [{:1.4E}]'.format(lSi, _lS))
    
    if lK > l :
        print('error Sum of clamped plates lK [{:1.4E}] > l [{:1.4E}]'.format(lK, l))
        sys.exit('error Sum of clamped plates lK [{:1.4E}] > l [{:1.4E}]'.format(lK, l))       
    
    if max(l - lK, 0) < Lt :
        print('error bolt l [{:1.4E}] has not enough thread engagement length lt [{:1.4E}]'
              .format(l, Lt))
        sys.exit('error bolt l [{:1.4E}] has not enough thread engagement length lt [{:1.4E}]'
                 .format(l, Lt))
    
    # print('====> lSi , lK ',lSi , lK)
    lGew = max((lK - lSi), 0)
    #
    return lSi, lGew, delta_i, beta_i
    #
#
#
def threadPropMetric(P, d):
    """
    P : Pitch of the iso metric threads (mm)
    d : Nominal diameter (mm)
    """
    #
    d2 = round(math.ceil(d) - 0.649519 * P, 3)
    
    d3 = round(math.ceil(d) - 1.226869 * P, 3)
    
    Ad3 = round(math.pi * d3**2 / 4.0, 2)
    
    As = round((math.pi / 4.0) * (math.ceil(d) - 0.9382 * P)**2, 1)
    #
    return d2, d3, Ad3, As
    #
#
#
def minBoltMetric(D, p, sigmay_b, sigmay_m, T = 0):
    """
    D : Nominal diameter (mm)
    P : Pitch of the iso metric threads (mm)
    sigmay b : Yield stress of the bolt material (N/mm2)
    sigmay m : Yield stress of the nut material (N/mm2)
    ---
    T :  Tension load (N)
    """
    #
    # Bolt shank to fail before thread stripping in the bolt
    # bolt
    Le1 = ((0.7454 * (D - 0.9382 * p)**2) 
           / (0.27125 * math.pi * (D - 0.54127 * p)))
    # nut
    Le2 = (((0.7454 * (D - 0.9382 * p)**2) * sigmay_b)
           / ((0.34875 * math.pi * (D - 0.54127 * p)) * sigmay_m))
    #
    
    # If the design tension load T for the joint in known
    if T > 0:
        # bolt
        Le1_T = (T / (0.27125 * math.pi 
                      * (D - 0.54127 * p) * sigmay_b))
        # nut
        Le2_T = (T / (0.34875 * math.pi 
                      * (D - 0.54127 * p) * sigmay_m))
        #
    #
    else : Le1_T = 0; Le2_T = 0
    
    #
    Le = max(Le1, Le2, Le1_T, Le2_T)
    
    # The minimum thread engagement length
    N = max(math.ceil(Le / p) + 2, 8)
    L = N * p
    #print("******** L = [{:1.6e}]".format(L))
    
    # Nut thread minor diameter
    D1 = round((math.ceil(D) - (5 * math.sqrt(3) / 8.0) * p), 3)
    # Nut thread pitch diameter
    D2 = round((math.ceil(D) - (3 * math.sqrt(3) / 8.0) * p), 3)
    
    #
    return N, L, D1, D2
    #
#
#
def checkMatInput(self):
    """
    """
    # ESV Tapped blind hole joint
    if self.jointType == 'ESV':
        try:
            sigmay_m = self.blindRp02min.convert('newton/millimeter^2').value
            E = self.blindE.convert('newton/millimeter^2').value
        
        except:
            print("error blind hole plate data not given")
            sys.exit("error blind hole plate data not given")     
    
    # DSV Through bolted joint with nut
    else:
        try:
            sigmay_m = self.nutRp02min.convert('newton/millimeter^2').value
            E = self.nutE.convert('newton/millimeter^2').value
        
        except:
            print("error nut data not given")
            sys.exit("error nut data not given")
    #
    #
    return sigmay_m, E
    #
#
#
def checkPlates(self):
    """
    """
    #
    if self.jointType == 'DSV':
        RziT = (self.plateNo - 1) * self.Rzi.convert('millimeter').value
        
        if self.plateNo < 2 :
            print('WARNING Number of clamping plates [{:1.4E}] < 2 for DSV joint type'
                  .format(self.plateNo))
        
            RziT = self.plateNo * self.Rzi.convert('millimeter').value
    #
    else: RziT = self.plateNo * self.Rzi.convert('millimeter').value
    
    if self.plateNo < 1 :
        print('error Number of clamping plates [{:1.4E}] < 1 for ESV joint type'
              .format(self.plateNo))
        sys.exit('error Number of clamping plates [{:1.4E}] < 1 for ESV joint type'
                 .format(self.plateNo))
    
    #
    lKi = []
    dei = []
    dii = []
    Ei = []
    pG = []
    for plateSec in self.clampedPlate.itervalues():
        #print (plateSec.name)
        _unitLength = plateSec.unitLength
        _lK = Number(float(plateSec.lKi), dims = _unitLength)
        _De = Number(float(plateSec.De), dims = _unitLength)
        _Di = Number(float(plateSec.Di), dims = _unitLength)
        #
        _pressure = plateSec.unitForce + '/' + plateSec.unitLength + '^2'
        _E = Number(float(plateSec.E), dims = _pressure)
        _pGi = Number(float(plateSec.pG), dims = _pressure)
        
        lKi.append(_lK.convert('millimeter').value)
        dei.append(_De.convert('millimeter').value)
        dii.append(_Di.convert('millimeter').value)
        Ei.append(_E.convert('newton/millimeter^2').value)
        pG.append(_pGi.convert('newton/millimeter^2').value)
        #
    
    lK = sum(lKi)
    #
    self.boltID, _dh = boltFinder(self.boltID)
    
    if min(dii) < _dh:
        print('error Di [{:1.4E}] < min bolt dh [{:1.4E}]'.format(min(dii), _dh))
        sys.exit('error Di [{:1.4E}] < min bolt dh [{:1.4E}]'.format(min(dii), _dh))
    #
    return RziT, lK, lKi, dei, dii, Ei, pG
    #
#
#-------------------------------------------------
#
class clampingPiece(object):
    """
    """
    __slots__ = ('name', 'number','De', 'Di', 'lKi', 'Rp02min', 'pG', 'E', 'alphaT',
                 'unitForce','unitLength','unitMass')
    
    def __init__(self, Name, Number, de, di, lK, Rp, pg, E, alpha,
                 ForceUnit = '', LengthUnit = '', massUnit = ''):
        #
        self.name = Name
        self.number = Number
        self.De = de
        self.Di = di
        self.lKi = lK
        self.Rp02min = Rp
        self.pG = pg
        self.E = E
        self.alphaT = alpha
        # Units
        self.unitLength = LengthUnit
        self.unitForce = ForceUnit
        self.unitMass = massUnit
        # section
        #
#
#
class boltPiece(object):
    """
    """
    __slots__ = ('name', 'number', 'di', 'li', 'Ai',
                 'unitForce', 'unitLength', 'unitMass')
    #
    def __init__(self, Name, Number, Di, Li,
                 ForceUnit = '', LengthUnit = '', massUnit = ''):
        #
        self.name = Name
        self.number = Number
        #
        self.di = Di
        self.li = Li
        self.Ai = math.pi / 4.0 * self.di**2
        # Units
        self.unitLength = LengthUnit
        self.unitForce = ForceUnit
        self.unitMass = massUnit
        #
    #
#
#
#-------------------------------------------------
#               Printing Section
#
def printBoltProp(l, P, d, b, d2, d3, Ad3, dK, dW, dh, lS, units = '', boltID = ''):
    #
    _units = printUnits(units)
    #
    print("R0 Nominal Diameter")
    print("{:s}".format(87*"_"))
    print("")
    print("{} FASTENER DETAILS".format(35*" "))
    print("")
    print("Bolt ID         P     [{length}]  d2    [{length}]  dK    [{length}]  l     [{length}]"
          .format(**_units))
    print("{:15s} d     [{length}]  d3    [{length}]  dW    [{length}]  lS    [{length}]"
          .format("",**_units))
    print("{:15s} b     [{length}]  Ad3 [{length}^2] {:12s} dh    [{length}]"
          .format("", "", **_units))
    print("{:s}".format(87*"."))
    #
    print("{:15s} {:1.4E}  {:1.4E}  {:1.4E}  {:1.4E}".
          format(boltID, P, d2, dK, l))
    print("{:>15} {:1.4E}  {:1.4E}  {:1.4E}  {:1.4E}".
          format("", d, d3, dW, lS))
    print("{:>15} {:1.4E}  {:1.4E} {:12s} {:1.4E}".
          format("", b, Ad3, "", dh))
    #
#
#
def printR0(d, dK, dW, P, d2, d3, lS, b, dh, units = '', boltID = ''):
    #
    _units = printUnits(units)
    #
    print("R0 Nominal Diameter")
    print("{:s}".format(87*"_"))
    print("")
    print("{} FASTENER DETAILS".format(35*" "))
    print("")
    print("Fastener ID     d     [{length}]  P     [{length}]  As   [{length}]   ls    [{length}]  As  [{length}^2]".format(**_units))
    print("{:15s} dK    [{length}]  d2    [{length}]  Ad3  [{length}]   b     [{length}]  Ad3 [{length}^2]".format("", **_units))
    print("{:15s} dW    [{length}]  d3    [{length}]  dh   [{length}]   e     [{length}]  AT  [{length}^2]".format("", **_units))
    print("{:s}".format(87*"."))
    #
    print("{:15s} {:1.4E}  {:1.4E}  {:1.4E}  {:1.4E}  {:1.4E}  {:1.4E}".
          format(boltID, d, P, lS, b, dh, dh))
    print("{:>15} {:1.4E}  {:1.4E}".
          format("", dK, d2))
    print("{:>15} {:1.4E}  {:1.4E}".
          format("", dW, d3))
    #
#
#
def printR1(alpha_A):
    #
    print("")
    print("R1 Tightening Factor")
    print("{:s}".format(87*"_"))
    print("")
    print("{:15s} alpha A".format(""))
    print("{:s}".format(87*"."))
    print("{:>15} {:1.4E}".
          format("", alpha_A))    
    #
#
#
def printR2():
    #
    print("")
    print("R2 Required Minimum Clamp Load")
    #print("{:s}".format(87*"_"))
    #print("")
    #
#
#
def printR2c(FAmax, MBmax, Ssym, a, u, AD, IBT, FKA, _length = '', _force = ''):
    #
    _units = printUnits(_length, _force)
    #
    print("{:s}".format(87*"_"))
    print("")
    print("c) Prevention   FKA  [{force}]  FAm  [{force}]  Ssym  [{length}]  u     [{length}]  AD  [{length}^2]"
          .format(**_units))
    print("   of Opening {:13s} MBm[{force}{length}]  a     [{length}] {:12s} IBT [{length}^3]"
          .format("","", **_units))
    print("{:s}".format(87*"."))
    print("{:14s} {: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E}".
              format("", FKA, FAmax, Ssym, u, AD)) 
    print("{:26s} {: 1.4E} {: 1.4E} {:11s} {: 1.4E}".
          format("", MBmax, a, " ", IBT))      
    print("")
    #   
#
#
def printR2total(FKerf, FKQ, FKP, FKA, FAmin, FAmax,
                 _length = '', _force = ''):
    #
    _units = printUnits(_length, _force)
    
    _FA = FAmin
    if (FKQ + FKP + FKA + FAmin) == 0 : _FA = FAmax
    #
    print("{:s}".format(87*"_"))
    print("")
    print("Summary {:7s} FKerf[{force}]  FKQ  [{force}]  FKP  [{force}]  FKA  [{force}]  FAmin[{force}]"
          .format("", **_units))
    print("{:s}".format(87*"."))
    
    print("{:14s} {: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E}".
          format(" ", FKerf, FKQ, FKP, FKA, _FA))    
    print("")
    #
#
#
def printR3_1():
    #
    print("")
    print("R3 Load factor and resilients")
    print("{:s}".format(87*"_"))
    print("")
    #
#
def printR3_2(Phi_n, Phi_K, n, delta_Pp, delta_Ppp, delta_S, lGew, Es, EBI,
              delta_SK, delta_Gew, delta_GM, delta_i,
              beta_SK, beta_Gew, beta_GM , beta_i,
              _length = '', _force = ''):
    #
    _units = printUnits(_length, _force)
    #
    #print("")
    print("{:15s} Phi n {:5s} delta P*    EB[{force}/{length}2] delta SK    beta SK     beta i"
          .format("","", **_units))
    print("{:15s} Phi K {:5s} delta P**   Es[{force}/{length}2] delta GM    beta GM     delta i"
          .format("", "", **_units))
    print("{:15s} n {:9s} delta S     lGew   [{length}] delta GeW   beta Gew"
             .format("", "", **_units))    
    print("{:s}".format(87*"."))
    print("{:15s} {:1.4E} {: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E}"
          .format("",Phi_n, delta_Pp, Es, delta_SK, beta_SK, beta_i))
    print("{:15s} {:1.4E} {: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E}"
              .format("", Phi_K, delta_Ppp, EBI, delta_GM, beta_GM, delta_i))
    print("{:15s} {:1.4E} {: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E}"
                  .format("", n, delta_S, lGew , delta_Gew, beta_Gew))
    #
    #
#
#
def printR4(FZ, delta_P, delta_S, Rzt, Rzn, RziT, _length = '', _force = ''):
    #
    _units = printUnits(_length, _force)
    fZ = Rzt + Rzn + RziT
    #
    print("")
    print("R4 Preload Changes")
    print("{:s}".format(87*"_"))
    print("")
    print("{:15s} Fz   [{force}]  Rz  Thread  fz    [{length}]  deltaP[{length}]"
          .format("", **_units))
    print("{:27s} RzNut/Head  {:11s} deltaS[{length}]"
          .format("", "", **_units))
    print("{:27s} RzPlateInt  "
          .format("", **_units))
    print("{:s}".format(87*"."))
    print("{:15s} {:1.4E}  {:1.4E} {: 1.4E} {: 1.4E}".
          format(" ", FZ, Rzt, fZ, delta_P))
    print("{:27s} {:1.4E} {:11s} {: 1.4E}".
          format(" ", Rzn, "",delta_S))
    print("{:27s} {:1.4E}".
          format(" ", RziT))
    print("")
    #
#
#
def printR5(FMmin, FZ, FAmax, FKerf, deltaFVth, Phi_n, _length = '', _force = ''):
    #
    _units = printUnits(_length, _force)
    #
    print("")
    print("R5 Minimum Assembly Preload")
    print("{:s}".format(87*"_"))
    print("")
    print("{:15s} FMmin[{force}]  Fkerf[{force}]  Fz   [{force}]  Phi en"
          .format("", **_units))
    print("{:27s} FAmax[{force}]  F'Vth[{force}]"
          .format("", **_units))
    print("{:s}".format(87*"."))
    
    print("{:15s} {:1.4E}  {:1.4E} {: 1.4E} {: 1.4E}".
          format("", FMmin, FKerf, FZ, Phi_n))
    print("{:27s} {:1.4E}  {:1.4E}".
          format("", FAmax, deltaFVth))   
    print("")
#
#
def printR6(FMmax, FMmin, alpha_A, _length = '', _force = ''):
    #
    _units = printUnits(_length, _force)
    #
    print("")
    print("R6 Maximum Assembly Preload")
    print("{:s}".format(87*"_"))
    print("")
    print("{:15s} FMmax[{force}]  FMmin[{force}]  alpha A"
          .format("", **_units))
    print("{:s}".format(87*"."))
    
    print("{:15s} {:1.4E}  {:1.4E} {: 1.4E}".
          format(" ", FMmax, FMmin, alpha_A))    
    print("")
#
#
def printR7(FMzul, FMmax, P, A0, d0, d2, Rp02min, muG, nu,
            _length = '', _force = ''):
    #
    _units = printUnits(_length, _force)
    UR = FMmax/FMzul
    _status = 'PASS'
    if UR > 1 : _status = 'FAIL'
    #
    print("")
    print("R7 Assembly Stress")
    print("{:s}".format(87*"_"))
    print("")
    print("{:15s} FMzul[{force}]  P     [{length}]  d0    [{length}]  muGmin      Rp02min "
          .format("", **_units))
    print("{:15s} FMmax[{force}]  A0  [{length}^2]  d2    [{length}]  v"
          .format("", **_units))
    print("Safety Factor   FMmax/FMzul")    
    print("{:s}".format(87*"."))
    
    print("{:15s} {:1.4E}  {:1.4E} {: 1.4E} {: 1.4E} {: 1.4E}".
          format(" ", FMzul, P, d0, muG, Rp02min))
    print("{:15s} {:1.4E}  {:1.4E} {: 1.4E} {: 1.4E}".
          format(" ", FMmax, A0, d2, nu))
    print("{:15s} {:10.2f}".
              format(_status, UR))    
    print("")
#
#
def printR8(sigma_zmax, sigma_redB, tau_max,
            As, P, FMzul, FAmax, Phi_n, dS, d2, Rp02min, muG,
            _length = '', _force = ''):
    #
    _units = printUnits(_length, _force)
    SFredB = sigma_redB / Rp02min
    SFzmax = sigma_zmax / Rp02min
    
    _status = 'PASS'
    if SFredB > 1 or SFzmax > 1 : _status = 'FAIL'    
    #
    print("")
    print("R8 Working Stress")
    print("{:s}".format(87*"_"))
    print("")
    print("{:15s} sigma redB  sigma zmax  tau max     FMzul[{force}]  P     [{length}]  muGmin"
          .format("", **_units))
    print("{:15s} Rp02min     Rp02min {:15s} FAmax[{force}]  dS    [{length}]  Phi n"
          .format("", "", **_units))
    print("Safety Factor   SredB/Rp02  Szmax/Rp02 {:12s} As  [{length}^2]  d2    [{length}]".
          format("", **_units))
    print("{:s}".format(87*"."))
    
    print("{:14s} {: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E}".
          format(" ", sigma_redB, sigma_zmax, tau_max, FMzul, P,  muG))
    print("{:14s} {: 1.4E} {: 1.4E} {:11s} {: 1.4E} {: 1.4E} {: 1.4E}".
          format(" ", Rp02min, Rp02min,"", FAmax, dS, Phi_n))
    print("{:15s} {:10.2f}  {:10.2f} {:11s} {: 1.4E} {: 1.4E}".
          format(_status, SFredB, SFzmax, "", As, d2))     
    print("")
#
#
def printR9_1(sigma_a, sigma_AS, Phi_en, As, FAmax, FAmin, d, rollTreat,
              _length = '', _force = ''):
    #
    _units = printUnits(_length, _force)
    _rolled = 'Before Heat'
    if rollTreat == 'SG' : _rolled = 'After Heat'
    
    SD = (sigma_a * 1.2) / sigma_AS
    _status = 'PASS'
    if SD > 1 : _status = 'FAIL'
    #
    print("")
    print("R9 Alternating Stress")
    print("{:s}".format(87*"_"))
    print("")
    print("BoltRollTreat   sigma_a     FAmax[{force}]  As  [{length}^2]  d     [{length}]  Phi_n"
          .format(**_units))
    print("{:15s} sigma_AS    FAmin[{force}]"
          .format("", **_units))
    print("Safety Factor   SD {:8s}"
          .format("", **_units))
    print("{:s}".format(87*"."))
    
    print("{:14s} {: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E}".
          format(_rolled, sigma_a, FAmax, As, d, Phi_en))
    print("{:14s} {: 1.4E} {: 1.4E}".
          format(" ", sigma_AS, FAmin))
    print("{:15s} {:10.2f}".
          format(_status, SD))    
    print("")
    #
#
#
def printR9_2(sigma_SAbo, sigma_SAbu, sigma_a, sigma_AS, rollTreat,
              FAmax, FAmin, As, IBers, Phi_n, 
              Ssym, a, lK, lers, d, dS, dh, Es, Ep, 
              _length = '', _force = ''):
    #
    _units = printUnits(_length, _force)
    _rolled = 'Before Heat'
    if rollTreat == 'SG' : _rolled = 'After Heat'
    
    SD = (sigma_a * 1.2) / sigma_AS
    _status = 'PASS'
    if SD > 1 : _status = 'FAIL'     
    #
    print("")
    print("R9 Alternating Stress")
    print("{:s}".format(87*"_"))
    print("")
    print("BoltRollTreat   sigma a     sigma SAbo  As  [{length}^2]  Phi n       d     [{length}]  a     [{length}]"
          .format(**_units))
    print("{:15s} sigma AS    sigma SAbu  FAmax[{force}]  lk  [{length}]    dS    [{length}]  Ssym  [{length}]"
          .format("", **_units))
    print("Safety Factor   SD {:20s} FAmin[{force}]  lers[{length}]    dh    [{length}]  IBers[{length}4]"
          .format("", **_units))
    print("{:s}".format(87*"."))
    
    print("{:15s} {:1.4E}  {:1.4E}  {:1.4E}  {:1.4E}  {:1.4E}  {:1.4E}".
          format(_rolled, sigma_a, sigma_SAbo, As, Phi_n, d, a))
    print("{:15s} {:1.4E}  {:1.4E}  {:1.4E}  {:1.4E}  {:1.4E}  {:1.4E}".
          format("", sigma_AS, sigma_SAbu, FAmax, lK, dS, Ssym))
    print("{:15s} {:10.2f}  {:10s}  {:1.4E}  {:1.4E}  {:1.4E}  {:1.4E}".
          format(_status, SD, "", FAmin, lers, dh, IBers))    
    print("")
#
#
def printR10(Pmax, FMzul, Apmin, pG,
             _length = '', _force = ''):
    #
    _units = printUnits(_length, _force)
    SP = Pmax / pG
    _status = 'PASS'
    if SP > 1 : _status = 'FAIL'
    #
    print("")
    print("R10 Surface Pressure")
    print("{:s}".format(87*"_"))
    print("")
    print("{:15s} Pmax [{force}]  FMzul[{force}]  Apmin[{length}2]"
          .format("", **_units))
    print("{:15s} pG   [{force}]"
          .format("", **_units))
    print("Safety Factor   SP ")    
    print("{:s}".format(87*"."))
    
    print("{:14s} {: 1.4E} {: 1.4E} {: 1.4E}".
          format(" ", Pmax, FMzul, Apmin))
    print("{:14s} {: 1.4E} ".
          format(" ", pG))
    print("{:15s} {:10.2f} ".
          format(_status, SP))    
    print("")
#
#
def printR11(FmS, Rs, C1, C3, meff, ASGM, FmGM,
             P, d, d2, s, D1, D2, As, tauBM, tauBS, RmS,
             angle, _length = '', _force = ''):
    #
    _units = printUnits(_length, _force)
    SP = FmS / FmGM
    _status = 'PASS'
    if SP > 1 : _status = 'FAIL'    
    #
    print("")
    print("R11 Length Engagement")
    print("{:s}".format(87*"_"))
    print("")
    print("{:15s} FmS  [{force}]  meff  [{length}]  RmS {:7s} P     [{length}]  s     [{length}]  tau_BM"
          .format("", "", "", **_units))
    print("{:15s} FmGM [{force}]  C1 {:8s} As   [{length}2]  d     [{length}]  D1    [{length}]  tau_BS"
          .format("", "", **_units))
    print("Safety Factor   SP {:7s}  C3 {:8s} ASGM [{length}2]  d2    [{length}]  D2    [{length}]  FlankAngle"
          .format("", "", **_units))
    print("{:s}".format(87*"."))
    
    print("{:14s} {: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E}".
          format(" ", FmS, meff, RmS, P, s, tauBM))
    print("{:14s} {: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E}".
          format(" ", FmGM, C1, As, d, D1, tauBS))
    print("{:15s} {:10.2f} {: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E}".
          format(_status, SP, C3, ASGM, d2, D2, angle))
    print("")
#
#
def printR12(FMzul, FAmax, FZ, alpha_A, Phi_en, deltaFVth,
             _length = '', _force = ''):
    #
    _units = printUnits(_length, _force)
    #
    print("")
    print("R12 Slipping, Shearing Stress")
    print("{:s}".format(87*"_"))
    print("")
    print("{:15s} FKRmn[{force}]  tauQmax     FMzul[{force}]  FZ   [{force}]  Phi en      Rm"
          .format("", **_units))
    print("{:15s} FKQef[{force}]  tauB        FAmax[{force}]  DFVth[{force}]  alpha A     FQmax[{force}]"
          .format("", **_units))
    print("Safety Factor   SG {:8s} SA {:8s} FQmax[{force}]  DFVth[{force}]  alpha A     Atau [{length}2]"
          .format("", "", "", "", **_units))
    print("{:s}".format(87*"."))
    print("")
#
#
def printR13(MA, FMzul, P, d2, DKm, muG, muK,
             _length = '', _force = ''):
    #
    _units = printUnits(_length, _force)
    #
    print("")
    print("R13 Tightening Torque")
    print("{:s}".format(87*"_"))
    print("")
    print("{:15s} MA [{force}{length}]  FMzul[{force}]  DKm   [{length}]  P     [{length}]  Mu G"
          .format("", **_units))
    print("{:15s} {:15s} {:15s}     d2    [{length}]  Mu K"
              .format("","", "", **_units))     
    print("{:s}".format(87*"."))
    
    print("{:14s} {: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E}".
          format(" ", MA, FMzul, DKm, P, muG))
    print("{:14s} {:11s} {:11s} {:11s} {: 1.4E} {: 1.4E}".
          format(" ", "", "", "", d2, muK))
    print("")
#
#
class boltAnalysis():
    """
    Analysis type:
    Initial bolt size
    Torque analysis
    Thread stripping analysis
    Joint analysis
    ---
    Load Type:
    Static(1x)
    Dynamic(2x)
    """
    #
    def __init__(self, analysisType, jointType):
        """
        ---
        Analysis type : 
        Load type: static/dynamic
        """
        #
        self.analysisType = analysisFinder(analysisType)
        self.jointType  = jointFinder(jointType)
        #self.loadType = loadingFinder(loadType)
        #
        self.unitLength = 'meter'
        self.unitForce = 'newton'
        self.unitMass = 'kilogram'
        self.localAxis = 'x y z'
        #self.materialType = 'steel'
        #
        self.gasget = ''
        #
        self.clampedPlate = {}
        self.plateNo = 0
        #
        self.boltSection = {}
        self.boltNo = 0
        #
        #self.jointType = 'ESV'
        self.rollTreat = 'SV'
        self.threadType = 'metric'
        self.loadCase = 'concentric'
        #
        self.deltaFVth = Number(float(0), dims = self.unitForce)
        # self.lS = Number(0.0, dims = 'millimeter')
        #
    #
    #
    #-------------------------------------------------
    #                   General Data
    #-------------------------------------------------   
    #
    def units(self, Force = 'newton', Length = 'meter', Mass = 'kilogram'):
        """
        Force  : newton (default)
        Length : meter (default)
        Mass   : kilogram (default)
        """
        #
        self.unitForce  = FindForce(Force)
        self.unitLength = FindLength(Length)
        self.unitMass = FindMass(Mass)
        #     
    #
    #
    #-------------------------------------------------
    #                 Bolt Details
    #-------------------------------------------------
    #
    def predimentioning(self):
        pass
    #
    #
    def boltData(self, boltName, l, grade, material):
        """
        boltClass: Bolt Name/ID
        
        l  : Bolt length up to head
        
        grade   :
                4.6
                4.8
                5.8
                8.8
                9.8
                10.9
                12.9
                user : provide Rp02min, RmS and E in boltMaterial card
        
        material:
                steel
                austenitic heat treated
                austenitic F60/90
                grey cast iron
                aluminium
                titanium
                user : provide tauB in boltMaterial card
        --
        """
        #
        #_pressure = self.unitForce + '/' + self.unitLength + '^2'
        
        self.boltID = boltName
        self.l = Number(float(l), dims = self.unitLength)
        
        #
        self.boltGrade, _Rp02min, _RmS  = matGradeFinder(grade)
        
        if self.boltGrade == 'user':
            print("boltMaterial card is needed")
        #
        else:
            self.Rp02min = Number(float(_Rp02min), dims = 'newton/millimeter^2')
            self.RmS = Number(float(_RmS), dims = 'newton/millimeter^2')
            self.E = Number(205000, dims = 'newton/millimeter^2')
        
        #
        _material, _shearR = matFinder(material)
        
        if _material == 'user':
            print("boltMaterial card is needed")
        
        else:
            _RmS = self.RmS.convert('newton/millimeter^2').value
            _tauB = _RmS * _shearR
            self.tauBS = Number(float(_tauB), dims = 'newton/millimeter^2')
        #
        #_unitArea = str(self.unitLength) + '^2'
        #self.As = Number(float(As), dims = _unitArea)
        #   
    #
    #
    def boltShank(self, di, li, bsID =""):
        """
        di : Diameter of an individual cylindrical bolt
        li : Length of an individual cylindrical bolt
        """
        #
        self.boltNo += 1
        self.boltSection[self.boltNo] = boltPiece(bsID, self.boltNo, di, li,
                                                  self.unitForce, self.unitLength, self.unitMass)
        #
    #
    #
    def boltThread(self, Type, P, d, b, d2 = 0, d3 = 0, Ad3 = 0, alpha = 60,
                   rollTreat = 'SV'):
        """
        Type : Bolt thread type
                metric
                US
                user
        P  : Pitch
        d  : Major diameter of the thread
        b  : Thread length
        ---
        d2 : Pitch diameter of the thread
        d3 : Minor diameter of the thread
        Ad3 : Cross sec at minor diam
        alpha : Thread (flank) angle (default = 60 degrees)
        ---
        rollTreat : Roll thread heat treatment = before(SV)/after(SG)
        ---
        """
        #
        self.threadType = Type
        self.P = Number(float(P), dims = self.unitLength)
        self.d = Number(float(d), dims = self.unitLength)
        self.b = Number(float(b), dims = self.unitLength)
        #
        _unitArea = str(self.unitLength) + '^2'
        self.d2 = Number(float(d2), dims = self.unitLength)
        self.d3 = Number(float(d3), dims = self.unitLength)
        self.Ad3 = Number(float(Ad3), dims = _unitArea)
        
        self.flankAngle = alpha
        self.rollTreat = rollTreatFinder(rollTreat)
        #
    #
    #
    def boltHead(self, Type, dk, dw):
        """
        Type :
            Hex head
            Slotted hex head
            Socket head
            Cheese head
            Binding head
            Flat [countersunk, double countersunk] head
            Pan head
            Filser, fillister head
            Oval [countersunk rised] head
            Bugle head
            Round [domed] head
            Button head
            Flange, truss, oven or stove head
            Washer hex head
            ---
            hexagon socket head bolt
            hexagon heat bolt
            slotted countersunk screw
            slotted raised countersunk screw
            hexagon socket countersunk head screw
            slotted cheese head screw
            special head bolt
            hex flange bolt
            user
        
        dK : Head diam of the bolt 
        dW : Effective head diam
        """
        #
        self.boltHead = boltHeadFinder(Type)
        self.dK = Number(float(dk), dims = self.unitLength)
        self.dW = Number(float(dw), dims = self.unitLength)
        #
        #else:
        #    print('error bolt head type {} no yet available. Use head : "user defined" instead'.format(head))
        #    sys.exit('error bolt head type {} no available'.format(head))             
        #
    #
    #
    def boltMaterial(self, Rp02min, RmS, E, tauB):
        """
        ---
        tauBS : Ultimate shear strength of the bolt threads        
        ---
        Rp02min : 2% proof stress of the bolt
        RmS : Tensile strength of the bolt
        E : Young's modulus of the bolt
        """
        #
        _pressure = self.unitForce + '/' + self.unitLength + '^2'
        
        #
        # Material Input by User
        
        if Rp02min == 0:
            print('error Rp02min [{}] no given'.format(Rp02min))
            sys.exit('error Rp02min [{}] no given'.format(tRp02min)) 
        
        self.Rp02min = Number(float(Rp02min), dims = _pressure )
        
        if RmS == 0:
            print('error RmS [{}] no given'.format(RmS))
            sys.exit('error RmS [{}] no given'.format(RmS))
            
        self.RmS = Number(float(RmS), dims = _pressure )
        
        if E == 0:
            print('error E [{}] no given'.format(E))
            sys.exit('error E [{}] no given'.format(E))
            
        self.E = Number(float(E), dims = _pressure )
        
        #
        # material input
        try : self.tauBS.value
        #
        except :
            if tauB == 0:
                print('error tauB [{}] no given'.format(tauB))
                sys.exit('error tauB [{}] no given'.format(tauB))
            #
            else: self.tauBS = Number(float(tauB), dims = _pressure)
        #
    #
    #
    #-------------------------------------------------
    #                 Nut Details
    #-------------------------------------------------
    #
    def nut(self, grade, material = 'steel', Rp02min = 0, RmS = 0, E = 0, tauB = 0):
        """
        grade  :
                4.6
                4.8
                5.8
                8.8
                9.8
                10.9
                12.9
                user : provide Rp02min, RmS and E
        ---
        material:
                steel
                austenitic heat treated
                austenitic F60/90
                grey cast iron
                aluminium
                titanium
                user : provide tauB
        ---
        Rp02min : Minimum 0.20% yield strength of the internal (nut) threads
        RmM : Ultimate tensile strength of the internal (nut) threads
        E : Young's modulus of the nut
        ---
        tauBM : Ultimate shear strength of the nut threads    
        """
        #
        self.nutGrade, _Rp02min, _RmS  = matGradeFinder(grade)
        _pressure = self.unitForce + '/' + self.unitLength + '^2'
        
        if self.nutGrade == 'user':
            self.nutRp02min = Number(float(Rp02min), dims = 'newton/millimeter^2')
            self.nutRmS = Number(float(RmS), dims = 'newton/millimeter^2')
            self.nutE = Number(float(205000), dims = 'newton/millimeter^2')
        
        #
        else:
            self.nutRp02min = Number(float(_Rp02min), dims = _pressure )
            self.nutRmM = Number(float(_RmS), dims = _pressure )
            self.nutE = Number(float(E), dims = _pressure )
        
        #
        _material, _shearR = matFinder(material)
        
        if _material == 'user':
            if tauB == 0:
                print('error tauB [{}] no given'.format(tauB))
                sys.exit('error tauB [{}] no given'.format(tauB))
            
            self.tauBM = Number(float(tauB), dims = _pressure)
        #
        else: 
            _RmS = self.RmS.convert(_pressure).value
            _tauB = _RmS * _shearR
            self.tauBM = Number(float(_tauB), dims = _pressure)           
        #
        #self.jointType = 'DSV'
        #
    #
    #
    def blindHole(self, grade, material = 'steel', Rp02min = 0, RmS = 0, E = 0, tauB = 0, PG = 0):
        """
        Grade : Blind plate material grade
        ---
        material:
                steel
                austenitic heat treated
                austenitic F60/90
                grey cast iron
                aluminium
                titanium
                user : provide tauB
        ---
        Rp02min : Minimum 0.20% yield strength of the internal threads
        RmM : Ultimate tensile strength of the internal threads
        E : Young's modulus of the nut
        PG : Limiting surface pressure
        ---
        tauBM : Ultimate shear strength of the threads  
        ---
        """
        #
        self.blindGrade, _Rp02min, _RmS  = matGradeFinder(grade)
        _pressure = self.unitForce + '/' + self.unitLength + '^2'
        
        if self.blindGrade == 'user':
            self.blindRp02min = Number(float(_Rp02min), dims = _pressure )
            self.blindRmS = Number(float(_RmS), dims = _pressure )
            self.blindE = Number(float(E), dims = _pressure )
            
            self.blindpG = Number(float(_PG), dims = _pressure )
        #
        else:
            self.blindRp02min = Number(float(_Rp02min), dims = 'newton/millimeter^2')
            self.blindRmS = Number(float(_RmS), dims = 'newton/millimeter^2')
            self.blindE = Number(float(205000), dims = 'newton/millimeter^2')
        #
        _material, _shearR = matFinder(material)
        
        if _material == 'user':
            if tauB == 0:
                print('error tauB [{}] no given'.format(tauB))
                sys.exit('error tauB [{}] no given'.format(tauB))
            
            self.tauBM = Number(float(tauB), dims = _pressure)
        #
        else: 
            _RmS = self.RmS.convert(_pressure).value
            _tauB = _RmS * _shearR
            self.tauBM = Number(float(_tauB), dims = _pressure)           
        #        
        #self.joint = jointFinder(joint)
        #self.jointType = 'DSV'
        # print('ok')
        #
    #
    #
    #-------------------------------------------------
    #                   Joint Details
    #-------------------------------------------------
    #
    def gasketRing(self, gasket = 'yes'):
        """
        """
        #
        self.gasget = gasket
        #
    #
    #
    def clampingPlate(self, Dei, Dii, LKi, Rpmaxi, Ei, pG, alphaTi = 0, ID =""):
        """
        Dei : Outer diameter of plate i
        Dii : Inner diameter of plate i
        LKi : Thickness of plate i
        Ei  : Young's modulus
        Rpmaxi : 2% proof stress
        pG : Limiting surface pressure
        alphaTi : Coefficient of thermal expansion
        """
        #
        self.plateNo += 1
        self.clampedPlate[self.plateNo] = clampingPiece(ID, self.plateNo , Dei, Dii, LKi,
                                                        Rpmaxi, pG, Ei, alphaTi,
                                                        self.unitForce, self.unitLength, self.unitMass)
        #
    #
    #
    #-------------------------------------------------
    #                 Tightening Details
    #-------------------------------------------------
    #
    def tightening(self, alpha_A, v = 0.90, tau_k = 0.50, tightmethod = ''):
        """
        alpha_A : Tightening factor
        v : Yield point factor for tightening
        tau_k : Reduction coefficient k
        ---
        Tight of the bolt:
        With screwdriver (4x)
        with torque wrench (2x)
        Rotation-angle controled or yielding point controled (1x)
        """
        
        #self.tightMethod = tigthtFinder(tightmethod)
        self.alpha_A = alpha_A
        self.nu = v
        self.tau_k = tau_k
        #
    #
    #
    #-------------------------------------------------
    #                 Forces
    #-------------------------------------------------
    #
    def load(self, FAmax, FAmin = 0, FQ = 0, MB = 0, MT = 0, FR = 0):
        """
        Load Application:
        concentric(1x)
        eccentric(2x)
        Transverse(4x)
        ---
        FAmax : Axially applied load directed in the bolt axis (+ve tension)
        FAmin : Lower limit of the dynamic axial force (+ve tension)
        FQ    : Transverse load, working load normal to the bolt axis
        ---
        MB : Working bending moment acting at the bolt point
        MT : Torque at individual bolting point
        ---
        FR : Residual clamp force (+ve tension)
        """
        #
        _unitMoment = str(self.unitForce) + '*' + str(self.unitLength)
        #
        self.FAmax = Number(abs(float(FAmax)), dims = self.unitForce)
        self.FQ = Number(abs(float(FQ)), dims = self.unitForce)
        self.FAmin = Number(abs(float(FAmin)), dims = self.unitForce)
        #
        self.MB = Number(float(MB), dims = _unitMoment)
        self.MT = Number(float(MT), dims = _unitMoment)
        #
        self.FR = Number(abs(float(FR)), dims = self.unitForce)
        #
        #
    #
    #
    def eccentricity(self, a, e, cT,
                     t = 0, IBT = 0, AD = 0,
                     Ssym = 0, IBers = 0, I_Bers = 0):
        """
        
        a    : Distance of the substitutional line of action of 
               the load FA from the axis of the imaginary 
               laterally symmetrical deformation solid 
        
        e    : Distance of the bolt axis from edge of the interface
               on the side at risk of opening
        
        cT   : Measurement of the interface area perpendicular to 
               the width b
        
        ---
        t  : Bolt spacing in a multi-bolted joint
        
        AD   : Sealing area (at most interface area less the 
               through hole for the bolt)
        IBT  : Moment of gyration of the interface area
        ---
        Ssym   : Distance of the bolt axis from the axis of the 
                 imaginary laterally symmetrical deformation solid        
        IBers  : Equ moment of gyration bending body
        I'Bers : Equ moment of gyration bending holl body
        """
        #
        self.a = Number(float(a), dims = self.unitLength)
        self.e = Number(float(e), dims = self.unitLength)
        self.cT = Number(float(cT), dims = self.unitLength)
        # --
        self.t = Number(float(t), dims = self.unitLength)
        #
        _unitArea = str(self.unitLength) + '^2'
        self.AD = Number(float(AD), dims = _unitArea)
        _unitGyr = str(self.unitLength) + '^4'
        self.IBT = Number(float(IBT), dims = _unitGyr)
        #self.Ssym = Number(float(Ssym), dims = self.unitLength)
        #
        self.loadCase = 'eccentric'
        #
    #
    #
    def embedding(self, Rz, Rzt = 0, Rzn = 0, Rzi = 0):
        """
        Rz : Average roughness heigth
        ---
        Rzt : Amount of embedding in the thread
        Rzn : Amount of embedding in the nut
        Rzi : Amount of embedding per inner interface
        """
        #
        self.Rz = Number(float(Rz), dims = self.unitLength)
        #
        _Rz = self.Rz.value * 1000000
        # check this chava
        _Rzt, _Rzn, _Rzi = table541(_Rz)
        
        if Rzt == 0: 
            self.Rzt = Number(float(_Rzt/1000.0), dims = 'millimeter')
            
        else: self.Rzt = Number(float(Rzt), dims = self.unitLength)
        
        if Rzn == 0:
            self.Rzn = Number(float(_Rzn/1000.0), dims = 'millimeter')
        
        else: self.Rzn = Number(float(Rzn), dims = self.unitLength)
        
        if Rzi == 0: 
            self.Rzi = Number(float(_Rzi/1000.0), dims = 'millimeter')
        
        else: self.Rzi = Number(float(Rzi), dims = self.unitLength)        
        #
    #
    #
    def loadFactor(self, n):
        """
        n  : Load introduction factor for joint types
           SV1 to SV6 according to table 5.2/1        
        """
        self.n = n
        #
    #
    #
    #-------------------------------------------------
    #                Temperature Details
    #-------------------------------------------------
    #
    def temperature(self, deltaFVth = 0):
        #
        self.deltaFVth = Number(float(deltaFVth), dims = self.unitForce)
        #
    #
    #
    #-------------------------------------------------
    #                Additional
    #-------------------------------------------------
    #
    def flange(self):
        pass
    #
    #
    #-------------------------------------------------
    #                 Analysis Data
    #-------------------------------------------------
    #
    def enduranceStrength(self):
        """
        Endurance Strength:
        RTBHT
        RTAHT
        Database
        Input
        """
    #
    #
    def friction(self, muG, muK, FMTab = 0, MA = 0, sigma = 3):
        """
        muG : Coefficient of friction in the thread
        muK : Coefficient of friction in the bolt head ot nut bearing area
        muT : Coefficient of friction between the clamp plate interfaces
        ---
        FMTab : Assembly preload
        MA : Tightening torque
        sigma : ?
        ---
        """
        #
        self.muG = Number(float(muG), dims = self.unitLength)
        self.muK = Number(float(muK), dims = self.unitLength)
        self.FMTab = Number(float(FMTab), dims = self.unitForce)
        
        _torque = self.unitForce + '*' + self.unitLength
        self.MA = Number(float(MA), dims = _torque)
        
        self.sigma = sigma
        #
    #
    #
    def printResult(self):
        """
        """
        #
        print("")
        print("----------------------------------------------")
        print("            VDI 2230 Bolt Design Tool")
        print("                 Alpha Version")
        print("                   18/05/12")
        print("")
        print("-------------- PRINTING RESULTS --------------")
        #
        print("")
        #
        # check No of plates
        _RziT, _lK, _lKi, _dei, _dii, _Ei, _pGi = checkPlates(self)
        _DAa = max(_dei); _DA = min(_dei); _dh = min(_dii); _hmin = min(_lKi)
        _Ep = min(_Ei); _pG = min(_pGi)
        
        self.RziT = Number(float(_RziT), dims = 'millimeter')
        
        self.dh = Number(float(_dh), dims = 'millimeter')
        
        _P = self.P.convert('millimeter').value
        _d = self.d.convert('millimeter').value
        _dW = self.dW.convert('millimeter').value
        
        if _dh >= _dW :
            print('error dh [{:1.4e}] >= dW [{:1.4e}]'.format(_dh, _dW))
            sys.exit('error dh [{:1.4e}] >= dW [{:1.4e}]'.format(_dh, _dW))
        
        #_load = "axial"
        #if self.FQ.value != 0:
        #    _load = "shear"
        
        # check bolt input data
        if self.threadType == 'metric':
            #
            _d2, _d3, _Ad3, _As = threadPropMetric(_P, _d)
            
            if self.d2.value == 0:
                self.d2 = Number(float(_d2), dims = 'millimeter')
            
            if self.d3.value == 0:
                self.d3 = Number(float(_d3), dims = 'millimeter')
            
            if self.Ad3.value == 0:
                _unitArea = 'millimeter^2'
                self.Ad3 = Number(float(_Ad3), dims = _unitArea)
            
            # check material input
            _sigmay_m, _E_m = checkMatInput(self)
            
            self.EBI = Number(float(_E_m), dims = 'newton/millimeter^2') 
            _sigmay_b = self.Rp02min.convert('newton/millimeter^2').value
            _T = self.FAmax.convert('newton').value
            
            #
            _N, _Lt, _D1, _D2 = minBoltMetric(_d, _P, _sigmay_b, _sigmay_m, _T)
            #
        #
        else:
            print("N/A yet")
            sys.exit("N/A yet")
        
        #
        
        # lGew Length of the free loaded thread
        _l = self.l.convert('millimeter').value
        _b = self.b.convert('millimeter').value
        _Es = self.E.convert('newton/millimeter^2').value
        _lS, _lGew, _delta_i, _beta_i = checkInput(_l, _b, _Lt, _lK, 
                                                   self.boltSection, self.clampedPlate, _Es)
        self.lGew = Number(float(_lGew), dims = 'millimeter')
        self.lS = Number(float(_lS), dims = 'millimeter') 
        #
        # ---> R1 Tightening factor
        #
        l = self.l.convert(self.unitLength).value
        
        dK = self.dK.convert(self.unitLength).value
        dW = self.dW.convert(self.unitLength).value
        
        P = self.P.convert(self.unitLength).value
        d = self.d.convert(self.unitLength).value
        b = self.b.convert(self.unitLength).value
        d2 = self.d2.convert(self.unitLength).value
        d3 = self.d3.convert(self.unitLength).value
        Ad3 = self.Ad3.convert(self.unitLength + '^2').value
        dh = self.dh.convert(self.unitLength).value
        lS = self.lS.convert(self.unitLength).value
        
        printBoltProp(l, P, d, b, d2, d3, Ad3, dK, dW, dh, lS,
                      self.unitLength, self.boltID)
        #
        printR1(self.alpha_A)
        #
        # ---> R2 Required minimum clamp load
        #
        printR2()
        _FAmax = self.FAmax.convert('newton').value
        _FAmin = self.FAmin.convert('newton').value
        _FQ    = self.FQ.convert('newton').value
        _MBmax = self.MB.convert('newton*millimeter').value
        _Mt    = self.MT.convert('newton*millimeter').value
        _Mymax = max(_MBmax, _Mt)
        _deltaFVth = self.deltaFVth.convert('newton').value
        
        # a) Friction grip to transmit transversw load FQ
        #    and/or a torque about the bolt axis
        _FKQ  = 0
        if _FQ != 0 or _Mymax != 0 :
            FKQ = R2_a(_FQ, _Mymax, _qF, _qM, _ra, _mu_Tmin)
        
        # b) Sealing agains a medium
        _FKP = 0
        if self.gasget == 'yes': FKP = R2_b(_Pimax, _AD)
        
        # c) Prevention of opening
        _FKA = 0
        #if self.FAmax.value != 0 or self.MB.value != 0:
        if self.loadCase == 'eccentric':
            
            _e = self.e.convert('millimeter').value
            _cT = self.cT.convert('millimeter').value
            _t = self.t.convert('millimeter').value
           
            # calcs in N, mm
            
            _a = self.a.convert('millimeter').value
            
            _DAmin, _G, _IBT, _AD, _Ssym, _u = VD_5122_1(self.jointType, 
                                                         _e, _cT, _dW, 
                                                         _hmin, _dh, _t)
            
            self.u = Number(float(_u), dims = 'millimeter')
            self.IBT = Number(float(_IBT), dims = 'millimeter^4')
            self.AD = Number(float(_AD), dims = 'millimeter^2')
            self.Ssym = Number(float(_Ssym), dims = 'millimeter')
            
            _FKA = R2_c(_FAmax, _MBmax, _Ssym, _a, _u, _AD, _IBT)
            self.FKA = Number(_FKA, dims = 'newton')
            
            # print in defined units
            FAmax = self.FAmax.convert(self.unitForce).value
            MBmax = self.MB.convert(self.unitForce+'*'+self.unitLength).value
            Ssym = self.Ssym.convert(self.unitLength).value
            a = self.a.convert(self.unitLength).value
            u = self.u.convert(self.unitLength).value
            # check this
            AD = self.AD.convert(self.unitLength + '^2').value
            IBT = self.IBT.convert(self.unitLength + '^4').value
            FKA = self.FKA.convert(self.unitForce).value
            
            printR2c(FAmax, MBmax, Ssym, a, u, AD, IBT, FKA,
                     self.unitLength, self.unitForce)
        #
        #
        #
        #
        _FKerf = max(_FKQ, _FKP, _FKA, _FAmin) # in mm
        #if _FKerf == 0 : _FKerf = _FAmax
        
        printR2total(_FKerf, _FKQ, _FKP, _FKA, _FAmin, _FAmax,
                     self.unitLength, self.unitForce)
        #
        # ---> R3 Load factor and resilients
        #
        printR3_1()
        
        # converting units
        _d = self.d.convert('millimeter').value
        _d3 = self.d3.convert('millimeter').value
        #_lGew = self.lGew.convert('millimeter').value
        #_Es = self.E.convert('newton/millimeter^2').value
        _EBI = self.EBI.convert('newton/millimeter^2').value
        
        #
        # Axial Resilience
        _delta_SK, _delta_Gew, _delta_GM = VD_5111(self.jointType, self.boltHead,
                                                   _d, _d3, _lGew, _Es, _EBI)
        
        _delta_i_sum = sum(_delta_i)
        _delta_S = _delta_i_sum + _delta_SK + _delta_Gew + _delta_GM
        
        self.delta_SK = Number(float(_delta_SK), dims = 'millimeter/newton')
        self.delta_Gew = Number(float(_delta_Gew), dims = 'millimeter/newton')
        self.delta_GM = Number(float(_delta_GM), dims = 'millimeter/newton')
        
        #
        # Bending resilience
        _beta_SK, _beta_Gew, _beta_GM = VD_5112(self.jointType, self.boltHead,
                                                   _d, _d3, _lGew, _Es, _EBI)        
        
        _beta_i_sum = sum(_beta_i)
        _beta_S = _beta_SK + _beta_i_sum + _beta_GM + _beta_Gew
        
        _lers = VD_5112_2(_beta_S, _d3, _Es)
        
        _tan_phi, _DAGr, _w = VD_512(self.jointType, _dW, _DAa, _lK)
        
        # concentric loading
        if self.loadCase == 'concentric':
            _delta_P = VD_5121_1(_dh, _dW, _lK, _DA, _Ep, _tan_phi, _DAGr, _w)
            
            _Phi_K , _Phi_en, _FSA = VD_531(_delta_P, _delta_S, self.n)
            _delta_Pp = _delta_P; _delta_Ppp = _delta_P
        
        # eccentric loading
        else:
            _delta_P, _lV, _lH = VD_5121_2(_dh, _dW, _lK, _DA, _Ep, _tan_phi, _w)
        
            # Resiliences for eccentric loading and clamping
            #_Ssym = self.Ssym.convert('millimeter').value
            _cT = _DA # mm Width of the interface
            _delta_Pp, _IBers = VD_5122_2(_delta_P, _DA, _DAGr, _dW, _Ssym, 
                                          _b, _cT, _w, _lV, _lH, _lK, _Es)        
            
            _a = self.a.convert('millimeter').value
            _delta_Ppp = VD_5123(_delta_P, _a, _Ssym, _lK, _Es, _IBers)
        
            # after interpolation in Table 5.2/1
            _Phi_K, _Phi_en, _FAab, _FSA, _FPA = VD_5312(_delta_Pp, _delta_Ppp, _delta_S, self.n)
            #_Phi_n = _Phi_en
        
        #
        _d = self.d.convert(self.unitLength).value
        _d3 = self.d3.convert(self.unitLength).value
        _lGew = self.lGew.convert(self.unitLength).value
        _pressure = self.unitForce + '/' + self.unitLength + '^2'
        _Es = self.E.convert(_pressure).value
        _EBI = self.EBI.convert(_pressure).value
        
        _delta = self.unitLength + '/' + self.unitForce
        delta_SK  = self.delta_SK.convert(_delta).value
        delta_Gew = self.delta_Gew.convert(_delta).value
        delta_GM  = self.delta_GM.convert(_delta).value
        
        #
        printR3_2(_Phi_en, _Phi_K, self.n, 
                  _delta_Pp, _delta_Ppp, _delta_S, 
                  _lGew, _Es, _EBI, 
                  delta_SK, delta_Gew, delta_GM, _delta_i_sum,
                  _beta_SK, _beta_Gew, _beta_GM, _beta_i_sum,
                  self.unitLength, self.unitForce)
        #
        
        #
        # ---> R4 Preload changes
        _fZ = (self.RziT.convert('millimeter').value +
               self.Rzt.convert('millimeter').value +
               self.Rzn.convert('millimeter').value)

        _FZ = R4(_fZ, _delta_P, _delta_S)
        #
        _Rzt = self.Rzt.convert(self.unitLength).value
        _Rzn = self.Rzn.convert(self.unitLength).value
        _RziT = self.RziT.convert(self.unitLength).value
        
        printR4(_FZ, _delta_P, _delta_S, _Rzt, _Rzn, _RziT,
                self.unitLength, self.unitForce)
        #
        # ---> R5 Minimum assembly preload
        _FMmin = R5(_FZ, _FAmax , _FKerf, _Phi_en)
        #
        printR5(_FMmin, _FZ, _FAmax , _FKerf, _deltaFVth, _Phi_en,
                self.unitLength, self.unitForce)
        #
        # ---> R6 Maximum assembly preload
        _FMmax = R6(_FMmin, self.alpha_A)
        
        printR6(_FMmax, _FMmin, self.alpha_A,
                self.unitLength, self.unitForce)
        #
        # ---> R7 Assembly stress
        #
        _Ai = math.pi / 4.0 * _DA**2
        _A0 = min(_Ad3, _As, _Ai)     # minimum cross sectional area
        _d0 = min(_d, _d2, _d3, _DA)  # minimum diameter
        _Rp02min = self.Rp02min.convert('newton/millimeter^2').value
        _muG = self.muG.convert('millimeter').value
        _FMTab = self.FMTab.convert('newton').value
        
        _sigma_redM, _FMzul = R7(_P, _A0, _d0, _d2, _Rp02min, _muG, self.nu)
        
        if _FMTab > 0 : _FMzul = _FMTab
        
        printR7(_FMzul, _FMmax, _P, _A0, _d0, _d2, _Rp02min, _muG, self.nu, 
                self.unitLength, self.unitForce)
        #
        # ---> R8 Working Stress
        #
        _dS = (_d2 + _d3) / 2.0
        sigma_zmax, sigma_redB, tau_max = R8(_As, _P, _FMzul, _FAmax, _Phi_en,
                                             _dS, _d2, _Rp02min, _muG)
        
        printR8(sigma_zmax, sigma_redB, tau_max,
                _As, _P, _FMzul, _FAmax, _Phi_en, _dS, _d2, _Rp02min, _muG,
                self.unitLength, self.unitForce)
        #
        # ---> R9 Alternating stress
        #
        # concentric loading
        if self.loadCase == 'concentric':
            
            _sigma_a, _sigma_AS = R9_1(_Phi_en, _As, _FAmax, _FAmin, _d, self.rollTreat)
            #print('sigma_a = {}, sigma_AS = {} N/mm2'.format(sigma_a, sigma_AS))
            printR9_1(_sigma_a, _sigma_AS, _Phi_en, _As, _FAmax, _FAmin, _d, self.rollTreat,
                      self.unitLength, self.unitForce)
        
        # eccentric loading
        else:
            _sigma_SAbo = VD_553(_FAmax, _As, _IBers, _Phi_en, _Ssym, _a, _lK, _lers, 
                                _dS, _dh, _Es, _Ep)
            #
            _sigma_SAbu = VD_553(_FAmin, _As, _IBers, _Phi_en, _Ssym, _a, _lK, _lers,
                                _dS, _dh, _Es, _Ep)
            #
            _sigma_a, _sigma_AS = R9_2(_As, _sigma_SAbo, _sigma_SAbu, _d, self.rollTreat)
            
            printR9_2(_sigma_SAbo , _sigma_SAbu, _sigma_a, _sigma_AS, self.rollTreat,
                      _FAmax, _FAmin, _As, _IBers, _Phi_en, _Ssym, _a, _lK, _lers,
                      _d, _dS, _dh, _Es, _Ep,
                      self.unitLength, self.unitForce)
        #
        # ---> R10 Determining the surface pressure
        _Apmin = (math.pi/4.0)*(_dW**2 - _dh**2)
        Pmax = R10_assembly(_FMzul, _Apmin, _pG)
        if _FZ < _Phi_en * _FAmax :
            Pmax = R10_working(_FVmax, _FSAmax, _Apmin, _pG)
        
        printR10(Pmax, _FMzul, _Apmin, _pG,
                 self.unitLength, self.unitForce)
        
        # ---> R11 Minimun length of engagement
        _s = _dW
        _tauBM = self.tauBM.convert('newton/millimeter^2').value
        _tauBS = self.tauBS.convert('newton/millimeter^2').value
        _RmS = self.RmS.convert('newton/millimeter^2').value
        _Rs, _C1, _C2, _C3, _meff, _ASGM , _FmGM = VD_555(_P, _d, _d2, _s, 
                                                          _D1, _D2, _As,
                                                          _tauBM, _tauBS, _RmS)
        _FmS = R11(_As, _RmS, _FmGM)
        
        _angle = self.flankAngle / 2.0
        printR11(_FmS, _Rs, _C1, _C3, _meff, _ASGM, _FmGM,
                 _P, _d, _d2, _s, _D1, _D2, _As, _tauBM, _tauBS, _RmS,
                 _angle, self.unitLength, self.unitForce)
        
        # ---> R12 Safety Margin agains sleeping
        R12(_FMzul, _FAmax, _FZ, self.alpha_A, _Phi_en)
        
        printR12(_FMzul, _FAmax, _FZ, self.alpha_A, _Phi_en,
                 _deltaFVth, self.unitLength, self.unitForce)
        
        # ---> R13 Determining the tightening torque
        _muK = self.muK.convert('millimeter').value
        #mu_Gmin = 0.1 # according Table A1
        #mu_Kmin = mu_Gmin
        DKm = _dW
        MA = R13(_FMzul, _P, _d2, DKm, _muG, _muK)
        
        printR13(MA, _FMzul, _P, _d2, DKm, _muG, _muK,
                 self.unitLength, self.unitForce)
        
        print("")
        #print('MA = {} N/m'.format(MA/1000))
        print('ok')
    #
#
#
if __name__=='__main__':
    unittest.main()