# Copyright (c) 2009-2022 steelpy
#
# Python stdlib imports

# package imports

#
def cibeam2(SectionType, orad, D, Tw=0, Bft=0, Tft=0, Bfb=0, Tfb=0):
    """
    """
    #global _D, _Tw, _Bft, _Tft, _Bfb, _Tfb
    #global _R, _Area, _Ic
    
    _D = float(D)
    _Tw = float(Tw)
    _Bft = float(Bft)
    _Tft = float(Tft)
    _Bfb = float(Bfb)
    _Tfb = float(Tfb)
    
    _SectionType = secProp.SecFinder(SectionType)
    #_R = orad
    #
    if _SectionType == 'I SECTION':
        
        if _Bfb == 0.0:
            _Bfb = _Bft
        
        if _Tfb == 0.0:
                _Tfb = _Tft
                
        # _PrintOption = '', _Units = '', _NameOut = 'test.out'
        #
        #PrintHeader()
        (_Area, _CoV, _CoH, _Iip, _Zeip, _Zpip, _rip, 
         _Iop, _Zeop, _Zpop, _rop) = secProp.Isection(_D, _Tw, _Bft, _Tft, _Bfb, _Tfb)
        #
        _b = _Bfb
        _t = _Tfb
        _b1 = _Bft
        _t1 = _Tft
        _d = _D
        _b2 = _Tw
        
        # shear area
        _warea = _d * _b2
        
        # extreme inner fibre distance c
        _c = (_d * (((_b1/_b - _b2/_b)*(2-_t1/_d)*(_t1/_d) 
                       + (1-_b2/_b)*(_t/_d)**2 + (_b2/_b))
                    / (2*_Area / (_b*_d))))
        # extreme outer fibre distance c
        _c1 = _d - _c
        
        # centroidal radius
        _R = orad - _c1
    
        # Shift of neutral axis from neutral axis
        _e = (_c * ((_R/_c)-(((_Area/(_b*_d))*(_d/_c)) 
                             / (math.log((_R/_c + _t/_c - 1)/(_R/_c - 1)) 
                                + ((_b2/_b)*math.log((_R/_c + _c1/_c - _t1/_c )
                                                     / (_R/_c + _t/_c - 1)))
                                + ((_b1/_b)*math.log((_R/_c + _c1/_c)
                                                     / (_R/_c + _c1/_c - _t1/_c)))))))
        
        # where
        #_Ic = ((_Area * _c**2) * ((((_d/_c)**2 
        #                            * ((_b1/_b +(1-_b2/_b)*(_t/_d)**3 
        #                                - (_b1/_b - _b2/_b)*(1-_t1/_d)**3)
        #                               / (_b1/_b +(1-_b2/_b)*(_t/_d) 
        #                                  - (_b1/_b - _b2/_b)*(1-_t1/_d))))
        #                           / 3.0) - 1.0))
        _Ic = _Iip
        
        # stress factors Ki
        _ki = ((_Ic / (_Area * _c**2 * (_R/_c - 1.0))) 
               * ((1.0 - _e / _c) / (_e / _c)))
        
        # stress factors Ko
        _ko = ((_Ic / (_Area * _c**2 * (_e/_c ))) 
               * ((_d/_c + _e/_c - 1.0) / (_R/_c  + _d/_c - 1.0))
               * (1.0  / (_d / _c - 1.0)))
        
        # Shape factor (section 8.10)
        _nai = _c - _e    # neautral axis inner fiber
        _nao = _c1 + _e   # neautral axis outer fiber
        
        if _nai <= _nao:
            _D1 = _nai - _Tfb
            _D2 = _nai 
            _t1 = _Tw
            _t2 = _Bfb
            _r = _rip
            print ('inner fiber ', _nai)
        
        else:
            _D1 = _nao - _Tfb
            _D2 = _nao 
            _t1 = _Tw
            _t2 = _Bft
            _r = _rip
            print ('outer fiber ', _nao)
        
        _F = ((1 + (((3*(_D2**2 - _D1**2)*_D1)/(2.0*_D2**3)) * (_t2/_t1 - 1.0)))
              * (4*_D2**2 / (10*_r**2)))
        
        _shearFactor = shear_factor(_c, _c1, _R, _e)
        
    
    elif _SectionType == 'TEE':
        
        #
        #PrintHeader()
        (_Area, _CoV, _CoH, _Iip, _Zeip, _Zpip, _rip, 
         _Iop, _Zeop, _Zpop, _rop) = secProp.Tee(_D, _Tw, _Bft, _Tft)
        #
        _b = _Bft
        _b1 = _Tw
        _t = _Tft
        _d = _D
        
        # shear area
        _warea = _d * _b1
        
        # extreme fibre distances c
        _c = (_d * (((_b1 / _b) + (1.0 - (_b1 / _b))*(_t / _d)**2) /
                    (2.0*((_b1 / _b) + (1.0 - (_b1 / _b))*(_t / _d)))))
        
        _c1 = _c * ((_d / _c) - 1.0)
        
        # centroidal radius
        _R = orad - _c
        
        # Shift of neutral axis from neutral axis
        _e = (_c * ((_R/_c) - (((_d/_c)*(_b1/_b + (1.0 - _b1/_b)*(_t/_d))) / 
                               (((_b1/_b) * math.log((_d/_c + _R/_c -1.0) / 
                                                     ((_d/_c)*(_t/_d) + _R/_c - 1.0))) +
                                math.log(((_d/_c)*(_t/_d) + _R/_c - 1.0) /
                                         (_R/_c - 1.0))))))
        
        # where
        _Ic = ((_Area * _c**2) * (((((_d/_c)**2 *((_b1/_b + (1.0 - _b1/_b)*(_t/_d)**3)
                                                  / (_b1/_b + (1.0 - _b1/_b)*(_t/_d))))) 
                                   / 3.0) - 1.0))
        
        # stress factors Ki
        _ki = ((_Ic / (_Area * _c**2 * (_R/_c - 1.0))) 
               * ((1.0 - _e / _c) / (_e / _c)))
        
        # stress factors Ko
        _ko = ((_Ic / (_Area * _c**2 * (_e/_c))) 
               * ((_d/_c + _e/_c -1.0) / (_R/_c + _d/_c - 1.0)) 
               * (1.0 / (_d/_c - 1.0)))
        
        _F = 1.0
        
    elif _SectionType == 'CHANNEL':
        
        #
        #PrintHeader()
        (_Area, _CoV, _CoH, _Iip, _Zeip, _Zpip, _rip, 
         _Iop, _Zeop, _Zpop, _rop) = secProp.Channel(_D, _Tw, _Bft, _Tft)
        #
        _b = _D
        _b1 = 2*_Tft
        _t = _Tw 
        _d = _Bft
        
        # shear area
        _warea = _d * _b1
        
        # extreme fibre distances c
        _c = (_d * (((_b1 / _b) + (1.0 - (_b1 / _b))*(_t / _d)**2) /
                    (2.0*((_b1 / _b) + (1.0 - (_b1 / _b))*(_t / _d)))))
        
        _c1 = _c * ((_d / _c) - 1.0)
        
        # centroidal radius
        _R = orad - _c1
        
        # Shift of neutral axis from neutral axis
        _e = (_c * ((_R/_c) - (((_d/_c)*(_b1/_b + (1.0 - _b1/_b)*(_t/_d))) / 
                               (((_b1/_b) * math.log((_d/_c + _R/_c -1.0) / 
                                                     ((_d/_c)*(_t/_d) + _R/_c - 1.0))) +
                                math.log(((_d/_c)*(_t/_d) + _R/_c - 1.0) /
                                         (_R/_c - 1.0))))))
        
        #
        _Ic = ((_Area * _c**2) * (((((_d/_c)**2 *((_b1/_b + (1.0 - _b1/_b)*(_t/_d)**3)
                                                  / (_b1/_b + (1.0 - _b1/_b)*(_t/_d))))) 
                                   / 3.0) - 1.0))
        
        # stress factors Ki
        _ki = ((_Ic / (_Area * _c**2 * (_R/_c - 1.0))) 
               * ((1.0 - _e / _c) / (_e / _c)))
        
        # stress factors Ko
        _ko = ((_Ic / (_Area * _c**2 * (_e/_c))) 
               * ((_d/_c + _e/_c -1.0) / (_R/_c + _d/_c - 1.0)) 
               * (1.0 / (_d/c - 1.0)))
        
    elif _SectionType == 'BOX':
        #
        #
        #PrintHeader()
        (_Area, _CoV, _CoH, _Iip, _Zeip, _Zpip, _rip, 
         _Iop, _Zeop, _Zpop, _rop) = secProp.Box(_D, _Tw, _Bft, _Tft)
        
        _b = _Bft
        _b1 = 2*_Tw
        _t = _Tft
        _d = _D
        
        # shear area
        _warea = _d * _b1
        
        # extreme fibre distances c
        _c = _D/2.0
        
        _c1 = _d - _c
        
        # centroidal radius
        _R = orad - _c1
        
        # Shift of neutral axis from neutral axis
        _e = (_c *((_R/_c)- ((2.0*(_t/_c + (1 - _t/_c)*(_b1/_b))) 
                             / ((math.log(((_R/_c)**2 + (_R/_c + 1)*(_t/_c) - 1.0) 
                                          / ((_R/_c)**2 - (_R/_c - 1.0)*(_t/_c) - 1.0))) 
                                + ((_b1/_b)*math.log((_R/_c - _t/_c + 1.0) 
                                                     /(_R/_c + _t/_c - 1.0)))))))
        
        # where
        #_Ic = ((_Area * _c**2)*((((1.0 - (1 - _b1/_b))*(1 - _t/_c)**3) 
        #                        / ((1.0 - (1 - _b1/_b))*(1 - _t/_c))) 
        #                        / 3.0))
        _Ic = _Iip
        
        # stress factors Ki
        _ki = ((_Ic / (_Area * _c**2 * (_R/_c - 1.0))) 
               * ((1.0 - _e / _c) / (_e / _c)))
        
        # stress factors Ko
        _ko = ((_Ic / (_Area * _c**2 * (_R/_c + 1.0))) 
               * ((1.0 + _e / _c) / (_e / _c)))
        
        # Shape factor (section 8.10)
        _nai = _c - _e    # neautral axis inner fiber
        _nao = _c1 + _e   # neautral axis outer fiber
        
        _D1 = _nai - _Tfb
        _D2 = _nai 
        _t1 = 2*_Tw
        _t2 = _Bfb
        _r = _rip
        
        _F = ((1 + (((3*(_D2**2 - _D1**2)*_D1)/(2.0*_D2**3)) * (_t2/_t1 - 1.0)))
              * (4*_D2**2 / (10*_r**2)))
        
        
    elif _SectionType == 'RECTANGULAR BAR':
        #
        #
        #PrintHeader()
        (_Area, _CoV, _CoH, _Iip, _Zeip, _Zpip, _rip, 
         _Iop, _Zeop, _Zpop, _rop) = secProp.BarRactangular(_D, _Tw)
        
        # shear area
        _warea = _Area
        
        # extreme fibre distances c
        _c = _D/2.0
        
        _c1 = _D - _c
        
        # centroidal radius
        _R = orad - _c1
        
        # Shift of neutral axis from neutral axis
        _e = _c*((_R/_c) - (2.0 / (math.log(((_R/_c) + 1.0) /
                                            ((_R/_c) - 1.0)))))
        
        # where
        _Ic = _Iip
        
        # stress factors Ki
        _ki = ((1.0 / (3.0*_e / _c)) * 
               ((1.0 - (_e / _c)) / ((_R / _c) - 1.0)))
        
        # stress factors Ko
        _ko = ((1.0 / (3.0*_e / _c)) * 
               ((1.0 + (_e / _c)) / ((_R / _c) + 1.0)))
        
        # Modulus of rigidity factor (section 8.10)
        _F = 6.0/5.0
        
        # Shear factor (section 8.1 equ 8.1-13)
        _alpha = 3.0 / 2.0 
        _shearFactor = _alpha / _Area
        
    elif _SectionType == 'SOLID CIRCLE':
        #
        #
        #PrintHeader()
        (_Area, _CoV, _CoH, _Iip, _Zeip, _Zpip, _rip, 
         _Iop, _Zeop, _Zpop, _rop) = secProp.SolidCircle(_D)
        
        # shear area
        _warea = _Area
        
        # extreme fibre distances c
        _c = _D/2.0
        
        _c1 = _D - _c
        
        # centroidal radius
        _R = orad - _c1
        
        # Shift of neutral axis from neutral axis
        _e = _c*(((_R/_c) - math.sqrt((_R/_c)**2 - 1.0))/2.0)
        
        # where
        _Ic = _Iip
        
        # stress factors Ki
        _ki = ((1.0 / (4.0*_e / _c)) * 
               ((1.0 - (_e / _c)) / ((_R / _c) - 1.0)))
        
        # stress factors Ko
        _ko = ((1.0 / (4.0*_e / _c)) * 
               ((1.0 + (_e / _c)) / ((_R / _c) + 1.0)))
        
        # Modulus of rigidity factor (section 8.10)
        _F = 10.0/9.0
        
        # Shear factor (section 8.1 equ 8.1-13)
        _alpha = 4.0 / 3.0 
        _shearFactor = _alpha / _Area
        
    elif _SectionType == 'TUBULAR':
        #
        #
        #PrintHeader()
        (_Area, _CoV, _CoH, _Iip, _Zeip, _Zpip, _rip, 
         _Iop, _Zeop, _Zpop, _rop) = secProp.Tubular(_D, _Tw)
        
        # shear area
        _warea = _Area
        
        # extreme fibre distances c
        _c = _D/2.0
        
        _c1 = _c - _Tw
        
        # centroidal radius
        _R = orad - _c1
        
        # Shift of neutral axis from neutral axis
        _e = (_c * ((2.0*_R/_c)- math.sqrt((_R/_c)**2 - 1.0) -
                    math.sqrt((_R/_c)**2 - (_c1/_c)**2))/2.0)
        
        # where
        _Ic = _Iip
        
        # stress factors Ki
        _ki = ((1.0 / (4.0*_e / _c)) * 
               ((1.0 - (_e / _c)) / ((_R / _c) - 1.0)) *
               (1.0 + (_c1/_c)**2))
        
        # stress factors Ko
        _ko = ((1.0 / (4.0*_e / _c)) * 
               ((1.0 + (_e / _c)) / ((_R / _c) + 1.0)) *
               (1.0 + (_c1/_c)**2))
        
        # Shape factor (section 8.10)
        _F = 2.0
        
        # Shear factor (section 8.1 equ 8.1-13)
        _alpha = 2.0
        _shearFactor = _alpha / _Area
    
    else:
        print('Section no supported')
        sys.exit('Section no supported')
    
    #
    _F = max(1.0, _F)
    
    print('')
    print('=================0====================')
    print('R/c =', _R/_c, 'e =',_e, 'e/c =',_e/_c, 'F =', _F)
    #print (_Area, _warea, _Ic, _R, _c, _c1)
    #print('z outer :',_Ic/_c1, 'z inner :',_Ic/_c, 'F :', _F)
    #print('stress factors  ki:  {} ,  ko {}: '.format(_ki, _ko))
    print('')
    
    if _R/_c <= 1.0:
        print('error : R/c < 1')
        exit()
    
    return _Area, _R, _Ic, _F, _e, _c, _c1, _ki, _ko, _shearFactor
    #
#
#
def cibeam(orad, fow, fot, fiw, fit, wlh, wth):
    """
    """
    global smac,area,warea,ceni,ceno,fki,fko,crad
    global _orad, _fow, _fot, _fiw, _fit, _wlh, _wth
    
    _orad = float(orad)   # outside radious of the ring
    _fow  = float(fow)    # flange outside w
    _fot  = float(fot)    # flange outside thickness
    _fiw  = float(fiw)    # flange inside w
    _fit  = float(fit)    # flange inside thickness
    _wlh  = float(wlh)    # web heigh
    _wth  = float(wth)    # web thickness
    
    if _fiw == 0 :
        _fiw = _wth
        
        if _wth == 0:
            _wth = _fow
            _fiw = _fow
    
    dth = _fot + _fit + _wlh
    
    # area
    area = dth * _fow + (_fiw - _wth) * _fit - (_fow - _wth) * (dth - _fot)
    
    # shear area
    warea = dth * _wth
    
    # extreme fibre distances ci
    ceni = (((_fow - _wth) * (2 * dth - _fot) * _fot + (_fiw - _wth) * 
             _fit * _fit + _wth * dth * dth) / float(2*area))
    
    # extreme fibre distances co
    ceno = dth - ceni
    
    # centroidal radius
    crad = _orad - ceno
    
    # Second moment of area @ centroid
    anum = ((dth**3) * _fow + (_fit**3) * (_fiw - _wth) -
            ((dth - _fot)**3) * (_fow - _wth))
    
    den = (dth * _fow + _fit * (_fiw - _wth) - 
           (dth - _fot) * (_fow - _wth))
    
    smac = (anum / (3.0 * den) - ceni**2) * area
    
    # stress factors
    a = (crad + _fit - ceni) / float(crad - ceni)
    a = math.log(a)
    
    b = (crad + ceno - _fot) / float(crad + _fit - ceni)
    b = (_wth / float(_fiw)) * math.log(b)
    
    c = (crad + ceno) / float(crad + ceno - _fot)
    c = (_fow / float(_fiw)) * math.log(c)
    
    d = smac / float(area * ceni)
    # xxx prob bouble presition need it.
    h = crad - (area / float(_fiw * (a + b + c)))
    print ('h (e?) = 1.8182626e-2 ====> ',h)
    # from fortran:
    #h = 1.8182626e-2
    
    # stress factors Ki
    fki = d * (ceni - h) / float((crad - ceni) * h)
    
    # stress factors Ko
    fko = (((dth + h - ceni) / float(crad + dth - ceni)) *(ceni / float(dth - ceni)) * (d / float(h)))
    
    return smac, area, warea, ceni, ceno, fki, fko, crad
    #
    #
#