# Copyright (c) 2009-2023 steelpy
#

# Python stdlib imports

# package imports
from steelpy.utils.units.main import Units
#
#
# *******************************************
#                 Bug History
#
# DNV Code development     SVO     01/03/2011
# Checked against Mathcad R1 A.Aal 06/12/2011
#
# *******************************************
#
import math
import datetime
#
#
#
def PlateStiffener (b, t, hw, tw, bf, tf, L, fyp, fyw = 0, fyf = 0, E = 210000):
    """ """
    # plate (flange) section
    _b = b
    _t = t
    # web section
    _hw = hw
    _tw = tw
    # flange section
    _bf = bf
    _tf = tf
    # Fy plate
    _Fyp = fyp
    # Fy web
    if fyw == 0: _Fyw = _Fyp
    else: _Fyw = fyw
    # fy flange
    if fyf == 0 : 
        _Fyf = _Fyp
    else: 
        _Fyf = fyf
    # section length
    _L = L
    # Elatic modulus
    _E = E
    #
    # Cross Sectional Area
    # area plate (flange)
    _Ap = _b * _t
    # area web
    _Aw = _hw * _tw
    # area flange
    _Af = _bf * _tf
    # cross sectional area of stiffener
    _As = _Aw + _Af
    # total area
    _A = _Ap + _Aw + _Af
    #
    #
    _ef = _tw / 2.0
    #
    #
    # Equivalent Yield Strength over
    # the cross-section
    _Fyeq = (_Ap * _Fyp + _Aw * _Fyw + _Af * _Fyf) / _A
    #
    # Distance from outer surface of attached
    # plating to elastic horizontal neutral axis
    #
    Zp = (((0.50 * _b * _t**2) + _Aw * (_t + 0.50 * _hw) 
           + _Af * (_t + _hw + 0.50 * _tf)) / _A)
    #
    Ztf = (t + _hw + _tf - Zp)
    #
    #print('Z0 ==> ',Zp, Ztf )
    #
    # Moment of Inertia
    #
    _I = ((_b * _t**3 / 12.0) + (_Ap * (Zp - _t / 2.0)**2) 
          + (_hw**3 * _tw / 12.0) + _Aw * (Zp - _t - _hw / 2.0)**2 
          + (_bf * _tf**3 / 12.0) + _Af * (_t + _hw + _tf / 2.0 - Zp)**2)
    #
    #print('Ixx =',_I)
    #
    # Section Modulus
    #
    _Wep = _I / Zp
    #
    _Wes = _I / Ztf
    #
    _W = min(_Wep, _Wes)
    #
    #print ('--->', _Wep, _Wes, _W)
    #
    # Radius of gyration
    #
    _r = math.sqrt(_I / _A)
    #
    #
    # Column Slenderness Ratio
    #
    Lambda = (_L / (math.pi * _r) * math.sqrt(_Fyeq / _E))
    #
    #
    # Torsional Moment of Inertia
    Iz = (_Af * _bf**2 / 12.0) + (_ef**2 * _Af / float(1 + (_Af / _Aw)))
    #
    #print('Iz ===>',Iz)
    #
    print ('Af = ',_Af)
    print ('Aw = ',_Aw)
    print ('Iz = ',Iz)
    print ('ef = ',_ef)
    print ('bf = ',_bf)
    # Plate Slenderness Ratio
    try:
        Beta = _b / _t * math.sqrt(_Fyp / _E)
    except :
        Beta = 0
    #
    return _A, _As, Zp, _I, Iz, _Wes, _Wep, _Fyeq
#
#
#def StressDefinition(Sigma1Sd, Sigma2Sd, L, L1):
#    """ """
#    if Sigma2Sd == 0 :
#        Sigma2Sd = Sigma1Sd
#        sigmaSd = Sigma1Sd
#        sigmaType = 'UNIFORM'
#    elif Sigma1Sd == Sigma2Sd :
#        # sigmaSd = Sigmay1Sd
#        sigmaSd = Sigma1Sd
#        sigmaType = 'UNIFORM'
#    else:
#        # Average SigmaSd
#        sigmaSd = (max( (Sigma2Sd 
#                         + (Sigma1Sd - Sigma2Sd) 
#                         * (L - L1) / L ) , 0.75 * Sigma1Sd ))
#        #
#        sigmaType = 'VARYING'
#    #
#    return Sigma2Sd, sigmaSd, sigmaType 
#
#
def StressSign(SigmaxSd, SigmaySd):
    """ """
    stress = 'TENSION'
    if SigmaxSd and SigmaySd != 0:
        signX = SigmaxSd / abs(SigmaxSd)
        signY = SigmaySd / abs(SigmaySd)

        if signX == 1.0 and signY == 1.0:
            stress = 'COMPRESSION'

    else:
        if SigmaxSd !=  0:
            signX = SigmaxSd / abs(SigmaxSd)
            if signX == 1.0:
                stress = 'COMPRESSION'

        if SigmaySd !=  0:
            signY = SigmaySd / abs(SigmaySd)
            if signY == 1.0:
                stress = 'COMPRESSION'
    #
    return stress
#
#
class SectionProperty:

    def BeamSection(self):
        """ """
        _s = (self.s1 + self.s2)/2
        # Area
        self.Apf = _s * self.t
        # print ('Area :', self.Apf)
        #
        #
        # Cross sectional area of stiffener
        _Aw = self.hw*self.tw
        _Af = self.b * self.tf
        _As = _Aw + _Af
        # print ('Cross sectional area of stiffener :',_As)
        #
        # Total Area
        self.Asf = self.Apf + _Aw + _Af
        # print ('Total Area :', self.Asf )
        #
        #
        # Neutral Axis Location
        self.Ztf = ((self.Apf*((0.5*self.t + self.hw + self.tf) + 
                               (_Aw * (0.5*self.hw + self.tf) + 
                                0.5*self.tf*_Af))) / self.Asf)
        #
        # print ('Ztf:', self.Ztf )
        #
        Zpf = 0.5*self.t + self.hw + self.tf - self.Ztf
        # print ('Zpf:', Zpf )
        #
        # Moment of Inertia
        self.Ipf = (((self.Apf * self.t**2)/12) + 
                    self.Apf * (0.5*self.t + self.hw + self.tf - self.Ztf)**2)
        # print ('Ipf:', self.Ipf )
        #
        self.Iwf = (((self.tw * self.hw**3)/12) + 
                    _Aw * (0.5*self.hw + self.tf - self.Ztf)**2)
        # print ('Iwf :', self.Iwf )
        #
        self.Iff = (((self.b * self.tf**3)/12) + 
                    _Af * (0.5*self.tf - self.Ztf)**2)
        # print ('Iff :', self.Iff )
        #
        self.Is = self.Ipf + self.Iwf + self.Iff 
        #
        # print ('Moment of Inertia:', self.Is )
    #
    def PlateStiffener (self):
        """ """
        # Cross Sectional Area
        #
        _Ap = _b * _t
        #
        _Ape = _be * _t
        #
        _Aw = _hw * _tw
        #
        _Af = _bf * _tf
        #
        _A = _Ap + _Aw + _Af
        #
        _Ae = _Ape + _Aw + _Af
        #
        # Equivalent Yield Strength over
        # the cross-section
        _Fyeq = (_Ap * _Fyp + _Aw * _Fyw + _Af * _Fyf) / _A
        #
        # Distance from outer surface of attached
        # plating to elastic horizontal neutral axis
        #
        _Z0 = (((0.50 * _b * _t**2) + _Aw * (_t + 0.50 * _hw) 
                + _Af * (_t + _hw + 0.50 * _tf)) / _A)
        #
        #
        Zp = (((0.50 * _be * _t**2) + _Aw * (_t + 0.50 * _hw) 
               + _Af * (_t + _hw + 0.50 * _tf)) / _Ae)
        #
        # Moment of Inertia
        #
        _I = ((_b * _t**3 / 12.0) + (_Ap * (_Z0 - _t / 2.0)**2) 
              + (_hw**3 * _tw / 12.0) + _Aw * (_Z0 - _t - _hw / 2.0)**2 
              + (_bf * _tf**3 / 12.0) + _Af * (_t + _hw + _tf / 2.0 - Z0)**2)
        #
        #
        ie = ((_be * _t**3 / 12.0) + (_Ape * (Zp - _t / 2.0)**2) 
              + (_hw**3 * _tw / 12.0) + _Aw * (Zp - _t - _hw / 2.0)**2 
              + (_bf * _tf**3 / 12.0) + _Af * (_t + _hw + _tf / 2.0 - Zp)**2)
        #
        # Radius of gyration
        #
        _r = math.sqrt(_I / _A)
        #
        _re = math.sqrt(ie / _A)
        #
        # Column Slenderness Ratio
        #
        Lambda = (_L / (math.pi * _r) * math.sqrt(_Fyeq / _E))
        #
        Lambdae = (_L / (math.pi * _re) * math.sqrt(_Fyeq / _E))
        #
        # Plate Slenderness Ratio
        #
        Beta = _b / _t * math.sqrt(_Fyp / _E)
#
#
#
class DNVRPC201:

    def __init__(self):
        """ """
        #self.ur = 0.0
        self.ur_combined = 0.0
        self.ur_combined_Plate = 0.0
        self.ur_combined_Girder = 0.0
        self.ur_lateral = 0.0
        self.ur_longcomp = 0.0
        self.ur_transComp = 0.0
        self.ur_biaxial = 0.0
        self.msg = ''
        self._tau_crl = 0.0

    #
    def fT(self, LT, Beta, Iz):
        """ Torsional Buckling Strengt """
        # area web
        _Aw = self.hw * self.tw
        # area flange
        _Af = self.bf * self.tf
        #
        # For L- and T-stiffeners fET may be calculated as:
        if self.stiffener_type == 'L' or self.stiffener_type == 'T':
            # (7.33)
            #Iz = (((1.0/12.0) * _Af * _b**2) 
            #       + (_ef**2 * (_Af / (1.0 + _Af / _Aw))))
            # (7.32)
            fET = ((Beta * ((_Aw + (self.tf / self.tw)**2 * _Af) 
                            / (_Aw + 3 * _Af)) 
                    * self.G * (self.tw / self.hw)**2) 
                   + ((math.pi**2 * self.E * Iz) 
                      / ((_Aw / 3.0 + _Af) * LT**2)))
        # For flatbar stiffeners fET may be calculated as:
        elif self.stiffener_type == 'F':
            # (7.34)
            fET = ((Beta + 2 * (self.hw / LT)**2) 
                   * self.G * (self.tw / self.hw)**2)
        # Generally fET may be calculated as:
        else:
            _G = self.E / (2.0 * ( 1.0 + self.Poisson)) 
            # (7.31)
            fET = ((Beta * (_G * _It) / (Ipo)) 
                   + (math.pi**2 * (self.E * _hs**2 * Iz) 
                      / (_Ipo * LT**2)))
        #
        # (7.30)
        lambdaT = math.sqrt(self.fy / fET)
        #
        # (7.29)
        MuT = 0.35 * (lambdaT - 0.60)
        #
        # The torsional buckling strength may be calculated as:
        # (7.28)
        if lambdaT > 0.60:
            fT = (self.fy * (1 + MuT + lambdaT**2 
                             - math.sqrt((1 + MuT
                                          + lambdaT**2)**2 
                                         - 4 * lambdaT**2))
                  / (2 * lambdaT**2))
            #
            #print('==>',1 + MuT + lambdaT**2 )
            #print(math.sqrt((1 + MuT
            #                 + lambdaT**2)**2 
            #                - 4 * lambdaT**2) )
            #print(2 * lambdaT**2)
        else: # (7.27)
            fT = self.fy
        #
        return fET, lambdaT, MuT, fT
    #
    def fk(self, sideP, fT, fE, ie, Zt, Zp):
        """ """
        # for check at stiffener side
        if sideP == 'P':
            _fr = self.fy
        elif sideP == 'S':
            _fr = fT
        else: # for check at stiffener side if LambdaT <= 0.6,
            self.msg = self.msg + '\n' + '**error**'
            exit
        #
        # (7.23)
        Lambda = math.sqrt(_fr / fE)
        #
        # for check at plate side
        # (7.25)
        if sideP == 'P':
            Mu = ((0.34 + 0.08 * (Zp / ie)) 
                  * (Lambda - 0.20))
        elif sideP == 'S':
            # for check at stiffener side
            # (7.26)	    
            Mu = ((0.34 + 0.08 * (Zt / ie)) 
                  * (Lambda - 0.20))
        else:
            self.msg = self.msg + '\n' + '**error**'
            exit
        #
        # The characteristic buckling strength for stiffeners 
        # may be found from:
        #
        # (7.22)
        # Plate
        if Lambda > 0.20:
            _fk = (_fr * (1 + Mu + Lambda**2 
                          - math.sqrt((1 + Mu 
                                       + Lambda**2)**2 
                                      - 4 * Lambda**2))
                   / (2 * Lambda**2))
        else: # (7.21)
            _fk = _fr
        #
        return _fr, Lambda, Mu, _fk
    #
    def TauCR(self, lambda_cE):
        """ eq (8.6) """
        if lambda_cE > 1.0:
            return (0.60*self.fy / lambda_cE**2)
        else:
            return (0.60*self.fy)
    # 
    def fTG(self, lambdaTG, Mu):
        """ eq (8.27) """
        if lambdaTG > 0.60 :
            fTG = (self.fyG * ((1.0 + Mu + lambdaTG**2 
                                - math.sqrt((1.0 + Mu + lambdaTG**2)**2 
                                            - 4.0 * lambdaTG**2)) 
                               / (2.0 * lambdaTG**2)))
        else:
            # lambdaTG <= 0.60
            fTG = self.fyG
        #
        return fTG
    #
    def frG(self, ftG, side):
        """ """
        if side == 'P':
            return self.fyG
        else:
            return ftG
    #
    def CombinedUnityCheck(self, Stiffener, NSd, _L, qSdp, qSds, NksRd, NkpRd, MpRd, MstRd, Ms1Rd, Ms2Rd, NE, NRd,  _z = 0.0 , _u = 0):
        """ Combined Unity Check 
            equations (7.50) to (7.57) or (7.59) to (7.64)
        """
        # In the equations (7.50) to (7.57) or (7.59) to (7.62) u = 0 for
        # girders.
        #
        # Girders may be checked for shear forces similar to stiffeners
        # see sec. 7.8.
        #
        # For simplification, assuming:
        self.msg = self.msg + '\n' + 'z =' + str(_z )
        # where z is the distance from the neutral axis of the effective
        # section to the working point of axial force.
        #
        self.msg = self.msg + '\n' + 'u =' + str( _u)
        #
        # M1Sd for continuous stiffeners with equal spans
        #      and equal lateral pressure in all spans
        #      = absolute value of the actual largest support
        #      moment for continuous stiffeners with unequal spans
        #      and/or unequal lateral pressure in adjacent spans
        #
        # Lateral pressure on plate side:
        #print('')
        self.msg = self.msg + '\n' + 'Lateral pressure on plate side'
        self.msg = self.msg + '\n' + '----------'
        #
        # Maximum end support moment
        M1Sd = abs(qSdp * _L**2 / 12.0)
        self.msg = self.msg + '\n' + 'M1Sd =' +str(M1Sd ) + ' ' + str(_L)
        #
        # Maximum mid-beam moment
        M2Sd = M1Sd / 2.0
        self.msg = self.msg + '\n' + 'M2Sd =' + str (M2Sd )
        #
        # (7.50)
        _UR_p750 = ((NSd / NksRd) 
                    + ((M1Sd - NSd * _z) 
                       / (Ms1Rd * (1.0 - NSd / NE))) 
                    + _u)
        #
        self.msg = self.msg + '\n' + 'URp eq7.50' + str( _UR_p750)
        #
        # (7.51)
        _UR_p751 = ((NSd / NkpRd) - (2 * NSd / NRd)
                    + ((M1Sd - NSd * _z) 
                       / (MpRd * (1.0 - NSd / NE))) 
                    + _u)
        #
        self.msg = self.msg + '\n' + 'URp eq7.51' + str(_UR_p751)
        #
        # (7.52)
        _UR_p752 = ((NSd / NksRd) - (2 * NSd / NRd)
                    + ((M2Sd + NSd * _z) 
                       / (MstRd * (1.0 - NSd / NE))) 
                    + _u)
        #
        self.msg = self.msg + '\n' + 'URp eq7.52' + str(_UR_p752)
        #
        # (7.53)
        _UR_p753 = ((NSd / NkpRd) 
                    + ((M2Sd - NSd * _z) 
                       / (MpRd * (1.0 - NSd / NE))) 
                    + _u)
        #
        self.msg = self.msg + '\n' + 'URp eq7.53' + str(_UR_p753)
        #
        _URp = max(_UR_p750, _UR_p751, _UR_p752, _UR_p753)
        self.msg = self.msg + '\n' + 'URp =' + str(_URp)
        #
        #
        # Lateral pressure on stiffener/Girder side:
        #print('')
        self.msg = self.msg + '\n' + 'Lateral pressure on stiffener/Girder side:'
        #
        M1Sd = abs(qSds * _L**2 / 12.0)
        #
        #print('----------')
        self.msg = self.msg + '\n' + 'M1Sd =' + str( M1Sd ) + ' ' + str( _L)
        #
        M2Sd = M1Sd / 2.0
        #
        self.msg = self.msg + '\n' + 'M2Sd =' + str( M2Sd)
        #
        # Note: The above moments calculated are for the case of equal
        #       span with equal lateral pressure. If the stiffener's spans
        #       or lateral pressure are unequal, the above moments should
        #       be re-calculated to suit.
        # (7.54)
        _UR_s754 = ((NSd / NksRd) - (2 * NSd / NRd)
                    + ((M1Sd + NSd * _z) / (MstRd * (1.0 - NSd / NE))) 
                    + _u)
        #
        self.msg = self.msg + '\n' + 'URp eq7.54' + str( _UR_s754)+ ' ' + str(M1Sd + NSd * _z) + ' ' + str((MstRd * (1.0 - NSd / NE)))
        #
        # (7.55)
        _UR_s755 = ((NSd / NkpRd) 
                    + ((M1Sd + NSd * _z) / (MpRd * (1.0 - NSd / NE))) 
                    + _u)
        #
        self.msg = self.msg + '\n' + 'URp eq7.55' + str( _UR_s755)
        #
        # (7.56)
        _UR_s756 = ((NSd / NksRd) 
                    + ((M2Sd - NSd * _z) / (Ms2Rd * (1.0 - NSd / NE))) 
                    + _u)
        #
        self.msg = self.msg + '\n' + 'URp eq7.56' + str( _UR_s756)
        #
        # (7.57)
        _UR_s757 = ((NSd / NkpRd) - (2 * NSd / NRd)
                    + ((M2Sd - NSd * _z) / (MpRd * (1.0 - NSd / NE))) 
                    + _u)
        #
        self.msg = self.msg + '\n' + 'URp eq7.57' +str(_UR_s757)
        #
        _URs = max(_UR_s754, _UR_s755, _UR_s756, _UR_s757)
        self.msg = self.msg + '\n' + 'URs =' + str( _URs)
        #
        # 7.7.2 Simple supported stiffener (sniped stiffener)
        # ---------------------------------------------------
        #
        #print(' ')
        self.msg = self.msg + '\n' + 'Simple supported stiffener (sniped stiffener)'
        #
        # Simple supported stiffener (sniped stiffener):
        #
        # Lateral pressure on plate side:
        # (7.59)
        _UR_p759 = ((NSd / NksRd) - (2 * NSd / NRd) 
                    + ((abs(qSdp * _L**2 / 8.0) + NSd * _z) 
                       / (MstRd * (1.0 - NSd / NE))) + _u)
        #
        #print('')
        self.msg = self.msg + '\n' + 'Lateral pressure on plate side :'
        #print('----------')
        self.msg = self.msg + '\n' + 'URp eq7.59 =' + str(_UR_p759)
        #
        # (7.60)
        _UR_p760 = ((NSd / NkpRd) 
                    + ((abs(qSdp * _L**2 / 8.0) + NSd * _z) 
                       / (MpRd * (1.0 - NSd / NE))) + _u)
        #
        self.msg = self.msg + '\n' + 'URp eq7.60 =' + str( _UR_p760)
        #
        _URps = max(_UR_p759, _UR_p760)
        self.msg = self.msg + '\n' + 'URps =' + str( _URps)
        #
        # Lateral pressure on stiffener side:
        #
        # (_qSd * _L**2 / 8.0) <= (NSd * _Zc)
        #
        # Section at middle stiffener side
        # (7.61)
        _UR_s761 = ((NSd / NksRd) 
                    + ((abs(qSds * _L**2 / 8.0) - NSd * _z) 
                       / (Ms2Rd * (1.0 - NSd / NE))) + _u)
        #
        # Section at middle plate side
        # (7.62)
        _UR_s762 = ((NSd / NkpRd) - (2 * NSd / NRd) 
                    + ((abs(qSds * _L**2 / 8.0) - NSd * _z) 
                       / (MpRd * (1.0 - NSd / NE))) + _u)
        #
        #
        #print('')
        self.msg = self.msg + '\n' + 'Lateral pressure on stiffener side:'
        self.msg = self.msg + '\n' + '----------'
        self.msg = self.msg + '\n' + 'URs eq7.61 =' + str(_UR_s761)
        self.msg = self.msg + '\n' + 'URs eq7.62 =' + str( _UR_s762)
        #
        # Section at middle stiffener side
        # (7.63)
        _UR_s763 = ((NSd / NksRd) - (2 * NSd / NRd) 
                    + (( NSd * _z - abs(qSds * _L**2 / 8.0)) 
                       / (MstRd * (1.0 - NSd / NE))) + _u)
        #
        # Section at middle plate side
        # (7.64)
        _UR_s764 = ((NSd / NkpRd) 
                    + ((NSd * _z - abs(qSds * _L**2 / 8.0)) 
                       / (MpRd * (1.0 - NSd / NE))) + _u)
        #
        self.msg = self.msg + '\n' + '----------'
        self.msg = self.msg + '\n' + 'URs eq7.63 =' + str( _UR_s763)
        self.msg = self.msg + '\n' + 'URs eq7.64 =' + str( _UR_s764)
        #
        #
        if (qSds * _L**2 / 8.0) < (NSd * _z):
            _URss1 = _UR_s763
            _URss2 = _UR_s764
        else:
            _URss1 = _UR_s761
            _URss2 = _UR_s762
        #
        self.msg = self.msg + '\n' + '----------'
        self.msg = self.msg + '\n' + 'URss1 =' + str( _URss1)
        self.msg = self.msg + '\n' + 'URss2 =' + str( _URss2)
        #
        _URss = max(_URss1, _URss2)
        #
        self.msg = self.msg + '\n' + 'URss =' + str( _URss)
        #
        if Stiffener == 'C':
            _URstiffP = _URp
            _URstiffS = _URs
        elif Stiffener == 'S':
            _URstiffP = _URps
            _URstiffS = _URss
        else:
            self.msg = self.msg + '\n' + 'Check Stiffener'
            exit
        #
        #print('')
        self.msg = self.msg + '\n' + '----------'
        self.msg = self.msg + '\n' + 'URstiffP =' + str( _URstiffP)
        self.msg = self.msg + '\n' + 'URstiffS =' + str( _URstiffS)
        #
        self.ur_combined = max(_URstiffP, _URstiffS)
        #
        #print('')
        self.msg = self.msg + '\n' + '----------'
        self.msg = self.msg + '\n' + 'UR total combined =' + str( self.ur_combined)
    #
    def PlateBucklingCheck(self):
        """ Table 3-1 """
        # Reference table for buckling checks of plates
        self.msg = self.msg + '\n' + 'ok'
    #
    # Section 5
    def LateralLoadedPlates(self):
        """ Section 5 """
        #print(' ')
        self.msg = self.msg + '\n' + 'Lateral Loaded Plates'
        # For plates subjected to lateral pressure, either alone or in
        # combination with in-plane stresses, the stresses may be
        # checked by the following formulas:
        #
        # (5.4)
        SigmajSd = (math.sqrt(self.sigmax_Sd**2 + self.sigmay_Sd**2 
                              - self.sigmax_Sd * self.sigmay_Sd
                              + 3 * self.tau_Sd**2))
        #
        # (5.3)
        Psix = ((1 - (SigmajSd / self.fy)**2) 
                / math.sqrt(1 - (3.0/4.0) * (self.sigmay_Sd / self.fy)**2 
                            - 3 * (self.tau_Sd / self.fy)**2))
        #
        # (5.2)
        Psiy = ((1 - (SigmajSd / self.fy)**2) 
                / math.sqrt(1 - (3.0/4.0) * (self.sigmax_Sd / self.fy)**2 
                            - 3 * (self.tau_Sd / self.fy)**2))
        #
        # (5.1 ) - PSd = design lateral pressure
        self.PSR = (4.0 * (self.fy / self.GammaM) * (self.t/ self.S)**2 
                    * (Psiy + (self.S / self.L)**2 * Psix))
        #
        self.ur_laterl = self.PSd/self.PSR
        self.msg = self.msg + '\n' + 'PSd ='  + str(self.PSd)  + 'PSa =' + str(self.PSR)
        self.msg = self.msg + '\n' + 'UR =' + str(self.PSd/self.PSR)
        #
        #
        # This formula for the design of a plate subjected to lateral
        # pressure is based on yield-line theory, and accounts for the
        # reduction of the moment resistance along the yield-line due
        # to applied in-plane stresses. The reduced resistance is
        # calculated based on von Mises equivalent stress. It is
        # emphasised that the formulation is based on a yield pattern
        # assuming yield lines along all four edges, and will give
        # uncertain results for cases where yield-lines can not be
        # developed along all edges. Furthermore, since the formula
        # does not take account of second-order effects, plates
        # subjected to compressive stresses shall also fulfil the
        # requirements of Chapter 6 and 7 whichever is relevant.
        #
    #
    # Section 6
    #def BucklingOfUnstiffenedPlates(self):
    #
    # 6.1 General
    # This section presents recommendations for calculating the
    # buckling resistance of unstiffened plates.
    # For plates that are part of a stiffened panel, the plate are
    # checked as part of the buckling checks according to Chapter 7.
    # Then additional check of the plate according to this section
    # is not required.
    # Buckling checks of unstiffened plates in compression shall
    # be made according to the effective width method. The
    # reduction in plate resistance for in-plane compressive forces
    # is expressed by a reduced (effective) width of the plate which
    # is multiplied by the design yield strength to obtain the design
    # resistance, see Figure 6-1 of DNV.
    #
    def BucklingOfUnstiffenedPlatesLongitudinalUniformCompression(self):
        """ 6.2 Buckling of unstiffened plates under
            longitudinally uniform compression"""
        #print(' ')
        self.msg = self.msg + '\n' + '6.2 Buckling of unstiffened plates under'
        self.msg = self.msg + '\n' + '    longitudinally uniform compression'
        #
        # The design buckling resistance of an unstiffened plate under
        # longitudinal compression force may be calculated as:
        #
        # (6.3)
        lambda_p = 0.525 * (self.S / self.t) * math.sqrt(self.fy / self.E)
        #
        # (6.2)
        if lambda_p > 0.673 :
            _Cx = ((lambda_p - 0.22) / lambda_p**2)
        else: 
            _Cx = 1.0
        #
        # (6.1)
        self.sigmax_Rd = (_Cx * (self.fy / self.GammaM))
        #
        # in which
        # s = plate width
        # t = plate thickness
        # fcr = critical plate buckling strength
        # The resistance of the plate is satisfactory when:
        #
        self.ur_longcomp = self.sigmax_Sd/self.sigmax_Rd
        self.msg = self.msg + '\n' + 'SigmaxSd =' + str( self.sigmax_Sd)
        self.msg = self.msg + '\n' + 'SigmaxRd =' + str(self.sigmax_Rd)
        self.msg = self.msg + '\n' + 'UR =' + str( self.sigmax_Sd/self.sigmax_Rd)
    #
    def BucklingOfUnstiffenedPlatesTransverseCompression(self):
        """ 6.3 Buckling of unstiffened plates with
            transverse compression"""
        #print(' ')
        self.msg = self.msg + '\n' + '6.3 Buckling of unstiffened plates with'
        self.msg = self.msg + '\n' + '    transverse compression'
        #
        # The design buckling resistance of a plate under transverse
        # compression force may be found from:
        #
        # (6.8)
        lambda_c = 1.10 * (self.S / self.t) * math.sqrt(self.fy / self.E)
        self.msg = self.msg + '\n' + 'Lambdac =' + str( lambda_c)
        #
        # (6.9)
        Mu = 0.21 * (lambda_c - 0.20)
        self.msg = self.msg + '\n' + 'Mu ='  + str(Mu)
        #
        # (6.7)
        if lambda_c >= 2.0:
            _k = (1.0/(2.0 * lambda_c**2)) + 0.070
        elif lambda_c <= 0.20:
            _k = 1.0
        else: # 0.20 > lambda_c < 2.0
            _k = ((1.0/(2.0 * lambda_c**2)) 
                  * (1 + Mu + lambda_c**2 
                     - math.sqrt((1 + Mu + lambda_c**2 )**2 
                                 - 4 * lambda_c**2)))
        #
        self.msg = self.msg + '\n' + 'k = '  +str( _k)
        #
        # t = plate thickness
        # l = plate length
        # s = plate width
        # The reduction factor due to lateral load kp
        # (6.11)
        _halpha = max((0.050 * (self.S/self.t) - 0.75) , 0)
        #
        # (6.10)
        if self.PSd <= (2 * (self.t/self.S)**2 * self.fy):
            _kp = 1.0
        else:
            _kp = max(1.0 - _halpha * ((self.PSd/self.fy) - 2 * (self.t/self.S)**2),0.0)
        #
        # print ('kp =',_kp)
        #
        # (6.6)
        self.sigmay_R = (((1.3 * self.t / self.L) 
                          * math.sqrt(self.E/self.fy) 
                          + _k * (1.0 - (1.3 * self.t / self.L) 
                                 * math.sqrt(self.E/self.fy))) 
                         * self.fy * _kp)
        #
        self.msg = self.msg + '\n' + 'SigmayR = ' + str( self.sigmay_R)
        #
        # (6.5)
        self.sigmay_Rd = self.sigmay_R / self.GammaM
        self.msg = self.msg + '\n' + 'SigmayRd= ' + str( self.sigmay_Rd)
        #
        # The resistance of the plate is satisfactory when:
        # (6.12)
        self.ur_transComp = self.sigmay_Sd/ self.sigmay_Rd
        self.msg = self.msg + '\n' + 'UR = '  + str(self.sigmay_Sd/ self.sigmay_Rd)
    #
    def BucklingOfUnstiffenedPlatesShear(self):
        """ 6.4 Buckling of unstiffened plate with shear """
        # print (' ')
        # print ('6.4 Buckling of unstiffened plate with shear')
        #
        # Shear buckling of a plate can be checked by
        #
        # (6.17)
        if self.L < self.S:
            _kl = 5.34 * (self.S / self.L)**2 + 4.0
        else: # L >= S
            _kl = 5.34 + 4 * (self.S / self.L)**2
        #
        self.msg = self.msg + '\n' + 'kl ='   + str( _kl)
        #
        # (6.16)
        lambda_w = 0.795 * (self.S / self.t) * math.sqrt(self.fy / (self.E * _kl))
        #
        self.msg = self.msg + '\n' + 'Lambdaw ='  + str( lambda_w)
        #
        # (6.15)
        if lambda_w > 1.20:
            C_tau = 0.90 / lambda_w 

        elif lambda_w <= 0.80:
            C_tau = 1.0

        else: # 0.80 < Lambdaw <= 1.20
            C_tau = 1.0 - 0.625 * ( lambda_w - 0.80)
        #
        self.msg = self.msg + '\n' + 'Ctau ='  + str( C_tau)
        #
        # (6.14)
        self.tau_Rd = (C_tau/self.GammaM) * (self.fy / math.sqrt(3.0))
        #
        self.msg = self.msg + '\n' + 'Tau =' +  str( self.tau_Rd)
        #
        # (6.13)
        self.msg = self.msg + '\n' + 'URs = '   + str( self.tau_Sd / self.tau_Rd)
    #
    def BucklingOfUnstiffenedPlatesBiaxiallyLoadedShear(self):
        """ 6.5 Buckling of unstiffened biaxially loaded
            plates with shear"""
        # print (' ')
        # print ('6.5 Buckling of unstiffened biaxially loaded')
        # print ('    plates with shear')
        #
        #
        # A plate subjected to biaxially loading with shear should fulfil
        # the following requirement:
        #
        # where if both SigmaxSd and SigmaySd is compression (positive) then
        if self.stress == 'COMPRESSION':
            if (self.S / self.t) > 1.20:
                _ci = 0
            else: 
                _ci = (1.0 - (self.S / (120 * self.t)))
        # If either of SigmaxSd and SigmaySd or both is in tension (negative), then
        else: 
            _ci = 1.0
        #
        # SigmaxRd is given by eq. (6.1) and SigmayRd is given by eq. (6.5).
        # In case of tension, apply fy/GammaM.
        # TauRd is given by eq. (6.19) in cases where SigmaySd is positive
        # (compression) and by eq. (6.14) in cases where SigmaySd is zero
        # or negative (in tension).
        #
        # Shear buckling of a plate can be checked by
        #
        # (6.17)
        if self.L < self.S:
            _kl = 5.34 * (self.S / self.L)**2 + 4.0
        else: # L >= S
            _kl = 5.34 + 4 * (self.S / self.L)**2
        #
        print('kl =', _kl)
        #
        # (6.16)
        lambda_w = 0.795 * (self.S / self.t) * math.sqrt(self.fy / (self.E * _kl))
        #
        self.msg = self.msg + '\n' + 'Lambdaw =' + str( lambda_w)
        #
        # (6.20)
        if lambda_w > 1.25:
            C_taue = 1.0 / lambda_w**2
        elif lambda_w <= 0.80:
            C_taue = 1.0
        else: # 0.8 > lambda_w < 1.25
            C_taue = 1.0 - 0.80*( lambda_w - 0.80)
        #
        # (6.19)
        self.tau_Rd = (C_taue / self.GammaM) * (self.fy / math.sqrt(3))
        #
        self.msg = self.msg + '\n' + 'Tau =' + str( self.tau_Rd)
        #
        # (6.18)
        self.URd = ((self.sigmax_Sd / self.sigmax_Rd)**2 
                    + (self.sigmay_Sd / self.sigmay_Rd)**2 
                    - _ci * (self.sigmax_Sd / self.sigmax_Rd)*(self.sigmay_Sd / self.sigmay_Rd)
                    + (self.tau_Sd / self.tau_Rd)**2)
        #
        self.ur_biaxial = self.URd
        self.msg = self.msg + '\n' + 'UR = ' + str( self.URd)
    #
    def BucklingOfUnstiffenedPlatesVaryingLongStress(self):
        """6.6 Buckling of unstiffened plates with varying
           longitudinal stress. Internal compression
           elements"""
        # print (' ')
        # print ('6.6 Buckling of unstiffened plates with varying')
        # print ('    longitudinal stress. Internal compression')
        # print ('    elements')
        #
        #
        # The buckling resistance of an unstiffened plate with
        # varying longitudinal stress may be found from:
        #
        # s = plate width
        # Psi = Sigma2/Sigma1 Stress ratio. s1 is largest stress with
        # compressive stress taken as positive.
        # t = plate thickness
        # fcr = critical plate buckling strength
        #
        #Psi = self.Sigma2 / self.Sigma1
        Psi = self.sigmax_2Sd / self.sigmax_1Sd
        #
        epsilon = math.sqrt(235.0 / self.fy)
        #
        if Psi <= 0:
            if Psi >= -2.0:
                k_sigma = 2.98 * (1.0 - Psi)**2
            else: # Psi >= -1.0:
                k_sigma = 7.81 - 6.29 * Psi + 9.78 * Psi**2
        else: # Psi > 0:
            if Psi <= 1.0:
                k_sigma = 8.20 / (1.05 - Psi)
            else:
                # print ('Psi > 1.0 ==> ksigma not available')
                pass
        #
        # where Lambdap is the plate slenderness given by:
        # (6.24)
        lambda_p = (self.S / self.t) * 1.0 / (28.4 * epsilon * math.sqrt(k_sigma))
        # 
        # (6.23)
        if lambda_p > 0.73 :
            _Cx = (lambda_p - 0.055 * (3 + Psi)) / lambda_p**2
        else: # (6.22)
            _Cx = 1.0
        #
        # (6.21)
        self.sigmax_Rd = _Cx * self.fy / self.GammaM
        self.msg = self.msg + '\n' + 'SigmaxRd ='  + str( self.sigmax_Rd)
        #
        # The resistance of the plate is satisfactory when:
        self.URd = self.sigmax_Sd/self.sigmax_Rd
        # print('URd =', self.URd)
        #
        # In order to perform cross sectional checks for members
        # subjected to plate buckling the local buckling effects 
        # can be accounted for by checking the resistance by using
        # the effective width according to Table 6-1.
    #
    def BucklingOfUnstiffenedPlatesOutstandCompElemnts(self):
        """ 6.7 Buckling of outstand compression elements """
        self.msg = self.msg + '\n' + ' '
        self.msg = self.msg + '\n' + '6.7 Buckling of outstand compression elements'
        #
        # The buckling resistance of an outstand compression element
        # with varying or constant longitudinal stress may be found
        # from:
        #
        # in which
        # s = plate width
        # t = plate thickness
        # fcr = critical plate buckling strength
        #
        epsilon = math.sqrt(235.0/ self.fy)
        #
        self.msg = self.msg + '\n' + 'Epsilon =' + str( epsilon)
        #
        # For outstand with largest compression stress at free edge:
        if self.outstand_edge == 'FREE':
            if Psi >= -1.0 and Psi <= 1.0:
                k_sigma = 0.57 - 0.21 * Psi + 0.07 * Psi**2
            else:
                self.msg = self.msg + '\n' + 'ERROR ==> Psi < -1 or Psi > 1'
        #
        # For outstand with largest compression stress at supported
        # edge:
        else:
            if Psi >= 0 and Psi <= 1.0:
                k_sigma = 0.578 / (0.34 + Psi)
            elif Psi >= -1 and Psi <= 0:
                k_sigma = 1.70 - 5.0 * Psi + 17.10 * Psi**2
            else:
                self.msg = self.msg + '\n' + 'ERROR ==> Psi < -1 or Psi > 1'
        #
        self.msg = self.msg + '\n' + 'ksigma ='  + str( k_sigma)
        #
        # where Lambdap is the plate slenderness given by:
        # (6.29)
        lambda_p = ((self.S / self.t) 
                    * (1.0 / (28.4 * epsilon * math.sqrt(k_sigma))))
        #
        # (6.28)
        if lambda_p > 0.749:
            _Cx = (lambda_p - 0.188) / lambda_p**2
        else: # when lambda_p <= 0.749
            _Cx = 1.0
        #
        self.msg = self.msg + '\n' + 'Cx =' + str(_Cx)
        #
        # (6.26)
        self.sigmax_Rd = _Cx * self.fy / self.GammaM
        #
        self.msg = self.msg + '\n' + 'SigmaxRd ='  + str(self.sigmax_Rd)
        #
        # Cross sectional checks of members subjected to plate
        # buckling local buckling effects can be accounted for by
        # checking the resistance by using the effective width
        # according to Table 6-2 and Table 6-3 for outstand elements
        # with largest compression stress at free edge or supported
        # edge respectively.
        #
    #
    def BucklingOfUnstiffenedPlates(self):
        """6.8 Buckling of unstiffened plates with varying 
           transverse stress"""
        if self.load_case == 7:
            #
            # In case of linear varying transverse stress the capacity check
            # can be done by use of the design stress value at a distance l1
            # from the most stressed end of the plate, but not less than 0.75
            # of maximum sy,Sd. The resistance sy,Rd should be calculated
            # from eq. (6.5).
            # l1 = minimum of 0.25 l and 0.5 s
            self.msg = self.msg + '\n' + '6.8 not finish'
        #
        # 6.9 Buckling of unstiffened plate with
        #     longitudianal and transverse varying
        #      stress and with shear stress
        elif self.load_case == 8:
            # The check of combined varying loads may be done according
            # to eq. (6.18) with the resistance calculated according to eq.
            # (6.21) and eq. (6.5) using the stress point defined in sec. 6.8.
            self.msg = self.msg + '\n' + '6.9 not finish'
        else:
            self.msg = self.msg + '\n' + 'Case not defined'
    #
    # Section 7
    def BucklingOfStiffenedPlates(self):
        """ """
        self.msg = self.msg + '\n' + ''
        self.msg = self.msg + '\n' + 'Buckling of Stiffened Plates'
        #
        # 7.1 General
        # -----------
        #
        # This chapter deals with stiffened plate panels subjected to
        # axial stress in two directions, shear stress and lateral load.
        # There are different formulas for stiffeners being continuous
        # (or connected to frames with their full moment resistance)
        # and simple supported (sniped) stiffeners.
        #
        # An example of a stiffened plate panel is shown in Figure 3-1.
        # The stiffener cross section needs to fulfil requirements to
        # avoid local buckling given in Chapter 9.
        #
        # For shear lag effects see Commentary Chapter 10.
        #
        # The plate between stiffeners will normally be checked
        # implicitly by the stiffener check since plate buckling is
        # accounted for by the effective width method. However, in
        # cases where sy,Sd stress is the dominant stress it is necessary
        # to check the plate resistance according to eq. (7.19).
        #
        # For slender stiffened plates the load carrying resistance in the
        # direction transverse to the stiffener may be neglected. Then
        # sy,Sd stresses may be assumed to be carried solely by the
        # girder. In such cases the effective girder flange may be
        # determined by disregarding the stiffeners, and the stiffener
        # with plate may be checked by neglecting sy,Sd stresses
        # (method 2 in sec. 8.4). See also Commentary to 8 in Chapter
        # 10.
        #
        #
        self.msg = self.msg + '\n' + ''
        self.msg = self.msg + '\n' + '7.2 Forces in the idealised stiffened plate'
        self.msg = self.msg + '\n' + ''
        #
        # 7.2 Forces in the idealised stiffened plate
        # -------------------------------------------
        #
        # Stiffened plates subjected to combined forces, see Figure 7-1
        # should be designed to resist an equivalent axial force
        # according to eq. (7.1) and an equivalent lateral load
        # according to eq. (7.8).
        #
        # Assumption of tension field action implies that no (or
        # negligible) resistance of the plate against transverse
        # compression stresses (Sigmay) can be assumed. See also
        # Commentary Chapter 10.
        #
        #
        # Where
        # (7.7)
        if self.L < self.S:
            _kl = 5.34 * (self.S / self.L)**2 + 4
        else: # self.L >= self.S
            _kl = 5.34 + 4*(self.S / self.L)**2
        #
        self.msg = self.msg + '\n' + 'kl ='  + str(_kl)
        #
        # Taucrl = critical shear stress for the plate panel 
        # between twostiffeners, according to eq. (7.6).
        # (7.4)
        self._tau_crl = _kl * 0.904 * self.E * (self.t / self.S)**2
        print ('Tau = ' + str(self._tau_crl))
        #
        self.msg = self.msg + '\n' + 'Taucrl : ' + str( self._tau_crl)
        #
        # (7.5)
        if self.L > self.LG:
            _kg = 5.34 *(self.L / self.LG)**2 + 4
        else: # self.L <= self.LG
            _kg = 5.34 + 4*(self.L / self.LG)**2
        #
        self.msg = self.msg + '\n' + 'kg ='  + str( _kg)
        #
        # Taucrg = critical shear stress for the plate with the 
        # stiffeners removed, according to eq. (7.4).
        # (7.6)
        tau_crg = _kg * 0.904 * self.E * (self.t / self.L)**2
        #
        self.msg = self.msg + '\n' + 'Taucrg : '  + str( tau_crg)
        #
        # (7.2)
        if self.tau_Sd > (self._tau_crl / self.GammaM):
            # tension field action is allowed
            self.tau_tf = self.tau_Sd - tau_crg
        else: # (7.3)
            self.tau_tf = 0
        #
        self.msg = self.msg + '\n' + 'Tautf : ' + str(self.tau_tf)
        #
        # The equivalent axial force should be taken as:
        #
        # where
        # As = cross sectional area of stiffener
        # s = distance between stiffeners
        # t = plate thickness
        # SigmaxSd = axial stress in plate and stiffener
        #            with compressive stresses as positive
        #
        # (7.1)
        self.NSd = (self.sigmax_Sd * (self.As + self.S * self.t) 
                    + self.tau_tf * self.S * self.t)
        #
        self.msg = self.msg + '\n' + 'Equivalent axial force NSd : ' + str(self.NSd)
        #
        # Sigmay1Sd = larger design stress in the transverse direction,
        # with tensile stresses taken as negative
        #
        # Sigmay2Sd = smaller design stress in the transverse direction,
        # with tensile stresses taken as negative
        #
        Psi = self.sigmay_2Sd / self.sigmay_1Sd
        #
        self.msg = self.msg + '\n' + 'Psi =' + str(Psi)
        #
        # Wes = section modulus for stiffener with effective plate
        # at flange tip
        # mc = 13.3 for continuous stiffeners or,
        # mc = 8.9 for simple supported stiffeners (sniped stiffeners)
        # Is = moment of inertia of stiffener with full plate width
        #
        # (7.12)
        _kc = (2 * (1 + math.sqrt(1 + (10.9 * self.Is) 
                                  / (self.t**3 * self.S))))
        #
        self.msg = self.msg + '\n' + 'kc : ' + str(_kc)
        #
        #
        #
        # 7.3 Effective plate width
        # -------------------------
        #
        self.msg = self.msg + '\n' + ''
        self.msg = self.msg + '\n' + '7.3 Effective plate width'
        #
        # Only applicable for continous stifener??
        #
        # (7.15)
        lambda_p = 0.525 * (self.S / self.t) * math.sqrt(self.fy / self.E)
        #
        self.msg = self.msg + '\n' + 'Lambdap : ' + str( lambda_p)
        #
        # The reduction factor due to stresses in the longitudinal
        # direction, Cxs, is
        #
        # (7.14)
        if lambda_p > 0.673:
            self.Cxs = (lambda_p - 0.22)/ lambda_p**2
        else:
            self.Cxs = 1.0
        #
        self.msg = self.msg + '\n' + 'Cxs : '  +str(self.Cxs)
        # 
        if (self.S / self.t) > 120:
            _ci = 0
        else:
            _ci = 1 - self.S / (120 * self.t)
        #
        self.msg = self.msg + '\n' + 'ci : ' + str( _ci)
        #
        #
        # and the reduction factor for compression stresses in the
        # transverse direction, Cys, is found from:
        if self.stress == 'COMPRESSION':
            # (7.16)
            self.Cys = math.sqrt(1 - (self.sigmay_Sd / self.sigmay_R)**2 
                                 + _ci * ((self.sigmax_Sd * self.sigmay_Sd)
                                          / (self.Cxs * self.fy * self.sigmay_R)))
            #
            # SigmayR is calculated according to eq. (6.6).
            # In case of linear varying stress, SigmaySd may be determined as
            # described in sec. 6.8
        #
        # The reduction factor for tension stresses in the transverse
        # direction, Cys, is calculated as:
        else:
            # (7.17)
            self.Cys1 = ((1.0/2.0) * (math.sqrt(4 - 3 * (self.sigmay_Sd / self.fy)**2) 
                                      + (self.sigmay_Sd / self.fy)))
            #
            self.Cys = min(self.Cys1, 1.0)
            #
            # Tensile stresses are defined as negative.
        #
        self.msg = self.msg + '\n' + 'Cys =' + str(self.Cys)
        #
        #
        # The effective plate width for a continuous stiffener subjected
        # to longitudinal and transverse stress and shear is calculated
        # as:
        # (7.13)
        self.Se = self.S * self.Cxs * self.Cys
        #
        self.msg = self.msg + '\n' + 'Se =' + str(self.Se)
        #
        #
        #
        # The following resistance parameters are used in the
        # interaction equations for stiffeners:
        #
        # effective elastic section modulus on plate side,
        # see Figure 7-3.
        #_Wep = ie / Zp
        #
        # effective elastic section modulus on stiffener 
        # side, see Figure 7-3.
        #_Wes = ie / Zt
        #
        # effective area of stiffener and plate
        #_Ae = (_As + self.Se * self.t) 
        #
        # As = cross sectional area of stiffener
        # Se = effective width, see sec. 7.3
        #
        # Section Property of Stiffener with Efective Plate
        self.msg = self.msg + '\n' + ''
        self.msg = self.msg + '\n' + 'Section Property of Stiffener with Efective Plate'
        #
        _Ae, _Aec, Zp, ie, Iz, _Wes, _Wep, _fy = PlateStiffener(self.Se, self.t,
                                                                self.hw, self.tw,
                                                                self.bf, self.tf, 
                                                                   self.L, 
                                                                   self.fyp, self.fyS, self.fyS,
                                                                    self.ES)
        #
        #
        Zt = (self.t + self.hw + self.tf - Zp)
        self.msg = self.msg + '\n' + 'fy ='  + str(_fy)
        #self.msg = self.msg + '\n' + 'Flang Cross Sectional Area Af =' + str( _Af)
        #self.msg = self.msg + '\n' + 'Web Cross Sectional Area Aw =' + str( _Aw)
        self.msg = self.msg + '\n' + 'Total Cross Sectional Area Ae =' + str( _Ae)
        self.msg = self.msg + '\n' + 'Neautral Axis Loacation Zt =' + str( Zt)
        self.msg = self.msg + '\n' + 'Neautral Axis Loacation Zp =' + str( Zp)
        self.msg = self.msg + '\n' + 'Moment of Inertia Ie =' + str( ie)
        self.msg = self.msg + '\n' + 'Torsional Moment of Inertia Iz =' + str( Iz)
        self.msg = self.msg + '\n' + 'Section Modulus = Wes' + str( _Wes)
        self.msg = self.msg + '\n' + 'Section Modulus = Wep' + str(_Wep)
        self.msg = self.msg + '\n' + ''
        #
        # (7.11)
        _C0 = ((_Wes * self.fy * self.mc) 
               / (_kc * self.E * self.t**2 * self.S))
        #
        self.msg = self.msg + '\n' + 'C0 ='  + str( _C0)
        #
        # p0 shall be applied in the direction of the external pressure
        # pSd. For situations where pSd is less than p0, the stiffener need
        # to be checked for p0 applied in both directions (i.e. at plate
        # side and stiffener side).
        #
        if self.sigmay_Sd > 0:
            # (7.9)
            if Psi > -1.50:
                _p0 = (0.60 + 0.40 * Psi) * _C0 * self.sigmay_1Sd
            else: # (7.10)
                _p0 = 0
        else: 
            _p0 = 0
        #
        self.msg = self.msg + '\n' + 'p0 =' + str( _p0)
        #
        # The equivalent lateral line load should be taken as:
        #
        # pSd = design lateral pressure
        # s = stiffener spacing
        #
        # (7.8)
        #
        # Equivalent lateral line load on plate side
        if self.PSd < 0:
            if _p0 < abs(self.PSd):
                self.qsdp = 0
            else:
                self.qsdp = (self.PSd + _p0) * self.S
        else:
            self.qsdp = (self.PSd + _p0) * self.S
        #
        self.msg = self.msg + '\n' + 'qsdp =' + str( self.qsdp)
        #
        #
        # Equivalent lateral line load on stiffener side
        if self.PSd < 0:
            self.qsds = (_p0 - self.PSd) * self.S
        else:
            if _p0 < abs(self.PSd):
                self.qsds = 0
            else:
                self.qsds = (_p0 - self.PSd) * self.S
        #
        self.msg = self.msg + '\n' + 'qsds =' + str( self.qsds)
        #
        #
        #
        # 7.4 Resistance of plate between stiffeners
        # ------------------------------------------
        #
        self.msg = self.msg + '\n' + ''
        self.msg = self.msg + '\n' + '7.4 Resistance of plate between stiffeners'
        #
        # The plate between stiffeners shall be checked for:
        #
        # (7.20)
        _ksp = math.sqrt(1.0 - 3.0 * (self.tau_Sd / self.fy)**2)
        #
        self.msg = self.msg + '\n' + 'ksp =' + str( _ksp)
        #
        # (7.19)
        _URs = (_ksp * self.sigmay_Rd)/ self.sigmay_Sd
        # SigmayRd is determined from eq. (6.5).
        #
        self.msg = self.msg + '\n' + 'URs ='  + str( _URs)
        #
        # (7.18)
        self.tau_Rd = self.fy / (math.sqrt(3.0) * self.GammaM)
        #
        self.msg = self.msg + '\n' + 'TauRd =' + str(self.tau_Rd )
        #
        _URt = self.tau_Sd / self.tau_Rd 
        #
        self.msg = self.msg + '\n' + 'URt =' + str( _URt)
        #
        _URs = self.sigmay_Sd / (_ksp * self.sigmay_Rd)
        #
        self.msg = self.msg + '\n' + 'URs ='  +  str( _URs)  + 'SigmaySd ='  + str( self.sigmay_Sd) + 'SigmayRd =' + str(self.sigmay_Rd)
        #
        #
        #
        # When this check and stiffener check according to sec. 7.7 is
        # carried out it is not necessary to check the plate between
        # stiffeners according to Chapter 6.
        # See also Commentary Chapter 10.
        #
        #
        # 7.5 Characteristic buckling strength of stiffeners
        # --------------------------------------------------
        #
        self.msg = self.msg + '\n' + ''
        self.msg = self.msg + '\n' + '7.5 Characteristic buckling strength of stiffeners'
        #
        # 7.5.1 General
        # -------------
        #
        # where
        #
        # (7.38)
        self.SigmajSd = (math.sqrt(self.sigmax_Sd**2 + self.sigmay_Sd**2 
                                   - self.sigmax_Sd * self.sigmay_Sd 
                                   + 3 * self.tau_Sd**2))
        #
        self.msg = self.msg + '\n' + 'SigmajSd ='  + str( self.SigmajSd)
        # where:
        #
        # (7.41)
        _c = 2.0 - (self.S / self.L)
        #
        # (7.42)
        fEpx = 3.62 * self.E * (self.t / self.S)**2
        #
        # (7.43)
        fEpy = 0.90 * self.E * (self.t / self.S)**2
        #
        # (7.44)
        fEptau = 5.0 * self.E * (self.t / self.S)**2
        #
        self.msg = self.msg + '\n' + 'fEptau =' + str( fEptau)
        # 
        # SigmaxSd and SigmaySd should be set to zero if in tension
        if self.stress == 'COMPRESSION':
            _SigmaxSd = self.sigmax_Sd
            _SigmaySd = self.sigmay_Sd
        else:
            _SigmaxSd = 0
            _SigmaySd = 0
        #
        # (7.40)
        Lambdae = (math.sqrt((self.fy / self.SigmajSd) 
                             * math.pow(((_SigmaxSd / fEpx)**_c 
                                         + (_SigmaySd / fEpy)**_c 
                                          + (self.tau_Sd / fEptau)**_c)
                                        , (1.0 / _c))))
        #
        self.msg = self.msg + '\n' + 'Lambdae =' + str(Lambdae)
        #
        # (7.39)
        fEp = (self.fy / math.sqrt(1 + math.pow(Lambdae, 4.0)))
        #
        self.msg = self.msg + '\n' + 'fep =' + str( fEp)
        #
        # (7.37)
        _Eta = min((self.SigmajSd / fEp), 1.0)
        #
        self.msg = self.msg + '\n' + 'Eta =' + str( _Eta)
        #
        # t = plate thickness
        # tf = thickness of flange
        # tW = thickness of web
        # (7.36)
        _C = ((self.hw / self.S) * (self.t / self.tw) 
              * math.sqrt( 1 - _Eta))
        #
        self.msg = self.msg + '\n' + 'C =' + str( _C)
        #
        # Beta = 1.0
        # 
        if self.S > self.L:
            Beta = 1.0
        # or may for stocky plates alternatively be calculated as
        # per eq. (7.35) for s <= l
        # (7.35)
        else:
            Beta = ((3 * _C + 0.20) / (_C + 0.20))
        #
        self.msg = self.msg + '\n' + 'Beta =' + str(Beta)
        #
        # 7.5.2 Torsional buckling of stiffeners
        # --------------------------------------
        #
        self.msg = self.msg + '\n' + ''
        self.msg = self.msg + '\n' + '7.5.2 Torsional buckling of stiffeners'
        #
        # The torsional buckling strength may be calculated as:
        #
        # Were :
        #
        # Beta = 1.0,
        #        or may for stocky plates alternatively be 
        #        calculated as per eq. (7.35) for s = l
        # Af = cross sectional area of flange
        # AW = cross sectional area of web
        # G = shear modulus
        # Ipo = polar moment of inertia = Integral (r**2 dA)
        # where r is measured from the connection between the
        #         stiffener and the plate
        # It = stiffener torsional moment of inertia (St. Venant
        #      torsion)
        # Iz = moment of inertia of the stiffeners neutral axis normal
        #      to the plane of the plate
        # b = flange width
        # ef = flange eccentricity, see Figure 7-3
        # hw = web height
        # hs = distance from stiffener toe (connection between
        #      stiffener and plate) to the shear centre of the stiffener
        # lT = distance between sideways supports of stiffener,
        #      distance between tripping brackets (torsional buckling
        #      length).
        # t = plate thickness
        # tf = thickness of flange
        # tW = thickness of web
        #
        # where
        #
        #
        # Generally fET may be calculated as:
        # (7.31)
        #fET = (Beta * (self.G * _It / _Ipo) 
        #        + math.pi**2 * ((self.E * _hs**2 * Iz) 
        #                        / (_Ipo * LT**2)))
        #
        fET = {}
        lambdaT = {}
        MuT = {}
        fT = {}
        #
        fET[0], lambdaT[0], MuT[0], fT[0]  = self.fT(self.Lt, Beta, Iz)
        #
        self.msg = self.msg + '\n' + 'fET(x) =' + str(fET[0]) + ' LambdaT=' + str( lambdaT[0]) + ' MuT=' +  str( MuT[0]) + ' fT=' + str(fT[0]) + ' x = Lt =' + str(self.Lt)
        #
        #
        LT1 = min(0.40*self.L, self.Lt)
        fET[1], lambdaT[1], MuT[1], fT[1] = self.fT(LT1, Beta, Iz)
        #
        self.msg = self.msg + '\n' + 'fET(x) =' + str(fET[1]) + ' LambdaT=' + str(lambdaT[1]) + ' MuT=' + str( MuT[1])  + ' fT1=' + str( fT[1])  + ' x = Lt1 ='+ str(LT1)
        #
        #
        LT2 = min(0.80*self.L, self.Lt)
        fET[2], lambdaT[2], MuT[2], fT[2] = self.fT(LT2, Beta, Iz)
        #
        self.msg = self.msg + '\n' + 'fET(x) =' + str( fET[2]) + ' LambdaT=' + str( lambdaT[2]) + ' MuT=' + str( MuT[2]) + ' fT2=' + str( fT[2]) + ' x = Lt2 =' + str(LT2)
        #
        self.msg = self.msg + '\n' + ''
        self.msg = self.msg + '\n' + '7.5.1 General'
        #
        # lk see eq. (7.74)
        #
        # (7.73)
        # Effective radius of gyration
        # Ie effective moment of inertia
        # Ae effective area
        ie = math.sqrt(ie / _Ae)
        #
        self.msg = self.msg + '\n' + 'ie =' +  str(ie)
        #
        # For a continuous stiffener the buckling length may be
        # calculated from the following equation where :
        #
        # pSd is design lateral pressure
        # Pf is the lateral pressure giving yield in outer-fibre 
        # at support.
        #
        # W = the smaller of Wep and Wes
        # l = span length
        #
        _W = min(_Wep, _Wes)
        #
        self.msg = self.msg + '\n' + 'W =' + str( _W)
        #
        # (7.75)
        _Pf = ((12.0 * _W / (self.L**2 * self.S)) 
               * (self.fy / self.GammaM))
        #
        self.msg = self.msg + '\n' + 'Pf =' + str( _Pf)
        #
        # In case of varying lateral pressure, pSd in eq. (7.74) should be
        # taken as the minimum of the value in the adjoining spans.
        #
        #
        # Lk see eq(7.74)
        if self.Stiffener == 'C':
            _Lk = self.L * (1.0 - 0.50 * abs(self.PSd / _Pf))
        elif self.Stiffener == 'S':
            # For simple supported stiffener 
            _Lk = 1.0 * self.L
        else:
            self.msg = self.msg + '\n' + '****error***'
            exit
        #
        self.msg = self.msg + '\n' + 'Lk =' +  str(_Lk)
        #
        #
        # where:
        # for check at plate side :
        _frp = self.fy 
        #
        self.msg = self.msg + '\n' + 'frp =' + str( _frp)
        #
        # fT may be calculated according to sec. 7.5.2
        # LambdaT see eq. (7.30)
        #
        # (7.24)
        fE = math.pi**2 * self.E * (ie / _Lk)**2
        #
        self.msg = self.msg + '\n' + 'fE =' + str( fE)
        #
        sideP = 'P'
        _fr, Lambda, Mu, _fkp = self.fk(sideP, fT[0], fE, ie, Zt, Zp)
        #
        self.msg = self.msg + '\n' + ''
        self.msg = self.msg + '\n' + '==> ' + 'Side=' + str(sideP) + ' fr=' + str( _fr) + ' Lamda=' + str( Lambda) + ' Mu=' + str( Mu)
        self.msg = self.msg + '\n' + '_fkp' + str(  _fkp)
        #
        sideP = 'S'
        _fr, Lambda, Mu, _fks = self.fk(sideP, fT[0], fE, ie, Zt, Zp)
        #
        self.msg = self.msg + '\n' + ''
        self.msg = self.msg + '\n' + '==> ' + 'Side=' + str( sideP) + ' fr=' + str( _fr) + ' Lamda=' + str(Lambda) + ' Mu=' + str( Mu)
        # print('_fk', _fks)
        #
        # 7.7.3 Resistance parameters for stiffeners.  
        # ------------------------------------------
        #
        self.msg = self.msg + '\n' + ' '
        self.msg = self.msg + '\n' + '7.7.3 Resistance parameters for stiffeners'
        #
        # (7.65)
        NRd = _Ae * self.fy / self.GammaM
        #
        self.msg = self.msg + '\n' + 'NRd =' + str( NRd )
        #
        # where fks is calculated from sec 7.5 using eq(7.26)
        # (7.66)
        NksRd = _Ae * _fks / self.GammaM
        #
        self.msg = self.msg + '\n' + '-----'
        self.msg = self.msg + '\n' + 'NksRd =' + str( NksRd )
        # 
        # where fkp is calculated from sec 7.5 using eq(7.25)
        # (7.67)
        NkpRd = _Ae * _fkp / self.GammaM
        #
        self.msg = self.msg + '\n' + 'NkpRd =' + str( NkpRd )
        #
        # where fr is calculated from sec. 7.5 for stiffener side using
        #LT = 0.4 * self.L 
        # or distance between lateral support if this is less.
        # (7.68)
        #
        sideP = 'S'
        _fr1, Lambda, Mu, _fks1 = self.fk( sideP, fT[1], fE, ie, Zt, Zp)
        #
        self.msg = self.msg + '\n' + '-----'
        self.msg = self.msg + '\n' + '_fk' + str( _fks1)
        #
        Ms1Rd = _Wes * _fr1 / self.GammaM
        #
        self.msg = self.msg + '\n' + 'Ms1Rd =' + str( Ms1Rd)
        #
        # where fr is calculated from sec. 7.5 for stiffener side using
        #LT = 0.8 * self.L 
        # or distance between lateral support if this is less.
        # (7.69)
        #
        #
        sideP = 'S'
        _fr2, Lambda, Mu, _fks2 = self.fk( sideP, fT[2], fE, ie, Zt, Zp)
        #
        self.msg = self.msg + '\n' + '_fk' + str( _fks2)
        #
        Ms2Rd = _Wes * _fr2 / self.GammaM
        #
        self.msg = self.msg + '\n' + 'Ms1Rd =' + str( Ms2Rd)
        #
        # (7.70)
        MstRd = _Wes * self.fy / self.GammaM
        #
        self.msg = self.msg + '\n' + '-----'
        self.msg = self.msg + '\n' + 'MstRd =' + str(MstRd)
        #
        # (7.71)
        MpRd = _Wep * self.fy / self.GammaM
        #
        self.msg = self.msg + '\n' + 'MpRd =' + str( MpRd)
        #
        #
        # (7.72)
        NE = (math.pi**2 * self.E * _Ae) / (_Lk / ie)**2
        #
        self.msg = self.msg + '\n' + '-----'
        self.msg = self.msg + '\n' + 'NE =' + str( NE)
        #
        # 7.6 Resistance of stiffened panels to shear stresses
        # ----------------------------------------------------
        #
        self.msg = self.msg + '\n' + ''
        self.msg = self.msg + '\n' + '7.6 Resistance of stiffened panels to shear stresses'
        #
        # The resistance towards shear stresses TauRd is found as the
        # minimum of TauRdy, TauRdl and TauRds according to the following:
        #
        # where Taucrl is obtained from eq. (7.6) and Taucrs is obtained
        # from:
        #
        # Is= moment of inertia of stiffener with full plate width.
        # (7.49)
        _Ip = (self.t**3 * self.S) / 10.90
        #
        self.msg = self.msg + '\n' + 'Ip =' + str( _Ip)
        #
        # (7.48)
        tau_crs = ((36.0 * self.E / (self.S * self.t * self.L**2)) 
                   * math.pow((_Ip * self.Is**3), 1.0 / 4.0))
        #
        self.msg = self.msg + '\n' + 'Taucrs =' + str( tau_crs)
        #
        # (7.47)
        tau_Rds = tau_crs / self.GammaM
        #
        self.msg = self.msg + '\n' + 'TauRds =' + str( tau_Rds)
        #
        # (7.45)
        tau_Rdy = self.fy / (math.sqrt(3) * self.GammaM)
        #
        self.msg = self.msg + '\n' + 'TauRdy =' + str( tau_Rdy)
        #
        # (7.46)
        tau_Rdl = self._tau_crl / self.GammaM
        #
        self.msg = self.msg + '\n' + 'TauRdl =' + str( tau_Rdl)
        #
        # The resistance towards shear stress in then:
        self.tau_Rd = min(tau_Rdy, tau_Rdl, tau_Rds)
        #
        self.msg = self.msg + '\n' + 'TauRd =' + str( self.tau_Rd)
        #
        # 7.7 Interaction formulas for axial compression
        #      and lateral pressure
        # ----------------------------------------------
        #
        # 7.7.1 Continuous stiffeners
        # ---------------------------
        #
        self.msg = self.msg + '\n' + ''
        self.msg = self.msg + '\n' + '7.7.1 Continuous stiffeners'
        #
        # For continuous stiffeners the following four interaction
        # equations need to be fulfilled in case of:
        #
        # When tension field action is assumed according to eq. (7.2)
        # then u = 0.
        # For resistance parameters see sec. 7.7.3 for stiffener and sec.
        # 8.3 for girders.
        #
        # (7.58)
        if self.tau_tf == 0:
            _u = (self.tau_Sd / self.tau_Rd)**2
        else: 
            _u = 0
        #
        self.msg = self.msg + '\n' + 'u ='+ str( _u)
        #
        # qsd is given in eq. (7.8)
        # l  is the span length
        # z* is the distance from the neutral axis of the effective section
        #    to the working point of the axial force. z* may be varied in
        #    order to optimise the resistance. z* should then be selected so
        #    the maximum utilisation found from the equations (7.50) to
        #    (7.53) or (7.54) to (7.57) is at its minimum, see also
        #    Commentary Chapter 10. The value of z* is taken positive
        #    towards the plate. The simplification z* = 0 is always
        #    allowed.
        #
        self.msg = self.msg + '\n' + ''
        self.msg = self.msg + '\n' + '=====> ******'
        #
        self.CombinedUnityCheck(self.Stiffener, self.NSd, self.L, self.qsdp, 
                                self.qsds, NksRd, NkpRd, MpRd, MstRd, 
                                Ms1Rd, Ms2Rd, NE, NRd,  self.Z , _u)
        self.ur_combined_Plate = self.ur_combined
        #
        #
        # 7.8 Check for shear force
        # -------------------------
        #
        self.msg = self.msg + '\n' + ' '
        # print('7.8 Check for shear force')
        #
        # The stiffener should in all sections satisfy:
        #
        # where:
        # VSd = design shear force
        # VRd = design shear resistance
        # Anet = net shear area (shear area minus cut outs)
        #
        # If VSd > 0.5 VRd then the stiffener section modulus and
        # effective area need to be reduced to account for the
        # interaction of the shear with the moment and axial force in
        # the stiffener.
        #
        _Anet = _Ae
        #
        self.VRd = _Anet * (self.fy / (self.GammaM * math.sqrt(3.0)))
        #
        self.msg = self.msg + '\n' + 'VRd =' + str( self.VRd)
        #
        #_URv = 2 * self.VSd / self.VRd
        #self.msg = self.msg + '\n' + 'URv =' + str( _URv)
    #
    # Section 8
    def BucklingOfGirders(self):
        """ """
        self.msg = self.msg + '\n' + ''
        self.msg = self.msg + '\n' + 'Section 8'
        #
        self.msg = self.msg + '\n' + ''
        self.msg = self.msg + '\n' + 'Girder Section property without associated plate'
        #
        _Asf, _AG,Zpf, _Is, IzG, _Wes, _Wep, _fyG = PlateStiffener(0.0, 0.0,
                                                                   self.hwG, self.twG, 
                                                                   self.bG, self.tfG, 
                                                                     self.LG,
                                                                     self.fyG, self.fyG, self.fyG,
                                                                     self.EG)
        #
        _AfG = self.bG * self.tfG
        _AwG = self.hwG * self.twG
        #
        self.msg = self.msg + '\n' + ''
        self.msg = self.msg + '\n' + 'fyG =' + str(_fyG)
        self.msg = self.msg + '\n' + 'Total Cross Sectional Area =' + str(  _Asf)
        self.msg = self.msg + '\n' + 'Cross Sectional Area of stiffener =' + str( _AG)
        self.msg = self.msg + '\n' + 'Neautral Axis Loacation =' + str( Zpf)
        self.msg = self.msg + '\n' + 'Moment of Inertia IG =' + str( _Is) + ' ' + str( IzG)
        self.msg = self.msg + '\n' + ''
        #
        #
        # 8.4 Effective widths of girders  
        # -------------------------------
        #
        self.msg = self.msg + '\n' + '8.4 Effective widths of girders '
        #
        # 8.4.1 General
        # -------------
        #
        # For the determination of the effective width the designer is
        # given two options denoted method 1 and method 2. These
        # methods are described in sec. 8.4.2 and 8.4.3 respectively:
        #
        # 8.4.2 Method 1
        # --------------
        #
        self.msg = self.msg + '\n' + ''
        self.msg = self.msg + '\n' + '8.4.2 Method 1'
        self.msg = self.msg + '\n' + '--------------'
        #
        # Calculation of the girder by assuming that the stiffened plate
        # is effective against transverse compression (sy) stresses. See
        # also Commentary Chapter 10 and sec. 7.1.
        #
        # In this method the effective width may be calculated as:
        #
        # Cxs is found from eq. (7.14).
        # (8.20)
        _fkx = self.Cxs * self.fy
        #
        self.msg = self.msg + '\n' + 'fkx =' + str( _fkx)
        #
        # (8.19)
        _CxG_1 = math.sqrt(1 - (self.sigmax_Sd / _fkx)**2)
        #
        self.msg = self.msg + '\n' + 'CxG 1 =' + str( _CxG_1)
        #
        # If the Sigmay stress in the plate is partly or complete in
        # compression CyG may be found from eq. (7.16).
        #
        # (8.22)
        if self.stress == 'COMPRESSION':
            #_CyG_1 = math.sqrt(1.0 - 3 * (self.tau_Sd / self.fy)**2)
            _CyG_1 = self.Cys
        #
        # If the Sigmay stress in the girder is in tension due to the combined
        # girder axial force and bending moment over the total span of
        # the girder CyG may be taken as:
        #
        # (8.21)
        else:
            _CyG_1 = 1.0
        #
        self.msg = self.msg + '\n' + 'CyG 1 =' + str( _CyG_1)
        #
        #
        # Le should not be taken larger than 0.3 LG for continuous
        # girders and 0.4 LG for simple supported girders when
        # calculating section modules Wep and WeG.
        #
        #
        # 8.4.3 Method 2
        # --------------
        #
        self.msg = self.msg + '\n' + ''
        self.msg = self.msg + '\n' + '8.4.3 Method 2'
        self.msg = self.msg + '\n' + '--------------'
        #
        #
        # Calculation of the girder by assuming that the stiffened plate
        # is not effective against transverse compression stresses (sy).
        # See also Commentary Chapter 10 and Sec. 7.1.
        #
        # In this case the plate and stiffener can be checked with sy
        # stresses equal to zero.
        #
        # In method 2 the effective width for the girder should be
        # calculated as if the stiffener was removed.
        # then:
        # (8.23)
        CxG_2 = math.sqrt(1.0 - (self.sigmax_Sd / self.fy)**2)
        #
        self.msg = self.msg + '\n' + 'CxG 2 ='  + str( CxG_2 )
        #
        # where
        # SigmaxSd is based on total plate and stiffener area in x-direction.
        #
        # (8.25)
        lambdaG = 0.525 * (self.L / self.t) * math.sqrt(self.fy / self.E)
        #
        self.msg = self.msg + '\n' + 'LambdaG =' + str( lambdaG )
        #
        #
        # (8.24)
        if lambdaG > 0.673 :
            _CyG_2 = (lambdaG - 0.22) / lambdaG**2
        else: # lambdaG <= 0.673 
            _CyG_2 = 1.0
        #
        #
        self.msg = self.msg + '\n' + 'CyG 2 =' + str( _CyG_2 )
        #
        #
        # Select correct CxG and CyG
        #
        self.msg = self.msg + '\n' + 'Effective PL Sigmay' + str( self.EffectivePLSigmay)
        #
        if self.EffectivePLSigmay == 'Y':
            _CxG = _CxG_1
            _CyG = _CyG_1
        else:
            _CxG = CxG_2
            _CyG = _CyG_2
        #
        self.msg = self.msg + '\n' + 'CxG =' + str(_CxG)
        self.msg = self.msg + '\n' + 'CyG =' + str( _CyG)
        #
        # (8.26)
        C_tauG = math.sqrt(1.0 - 3.0 * (self.tau_Sd / self.fy)**2)
        #
        self.msg = self.msg + '\n' + 'CtauG =' + str( C_tauG)
        #
        # The effective width for the plate of the girder is taken equal
        # to:
        #
        # (8.18)
        if self.GirderSupport == 'C':
            _Le = min((_CxG * _CyG * C_tauG * self.L), 0.30 * self.LG)
        else:
            _Le = min((_CxG * _CyG * C_tauG * self.L), 0.40 * self.LG)
        #
        self.msg = self.msg + '\n' + '_Le =' + str( _Le)
        #
        #
        #
        # Calculate Girder Section Property with Effective Plate
        # -----------------
        #
        self.msg = self.msg + '\n' + 'Section Property of Stiffener with Efective Plate'
        #
        _AGe, _ApG, ZpG, _IGe, _IpG, _WeG, _WepG, _fy = PlateStiffener(_Le, self.t,
                                                                       self.hwG, self.twG, 
                                                                       self.bG, self.tfG, 
                                                                        self.LG,
                                                                        self.fyG, self.fyG, self.fyG,
                                                                    self.EG)
        #
        #
        ZtG = (0.50*self.t + self.hwG + self.tfG - ZpG)
        _WG = min(_WeG, _WepG)
        #
        self.msg = self.msg + '\n' + ''
        self.msg = self.msg + '\n' + 'fy ='  + str(_fy)
        self.msg = self.msg + '\n' + 'Total Cross Sectional Area AGe =' + str( _AGe)
        self.msg = self.msg + '\n' + 'Neautral Axis Loacation ZpG ='+ str(ZpG)
        self.msg = self.msg + '\n' + 'Neautral Axis Loacation ZtG =' +  str( ZtG)
        self.msg = self.msg + '\n' + 'Moment of Inertia IGe ='+ str( _IGe)
        self.msg = self.msg + '\n' + 'Torsional Moment of Inertia IpG ='+ str( _IpG)
        self.msg = self.msg + '\n' + 'Section Modulus = WeG' + str( _WeG)
        self.msg = self.msg + '\n' + 'Section Modulus = WepG' + str( _WepG)
        self.msg = self.msg + '\n' + 'Total Section Modulus = WG' + str( _WG)
        self.msg = self.msg + '\n' + ''
        #
        # 8.2 Girder forces
        # -----------------
        #
        # print('')
        # print('8.2 Girder forces')
        #
        #
        # The axial force should be taken as:
        # (8.1)
        _NySd = self.sigmay_Sd * (self.L * self.t + _AG)
        self.msg = self.msg + '\n' + 'NySd ='+ str( _NySd)
        #
        # The lateral line load should be taken as:
        # (8.2)
        #_qSd = (self.PSd + _Po) * self.L
        #
        # where
        # pSd = design lateral pressure
        # p0 = equivalent lateral pressure
        # AG = cross sectional area of girder
        #
        # The calculation of the additional equivalent lateral pressure
        # due to longitudinal compression stresses and shear shall be
        # calculated as follows:
        #
        # LP = length of panel
        # hwG = web height of girder
        # As = cross sectional area of stiffener
        # LG = girder span
        # s = stiffener spacing
        # Is = moment of inertia of stiffener with full plate width
        #
        # For linear variation of SigmaxSd, the maximum value within
        # 0.25LG to each side of the midpoint of the span may be used
        #
        # TauSd should correspond to the average shear flow over the
        # panel
        #
        #
        tau_cel = ((18.0 * self.E / (self.t * self.L**2)) 
                   * math.pow(self.t * self.Is / self.S, 0.75))
        #
        self.msg = self.msg + '\n' + 'Taucel =' + str( tau_cel)
        #
        tau_ceg = tau_cel * self.L**2 / self.Lp**2
        #
        self.msg = self.msg + '\n' + 'Tauceg =' + str( tau_ceg)
        #
        # Taucrg = critical shear stress of panel with girders removed,
        #          calculated from eq.(8.6) with Lambdatau calculated 
        #          using:
        #          Tauce =  Tauceg If the stiffener is not continuous
        #                   through the girder tcrg = 0.
        #
        #
        # Taucrl = critical shear stress of panel between girders 
        #          calculated from eq. (8.6) with Lambdatau calculated 
        #          using Tauce = Taucel
        #
        #
        lambdaTau = lambda x: math.sqrt(0.60*self.fy / x)
        #
        #
        lambdaTau_ce = {}
        #
        lambdaTau_ce[0] = lambdaTau(tau_cel)
        self.msg = self.msg + '\n' + 'LambdaTau cel =' + str( lambdaTau_ce[0])
        #
        #
        lambdaTau_ce[1] = lambdaTau(tau_ceg)
        self.msg = self.msg + '\n' + 'LambdaTau ceg =' + str( lambdaTau_ce[1])
        #
        #
        tau_cr = {}
        #
        tau_cr[0] = self.TauCR( lambdaTau_ce[0])
        self.msg = self.msg + '\n' + 'Taucr cel =' + str( tau_cr[0])
        #
        tau_cr[1] = self.TauCR(lambdaTau_ce[1])
        self.msg = self.msg + '\n' + 'Taucr ceg =' + str( tau_cr[1])
        #
        #
        # (8.17)
        _iGe = math.sqrt(_IGe / _AGe)
        #
        self.msg = self.msg + '\n' + 'iGe =' + str( _iGe)
        #
        # (8.11)
        fEG = math.pi**2 * self.E * (_iGe / self.LGk)**2
        #
        self.msg = self.msg + '\n' + 'fEG =' + str( fEG)
        #
        # fEG is given in eq (8.11)
        #
        lambdaG = math.sqrt(self.fy / fEG)
        #
        self.msg = self.msg + '\n' + 'Lambda G =' + str( lambdaG)
        # 
        #
        _Q1 = max(lambdaG - 0.20, 0)
        _Q = min(_Q1 , 1.0)
        #
        self.msg = self.msg + '\n' + 'Q =' + str(_Q)
        #
        #
        if self.Stiffener == 'S':
            tau_crG = 0
        else:
            tau_crG = tau_cr[1]
        #
        self.msg = self.msg + '\n' + 'tau_crG =' + str( tau_crG)
        #
        #
        print ('1')
        if self.tau_Sd > tau_crG :
            # (8.4)
            _CG = (_Q * (7.0 - 5.0 * (self.S / self.L)**2) 
                   * ((self.tau_Sd - tau_crG) / self._tau_crl)**2)
        # self.tau_Sd <= tau_crg
        else:
            # (8.5) 
            _CG = 0
        #
        self.msg = self.msg + '\n' + 'C =' + str( _CG)
        #
        #
        # For tension in the x-direction:
        if self.sigmax_Sd < 0:
            # (8.7)
                        #_Po = (((0.40 * (self.t + _As / self.S)) 
            _Po = (((0.40 * (self.t + self.As / self.S)) 
                    / (self.hwG * (1.0 - self.S / self.LG))) 
                   * (self.fy / self.E) * (self.LG / self.L)**2 
                   * (_CG * self.tau_Sd))
        else: # For compression in the x-direction:
            # (8.3)
            _Po1 = (((0.40 * (self.t + self.As / self.S)) 
                     / (self.hwG * (1.0 - self.S / self.LG))) 
                    * (self.fy / self.E) * (self.LG / self.L)**2 
                    * (self.sigmax_Sd + _CG * self.tau_Sd))
            #
            self.msg = self.msg + '\n' + 'Po1 =' + str( _Po1) + ' C*Tsd = ' +str(( _CG * self.tau_Sd))
            # But not less than
            _Po2 = (0.020 * ((self.t + (self.As / self.S)) / self.L) 
                    * (self.sigmax_Sd + _CG * self.tau_Sd))
            #
            self.msg = self.msg + '\n' + 'Po2 =' + str( _Po2)
            #
            _Po = max(_Po1, _Po2)
        #
        # Po shall be applied in the direction of external pressure
        # Psd. For situtations where Psd is less than Po. the girder
        # need to be checked for Po applied in both directions
        # (i.e. at plate side and stiffener side)
        #
        self.msg = self.msg + '\n' + 'SigmaxSd =' + str( self.sigmax_Sd )
        self.msg = self.msg + '\n' + 'PSd =' + str(self.PSd)
        self.msg = self.msg + '\n' + 'p0 =' + str(_Po)
        #
        #
        # Equivalent lateral line load on plate side
        #
        # (8.2)
        #
        if self.PSd > 0:
            qSdp = (self.PSd + _Po) * self.L
        else:
            if _Po < abs(self.PSd):
                qSdp = 0.0
            else:
                qSdp = (self.PSd + _Po) * self.L
        #
        self.msg = self.msg + '\n' + 'qSdp =' + str( qSdp)
        #
        # Equivalent lateral line load on stiffener side
        #
        if self.PSd > 0:
            qSds = ( _Po - self.PSd ) * self.L
        else:
            if _Po < self.PSd:
                qSds = 0.0
            else:
                qSds = ( _Po - self.PSd ) * self.L
        #
        #
        self.msg = self.msg + '\n' + 'qSds =' + str( qSds)
        #
        # 8.5 Torsional buckling of girders
        # ---------------------------------
        #
        self.msg = self.msg + '\n' + ''
        # print('8.5 Torsional buckling of girders')
        #
        #
        # SigmaySd = compressive stress in the free flange
        # (8.32)
        self.PSd = 0.020 * self.sigmay_Sd * (_AfG + _AwG / 3.0)
        #
        self.msg = self.msg + '\n' + 'PSd =' + str( self.PSd )
        #
        #
        # Torsional buckling need not to be considered if tripping
        # brackets are provided so that the laterally unsupported 
        # length LGT, does not exceed the value LGT0 defined by:
        # where
        # b = flange width
        # C = 0.55 for symmetric flanges
        #     1.10 for one sided flanges
        #
        if self.efG == 0:
            _CGTO = 0.55
        else:
            _CGTO = 1.10
        #
        self.msg = self.msg + '\n' + 'C GTO =' + str( _CGTO)
        #
        # (8.31)
        LGTO = (self.bG * _CGTO *
                ( math.sqrt(self.EG * _AfG 
                            / (self.fyG * (_AfG + _AwG / 3.0)))))
        #
        self.msg = self.msg + '\n' + 'L GTO =' + str(LGTO)
        #
        self.msg = self.msg + '\n' + ''
        if LGTO < self.LGt:
            self.msg = self.msg + '\n' + 'Girder torsional buckling check need to be considered'
        else:
            self.msg = self.msg + '\n' + 'Girder torsional buckling not required'
        #
        # The torsional buckling strength of girders may be determined
        # as:
        #
        # LGT = distance between lateral supports
        # Af, Aw = cross sectional area of flange and web of girder
        # Iz = moment of inertia of girder (exclusive of plate flange)
        #      about the neutral axis perpendicular to the plate
        #
        LGt1 = min(0.40*self.LG, self.LGt)
        #
        self.msg = self.msg + '\n' + 'LGt 1 ' + str( LGt1)
        #
        #
        LGt2 = min(0.80*self.LG, self.LGt)
        #
        self.msg = self.msg + '\n' + 'LGt 2 ' +str(LGt2)
        self.msg = self.msg + '\n' + ''
        #
        # (8.30)
        fETG = lambda x: ((math.pi**2 * self.EG * IzG) 
                          / ((_AfG + _AwG / 3.0) * x**2))
        #
        fETG = {}
        #
        fETG[0] = fETG(self.LGt)
        self.msg = self.msg + '\n' + 'fETG-LGt ='+str( fETG[0]) + ' ' + str(self.LGt)
        #
        fETG[1] = fETG(LGt1)
        self.msg = self.msg + '\n' + 'fETG-LGt1 =' + str( fETG[1]) + ' ' + str(LGt1)
        #
        fETG[2] = fETG(LGt2)
        self.msg = self.msg + '\n' + 'fETG-LGt2 =' + str( fETG[2]) + ' ' + str(LGt1)
        #
        # print('')
        #
        # (8.28)
        LambdaTG = lambda x: math.sqrt(self.fy / x)
        #
        lambdaTG = {}
        #
        lambdaTG[0] = LambdaTG(fETG[0])
        self.msg = self.msg + '\n' + 'LambdaTG - LGt ='+ str( lambdaTG[0])
        #
        lambdaTG[1] = LambdaTG(fETG[1])
        self.msg = self.msg + '\n' + 'LambdaTG - LGt1 =' + str( lambdaTG[1])
        #
        lambdaTG[2] = LambdaTG(fETG[2])
        self.msg = self.msg + '\n' + 'LambdaTG - LGt2 =' + str( lambdaTG[2])
        #
        #
        # (8.29)
        MuTG = lambda x: 0.35 * (x - 0.60)
        #
        # print('')
        #
        MuTG = {}
        #
        MuTG[0] = MuTG(lambdaTG[0])
        self.msg = self.msg + '\n' + 'MuTG - LGt =' + str( MuTG[0])
        #
        MuTG[1] = MuTG(lambdaTG[1])
        self.msg = self.msg + '\n' + 'MuTG - LGt1 ='+ str( MuTG[1])
        #
        MuTG[2] = MuTG(lambdaTG[2])
        self.msg = self.msg + '\n' + 'MuTG - LGt2 =' + str( MuTG[2])
        #
        #
        self.msg = self.msg + '\n' + ''
        #
        fTG = {}
        #
        fTG[0] = self.fTG( lambdaTG[0], MuTG[0])
        self.msg = self.msg + '\n' + 'fTG - LGt =' + str( fTG[0])
        #
        fTG[1] = self.fTG( lambdaTG[1], MuTG[1])
        self.msg = self.msg + '\n' + 'fTG - LGt 1 =' + str( fTG[1])
        #
        fTG[2] = self.fTG( lambdaTG[2], MuTG[2])
        self.msg = self.msg + '\n' + 'fTG - LGt 2 =' + str( fTG[2])
        #
        # Tripping brackets are to be designed for a lateral force PSd,
        # which may be taken equal to (see Figure 8-2 ):
        #
        # 8.3 Resistance parameters for girders
        # -------------------------------------
        #
        self.msg = self.msg + '\n' + ''
        self.msg = self.msg + '\n' + '8.3 Resistance parameters for girders'
        self.msg = self.msg + '\n' + ''
        #
        fkG = {}
        frG = {}
        #
        frG[0], lambdaG0, MuG0, fkG[0] = self.fk( 'S', fTG[0], fEG, _iGe, ZtG, ZpG)
        self.msg = self.msg + '\n' + 'fk - S =' + str( fkG[0])
        #
        frG[1], lambdaG1, MuG1, fkG[1] = self.fk( 'P', fTG[0], fEG, _iGe, ZtG, ZpG)
        self.msg = self.msg + '\n' + 'fk - P =' + str( fkG[1])
        #
        frG[2], lambdaG1, MuG1, fkG[2] = self.fk( 'S', fTG[1], fEG, _iGe, ZtG, ZpG)
        self.msg = self.msg + '\n' + 'fk 1 - S =' + str( fkG[2])
        #
        frG[3], lambdaG1, MuG1, fkG[3] = self.fk( 'S', fTG[2], fEG, _iGe, ZtG, ZpG)
        self.msg = self.msg + '\n' + 'fk 2 - S =' + str( fkG[3])
        self.msg = self.msg + '\n' + ''
        #
        #
        # The resistance of girders may be determined by the
        # interaction formulas in sec. 7.7 using the following
        # resistance
        #
        # (8.8)
        NRd = (_AG + _Le * self.t) * (self.fy / self.GammaM)
        #
        self.msg = self.msg + '\n' + 'NRd =' + str( NRd)
        #
        # fk is calculated from sec 7.5 using Mu according to eq (7.26)
        # (8.9)
        NksRd = (_AG + _Le * self.t) * (fkG[0] / self.GammaM)
        self.msg = self.msg + '\n' + 'NksRd =' + str( NksRd)
        #
        # 
        # fk is calculated from sec. 7.5 using Mu according to eq.
        # (7.25) using:
        # fr = fy for check at plate side
        # fr = fTG for check at girder flange side
        #
        # (8.10)
        NkpRd = (_AG + _Le * self.t) * (fkG[1] / self.GammaM)
        self.msg = self.msg + '\n' + 'NkpRd =' + str( NkpRd)
        #
        #
        # (8.12)
        Ms1Rd = _WeG * (frG[2] / self.GammaM)
        self.msg = self.msg + '\n' + 'Ms1Rd =' + str( Ms1Rd)
        #
        # (8.13)
        Ms2Rd = _WeG * (frG[3] / self.GammaM)
        self.msg = self.msg + '\n' + 'Ms2Rd =' + str( Ms2Rd)
        #
        # (8.14)
        MstRd = _WeG * (self.fyG / self.GammaM)
        self.msg = self.msg + '\n' + 'MstRd =' + str( MstRd)
        #
        # (8.15)
        MpRd = _WepG * (self.fyG / self.GammaM)
        self.msg = self.msg + '\n' + 'MpRd =' + str( MpRd)
        #
        # (8.16)
        NE = (math.pi**2 * self.E * _AGe / (self.LGk / _iGe)**2)
        self.msg = self.msg + '\n' + 'NE =' + str( NE)
        #
        # 8.1 General
        # -----------
        #
        self.msg = self.msg + '\n' + ''
        self.msg = self.msg + '\n' + '8.1 General'
        self.msg = self.msg + '\n' + ''
        #
        # The check for girders is similar to the check for stiffeners of
        # stiffened plates in equations (7.50) to (7.57) or (7.59) to
        # (7.64) for continuous or sniped girders, respectively. Forces
        # shall be calculated according to sec. 8.2 and cross section
        # properties according to 8.4. Girder resistance should be
        # found from sec. 8.3. Torsional buckling of girders may be
        # assessed according to sec. 8.5.
        #
        #
        self.CombinedUnityCheck( self.GirderSupport, _NySd, self.LG, qSdp, qSds, NksRd, NkpRd, MpRd, MstRd, Ms1Rd, Ms2Rd, NE, NRd,  _z = 0.0 , _u = 0)
        self.ur_combined_Girder = self.ur_combined
    #
    # Section 9
    def LocalBuckling(self):
        """ """
        # 9 Local buckling of stiffeners, girders and
        # brackets
        # -------------------------------------------
        #
        # 9.1 Local buckling of stiffeners and girders
        # --------------------------------------------
        #
        # 9.1.1 General
        # -------------
        #
        # The methodology given in Chapter 7 and Chapter 8 is only
        # valid for webs and flanges that satisfy the the following
        # requirements or fulfils requirements to cross section type
        # III defined in Appendix A of DNV-OS-C101
        #
        # In lieu of more refined analysis such as in Chapter 7, web
        # stiffeners should satisfy the requirements given in sec. 9.1.2
        # and sec. 9.1.3. 
        #
        # Flange outstand for T or L stiffeners or girders should
        # satisfy:
        #
        epsilon = math.sqrt(235.0 / self.fy)
        #
        # For definition of c see Figure 7-3
        #
        # for welded sections
        # (9.1)
        if self.section_type == 'WELDED':
            if self.c > (14.0 * self.tf * epsilon):
                self.msg = self.msg + '\n' + '===> Fail'
        # for rolled sections
        else:
            if self.c > (15.0 * self.tf * epsilon):
                self.msg = self.msg + '\n' + '===> Fail'
        #
        #
        # Web of stiffeners and girders should satisfy:
        # (9.2)
        if self.hw > (42.0 * self.tw * epsilon):
            self.msg = self.msg + '\n' + '===> Fail'
        #
        #
        # 9.1.2 Transverse web stiffeners:
        # -------------------------------
        #
        # Is = moment of inertia of web stiffener with full web plate
        #      flange s
        # Lt = length of transverse web stiffener
        # S = distance between transverse web stiffeners
        #
        _Is93 = ((0.30 * self.Lt * self.S**2 * self.tw) 
                 * ((2.50 * self.Lt / self.S) - (2.0 * self.S / self.Lt))
                 * (self.fy / self.E))
        #
        # (9.3)
        if self.Is < _Is93 :
            self.msg = self.msg + '\n' + '===> Fail'
        #
        #
        # 9.1.3 Longitudinal web stiffener:
        # --------------------------------
        #
        # Is = moment of inertia of web stiffener with full web plate
        #      flange s.
        # As = cross sectional area of web stiffener exclusive web
        #      plating.
        # Ll = length of longitudinal web stiffener
        #  S = distance between longitudinal web stiffeners
        #
        _Is94 = ((0.25 * self.Ll**2) * (_As + self.S * self.tw) 
                 * (self.fy / self.E))
        #
        # (9.4)
        if self.Is < _Is94 :
            self.msg = self.msg + '\n' + '===> Fail'
        #
        #
        # 9.2 Buckling of brackets
        # Brackets should be stiffened in such a way that:
        #
        # tb = plate thickness of bracket.
        # Stiffeners as required in eq. (9.6) or eq. (9.7) may be
        # designed in accordance with Chapter 7. See Figure 9-3
        #
        # (9.5)
        if self.bracket_type == 'FREE':
            if self.d0 > (0.70 * self.tb * math.sqrt(self.E / self.fy)):
                self.msg = self.msg + '\n' + '===> Fail'
        elif self.bracket_type == 'SINGLE': # (9.6)
            if self.d1 > (1.650 * self.tb * math.sqrt(self.E / self.fy)):
                self.msg = self.msg + '\n' + '===> Fail'
        elif self.bracket_type == 'DOUBLE': # (9.7)
            if self.d2 > (1.350 * self.tb * math.sqrt(self.E / self.fy)):
                self.msg = self.msg + '\n' + '===> Fail'
        else:
            self.msg = self.msg + '\n' + 'Bracket type not supported'
    #
    #
    #
    #
