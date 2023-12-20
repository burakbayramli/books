# Copyright (c) 2015-2019 steelpy

# Python stdlib imports
import math
#import datetime

# package imports
from steelpy.utils.math.rootsearch import GoalSeeker
from steelpy.utils.units.main import Units
#from steelpy.sections.tee import Tee

#-------------------------------------------------
#
#-------------------------------------------------
#
#
def Roakr_flat_plate(_a, _b, _q, _SigmaMax):
    """
    From Roark (Formulas for stress & starin)
    7th edition, table 11.4
    Case 1a.- Uniform over entire plate
    """
    units = Units()
    _ab = _a / _b
    print("a/b : ",_ab)
    #
    _beta = {'1':2874, '1.2': 0.3762, '1.4': 0.4530, '1.6': 0.5172,
             '1.8': 0.5688, '2': 0.6102, '3': 0.7134, '4': 0.7410,
             '5': 0.7476, '>5': 0.750}
    #
    if _ab > 5:
        _Beta = _beta['>5']
    
    elif _ab > 4:
        _Beta = _ab *_beta['5'] / 5.0
    
    elif _ab > 3:
        _Beta = _ab * _beta['4'] / 4.0
    
    elif _ab > 2:
        _Beta = _ab * _beta['3'] / 3.0
    
    elif _ab > 1.8:
        _Beta = _ab * _beta['2'] / 2.0
    
    elif _ab > 1.6:
        _Beta = _ab * _beta['1.8'] / 1.8
    
    elif _ab > 1.4:
        _Beta = _ab * _beta['1.6'] / 1.6
    
    elif _ab > 1.2:
        _Beta = _ab * _beta['1.4'] / 1.4
    
    elif _ab > 1:
        _Beta = _ab * _beta['1.2'] / 1.2
    
    else:
        _Beta = _ab *_beta['1'] / 1.0
    #
    _t = (_Beta * _q * _b**2 / _SigmaMax)**0.50
    return _t
#
#
def AISC_J24(PlateThck):
    """
    From AISC WSD Table J2.4
    Minimum Size of Fillet Welds
    """
    #
    # To 1/4 inclusive
    if PlateThck < 6.35:
        # Minimum zise 1/8
        _MinWeldSize = 3.20
    # Over 3/4
    elif PlateThck > 19.0:
        # Minimum zise 5/16
        _MinWeldSize = 8.0
    
    else:
        # Over 1/4 to 1/2
        if PlateThck < 12.7:
            # Minimum zise 3/16
            _MinWeldSize = 4.80
        # Over 1/2 to 3/4
        else:
            # Minimum zise 1/4
            _MinWeldSize = 6.35
    #
    return _MinWeldSize
#
class OTH88283:
    """
    """
    def __init__(self, units, material):
        """
        """
        self._units = units
        self._material = material
    #
    def factors_of_safety(self):
        """
        Factors of Safety on clamp frictional capacity
        and lift-off
        """
        if "OPERATING" in self.design_condition.upper():
            self.gamma_friction = 2.25
            self.gamma_lift = 2.0
            # Increase in allowable stresses
            # for steel design
            self.Fy_factor = 1.0
        else:
            self.gamma_friction = 1.70
            self.gamma_lift = 1.50
            # Increase in allowable stresses
            # for steel design
            self.Fy_factor = 1.33
        print("-------------------------------------")
        print('Bolt load factors of Safety for {:} condition'
              .format(self.design_condition))
        print('gamma friction : {:1.3f}'.format(self.gamma_friction))
        print('gamma lift : {:1.3f}'.format(self.gamma_lift))
        print('Fy factor  : {:1.3f}'.format(self.Fy_factor))
    #
    def internal_frictional_force(self):
        """
        
        Assume that longitudinal force F1 is
        carried by one clamp shell and torsional
        moment M1 by both shells.
        """
        _actions = self.substrate.actions
        # Resultant longitudinal interfaceforce 
        # on one shell:
        self.Pp = (_actions.Fx**2 
                   + (_actions.Mx / self.substrate.section.D)**2)**0.50
        print(" ")
        print("Internal Frictional Force")
        print ("Pp = {:1.3f} N".format(self.Pp.convert('newton').value))
        
        # Resultant interface shear stress
        # on one shell:
        self.tau_is = self.Pp / self.As
        print ("Tau is = {:1.3f} MPa"
               .format(self.tau_is.convert('megapascal').value))
    #
    def tension_force_pull_clamp_shell(self):
        """
        Assume that loads F2, F3, M2 & M3
        are carried by one clamp shell:
        The area over which these forces act
        are as follows: F2 over half the clamp
        (one shell), F3 and M3 over quarter 
        clamp (half of shell) and M2 over 1/8th
        of  the clamp (1/4 of shell).
        
        """
        # Resultant tensile pressure stress on one
        _actions = self.substrate.actions
        # shell becomes:
        self.tau_P = (((_actions.Fy/self.As) + 
                       (6 * _actions.Mz/(self.shell.Lc*self.As)))**2 + 
                      (2 * _actions.Fz/self.As + 
                       (12 * _actions.My/(self.shell.Lc*self.As)))**2)**0.50
        #
        print(" ")
        print("Tension pressure pulling clamp shells apart")
        print("Tau P = {:1.3f} MPa".format(self.tau_P.convert('megapascal').value))
    #
    def number_bolts_required(self):
        """
        """
        #
        # The initial pretension in one
        # bolt is given by:
        self.Fpt = self.bolt.fp * self.bolt.bolts.material.Fy * self.bolt.bolts.tensile_area
        print(" ")
        print("Required Number of Bolts")
        print("Pretension factor fp : {:1.3f}".format(self.bolt.fp))
        print("Fp = {:1.3f} N".format(self.Fpt.convert('newton').value))
        #
        # Assume 15% relaxation in bolt
        # load over time. Long term bolt load:
        _Fb = 0.85 * self.Fpt
        print("Fb = {:1.3f} N".format(_Fb.convert('newton').value))
        #
        #
        _nest1 = (self.tau_is * self.gamma_friction *
                  self.shell.Dci * self.shell.Lc / (self.neoprene.Mu * _Fb))
        #
        # The number of bolts required per clamp is:
        if self.tau_P.value < self.tau_is.value:
            _nest2 = _nest1
        else:
            _nest2 = (self.tau_P * self.gamma_lift *
                      self.shell.Dci * self.shell.Lc / _Fb)
        #
        # Bolts for side
        if _nest2.value < 3 :
            _nb = 3
        else:
            _nb = math.floor(_nest2)
        #
        print("Min bolts for side =", _nb)
        #
        if self.shell.Nbp == 0:
            self.shell.Nbp = _nb
        print("Assume Bolts for side = ", self.shell.Nbp)
        #
        # Assuming Short Term/Immediate relaxation
        # on each bolt of 0.003"on a bolt length of
        # 580mm, set bolt pre-tension:
        _BoltRelax = 0.003 * 25.40
        self.Fset = (self.Fpt - (_BoltRelax * self.bolt.material.E
                                 * self.bolt.bolts.tensile_area / 580.0))
        #
        #print (_BoltRelax * self.Eb * self.Ab / 580.0)
        print("bolt pre-tension = {:1.3f} N"
              .format( self.Fset.convert('newton').value))
        #
    #
    def clamp_shell_thickness_required(self):
        """
        The required clamp shell thickness is 
        calculated based on the worst stress 
        distribution through the shell thickness
        based on hoop stress. 
        The uniform pressure exerted on the clamp
        shells by the bolt pretension is given by
        the largest value based on:
        i)  The complete shell - Using Length Lc
        ii) The smallest strip between stiffeners
            Using Length Lst
        """
        _substrate = self.substrate.section
        #
        # Smallest stiffener spacing :
        _Lstmin = self.shell.Lc / self.shell.Nbp
        if self.Lst.value == 0:
            self.Lst = _Lstmin
        #
        print(" ")
        print("Required Clamp Shell Thickness")
        print ("Actual stiffener spacing {:1.3f} mm"
               .format(self.Lst.convert('millimetre').value))
        #
        # Uniform clamping pressure on both shells
        # (Baxter Brown p. 253):
        if (self.shell.Nbp *self.Fpt/(_substrate.D*self.shell.Lc)).value < (self.Fpt/(_substrate.D*self.Lst)).value:
            self.q = 2.0 * self.Fpt / (_substrate.D * self.Lst)
        else:
            self.q = 2.0 * self.Fpt / (_substrate.D * self.Lst)
        print ("q = {:1.3f} MPa".format(self.q.convert('megapascal').value))
        #
        # Limit hoop stress in the shells to a basic 
        # allowable of 60% of yield. Assume a minimum
        # initial wall thicknessof 10 mm. 
        # The required shell thickness based on thick
        # wall theory (conservative) is:
        _Dest = self.shell.Dci + 20.0 * self.units.mm
        #
        # Estimated clamp external diameter:
        _ttk = (_Dest / 2.0 * 
                ((0.60 * self.Fy_factor * self.shell.material.Fy.value / 
                  ((0.60 * self.Fy_factor * self.shell.material.Fy.value) 
                            - 2 * self.q.value))**0.50 - 1.0))
        # check if ttk is < than min thck
        if _ttk.convert('millimetre').value < 15 :
            _ttk = 15.0 * self.units.mm
        else:
            _ttk = _ttk
        print("ttk : {:1.3f} mm".format(_ttk.convert('millimetre').value))
        #
        # Adopt clamp shell thickness of: 
        #
        if self.shell.tc.value == 0.0:
            self.shell.tc = _ttk
        print ("Adopt clamp shell thickness of: {:1.3f} mm"
               .format(self.shell.tc.convert('millimetre').value))
        #
        #
        _AnularGap = _substrate.D * 0.02
        print("AnularGap = {:1.3f} mm"
              .format(_AnularGap.convert('millimetre').value))
        #
        _Dci = _substrate.D + _AnularGap
        #
        # Clamp Check
        # Check if Internal Diameter of clamp
        # was given
        if self.shell.Dci == 0:
            self.shell.Dci = _Dci
        #
        print("Clamp Shell ID/Dci = {:1.3f} mm"
              .format(self.shell.Dci.convert('millimetre').value))
        #
        #
        # Clamp external diameter:
        self.shell.Dc = self.shell.Dci + 2.0 * self.shell.tc
        #
        print("Clamp internal diameter: {:1.3f} mm"
              .format(self.shell.Dci.convert('millimetre').value))
        print("Clamp External diameter: {:1.3f} mm"
              .format(self.shell.Dc.convert('millimetre').value))
        #
        # Hoop stress in clamp shell:
        if self.shell.Dc.value/self.shell.tc.value > 20.0:
            self.sigma_h = self.q * self.shell.Dc / (2*self.shell.tc)
        else:
            self.sigma_h = (2 * self.q * self.shell.Dc**2
                            / (self.shell.Dc**2 - self.shell.Dci**2))
        #
        print("Hoop stress in clamp shell: {:1.3f} MPa"
              .format(self.sigma_h.convert('megapascal').value))
    #
    def neoprene_thickness_check(self):
        """
        """
        #
        # Neoprene width
        _Bn = (math.pi * self.shell.Dci / 2.0)
        print(" ")
        print("Check Neoprene Thickness")        
        print("Neoprene width: {:1.3f} mm".format(_Bn.convert('millimetre').value))
        #
        # Shape Factor
        _S = ((self.neoprene.Ln.value * _Bn.value) 
              / (2 * self.neoprene.tn.value * (self.neoprene.Ln.value + _Bn.value)))
        print("Shape Factor : {:1.3f}".format(_S))
        #
        # Compression modulus For S > 3
        if _S > 3.0 :
            _Ecm = (5.0 * self.neoprene.material.G * _S**2)
            print("Compression modulus : {:1.3f} MPa"
                  .format(_Ecm.convert('megapascal').value))
        else:
            raise RuntimeError("S < 3 ==> Ecm Fail")
        #
        # If Ecm/Ealpha > 0.1 use modified value of
        # compression modulus as follows :
        if (_Ecm / self.neoprene.E_alpha).value > 0.1:
            _Ec = _Ecm / (1 + (_Ecm / self.neoprene.E_alpha))
        else:
            _Ec = _Ecm
        print("Ec : {:1.3f} MPa".format(_Ec.convert('megapascal').value))
        #
        # Deflection of neoprene
        self.Xn = (self.q * self.neoprene.tn / _Ec)
        print("Deflection of neoprene (Xn) : {:1.3f} mm"
              .format(self.Xn.convert('millimetre').value))
        #
        # Compressive strain in neoprene should be
        # kept to 10% to avoid non-linear behaviour
        if self.Xn.value/self.neoprene.tn.value > 0.1:
            _Xn_tn = "FAIL"
        else: 
            _Xn_tn = "OK"
        #
        print ("Xn/tn : {:1.3f}".format(self.Xn.value/self.neoprene.tn.value))
        print(_Xn_tn)
    #
    #-------------------------------------------------
    #       Discontinuous Flange Clamp Section
    #-------------------------------------------------
    #
    def stiffener_plate_height(self):
        """
        
        Stiffener plate height to prevent shell lift off
        Reference - DJ Williams - Recent clamps on the
        forties platform (Naval Architect 1988)
        """
        #
        print(" ")
        print("Stiffener Plate Height")
        #
        # Find bolt spacing
        if self.shell.stiffener.Bsp.value == 0:
            try:
                self.shell.stiffener.Bsp = self.shell.Lc / self.shell.Nbp
            except ZeroDivisionError: 
                pass        
        #
        # eccentricity
        #if self.Bsp != 0 :
        #    _eBolt = ((self.Bsp/2.0) 
        #              - (math.cos(math.radians(180 - self.thetaC) / 2.0)
        #                 * ((self.tubular.Dci + self.tc)/2.0)))
        #
        #if self.stiffener.e.value == 0:
        #    self.stiffener.e = elf.stiffener.Bsp / 2.0
        #print("Min eccentricity e : {:1.0f} mm"
        #      .format(self.stiffener.e.convert('millimetre').value))
        #print("Min Bolt eccentricity :", _eBolt)
        
        # Bolt eccentricity default
        try:
            self.shell.stiffener.eBolt.value
        except AttributeError:
            self.shell.stiffener.eBolt = self.bolt.bolts.spherical_washer_B
        
        print("Assumed Bolt eccentricity : {:1.0f} mm"
              .format(self.shell.stiffener.eBolt.convert('millimetre').value))
        
        # pull-up gap
        _Hgap = (math.sin(math.radians(180 - self.thetaC)) 
                 * ((self.shell.Dci + self.shell.tc)/2.0))
        print("Min pull-up gap : {:1.3f} mm".format(_Hgap.convert('millimetre').value))
        #
        if self.shell.Hgap.value == 0 :
            self.shell.Hgap = _Hgap
        print("Assumed pull-up gap (Hgap) : {:1.0f} mm"
              .format(self.shell.Hgap.convert('millimetre').value))
        
        # To avoid lift-off
        _alpha = math.atan(2*self.shell.Hgap.value / self.shell.Dc.value)
        print("alpha : {:1.3f} degrees".format(math.degrees(_alpha)))
        
        _beta = (_alpha + (4 * _alpha**2 
                           + (12 * self.shell.stiffener.eBolt.value 
                              / self.shell.Dc.value))**0.50)
        print('To avoid lift off')
        print("beta : {:1.0f} degrees".format(math.degrees(_beta)))
        
        # Height of stiffener:
        _hest = (((self.shell.Dc/2.0)*math.sin(_beta + _alpha)))
        #         - self.Hgap)
        print("Minimum Height of stiffener : {:1.0f} mm"
              .format(_hest.convert('millimetre').value))
        
        # check if h was user defined
        if self.shell.stiffener.h.value == 0:
            self.shell.stiffener.h = _hest
        print("Adopt stiffener height of : {:1.0f} mm"
              .format(self.shell.stiffener.h.convert('millimetre').value))
        
        # Modified Value for beta
        # These affect the weld design
        
        # Cf is an input svo
        if self.shell.stiffener.Cf.value == 0:
            self.shell.stiffener.Cf = 2 * self.shell.stiffener.eBolt
        print ("Cf : {:1.3f} mm".format(self.shell.stiffener.Cf.convert('millimetre').value))
        
        _b = self.shell.stiffener.Cf + self.shell.stiffener.eBolt
        print("b : {:1.3f} mm , Cf : {:1.3f} mm "
              .format( _b.convert('millimetre').value, 
                       self.shell.stiffener.Cf.convert('millimetre').value))
        # 
        # Total width of bolt bearing plate
        #self.Bf = self.eBolt + 0.50*self.tubular.Dc*(1.0 - math.cos(self.beta))
        #print("Width of bolt bearing plate Bf :", self.Bf)
        
        # assume tbp = tc
        _tbp = self.shell.tc
        #
        if self.Bf.value == 0:
            self.Bf = ((0.50 * self.shell.Dc + self.shell.stiffener.eBolt) 
                       - (((self.shell.Dc/2.0)**2 - (self.shell.stiffener.h - _tbp)**2 ))**0.50)
        print("Width of bolt bearing plate Bf : {:1.3f} mm"
              .format(self.Bf.convert('millimetre').value))
        
        # Total width of flange plate
        _Bbp = self.shell.stiffener.Cf + self.Bf
        print("Min width of flange plate : {:1.3f} mm"
              .format(_Bbp.convert('millimetre').value))
        
        try:
            self.shell.stiffener.Bbp.value
        except AttributeError:
            self.shell.stiffener.Bbp = _Bbp
        #
        if self.shell.stiffener.Bbp.value < _Bbp.value:
            self.shell.stiffener.Bbp = _Bbp
        print("Assume flange plate Bbp : {:1.3f} mm"
              .format(self.shell.stiffener.Bbp.convert('millimetre').value))
        
        # Check Solution
        print("----------------")
        print('Check Solution')
        # self.alpha = _alpha
        # self.beta = math.asin((2*self.stiffener.h.value / self.shell.Dc.value))
        self.beta = (_alpha + (4 * _alpha**2 
                               + (12 * self.shell.stiffener.eBolt.value 
                                  / self.shell.Dc.value))**0.50)
        print("beta : {:1.0f} degrees".format(math.degrees(self.beta)))
        #print('---->',math.degrees(1.0 - math.cos(self.beta)),math.degrees(math.cos(self.beta)))
        #
        # Arc length
        _r = (self.shell.Dc - self.shell.tc) / 2.0
        _L = self.beta * _r
        print("Arc length : {:1.0f} mm"
              .format(_L.convert('millimetre').value))
        
        # Clamp Radial Pessure, Rp
        _substrate = self.substrate.section
        _Rp = ((2 * self.shell.Nbp * self.Fpt) 
               / (self.shell.Lc * (_substrate.D * 0.50 * math.pi - 2 * self.shell.Hgap)))
        print('Rp : {:1.2f} MPa'.format(_Rp.convert('megapascal').value))
        
        _F1 = _Rp * _L * self.Lst
        print('F1 : {:1.2f} kN'.format(_F1.convert('kilonewton').value))
        
        # Moment at point x
        # Overturning moment
        _OTM = self.Fpt * (self.shell.stiffener.eBolt + self.shell.tc)
        print('OTM : {:1.2f} kN*m'.format(_OTM.convert('kilonewton*metre').value))
        
        # Restoring Moment
        _LR = 0.50 * _L
        _RM = _F1 * _LR
        print('Restoring Moment : {:1.2f} kN*m'.format(_RM.convert('kilonewton*metre').value))
        #
        if _RM.value < _RM.value :
            print('FAIL  ==> Restoring moment < OTM')
        else:
            # No lift off occurs and hence solution accurate
            print('OK  ==> Restoring moment > OTM')
    #
    def flange_plate_check(self):
        """
        The required bearing plate thickness will
        be determined using Yield Line - Work
        Method of Analysis.
        The bearing plate area at the outer bolt
        location is critical for the design, as
        there is no plate continuity over the
        outer clamp stiffener.
        """
        print(" ")
        print("Flange Plate Check")
        print("Fpt : {:1.3f} kN".format(self.Fpt.convert('kilonewton').value))
        #
        _af = self.Lst / 2.0
        print("af : {:1.3f} mm".format(_af.convert('millimetre').value))
        # provisional svo
        #self.Bbp = 130
        #self.Bf = 80
        #_af = 100
        print("Bbp {:1.3f} mm, bf : {:1.3f} mm"
              .format(self.shell.stiffener.Bbp.convert('millimetre').value, 
                      self.Bf.convert('millimetre').value))
        # Use yield line theory :
        #
        #              E = D
        #
        # E = Expended External Energy
        # D = Disipated Internal Energy
        # L = Length of yield line or projected length
        #     onto axis of rotation
        # phi = Roration of the region about its axis
        #       of rotation
        #
        # Disipation of internal energy in the yield lines
        # D = Sum(m x L x phi) ==>
        
        # Region A = m x L1 x phiA
        _RegionA = self.Lst * 1.0 / self.Bf
        # m' = m
        _RegionA = 2 * _RegionA
        #print('_RegionA', _RegionA)
        
        # Region B = m x Bbp x phiB
        _RegionB = self.shell.stiffener.Bbp * 1.0 / _af
        #print('_RegionB', _RegionB)
        
        # Region C = m x Bbp x phiC
        # m' = m
        _RegionC = 2 * (self.shell.stiffener.Bbp * 1.0 / _af)
        
        # Sum
        _summ = _RegionA  + _RegionB + _RegionC
        print('Summ : {:1.3f}'.format(_summ.value))
        
        # E = sum(N x delta)
        # delta = 1.0 (at point load application)
        #
        # **1.84 = WSD FoS + 10% allowance for simplified
        #          yield line solution (ie 1.1/0.60 = 1.84)
        # E = 1.84 x FPT
        
        # Therefore the required bearing plate thickness
        _tbp = ((4.0 * 1.84 * self.Fpt) / (_summ * self.shell.material.Fy))**0.50
        print("required bearing plate thickness tbp : {:1.0f} mm"
              .format(_tbp.convert('millimetre').value))
        
        # Adopt flange thickness
        # check if bearing plate was given by user
        try:
            self.shell.stiffener.tbp.value
        except AttributeError:
            self.shell.stiffener.tbp = _tbp
        
        # For good practice bearing plate should be as thick as
        # the clamp shell
        if self.shell.stiffener.tbp.value < self.shell.tc.value:
            self.shell.stiffener.tbp = self.shell.tc
        #
        print("Adopt flange thickness : {:1.0f} mm"
              .format(self.shell.stiffener.tbp.convert('millimetre').value))
        
    #
    def stiffener_plate_design(self):
        """
        The stiffener thickness is calculated based on buckling
        and yielding criteria.  
        The point load due to the bolt is assumed to be uniformly
        distributed at the edges of the flange plate . 
        The maximum load is determined using formula for continuos
        beams. 
        """
        #
        print(" ")
        print("Stiffener Plate Design")

        # As a conservative assumption this load is taken as 1.5 Fp
        self.Fp_factor = 1.50
        
        # rat hole to be defined chava
        self.rathole = 25.0 * self.units.mm
        
        # The required stiffener thickness is based on an allowable
        # stress of 60% of yield.
        
        # ----------------
        # A. Buckling
        print("----------------")
        print("A. Buckling")
        
        # The critical buckling stress in a plate is given by 
        #(ref. Blodgetp. 2.12-1):
        # Sigmacr = kp2E/[12(1-n2)](tcr/bf)2 = F/(bft)
        
        _BL = self.shell.stiffener.Bbp - self.rathole
        _h = (self.shell.stiffener.h - 0.50 * self.shell.stiffener.tbp)
        #print('Bbp ', self.Bbp, _BL, _h, self.tbp, self.Bbp)
        
        # The buckling coefficient k for a triangular plate is
        #(ref. Structural Steel Design p. 587):
        _k = (3.2 - 3.0*(_BL/(_h - self.shell.Hgap)) 
              + 1.10*((_BL/(_h - self.shell.Hgap))**2))
        print("k = {:1.3f}".format(_k.value))
        
        # Reducing the allowable critical stress to 60% of
        # the above value the critical stiffener thickness is
        _tcrb = (((12 * self.Fp_factor * self.Fpt * self.shell.stiffener.Bbp 
                  * (1 - self.shell.material.poisson**2) 
                  / (0.6*self.Fy_factor*_k*self.shell.material.E*math.pi**2)))**(1.0/3.0))
        print("tcrb : {:1.3f} mm".format(_tcrb.convert('millimetre').value))
        #
        # ----------------
        # B.     Yielding
        print("----------------")
        print("B. Yielding")
        #
        # The line of action of he force is approximately 
        # parallel to the stiffener edge and the critical 
        # cross section is at right angles to this (section x-x).
        
        # Cf is an input svo
        _b = self.shell.stiffener.Cf + self.shell.stiffener.eBolt
        
        # Angle
        _phi = math.atan(_b.value / (self.shell.stiffener.h.value 
                                     - self.shell.Hgap.value 
                                     - self.shell.stiffener.tbp.value))
        print("Phi : {:1.3f} degrees".format(math.degrees(_phi)))
        #print('-->',self.Fp_factor,self.Fy_factor, self.shell.material.Fy, _BL)
        
        _tcry = ((self.Fp_factor * self.Fpt) 
                 /(0.6*self.Fy_factor*self.shell.material.Fy*_BL*(math.cos(_phi))**2))
        print("Critical thickness (tcry): {:1.0f} mm".format(_tcry.convert('millimetre').value))
        
        # The required stiffener thickness is:
        _tst = _tcrb
        if _tcrb.value < _tcry.value:
            _tst = _tcry
        #else:
        #    _tst = _tcrb
        #
        print("Min stiffener thickness (tst): {:1.0f} mm"
              .format(_tst.convert('millimetre').value))
        #
        try:
            self.shell.stiffener.tsp.value
        except AttributeError:
            self.shell.stiffener.tsp = _tst
        
        # for goof practice stiffener plate should be as
        # thick as the clamp shell
        if self.shell.stiffener.tsp.value < self.shell.tc.value:
            self.shell.stiffener.tsp = self.shell.tc
        #
        print("Adopt stiffener thickness of : {:1.0f} mm"
              .format(self.shell.stiffener.tsp.convert('millimetre').value))
        
    #
    def weld_design(self, Fy_weld=480):
        """
        """
        #
        print(" ")
        print("Design of Welds")
        #
        #self.rathole = 25.0
        self.Fy_weld = Fy_weld * self.units.MPa
        #
        print("----------------")
        print("Weld A - Stiffener plate to clamp shell")
        # Stiffener to Shell  Weld A:
        # Stiffener plate to clamp shell
        #
        _beta1 = math.asin((self.shell.stiffener.h.value 
                            - self.shell.stiffener.tbp.value)
                           / (0.50 * self.shell.Dc.value))
        print('Beta1 : {:1.3f} degrees'.format(math.degrees(_beta1)))
        
        # length of the weld minus rat hole
        _LwA = ((0.50 * self.shell.Dc * 
                (_beta1 - self.alpha)) - self.rathole)
        
        print("Length of the weld - LwA : {:1.3f} mm"
              .format(_LwA.convert("millimetre").value))
        
        # Load required in weld (2 No fillet welds)
        _Fv = self.Fpt / (2 * _LwA)
        print("Fv = {:1.3f} kN/m".format(_Fv.convert('kilonewton/metre').value))
        
        # Required fillet weld size (E70 electrode)
        # Allowable weld stress
        _fwallow = 0.30 * self.Fy_weld
        print('Allowable weld stress : {:1.0f} MPa'
              .format(_fwallow.convert('megapascal').value))
        
        # Required weld leg length for fillet
        _Fwreq = _Fv * math.sqrt(2.0) / _fwallow
        print('Required weld leg length for fillet : {:1.3f} mm'
              .format(_Fwreq.convert("millimetre").value))
        
        _MinWeldSize = (AISC_J24(self.shell.stiffener.tsp.convert('millimetre').value)
                        * self.units.mm)
        #
        if self.FwminWA.value <_Fwreq.value :
            self.FwminWA = _Fwreq
        
        if self.FwminWA.value < _MinWeldSize.value :
            self.FwminWA = _MinWeldSize
        #
        print('Adopt minimum weld at A : {:1.3f} mm'
              .format(self.FwminWA.convert("millimetre").value))
        #
        print("----------------")
        print("Weld B - Bearin plate to shell")
        # Flange to Stiffener Weld B
        # Bearing plate to stiffener plate
        # For one stiffener-flange interface the
        # length of weld is:
        #_LwB = 2 * self.Bbp
        #print("Length of weld - LwB :", _LwB )
        # Choose Flange to stiffener weld to be a
        # butt weld in this case.
        
        # Section properties of effective T-section
        
        _L1 = self.Lst
        _BL = (self.shell.stiffener.Bbp - self.rathole)
        #
        # Stiffener Depth
        _Sd = (self.shell.stiffener.h - self.shell.Hgap - self.shell.stiffener.tbp)
        print('Stiffener Depth : {:1.0f} mm'.format(_Sd.convert("millimetre").value))
        
        # Area of flange
        _Af = (_L1 * self.shell.stiffener.tbp)
        print('Area of flange : {:1.0f} mm^2'.format(_Af.convert("millimetre^2").value))
        #
        # Area of web
        _Aw = ( _Sd * self.shell.stiffener.tsp)
        print('Area of web : {:1.0f} mm^2'.format(_Aw.convert("millimetre^2").value))
        #
        #_H = (self.stiffener.h - self.shell.Hgap)
        T_section = self.shell.stiffener.T_section
        T_section.H = self.shell.stiffener.h - self.shell.Hgap
        T_section.t_web = self.shell.stiffener.tsp
        T_section.base = self.Lst
        T_section.t_flange = self.shell.stiffener.tbp
        T_section.properties
        #_Area, _L2, _Ixx, _Zxx, _rx  = Tsection(_L1, self.stiffener.tbp, _H, 
        #                                        self.stiffener.tsp)
        #
        # Shear stress in weld is given by the general
        # formula: f = V.a.y / I.t
        # then shear stress per weld
        _fv = ((self.Fpt * _Af 
                * (T_section.Zc 
                   - 0.50 * self.shell.stiffener.tbp)) 
               / (2 * T_section.Iy))
        
        print('Shear stress per weld : {:1.0f} kN/m'
              .format( _fv.convert("kilonewton/metre").value))
        
        # Bearing stress per weld
        _fb = self.Fpt / (2 * _BL )
        print('Bearing stress per weld (fb) kN/m: {:1.3f} kN/m'
              .format(_fb.convert("kilonewton/metre").value))
        
        # Resultant weld stress
        _fr = (_fv**2 + _fb**2)**0.50
        print('Resultant weld stress (fr): {:1.3f} kN/m'
              .format(_fr.convert("kilonewton/metre").value))
        
        # Required weld leg length for fillet
        _FwreqB = _fr * 2.0**0.50 / _fwallow
        print('Required weld leg length for fillet : {:1.3f} mm'
              .format(_FwreqB.convert("millimetre").value))
        #
        _MinWeldSize = (AISC_J24(self.shell.stiffener.tbp.convert('millimetre').value) 
                        * self.units.mm)
        #
        if self.FwminWB.value <_FwreqB.value :
            self.FwminWB = _FwreqB
        
        if self.FwminWB.value < _MinWeldSize.value :
            self.FwminWB = _MinWeldSize
        #
        print('Adopt minimum weld at B : {:1.3f} mm'
              .format(self.FwminWB.convert("millimetre").value))
        print("----------------")
        print("Weld C")
        # Weld C
        # Since a natural preparation is formed between
        # the bolt bearing plate and the clmap shell
        # at location C, a full penetration butt well
        # will be provided at this location.
        # Since this is strong as the parent material
        # no design check is required
        print("Provide full penetration butt weld")
        #
        # There are two (deliberate?) mistakes in Weld B btw.
        # I think he takes the wrong length of the flange 
        # (BL instead of L1) and I think also he uses ^2 instread of ^3 
        # for the Ixx calc - perhaps I am wrong?
    #
    #-------------------------------------------------
    #         Continuous Flange Clamp Section
    #-------------------------------------------------
    #
    def stiffener_plate_height2(self):
        """
        Stiffener plate height to prevent shell lift off
        Reference - DJ Williams - Recent clamps on the
        forties platform (Naval Architect 1988)
        """
        _substrate = self.substrate.section
        #
        print('--------------------------------')
        print("Stiffener Plate Height")
        if self.shell.stiffener.e.value == 0:
            self.shell.stiffener.e = self.Bsp /2.0
        #
        print("Min eccentricity e : {:1.3f} mm"
              .format(self.shell.stiffener.e.convert('millimetre').value))
        print("Assumed Bolt eccentricity : {:1.3f} mm"
              .format(self.shell.stiffener.eBolt.convert('millimetre').value))
        
        _angle = (30*math.pi/180)
        _Adjacent = math.cos(_angle) * (0.50 * self.shell.Dc)
        _Opposite = math.sin(_angle) * (0.50 * self.shell.Dc)
        
        # Total width of flange plate
        _Bf = ((0.50*self.shell.Dc + self.shell.stiffener.eBolt) 
               - _Adjacent + 0.5 * self.shell.tc)
        print("===> Min width of flange plate : {:1.3f} mm"
              .format(_Bf.convert('millimetre').value))
        #
        # Height of stiffener:
        _TopPlateHeight = (_Opposite + _Adjacent- 0.50*self.shell.Dc)
        _TopPlateBase = (2 * _Adjacent)
        
        print("*** Top Plate Heigth : {:1.3f} mm"
              .format(_TopPlateHeight.convert('millimetre').value))
        print("*** Top Plate Base : {:1.3f} mm"
              .format(_TopPlateBase.convert('millimetre').value))
        
        # Height of stiffener:
        _hest = (_Adjacent - 0.50*self.shell.tc)
        #
        print("---> Minimum Height of stiffener : {:1.3f} mm"
              .format(_hest.convert('millimetre').value))
        
        # check if h was user defined
        try:
            self.shell.stiffener.h.value
        except AttributeError:
            self.shell.stiffener.h = _hest
        #
        print("Adopt stiffener height of : {:1.3f} mm"
              .format(self.shell.stiffener.h.convert('millimetre').value))
        
        # Total width of bolt bearing plate
        if self.Bf.value == 0:
            self.Bf = _Bf
        
        #print("_Bf2 :", self.Bf)
        print("Width of bolt bearing plate Bf : {:1.3f} mm"
              .format(self.Bf.convert('millimetre').value))
        
        # TODO: Cf is an input svo ??
        try:
            self.shell.stiffener.Cf.value
        except AttributeError:
            self.shell.stiffener.Cf = 2 * self.shell.stiffener.eBolt
        print ("Cf = {:1.2f} mm"
               .format(self.shell.stiffener.Cf.convert('millimetre').value))
        
        # Total width of flange plate
        _Bbp = self.shell.stiffener.Cf + self.Bf
        print("Min width of flange plate : {:1.3f} mm"
              .format(_Bbp.convert('millimetre').value))
        
        try:
            self.shell.stiffener.Bbp.value
        except AttributeError:
            self.shell.stiffener.Bbp = _Bbp
        
        if self.shell.stiffener.Bbp.value < _Bbp.value:
            self.shell.stiffener.Bbp = _Bbp
        print("Assume flange plate Bbp : {:1.3f} mm"
              .format(self.shell.stiffener.Bbp.convert('millimetre').value))
        #
        try:
            self.top_plate_base.value
        except AttributeError:
            self.top_plate_base = _TopPlateBase
        print("Adopt Total Top Plate Base : {:1.3f} mm"
              .format(self.top_plate_base.convert('millimetre').value))
        
        # Check Solution
        print("----------------")
        print('Check Solution')
        
        _alpha = math.atan(2 * _Opposite.value / self.shell.Dc.value)
        
        self.beta = (90 * math.pi / 180) - _alpha 
        # Arc length
        _r = ((self.shell.Dc - self.shell.tc)/2.0)
        _L = self.beta * _r
        print('Arc length : {:1.3f} mm'
              .format(_L.convert('millimetre').value))
        
        # Clamp Radial Pessure, Rp
        _Rp = ((2 * self.shell.Nbp * self.Fpt) 
               / (self.shell.Lc * (_substrate.D * 0.50 * math.pi 
                                   - 2 * self.shell.Hgap)))
        
        print('Rp : {:1.3f} MPa'.format(_Rp.convert('megapascal').value))
        
        _F1 = _Rp * _L * self.Lst
        print('F1 : {:1.3f} kN'.format(_F1.convert('kilonewton').value))
        
        # Moment at point x
        # Overturning moment
        _OTM = self.Fpt * (self.shell.stiffener.eBolt + self.shell.tc)
        print('OTM : {:1.3f} kN*m'.format(_OTM.convert('kilonewton*metre').value))
        
        # Restoring Moment
        _LR = 0.50 * _L
        _RM = _F1 * _LR
        print('Restoring Moment : {:1.3f} kN*m'
              .format(_RM.convert('kilonewton*metre').value))
        #
        if _RM.value < _RM.value :
            print('FAIL  ==> Restoring moment < OTM')
        else:
            # No lift off occurs and hence solution accurate
            print('OK  ==> Restoring moment > OTM')
    #
    def clamp_top_plate_hydro_check(self):
        """
        From Roark (Formulas for stress & starin)
        7th edition, table 11.4
        Case 1a.- Uniform over entire plate
        """
        #
        print("")
        print ("Check Clamp Top Plate for Hydro Loading")
        #
        _a = self.Lc
        _b = self.top_plate_base
        #_q = 0.533
        _q = self.Po
        print ('q max :', _q)
        _SigmaMax = (0.75 * self.shell.material.Fy)
        
        _t = Roakr_flat_plate(_a, _b, _q, _SigmaMax)
        
        print ("t required by Hydro Loading: ",_t)
        #
    #
    def stiffener_plate_design2(self):
        """
        """
        # Section properties of effective T-section
        #
        _b = self.Lst
        #_tf = self.tbp = 25
        _tf = self.shell.stiffener.tbp
        #_b = 200
        _h = self.shell.stiffener.h
        #_tw = self.tsp = 20
        _K = 1.0
        _Uc = 1.0
        T_section = self.shell.stiffener.T_section
        try:
            _tw  = self.shell.stiffener.tsp
            #_A, _Yc, _Ixx, _Zxx, _rx = Tsection(_b, _tf, _h, _tw)
            T_section.H = _h
            T_section.t_web = _tw
            T_section.base = _b
            T_section.t_flange = _tf
            T_section.properties
            #
            _fa = self.Fpt/ T_section.area
            
            _Cc = (2 * self.shell.material.E * math.pi**2
                   / self.shell.material.Fy)**0.50
            
            _Fa = (((1 - ((_K * _h.value / T_section.ry.value)**2)/(2*_Cc)) 
                    * self.shell.material.Fy) 
                   / (5/3. + (3 * (_K * _h.value / T_section.ry.value) / (8*_Cc)) 
                      - ((_K * _h.value / T_section.ry.value)**3 /( 8*_Cc**3))))
            
            _Ufa = _fa.value / _Fa.value
            _Mb = (105 * self.units.mm - T_section.Zc ) * self.Fpt
            _fb = _Mb / T_section.Zey
            _Sigmaf = _fa + _fb
            _Uc = _Sigmaf.value / (0.60 * self.shell.material.Fy.value)
        except AttributeError:
            _tw  = 10 * self.units.mm
            while _Uc > 0.80:
                _tw += 5 * self.units.mm
                #_A, _Yc, _Ixx, _Zxx, rx = Tsection.geometry(_b, _tf, _h, _tw)
                T_section.H = _h
                T_section.t_web = _tw
                T_section.base = _b
                T_section.t_flange = _tf
                T_section.properties
                #
                _fa = self.Fpt/ T_section.area
                _Cc = (2 * self.shell.material.E * math.pi**2 
                       / self.shell.material.Fy)**0.50
        
                _Fa = (((1 - (_K * _h / T_section.ry)**2 / (2*_Cc)) 
                        * self.shell.material.Fy) 
                       / (5/3. + ((3 * _K * _h / T_section.ry)/(8*_Cc))
                          - (((_K * _h / T_section.ry)**3)/(8*_Cc**3))))
        
                _Ufa = _fa.value / _Fa.value
                _Mb = (105 * self.units.mm - T_section.Zc ) * self.Fpt
                _fb = _Mb / T_section.Zey
                _Sigmaf = _fa + _fb
                _Uc = _Sigmaf.value / (0.60 * self.shell.material.Fy.value)
            self.shell.stiffener.tsp = _tw 
        #
        print("")
        print('b : {:1.2f} mm, tf : {:1.2f} mm'
              .format(_b.convert('millimetre').value, 
                      _tf.convert('millimetre').value))
        print('h : {:1.2f} mm, tw : {:1.2f} mm'
              .format(_h.convert('millimetre').value, 
                      _tw.convert('millimetre').value))
        print("Adopt Stiffener thickness: {:1.3f} mm"
              .format(self.shell.stiffener.tsp.convert('millimetre').value))
        print("Mb :",(105 *self.units.mm - T_section.Zc),  _Mb)
        print("fa : {:1.3f} MPa".format(_fa.convert('megapascal').value))
        print("fb : {:1.3f} MPa".format(_fb.convert('megapascal').value))
        print("Elastic Buckling Cc: {:1.3f}".format(_Cc.value))
        print("Effective Length Factor: {:1.3f}".format(_K))
        print("Allowable Compressive Stress: {:1.3f} MPa"
              .format(_Fa.convert('megapascal').value))
        print("Ufa : {: 1.3f}".format(_Ufa))
        print("Uc  : {: 1.3f}".format(_Uc))
        #
    #
    def saddle_plate_check(self):
        """
        
        The saddle plate must resist the
        hoop stress arising from the bolt
        clamping forces
        """
        _substrate = self.substrate.section
        #
        print("")
        print("Saddle Plate Check")
        print("Saddle Plate Thickness : {:1.3f} mm"
              .format(self.shell.tc.convert('millimetre').value))
        print("Bolt pretension before losses : {:1.3f} kN"
              .format(self.Fpt.convert('kilonewton').value))
        print('Bolt Pitch : {:1.3f} mm'
              .format(self.Lst.convert('millimetre').value))
        #
        _angle = (90*math.pi/180)
        # Angle from split to side plate
        _theta = _angle - self.beta 
        print('Theta : {:1.3f} degrees'.format(math.degrees( _theta)))
        #
        # Hoop Stress
        _Sigmah = ((self.Fpt/(self.Lst*self.shell.tc)) 
                   / math.cos(_theta))
        
        print("Hoop Stress: {:1.3f} MPa"
              .format(_Sigmah.convert('megapascal').value))
        
        _Ur = _Sigmah.value/(0.60*self.shell.material.Fy.value)
        
        print("Utilisation: {:1.3f}".format(_Ur))
        print("----------------")
        print("Uniform clamping pressure on both shells")
        # Uniform clamping pressure on both shells
        # (Baxter Brown p. 253):
        if (self.shell.Nbp *self.Fpt/(_substrate.D*self.shell.Lc)) < (self.Fpt/(_substrate.D*self.Lst)):
            self.q = 2.0 * (self.Fpt/(_substrate.D*self.Lst))
        else:
            self.q = 2.0 * (self.Fpt/(_substrate.D*self.Lst))
        #
        print ("q = {:1.3f} MPa"
               .format(self.q.convert('megapascal').value))
        print("----------------")
        print("Welding Check")
        print("Assume Weld thickness: {:1.3f} mm"
              .format(self.shell.stiffener.tsp.convert('millimetre').value))
        #
        _R = self.Fpt / math.cos(_theta)
        _SigmaW = (_R / (self.shell.stiffener.tsp * math.cos(_theta) * self.Lst))
        
        print("Stress in weld: {:1.3f} MPa"
              .format(_SigmaW.convert('megapascal').value))
        
        _Urw = _SigmaW.value/(0.40*self.shell.material.Fy.value)
        print("Weld Utilisation: {:1.3f}".format(_Urw))
    #
    #
    #-------------------------------------------------
    #     Clamped member collapse checks Section
    #-------------------------------------------------
    #
    def hydro_collapse_check(self):
        """
        Calculate the characteristic external pressure
        Pc required to cause collapse of a pipe when 
        external pressure is acting alone. 
        The equations of BS PD8010, Part 2 Annex G
        """
        _substrate = self.substrate.section
        #
        print ("Clamped member collapse checks")
        # (G.4) Maximun Ovality
        print("Maximun Ovality (fo) = {:1.2f} mm"
              .format(self.fo.convert('millimetre').value))
        
        # (G.3) Yield Pressure
        self.Py = (2*_substrate.material.Fy*(_substrate.t/_substrate.D))
        print ("Yield Pressure (Py) = {:1.2f} MPa"
               .format(self.Py.convert('megapascal').value))
        
        # (G.2) Elastic Critical Pressure
        self.Pe = (((2*_substrate.material.E)/(1.0 - _substrate.material.poisson**2)) 
                   * (_substrate.t/_substrate.D)**3)
        print ("Elastic Critical Pressure (Pe) = {:1.2f} MPa"
               .format(self.Pe.convert('megapascal').value))
        print ("q  (clamping) : {:1.2f} MPa".format(self.q.convert('megapascal').value))
        print ("Po (external) : {:1.2f} MPa".format(self.Po.convert('megapascal').value))        
        self.Po = self.Po + self.q 
        print ("Po + q : {:1.2f} MPa".format(self.Po.convert('megapascal').value))
        #print(self.Pe, self.Py, self.fo, self.tubular.D, self.tubular.tp)
        
        # (G.1)
        # Define Funtion 
        #
        _Pe = self.Pe.convert('megapascal').value
        _Py = self.Py.convert('megapascal').value
        _fo = self.fo.convert('millimetre').value
        _Dt = _substrate.D.value / _substrate.t.value
        #
        def function(x): 
            return (((x/_Pe) - 1.0) * ((x/_Py)**2 - 1.0) 
                    - 2 * x / _Py * (_fo * _Dt))        
        #
        # Find first root (Note that it may not be the minimum
        # use 'full' to find all roots, but process is slow)
        #self.Search = 'FAST'
        # Find Pc
        self.Pc = GoalSeeker(function, 
                             _substrate.material.Fu.convert('megapascal').value * 10, 
                             self.root_search)
        self.Pc *= self.units.MPa
        print("Pc (critical) : {:1.2f} MPa".format(self.Pc.convert('megapascal').value))
        #Ratio of external pressure to collapse pressure:
        print("Po+q / Pc     : {:1.2f}".format(abs(self.Po.value/self.Pc.value)))
        # Hoop stress in tubular due to clamping pressure:
        self.fh = (self.Po * _substrate.D / (2*_substrate.t))
        print("Hoop stress   : {:1.2f} MPa".format(self.fh.convert('megapascal').value))
    #
    def clamp_split_check(self):
        """
        Buckling capacity at clamp split
        """
        print('--------------------------------')
        print("Buckling capacity at clamp split")
        _substrate = self.substrate.section
        # Axial load offset
        G = self.shell.Hgap * 2  # gap between shells
        delta = (0.50 * _substrate.D 
                 - ((0.50 * _substrate.D)**2 - self.shell.Hgap**2)**0.50)
        print("Axial load offset : {:1.2f} mm".format(delta.convert('millimetre').value))
        # Section module for leg at clamp split
        Zc = self.shell.Lc * _substrate.t**2 / 6
        print("Z : {:1.0f} cm^3".format(Zc.convert('centimetre^3').value))
        # total clamp pretension (before bolt losses)
        Tb = 2 * self.shell.Nbp * self.Fpt
        print("Tb : {:1.2f} kN".format(Tb.convert('kilonewton').value))
        # bending stress at split
        self.fbs = Tb * delta / (2 * Zc)
        print("fbs : {:1.2f} MPa".format(self.fbs.convert('megapascal').value))
        # stress causing buckling
        self.f_buckling = self.fh + self.fbs
        print("fbuckling : {:1.2f} MPa"
              .format(self.f_buckling.convert('megapascal').value))
        # TODO
        # Allowable axial stress 
        #
    #
    def combined_stress(self):
        """
        Check for existing stresses + new clamping stresses
        """
        print('--------------------------------')
        print("Combined Stresses")        
        # _substrate = self.substrate.section
        # Axial load due to clamping hoop stress
        fah = self.substrate.material.poisson * self.fh
        print("fbs   : {:1.2f} MPa".format(fah.convert('megapascal').value))
        # TODO : combined VonMises stress
        _stress = self.substrate.stress
        _sigmab = abs(_stress.sigma_x) + abs(_stress.sigma_y) + abs(_stress.sigma_z)
        _sigmav = _stress.tau_x**2 + _stress.tau_y**2 + _stress.tau_z**2
        fcomb = ((_sigmab + self.fh)**2 + self.f_buckling**2 
                 + (_sigmab + self.fh) *  self.f_buckling 
                 + 3 * _sigmav)**0.50
        print("fcomb : {:1.2f} MPa".format(fcomb.convert('megapascal').value))
        # unity check
        fcomb_allow = 0.90 * self.substrate.material.Fy
        URcomb = fcomb.value / fcomb_allow.value
        print("URcomb : {:1.2f}".format(URcomb))
#
#
#
#