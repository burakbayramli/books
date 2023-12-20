# 
# Copyright (c) 2019-2021 steelpy
#
# Python stdlib imports
import datetime
import math
from typing import NamedTuple, Tuple

# package imports
#from steelpy.codes.process.process import ChapterResults, SummaryResults

#
#
class ChapterResults(NamedTuple):
    """
    """
    URy : float
    URz : float
    UR_flag : str
    allowable_y:float
    allowable_z:float
    #
    @property
    def status(self):
        if max(self.URy, self.URz) > 1.0:
            return "fail"
        return "pass"
#
#
class SummaryResults(NamedTuple):
    """
    """
    UR : float
    UR_flag : str
    
    @property
    def status(self):
        if self.UR > 1.0:
            return "fail"
        return "pass"
#
#
class APIwsd22ed:
    """
    6.2 ALLOWABLE STRESSES FOR CYLINDRICAL MEMBERS
    """
    #
    def __init__(self):
        """
        """
        self.name = ""
    #
    #-------------------------------------------------
    #                   Main Section
    #-------------------------------------------------
    #
    def axial_tension(self, section, material, stress):
        """
        6.2.1 Axial Tension
        """
        # The allowable tensile stress, Ft, for cylindrical members
        # subjected to axial tensile loads should be determined from:
        self.fa = max(abs(item) for item in stress.sigma_x)
        #
        #  (3.2.1-1)
        self.Ft = 0.60 * material.Fy
        #print ('Ft Allowable : {:1.2f} MPa'.format(self.Ft.convert('megapascal').value))
        #        
        # The utilization of a member Um
        URtension = abs(self.fa / self.Ft.value)
        tension_flag = '(3.2.1-1)'
        #print ('IR Tension = ',self.UmTension)
        #print (' ')
        self.axial_results = ChapterResults(URtension, URtension, tension_flag, self.Ft, self.Ft)
    #
    def axial_compression(self, Klr, section, material, stress):
        """
        6.2.2 Axial Compression
        Klr : 
        section :
        material : 
        """
        #  6.2.2.2 Local Buckling
        #fa = stress.sigma_x
        self.fa = max(abs(item) for item in stress.sigma_x)
        #
        # Unstiffened cylindrical members fabricated from structural
        # steels should be investigated for local
        # buckling due to axial compression when the D/t ratio is
        # greater than 60. When the D/t ratio is greater than 60 and less
        # than 300, with wall thickness t > 6 mm, both the
        # elastic (Fxe) and inelastic local buckling stress (Fxc) due to
        # axial compression should be determined from Eq. 6.4 and
        # Eq. 6.5 Overall column buckling should be determined
        # by substituting the critical local buckling stress (Fxe or Fxc,
        # whichever is smaller) for Fy in Eq. 6.2 and in the equation
        # for Cc.
        #
        # 6.2.2.2.2 Elastic Local Buckling Stress.
        # The elastic local buckling stress, Fxe, should be determined
        # from 6.4
        #  where
        # C = critical elastic buckling coefficient,
        # D = outside diameter m.
        # t = wall thickness m.
        #
        # The theoretical value of C is 0.6. However, a reduced value
        # of C = 0.3 is recommended for use in Eq. 6.4 to account
        # for the effect of initial geometric imperfections within API
        # Spec 2B tolerance limits.
        #
        self.C = 0.30
        self.Fxe = 2 * self.C * material.E * section.t / section.D
        #print('D/t : {:1.2f}'.format(section.D.value / section.t.value))
        #print('Fxe : {:1.2f} MPa'.format(self.Fxe.convert('megapascal').value))
        #
        # 6.2.2.2.3 Inelastic Local Buckling Stress.
        # The inelastic local buckling stress, Fxc, should be determined
        # from 6.5
        self.Fxc = self.Fxe
        _Fxc2 = (material.Fy * (1.64 - 0.23 * (section.D / section.t)**1/4))
        if self.Fxc.value > _Fxc2.value:
            self.Fxc = _Fxc2
        #self.Fxc = min(material.Fy * (1.64 - 0.23 * (section.D.value / section.t.value)**1/4),
        #               self.Fxe)
        
        if section.D / section.t <= 60:
            self.Fxc = material.Fy
            _Fymod = material.Fy
        else:
            _Fymod = self.Fxe
            if _Fymod.value > self.Fxc.value:
                _Fymod = self.Fxc
        #print('Fxc : {:1.2f} MPa'.format(self.Fxc.convert('megapascal').value))
        #
        #_Fymod = material.Fy
        #if section.D.value / section.t.value > 60:
        #    _Fymod = min(self.Fxe, self.Fxc.convert('megapascal').value)
        
        #  6.2.2.1 Column Buckling
        # E = Youngs Modulus of elasticity (MPa)
        # K = effective length factor, Section 6.3.2.4
        # l = unbraced length (m)
        # r = radius of gyration (m)
        #
        #self.Klr = max(self.Kx * self.Lx / self.r, 
        #               self.Ky * self.Ly / self.r)
        self.Klr = Klr
        #print('KLr : {:1.3f}'.format(self.Klr))
        
        self.Cc = (2 * material.E.value * math.pi**2 / _Fymod.value)**0.50
        #print('Cc : {:1.3f}'.format(self.Cc))
        # 
        # The allowable axial compressive stress, Fa, should be
        # determined from the following AISC formulas for members
        # with a D/t ratio equal to or less than 60:
        #
        if self.Klr < self.Cc :
            self.Fa = (((1 - (self.Klr**2 / (2 * self.Cc**2))) * _Fymod) 
                       / (5.0 / 3.0 + 3 * self.Klr / (8*self.Cc) 
                          - (self.Klr**3 / (8 * self.Cc**3))))
            Fa_flag = "(6.2)"
        else:
            self.Fa =  (12 * material.E * math.pi**2) / (23 * self.Klr**2)
            Fa_flag = "(6.3)"
        #print('Fa =', self.Fa, Fa_flag)
        #print ('Fa Allowable : {:1.2f} MPa'.format(self.Fa.convert('megapascal').value))
        #        
        URcomp = abs(self.fa / self.Fa.value)
        #print ('IR Compression =', self.UmComp, self.UmComp_flag)
        #print (' ')
        self.axial_results = ChapterResults(URcomp, URcomp, Fa_flag, self.Fa, self.Fa)
    # 
    def bending(self, section, material, stress):
        """
        6.2.3 Bending
        """
        #fbx = stress.sigma_y
        self.fbx = max(abs(item) for item in stress.sigma_y)
        #fby = stress.sigma_z
        self.fby = max(abs(item) for item in stress.sigma_z)
        _Dt = section.D / section.t
        # The allowable bending stress, Fb, should be determined
        # from:
        if  _Dt > 300:
            raise Warning("For D/t ratios greater than 300, refer to API Bulletin 2U.")
        elif _Dt <= 10_340 / material.Fy.convert('megapascal').value:
            self.Fb = 0.75 * material.Fy
            Fb_flag = "(6.6)"
        else:
            if _Dt > 20_680 / material.Fy.convert('megapascal').value:
                self.Fb = (0.72 - 0.58 * material.Fy * section.D / (material.E * section.t)) * material.Fy
                Fb_flag = "(6.8)"
            else:
                self.Fb = (0.84 - 1.74 * material.Fy * section.D / (material.E * section.t)) * material.Fy
                Fb_flag = "(6.7)"
        #
        #print ('Fb Allowable : {:1.2f} MPa'.format(self.Fb.convert('megapascal').value))
        # 
        URbIP = abs(self.fbx / self.Fb.value)
        URbOP = abs(self.fby / self.Fb.value)
        #self.UmBending_Flag = self.Fb_flag
        #
        #print ('IR Bending In Plane =', self.UmBendingIP)
        #print ('IR Bending Out Plane =', self.UmBendingOP)
        #print (' ')
        self.bending_results = ChapterResults(URbIP, URbOP, 
                                              Fb_flag, self.Fb, self.Fb)
    #
    def shear(self, section, material, stress):
        """
        6.2.4 Shear
        where :
        fv = the maximum shear stress, MPa
        V = the transverse shear force, MN,
        A = the cross sectional area, m2.
        """
        #
        #fvx = stress.tau_y
        self.fvx = max(abs(item) for item in stress.tau_y)
        #fvy = stress.tau_z
        self.fvy = max(abs(item) for item in stress.tau_z)
        # In Plane
        #self.fvy = self.Vip / (0.50 * section.area)
        # Out Plane
        #self.fvx = self.Vop / (0.50 * section.area)
        #
        # The allowable beam shear stress, Fv, should be determined
        # from:
        #
        self.Fv = 0.40 * material.Fy
        #print ('Fv Allowable : {:1.2f} MPa'.format(self.Fv.convert('megapascal').value))
        #
        # NOTE While the shear yield stress of structural steel has been variously estimated as between 
        # 1/2 and 5/8 of the tension and compression yield stress and is frequently taken as
        # Fy/3^0.5 , its permissible working stress value is given by AISC 335-89 as 2/3 the recommended
        # basic allowable tensile stress. 
        # For cylindrical members when local shear deformations may be substantial due to cylinder geometry,
        # a reduced yield stress may need to be substituted for Fy in Equation (6.12).        
        #
        URshear = (self.fvy**2 + self.fvx**2)**0.50 / self.Fv.value
        URshear_flag = "(3.2.4-2)"
        #
        #print ('IR Shear In Plane =', self.UmShear_IP)
        #print ('IR Shear Out Plane =', self.UmShear_OP)
        #print ('IR Shear =', self.UmShear)
        #print (' ')
        self.shear_results = ChapterResults(URshear, URshear, URshear_flag, self.Fv, self.Fv)
        #return self._shear(stress)
    #
    def torsional_shear(self, section, material, stress):
        """
        6.2.4.2 Torsional Shear
        """
        #
        #fvt = stress.tau_x
        self.fvt = max(abs(item) for item in stress.tau_x)
        # The maximum torsional shear stress, Fv, for cylindrical
        # members caused by torsion is 6.11 where :
        # fvt = maximum torsional shear stress, ksi (MPa),
        # Mt = torsional moment, kips-in. (MN-m),
        # Ip = polar moment of inertia, in.4 (m4),
        #
        #self.fvt = abs((self.Mvt * section.D)/(2 * section.Ip))
        #
        #
        # and the allowable torsional shear stress, Fvt, should be determined
        # from:
        # (3.2.4-4)
        self.Fvt = 0.40 * material.Fy
        #print ('Fvt Allowable: {:1.2f} MPa'.format(self.Fvt.convert('megapascal').value))
        #
        URtorsion = abs(self.fvt / self.Fvt.value)
        URtorsion_flag = "(3.2.4-4)"
        #
        #print ('IR Torsion =', self.UmTorsion)
        #print (' ')
        #
        self.torsion_results = ChapterResults(URtorsion, URtorsion, URtorsion_flag, self.Fvt, self.Fvt)
    #
    def hydrostatic_pressure(self, z, wave_data):
        """
        6.2.5 Hydrostatic Pressure (Stiffened and Unstiffened Cylinders)
        6.2.5.2 Design Hydrostatic Head
        """
        # 
        # The hydrostatic pressure to be used should be
        # determined from the design head, Hz, defined as follows:
        # where
        # z = depth below still water surface including tide, m. 
        #     z is positive measured downward from the still
        #     water surface. For installation, z should be the 
        #     maximum submergence during the launch or
        #     differential head during the upending sequence, 
        #     plus a reasonable increase in head to account for 
        #     structural weight tolerances and for deviations 
        #     from the planned installation sequence.
        # Hw = wave height, m,
        # k = with L equal to wave length, m,
        # d = still water depth, ft. (m),
        # Rhow = seawater density, 0.01005 MN/m3.
        #
        try:
            k = wave_data.k
        except AttributeError:
            wave_data.wave_length
            k = wave_data.k
        self.Hz = (z + wave_data.Hw / 2.0
                   * (math.cosh(k * (wave_data.d.value - z.value))
                      / math.cosh(k * wave_data.d.value)))
        print ('Hz : {:1.2f} m'.format(self.Hz.convert('metre').value))
        #
        self.p = wave_data.rho_w  * self.Hz
        print ('p  : {:1.2f} MPa'.format(self.p.convert('megapascal').value))
    #
    def hoop_buckling(self, Lr, section, material):
        """
        6.2.5 Hydrostatic Pressure (Stiffened and Unstiffened Cylinders)
        6.2.5.3 Hoop Buckling Stress
        """
        #
        # For tubular platform members satisfying API Spec 2B outof-
        # roundness tolerances, the acting membrane stress, fh, in Mpa
        # should not exceed the critical hoop buckling stress,
        # Fhc, divided by the appropriate safety factor:
        # where
        # fh = hoop stress due to hydrostatic pressure, ksi (MPa),
        # p = hydrostatic pressure, ksi (MPa),
        # SFh = safety factor against hydrostatic collapse (see
        #          Section 3.3.5).
        #
        #
        # The elastic hoop buckling stress, Fhe, and the critical hoop
        # buckling stress, Fhc, are determined from the following formulas.
        #
        print('D/t =',section.D/section.t)
        #
        # The geometric parameter, M, is defined as 6.17 where:
        # Lr = length of cylinder between stiffening rings, diaphragms,
        # or end connections, (m).
        #
        _LrOptimum = 1.6 * (section.D**3/(2*section.t))**0.50
        print('Lr : {:1.2f} !=  {:1.2f}'.format(Lr.value, _LrOptimum.value))
        # (6.17)
        self.M = (Lr.value / section.D.value
                  * (2 * section.D.value / section.t.value)**0.50)
        #
        # Note: For M > 1.6D/t, the elastic buckling stress is approximately
        # equal to that of a long unstiffened cylinder. Thus, stiffening rings, if
        # required, should be spaced such that M < 1.6D/t in order to be beneficial.
        _Mu_min = 1.6 * section.D.value / section.t.value
        print('M : {:1.2f} != {:1.2f}'.format(self.M, _Mu_min))
        #
        # The critical hoop buckling coefficient Ch includes the
        # effect of initial geometric imperfections within 
        #  API Spec 2B tolerance limits.
        #
        #if self.M < 1.5:
        #    self.Ch = 0.80
        #
        #else:
        if self.M > _Mu_min:
            self.Ch = 0.44 * section.t.value / section.D.value
        else:
            if self.M < 1.5:
                self.Ch = 0.80
            elif self.M < 3.5:
                self.Ch = 0.755 / (self.M - 0.559)
            elif self.M < 0.825 * section.D.value / section.t.value:
                self.Ch = 0.736 / (self.M - 0.636)
            else:
                self.Ch = (0.44 * section.t.value / section.D.value
                           + 0.21 * (section.D.value / section.t.value)**3 / self.M**4)
                #print (((0.21 * (section.D / section.t)**3 )))
        print('Ch = {:1.3f}'.format(self.Ch))
        #
        # 1. Elastic Hoop Buckling Stress. The elastic hoop buckling
        #    stress determination is based on a linear stress-strain 
        #    relationship from:
        self.Fhe = 2 * self.Ch * material.E * section.t.value / section.D.value
        print('fhe : {:1.2f} MPa'.format(self.Fhe.convert('megapascal').value))
        #
        # 2. Critical Hoop Buckling Stress. 
        #    The material yield strength relative to the elastic 
        #    hoop buckling stress  determines whether elastic or
        #    inelastic hoop buckling occurs and the critical hoop
        #    buckling stress, Fhc in MPa is defined by the 
        #    appropriate equation.
        #
        # Elastic Buckling
        if self.Fhe.value <= 0.55 * material.Fy.value:
            self.Fhc = self.Fhe
        else:
            # Inelastic Buckling:
            if self.Fhe.value > 6.2 * material.Fy.value:
                self.Fhc = material.Fy
            
            elif self.Fhe.value > 1.6 * material.Fy.value:
                self.Fhc = (1.31 * material.Fy / (1.15 + material.Fy/self.Fhe))
            
            else:
                self.Fhc = 0.45 * material.Fy + 0.18 * self.Fhe
        #
        self.Fhc /= self.SFh
        print('Fhc : {:1.2f}'.format(self.Fhc.convert('megapascal').value))
        #
        # (6.13)
        self.fh = self.p * section.D / (2 * section.t)
        #if self.fh.value > self.Fhc.value / self.SFh:
        #    self.fh = self.Fhc / self.SFh
        print('fh : {:1.2f} MPa'.format(self.fh.convert('megapascal').value))
        #
        # (13.2-31)
        URhs =  abs(self.fh.value / self.Fhc.value)
        URhp_flag = '(6.13)'
        #
        #print('IR hp =', self.UmHP, self.UmHP_flag)
        #print(' ')
        #
        self.hoop_buckling_results = ChapterResults(URhs, URhs, URhp_flag, self.Fhc, self.Fhc)
    #
    #
    def combination(self, KLrx, KLry, Cmx, Cmy,
                    material, stress):
        """
        6.3 COMBINED STRESSES FOR CYLINDRICAL MEMBERS
        """        
        #
        # Sections 6.3.1 and 6.3.2 apply to overall member behavior
        # while Sections 6.3.3 and 6.3.4 apply to local buckling
        #fa = stress.sigma_x
        #fa = max(abs(item) for item in stress.sigma_x)
        #fbx = stress.sigma_y
        #fby = stress.sigma_z
        #fbx = max(abs(item) for item in stress.sigma_y)
        #fby = max(abs(item) for item in stress.sigma_z)
        # FIXME : 
        self.flag_axial = 'COMPRESSION'        
        #
        # 6.3.3 Combined Axial Tension and Bending
        #if flag_axial == 'TENSION':
        # Cylindrical members subjected to combined tension and
        # bending should be proportioned to satisfy Eq. 6.21 at all
        # points along their length, where fbx and fby are the computed
        # bending tensile stresses
        # (6.21)
        IRt = abs(self.fa / (0.60 * material.Fy.value) 
                  + (self.fbx**2 + self.fby**2)**0.50 / self.Fb.value)
        IR_flag = '(6.21)'
        #print("UR combined : {:1.3f} {:}".format(self.IRt, self.IR_flag))
        #
        if self.flag_axial == 'COMPRESSION':
            # 6.3.1 Combined Axial Compression and Bending
            # 6.3.1.a Cylindrical Members
            #
            # When fa/Fa <= 0.15 the following formula may be used 
            # in lieu of the foregoing two formulas
            if abs(self.fa / self.Fa.value) <= 0.15:
                IRt = (abs(self.fa / self.Fa.value) 
                       + (self.fbx**2 + self.fby**2)**0.50/ self.Fb.value)
                IR_flag = '(6.22)'
            else:
                # Cylindrical members subjected to combined compression
                # and flexure should be proportioned to satisfy both the 
                # following requirements at all points along their length                
                #
                self.Fex =  (12 * material.E * math.pi**2 
                             / (23 * KLrx**2))
                #
                self.Fey =  (12 * material.E * math.pi**2 
                             / (23 * KLry**2))
                #
                # Eq. 3.3.1-1 assumes that the same values of Cm and Fe' are
                # appropriate for fbx and fby. If different values are applicable,
                # the following formula or other rational analysis should be
                # used instead of Eq. 3.3.1-1:
                _IR1 = (abs(self.fa / self.Fa.value) 
                        + (((Cmx * self.fbx / (1 - (self.fa / self.Fex)))**2 
                            + (Cmy * self.fby / (1 - (self.fa / self.Fey)))**2)**0.50 / self.Fb))
                #
                #
                # (3.3.1-2)
                #_IR2 = (self.fa/(0.60*material.Fy) + 
                #        (math.sqrt(self.fbx**2 +
                #                   self.fby**2))/self.Fb)
                #
                #
                IRt = max(_IR1.value, IRt)
                #
                if IRt == _IR1.value:
                    IR_flag = '(6.20)'
                #else:
                #    self.IRc_flag = '(3.3.1-2)'
                #
                #print ('IR =', self.IRc, self.IRc_flag)
                #
                # where the undefined terms used are as defined by the AISC
                # Specification for the Design, Fabrication, and Erection of
                # Structural Steel for Buildings
                #
                #
                #print("UR combined : {:1.3f} {:}".format(self.IRc, self.IRc_flag))
        #
        self.combined_results =  SummaryResults(IRt, IR_flag)
    #
    #
    def safety_factors(self, design_condition, material,
                       SFxt=1.67, SFb=None, SFxc=2.0, SFh=2.0):
        """
        6.3.6 Safety Factors
        """
        # Determine Safety Factors
        if design_condition.upper() == 'STORM':
            self.SFxt = 1.25
            self.SFb = material.Fy.value / (1.33 * self.Fb.value)
            self.SFxc = 1.50
            self.SFh = 1.50
        
        elif design_condition.upper() == 'OPERATING':
            self.SFxt = 1.67
            self.SFb = material.Fy.value / self.Fb.value
            self.SFxc = 2.00
            self.SFh = 2.00
        
        elif design_condition.upper() == 'USER':
            self.SFxt = SFxt
            if SFb:
                self.SFb = SFb
            else:
                self.SFb = material.Fy.value / self.Fb.value
            self.SFxc = SFxc
            self.SFh = SFh
    #
    def combination_hydro(self, material):
        """
        6.3.4 Axial Tension and Hydrostatic Pressure
        """
        #-------------------------------------------------
        # the term 'A' should reflect the maximum tensile
        # stress combination
        self.A = (((abs(self.fa.value) 
                    + (self.fbx.value**2 + self.fby.value**2)**0.50 
                    - (0.50 * self.fh.value))
                   / material.Fy.value) * self.SFxt)
        
        self.B = (self.fh.value / self.Fhc.value) * self.SFh
        # 6.26
        _AB = (self.A**2 + self.B**2 
               + 2 * material.poisson * abs(self.A) * self.B)        
        #
        # 6.3.4 Axial Tension and Hydrostatic Pressure
        if self.flag_axial == 'TENSION':
            # When member longitudinal tensile stresses and hoop 
            # compressive stresses (collapse) occur simultaneously, 
            # the following interaction equation should be satisfied:
            #
            IRch = _AB
            IRch_flag = '(6.26)'
            #print (' ')
            #print('IRch = {:1.3f} {:}'.format(self.IRth, self.IRth_flag))
        else:
            # 6.3.5 Axial Compression and Hydrostatic Pressure            
            # When longitudinal compressive stresses and hoop 
            # compressive stresses occur simultaneously, the 
            # following equations should be satisfied:
            #
            # fx should reflect the maximum
            # compressive stress combination
            _fx = (abs(self.fa) + ( self.fbx**2 + self.fby**2)**0.50 
                   + (0.50* self.fh))
            
            _Faa = self.Fxe / self.SFxc
            _Fha = self.Fhe / self.SFh            
            #
            # Eq. 6.27 should reflect the maximum compressive stress
            # combination.
            _IRch1 = (self.SFxc * ((abs(self.fa) + 0.5*self.fh) / self.Fxc)
                      + (self.SFb * (self.fbx**2 + self.fby**2)**0.50 / material.Fy))
            # (6.28)
            _IRch2 = self.SFh * self.fh / self.Fhc
            #
            # The following equation should also be satisfied when
            # fx > 0.5Fha
            _IRch3 = 0 * material.Fy / material.Fy
            # (6.29)
            if _fx.value > 0.5 * _Fha.value :
                _IRch3 = ((_fx - 0.50 * _Fha) / (_Faa - 0.50 * _Fha) 
                          + (self.fh / _Fha)**2)
            #
            # Note: If fb > fa + 0.5 fh, both Eq. 6.26 and 
            # Eq. 6.27 must be satisfied.
            _IRch4 = 0 * material.Fy / material.Fy
            if (self.fbx.value**2 + self.fby.value**2)**0.50 > abs(self.fa.value) :
                _IRch4 = _AB
            #
            IRch = max(abs(_IRch1.value), abs(_IRch2.value), 
                       abs(_IRch3.value), abs(_IRch4.value))
            #
            if IRch == abs(_IRch1.value):
                IRch_flag = '(6.27)'
            
            elif IRch == abs(_IRch2.value):
                IRch_flag = '(6.28)'
            
            elif IRch == abs(_IRch3.value):
                IRch_flag = '(6.29)'
            
            else:
                IRch_flag = '(6.26)'
            #
            #print ('')
            #print('IRch = {:1.3f} {:}'.format(IRch, IRch_flag))
        #
        self.combined_results =  SummaryResults(IRch, IRch_flag)
    #
    #
    def cilindrical_piles(self):
        """
        6.3.2.2
        """
        pass
    #
    def pile_overload(self):
        """
        6.3.2.3
        """
        pass
    #
    def ring_design(self, section, material):
        """
        6.2.5.4 Ring design
        """
        # Circumferential stiffening ring size may be selected on the
        # following approximate basis
        # (6.19)
        self.Ic = self.Fhe * section.t * section.D**2 * L / (8 * material.E)
    #
    #-------------------------------------------------
    #                   Print Section
    #-------------------------------------------------
    #
    def _header(self):
        """ """
        today = datetime.date.today()
        #output = []
        output = "\n"
        output += "***************************************************************************************\n"
        output += "*                                  CODE CHECK TOOL                                    *\n"
        output += "*                            Strength Of Tubular Members                              *\n"
        output += "*                                API RP2A-WSD-ED22                                    *\n"
        output += "*                                  ALPHA Version                             12/12/20 *\n"           
        output += "***************************************************************************************\n"
        output += "DATE: {:}{:} UNITS [N-mm]\n".format(56*" ", today)
        return output    
    #
    def _hydro_pressure(self, cls):
        """ """
        output = []
        #
        if self.M < (1.6* cls.section.D/cls.section.t):
            _MuLimitFlag = '< M'
            _Ring_flag = 'No Required'
        else:
            _MuLimitFlag = '> M'
            _Ring_flag = 'Required'
        #
        #
        _LrLimitit = (1.6 * (cls.section.D**3 / (2*cls.section.t))**0.50)
        #
        if cls.Lr < _LrLimitit :
            _LrLimititFlag = '< LrM'
            _RingSpacing = 'Lr < 1.6[D^3/2t]^0.5  OK'
        else:
            _LrLimititFlag = '> LrM'
            _RingSpacing = 'Lr > Limit  -->Reduce Lr'
        #
        if self.combined_results.UR > 1.0:
            _UmHP_flag = 'FAIL'
        else:
            _UmHP_flag = 'PASS'
            _Ring_flag = 'No Required'
            _RingSpacing = 'No Required'
        #
        #
        output.append("\n")
        output.append("_______________________________________________________________________________________\n")
        output.append("\n")
        output.append("                           HYDROSTATIC PRESSURE CALCULATIONS"+"\n")
        output.append("\n")
        output.append("Member ID      IR  hp  fh [N/mm2]  Hw    [mm]  p  [N/mm2]  "+"\n")
        output.append("               Equ IR  Fhc[N/mm2]  T    [sec]  z     [mm]  M  [N/mm2]  [1.6 D/t]\n")
        output.append("Result                 SFh         d     [mm]  Hz    [mm]  Ch [N/mm2]  Lr   [mm]\n")
        output.append("RingStiffener?         fh/Fhc/SFh  Lwave [mm]  k           Fhe[N/mm2]  LrM  [mm]\n")
        output.append(".......................................................................................\n")
        output.append("\n")
        #
        output.append("{:12s} {:3.4f} {: 1.4E} {: 1.4E} {: 1.4E}\n"
                      .format(self.name, self.combined_results.UR, 
                              self.fh.convert("megapascal").value, 
                              cls.wave.Hw.convert("metre").value, 
                              self.p.convert("megapascal").value))
        #
        output.append("{:}{:6s} {: 1.4E} {: 1.4E} {:1.4E} {:1.4E} {: 1.4E} {:14s}\n"
                      .format(12*" ", self.combined_results.UR_flag, 
                              self.Fhc.convert("megapascal").value,  
                              cls.wave.Tw.convert("second").value, 
                              cls.z.convert("metre").value, 
                              self.M, (1.6*cls.section.D.value/cls.section.t.value), 
                              _MuLimitFlag))
        #
        output.append("{:12s} {:} {: 1.4E} {: 1.4E} {:1.4E} {: 1.4E} {: 1.4E} {:2s}\n"
                      .format(_UmHP_flag, 11*" ", self.SFh,  
                              cls.wave.d.convert("metre").value, 
                              self.Hz.convert("metre").value, self.Ch, 
                              cls.Lr.convert("metre").value,
                              _LrLimititFlag))
        #
        output.append("{:12s} {:}{: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E} {:1.4E}\n"
                      .format(_Ring_flag, 11*" ", self.combined_results.UR, 
                              cls.wave.wave_length.convert("metre").value, 
                              cls.wave.k, self.Fhe.convert("megapascal").value, 
                              _LrLimitit.convert("metre").value))
        #
        return output
    #
    def _shear(self, stress):
        """ """
        #output = []
        output = "\n"
        output += "_______________________________________________________________________________________\n"
        output += "\n"
        output += "                                 SHEAR CHECK RESULTS\n"
        output += "\n"
        output += "Member ID     IR max   Vy    [kN]   Vx    [kN]   Mt   [MPa]\n"
        output += "                       fvy  [MPa]   fvx  [MPa]   fvt  [MPa]\n"
        output += "             Equ  Fv   Fv   [MPa]   Fv   [MPa]   Fvt  [MPa]\n"
        output += "Result       Equ Fvt   fvy/Fv       fvx/Fv       fvt/Fvt\n"
        output += ".......................................................................................\n"
        output += "\n"
        #
        URresult = max(self.shear_results.URy, self.torsion_results.URy)
        URresult_flag = 'PASS'
        if URresult > 1.0:
            URresult_flag = 'FAIL' 
        #
        output += "{:12s} {:3.4f}    {:1.4E}   {:1.4E}   {:1.4E}\n"\
            .format(self.name, URresult, self.shear_results.URy, 
                    self.shear_results.URz, self.torsion_results.URy)
        #
        output += "{:}{:1.4E}   {:1.4E}   {:1.4E}\n"\
            .format( 23*" ", self.fvy, self.fvx, self.fvt)
        #
        output += "{:}{:6s}  {:1.4E}   {:1.4E}   {:1.4E}\n"\
            .format(12*" ", self.shear_results.UR_flag, 
                    self.Fv.convert("megapascal").value, 
                    self.Fv.convert("megapascal").value, 
                    self.Fvt.convert("megapascal").value)
        #
        output += "{:11s} {:6s}  {:-1.4E}   {:-1.4E}   {:-1.4E}\n"\
            .format( URresult_flag, self.torsion_results.UR_flag, 
                     self.shear_results.URy, self.shear_results.URz, 
                     self.torsion_results.URy)
        return output
    #
    def _axial_bending_noHP(self, stress):
        """ """
        #output = []
        if self.flag_axial == 'COMPRESSION': 
            output = "\n"
            output += "{:}\n".format(80*"_")
            output += "\n"
            output += "                      AXIAL COMPRESSION AND BENDING CHECK RESULTS\n"
            output += "\n"
            output += "Member ID     IR Comb  Faxial[kN]  Mx   [kNm]  My   [kNm]  C           Lx     [m]  Kx\n"
            output += "              Equ IR   fa   [MPa]  fbx  [MPa]  fby  [MPa]  Cc          Ly     [m]  Ky\n"
            output += "              Equ Fa   Fa   [MPa]  Fb   [MPa]  Fb   [MPa]  Fxe  [MPa]  Fex  [MPa]  Cmx\n"
            #outpu += ("              Equ Umc  GammaRc     GammaRb     GammaRb     fe [N/mm2]  fez[N/mm2]  Cmz\n"
            output += "Result        Equ Fb   fa/Fa       fbx/Fb      fby/Fb      Fxc  [MPa]  Fey  [MPa]  Cmy\n"
            output += "{:}\n".format(80*".")
            output += "\n"
            #output += "{:12s}   {:3.4f}  {:1.4E}  {:1.4E}  {:1.4E}  {:1.4E}  {:1.4E}  {:1.2f}\n"\
            #    .format(self.name, self.axial_results.URy, 
            #            abs(cls.actions.Fx.convert("kilonewton").value), 
            #            abs(cls.actions.My.convert("kilonewton*metre").value), 
            #            abs(cls.actions.Mz.convert("kilonewton*metre").value),
            #            self.C, cls.Ly.convert("metre").value, cls.Ky)
            #                
            output += "{:}{:8s}  {:1.4E}  {:1.4E}  {:1.4E}  {:1.4E}\n"\
                .format(13*" ", self.combined_results.UR_flag, 
                        abs(self.fa), abs(self.fbx), abs(self.fby), self.Cc)
            #
            output += "{:}{:8s}  {:1.4E}  {:1.4E}  {:1.4E}  {:1.4E}\n"\
                .format(13*" ", self.axial_results.UR_flag, 
                        self.Fa.convert("megapascal").value, 
                        self.Fb.convert("megapascal").value, 
                        self.Fb.convert("megapascal").value, 
                        self.Fxe.convert("megapascal").value)
            #
            #output.append((13*" "+ "%6s" +"  "+"%1.4E" +"  "+ "%1.4E" +"  "+
            #             "%1.4E" +"  "+"%1.4E" +"  "+"%1.4E"+"  "+"%1.2f" +"\n")%
            #            (self.UmComp_flag, self.GammaRc, self.GammaRb, self.GammaRb,
            #             self.fe, self.fez, self.Cmy))
            #
            output += "{:12s} {:6s}    {:1.4E}  {:1.4E}  {:1.4E}  {:1.4E}\n"\
                .format(self.combined_results.status,
                        self.bending_results.UR_flag, 
                        self.axial_results.URy, 
                        self.bending_results.URy, 
                        self.bending_results.URz,
                        self.Fxc.convert("megapascal").value)
            output += "\n"
            #output += "_______________________________________________________________________________________\n"
            output += "\n"
        else:
            output = "\n"
            output += "_______________________________________________________________________________________\n"
            output += "\n"
            output += "                        AXIAL TENSION AND BENDING CHECK RESULTS\n"
            output += "\n"
            output += "Member ID    IR Comb   Faxial [N]   Mx   [Nmm]   My   [Nmm]   D/t\n"
            output += "             Equ IR    fa [N/mm2]   fbx[N/mm2]   fby[N/mm2]   Lx    [mm]\n"
            output += "             Equ Ft    Ft [N/mm2]   Fb [N/mm2]   Fb [N/mm2]   Ly    [mm]\n"
            #output += ("              Equ Umt  GammaRt     GammaRb     GammaRb                             "+"\n"
            output += "Result       Equ Fb    fa/Ft        fbx/Fb       fby/Fb\n"
            output += ".......................................................................................\n"
            output += "\n"
            output += "{:12s}    {:3.4f}   {:1.4E}   {:1.4E}   {:1.4E}  {:1.4E}\n"\
                .format(self.name, self.axial_results.URy, 
                        abs(cls.actions.Fx.convert("kilonewton").value), 
                        abs(cls.actions.My.convert("kilonewton*metre").value), 
                        abs(cls.actions.Mz.convert("kilonewton*metre").value),
                        (cls.section.D.value/cls.section.t.value))
            #                
            output += "{:}{:8s}  {:1.4E}   {:1.4E}  {:1.4E}  {:1.4E}\n"\
                .format(13*" ", self.combined_results.UR_flag, 
                        abs(self.fa.convert("megapascal").value),
                        self.fbx.convert("megapascal").value, 
                        self.fby.convert("megapascal").value,
                        cls.Lz.convert("metre").value)
            #
            output += "{:}{:8s}  {:1.4E}   {:1.4E}   {:1.4E}  {:1.4E}\n"\
                .format(13*" ", self.axial_results.UR_flag , 
                        self.Fa.convert("megapascal").value, 
                        self.Fb.convert("megapascal").value, 
                        self.Fb.convert("megapascal").value, 
                        cls.Ly.convert("metre").value)
            #
            #output += (13*" "+ "%6s" +"  "+"%1.4E" +"  "+ "%1.4E" +"  "+
            #             "%1.4E"  +"\n")%
            #            (self.UmTension_Flag, self.GammaRt, self.GammaRb, self.GammaRb)
            #                
            output += "{:12s} {:6s}    {:1.4E}   {:1.4E}   {:1.4E}\n"\
                .format(self.combined_results.status , 
                        self.bending_results.UR_flag, 
                        self.axial_results.URy, 
                        self.bending_results.URy, 
                        self.bending_results.URz)
            #                
            output += "\n"
            #output += "_______________________________________________________________________________________\n"
            output += "\n"
        #
        return output
    #
    def _axial_bending_andHP(self):
        """ """
        #output = []
        if self.flag_axial == 'COMPRESSION': 
            output = "\n"
            output += "_______________________________________________________________________________________\n"
            output += "\n"
            output += "         AXIAL COMPRESSION AND BENDING WITH HYDROSTATIC PRESSURE CHECK RESULTS\n"
            output += "\n"
            output += "Member ID     IR Comb  Faxial[kN]  Mx   [kNm]  My   [kNm]  C           Lx     [m]  Kx\n"
            output += "              Equ IR   fa   [MPa]  fbx  [MPa]  fby  [MPa]  Cc          Ly     [m]  Ky\n"
            output += "              Equ Fa   Fa   [MPa]  Fb   [MPa]  Fb   [MPa]  Fxe  [MPa]  SFx         Cmx\n"
            #output += ("              Equ Umc  GammaRc     GammaRb     GammaRb     fe [N/mm2]  fch[N/mm2]  Cmy\n"
            output += "Result        Equ Fb   fa/Fa       fbx/Fb      fby/Fb      Fxc  [MPa]  SFb         Cmy\n"
            output += ".......................................................................................\n"
            output += "\n"
            #                
            output += "{:12s}  {:3.4f}   {:1.4E}  {:1.4E}  {:1.4E}  {:1.4E}  {:1.4E}  {:1.2f}\n"\
                .format(self.name, self.combined_results.UR, 
                        abs(cls.actions.Fx.convert("kilonewton").value), 
                        abs(cls.actions.My.convert("kilonewton*metre").value), 
                        abs(cls.actions.Mz.convert("kilonewton*metre").value),
                        self.C, cls.Ly.convert("metre").value, cls.Ky)
            #                
            output += "{:}{:8s} {:1.4E}  {:1.4E}  {:1.4E}  {:1.4E}  {:1.4E}  {:1.2f}\n"\
                .format(14*" ", self.combined_results.UR_flag, 
                        abs(self.fa.convert("megapascal").value), 
                        abs(self.fbx.convert("megapascal").value), 
                        abs(self.fby.convert("megapascal").value),
                        self.Cc, cls.Lz.convert("metre").value, cls.Kz)
            #
            output += "{:}{:8s} {:1.4E}  {:1.4E}  {:1.4E}  {:1.4E}  {:1.4E}  {:1.2f}\n"\
                .format(14*" ", self.axial_results.UR_flag, 
                        self.Fa.convert("megapascal").value, 
                        self.Fb.convert("megapascal").value, 
                        self.Fb.convert("megapascal").value, 
                        self.Fxe.convert("megapascal").value, 
                        self.SFxc, cls.Cmy)
            #                
            #output.append((13*" "+ "%6s" +"  "+"%1.4E" +"  "+ "%1.4E" +"  "+
            #             "%1.4E" +"  "+"%1.4E" +"  "+"%1.4E"+"  "+ "%1.2f" +"\n")%
            #            (self.UmComp_flag, self.GammaRc, self.GammaRb, self.GammaRb,
            #             self.fe, self.fch, self.Cmx))
            #                
            output += "{:14s}{:8s} {:1.4E}  {:1.4E}  {:1.4E}  {:1.4E}  {:1.4E}  {:1.2f}\n"\
                .format(self.combined_results.status, 
                        self.bending_results.UR_flag, 
                        self.axial_results.URy,  
                        self.bending_results.URy, 
                        self.bending_results.URz,
                        self.Fxc.convert("megapascal").value, 
                        self.SFb, cls.Cmz)
            #                
            output += "\n"
            #output += ("_______________________________________________________________________________________\n"
            output += "\n"
            #
        else:
            output += "\n"
            output += "_______________________________________________________________________________________\n"
            output += "\n"
            output += "          AXIAL TENSION AND BENDING WITH HYDROSTATIC PRESSURE CHECK RESULTS\n"
            output += "\n"
            output += "Member ID     IR Comb  Faxial[kN]  My   [kNm]  Mz   [kNm]  A           Lx     [m]\n"
            output += "              Equ IR   fa   [MPa]  fbx  [MPa]  fby  [MPa]  B           Ly     [m]\n"
            output += "              Equ Ft   Ft   [MPa]  Fb   [MPa]  Fb   [MPa]  SFx\n"
            #outpu += "              Equ IRt  GammaRt     GammaRb     GammaRb                 fbh[N/mm2]\n"
            output += "Result        Equ Fb   fa/Ft       fbx/Fb      fby/Fb      Fhc  [MPa]\n"
            output += ".......................................................................................\n"
            output += "\n"
            #
            output += "{:12s}  {:3.4f}   {:1.4E}  {:1.4E}  {:1.4E}  {:1.4E}  {:1.4E}\n"\
                .format(self.name, self.combined_results.UR, 
                        abs(cls.actions.Fx.convert("kilonewton").value), 
                        abs(cls.actions.My.convert("kilonewton*metre").value), 
                        abs(cls.actions.Mz.convert("kilonewton*metre").value),
                        self.A, cls.Ly.convert("metre").value)
            #                
            output += "{:}{:8s} {:1.4E}  {:1.4E}  {:1.4E}  {:1.4E}  {:1.4E}\n"\
                .format(14*" ", self.combined_results.UR_flag, 
                        abs(self.fa.convert("megapascal").value), 
                        abs(self.fbx.convert("megapascal").value), 
                        abs(self.fby.convert("megapascal").value),
                        self.B, cls.Lz.convert("metre").value)
            #
            output += "{:}{:8s} {:1.4E}  {:1.4E}  {:1.4E}  {:1.4E}\n"\
                .format(14*" ", self.axial_results.UR_flag,
                        self.Fa.convert("megapascal").value, 
                        self.Fb.convert("megapascal").value, 
                        self.Fb.convert("megapascal").value, 
                        self.SFxt)
            #
            #output += (13*" "+ "%6s" +"  "+"%1.4E" +"  "+ "%1.4E" +"  "+
            #             "%1.4E" +14*" "+"%1.4E" +"\n")%
            #            (self.UmTension_Flag, self.GammaRt, self.GammaRb, self.GammaRb,
            #             self.fbh))
            #
            output += "{:14s}{:8s} {:1.4E}  {:1.4E}  {:1.4E}  {:1.4E}\n"\
                .format(self.combined_results.status.upper(), 
                        self.bending_results.UR_flag,
                        self.axial_results.URy,  
                        self.bending_results.URy, 
                        self.bending_results.URz,
                        self.Fhc.convert("megapascal").value)
            #                
            output += "\n"
            #output += "_______________________________________________________________________________________\n"
            output += "\n"
        return output
    #
    def __str__(self) -> str:
        """ """
        print('----')
        return self._header()
    #
#
#
#
