# *****************************************
#                 Log History
#
#
#
# *****************************************
#
from steelpy.wave.theory import WaveStokes5
#
import math
import datetime
#
#
#
#-------------------------------------------------
#                  PD8010 Section
#-------------------------------------------------
#
class PD8010_2004:
    #
    def __init__(self):
        pass
    #
    #
    # ===================================================
    #
    # Section Properties
    def SectionProperties(self):
        #
        #
        print (' ')
        print ('Section Properties ')
        print ('Do =', self.Do)
        print ('Tnom =',self.tnom)
        print ('Tmin =',self.tmin)
        print (' ')
        print ('Material Properties ')
        print ('Fy =',self.Sigmay)
        print ('Fu =',self.Sigmau)
        print ('E =',self.E)
        print ('G =',self.G)
        #
        #     
        #-------------------------------------------------
        #   Cross-Sectional Area
        #    def A(self):
        #
        self.A = ((math.pi / 4.)*
                  (self.Do**2 -(self.Do - 2*self.tmin)**2))
        #print ('circular area:',self.SecArea)
        #
        #
        #
        #
        #-------------------------------------------------
        #               Section Properties
        #-------------------------------------------------
        #
        #
        #   Second Moment of Area about Mayor Axis
        #   --------------------------------------
        #
        self.I = ((math.pi / 64.0)*
                  (self.Do**4 - (self.Do - 2*self.tmin)**4))
        # print ('circular Second Moment',self.Iip)
        #
        #
        #       
        #   Elastic Modulus about Mayor Axis
        #   --------------------------------------
        #
        self.Ze = ((math.pi/64.0) * 
                   (self.Do**4 - (self.Do - 2 * self.tmin)**4) /
                   (self.Do / 2.0))
        # print ('circular Elastic Modulus',self.Zeip)
        #
        #self.Zeop = self.Zeip
        #
        #
        #-------------------------------------------------
        #   Plastic Modulus about Mayor Axis
        #    def Zx(self):
        #
        self.Zp = ((self.Do**3 - 
                    (self.Do - 2 * self.tmin)**3) / 6.0 )
        # print ('circular Plastic Modulus',self.Zpip)
        #
        #
        #-------------------------------------------------
        #   Radius of gyration about Mayor Axis
        #
        self.r = (math.sqrt(self.I / self.A))
        # print ('circular Radius of gyration about Mayor Axis',self.rip)
        # 
        #self.rop = self.rip
        #
        #-------------------------------------------------
        #   Torsional Constant
        #
        self.J = (2 * self.I)
        # print ('Torsional Constant',self.J)
        # 
        #
        #-------------------------------------------------
        #   Polar Moment of Inertia
        self.Ip = ((math.pi / 32.0) *
                   (self.Do**4 - 
                    (self.Do - 2*self.tmin)**4))
        # print ('Polar Moment of Inertia Ip:',self.Ip)
        #
        #
        #-------------------------------------------------
        #  Mass
        self.mass = (self.A * self.Rhos)/1000**2
        #
    # 
    #
    #
    # ===================================================
    #    PD8010-1:2004 Part 1 : Steel Pipelines on Land
    # ===================================================
    #
    #
    # 5.2 Categorization of fluids
    def CategorizationFluids(self):
        #
        self.a = 0.72
        #
        if self.FluidsCategory == "B":
            self.a = 0.72
        #
        elif self.FluidsCategory == "C":
            self.a = 0.30
    #
    #
    # 6.4.3 Limits of calculated stress
    def LimitsCalculatedStress(self):
        #print("ok")
        #
        # 6.4.3.1 Allowable hoop stress
        # The allowable hoop stress (Sah) should be 
        # calculated using equation (11).
        #
        # The weld joint factor e should be 1.0 for pipe
        # conforming to ISO 3183-1, ISO 3183-2, BS ISO 3183-3
        # and/or API 5L when supplied as seamless, 
        # longitudinally welded or spirally welded pipe. 
        # If the pipe history is unknown, the weld joint 
        # factor e should not exceed 0.60 for pipe of 
        # 0.114 m (4.5 in) outside diameter or smaller, 
        # or 0.80 for pipe larger than 0.114 m (4.5 in)
        # outside diameter.
        #
        if self.WeldJointFactor == "UNKNOWN":
            #
            if self.Do > 114.0:
                self.e = 0.80
            #
            else:
                self.e = 0.60
        #
        else:
            self.e = 1.0
            #
        #
        self.Sah = (self.a * self.e * self.Sigmay)
        self.fa_hs = (self.a * self.e)
        print ("Allowable hoop stress Sah = ", self.Sah)
        #
        #
        # 6.4.3.2 Allowable equivalent stress
        #
        self.Sae = 0.90*self.Sigmay
        self.fa =  0.90
        print ("Allowable equivalent stress Sah = ", self.Sae)
        #
        print (" ")
        print ("Allowable Stress")
        #
        self.URhoop = (self.Sigmah / self.Sah )
        print (("URhoop : " +"%2.3f")%(self.URhoop))
        #
        self.UReq = ( self.Sigmae/ self.Sae)
        print (("UReq: " +"%2.3f")%(self.UReq))
    #
    #
    #
    #
    # ===================================================
    #      PD8010-2:2004 Part 2 : Subsea Pipelines
    # ===================================================
    #
    #
    def LongitudinalStress(self):
        #
        #
        print (" ")
        print ("Longitudinal Stress Calculation")
        #
        #
        # set Tnom - Tcorr thickness 
        _tnomcorr = self.tnom - self.tcorr
        #
        _Dinom = (self.Do - 2 * self.tnom)
        # Internal Area
        _Ai = (math.pi*_Dinom**2/4.0)
        # External Area
        _Ae = (math.pi*self.Do**2/4.0)
        # Pipe section area (nominal thickness)
        _CSA = ((math.pi / 4.)*
                (self.Do**2 -(self.Do - 2*self.tnom)**2))
        # Pipe section area (Tnom - Tcorr thickness)
        _Anomfab = ((math.pi / 4.)*
                (self.Do**2 -(self.Do - 2*_tnomcorr)**2))
        #
        #   Elastic Modulus (Tnom - Tcorr thickness)
        #   --------------------------------------
        #
        _Ze = ((math.pi/64.0) * 
                   (self.Do**4 - (self.Do - 2 * _tnomcorr)**4) /
                   (self.Do / 2.0))
        #
        #
        # Bending Stress due to temperature, weight
        # of pipe contents, insulation, snow and ice,
        # wind or earthquake is calculated by the 
        # following equation:
        #
        self.Sb = (math.sqrt((self.My * self.li)**2 +
                             (self.Mz * self.lo)**2) / 
                   _Ze)
        #
        print (("Bending Long Stress = "+"%2.3f")%(self.Sb))
        #
        # ------------------------------------------
        # Hoop Stress Component of von Mises Stress
        # S = H -DeltaPi*Ai*(1-2*poisson) - As*E*AlphaT*DeltaT
        # ------------------------------------------
        #
        # Temperature induced part of Force
        # - As*E*AlphaT*DeltaT
        #
        self.Stemp = -(_CSA * self.E * self.AlphaT * self.DeltaT)
        #
        print (("Temperature Long Stress = "+"%2.3f")%(self.Stemp))
        #
        # Pressure induced part of Force
        # -DeltaPi*Ai*(1-2*poisson)
        # 
        #print ("Area internal",_Ainternal, _Dinom)
        _Sp = -(self.Pi - self.Po)*_Ai*(1-2*self.Poisson)
        #
        print (("Pressure Long Stress = "+"%2.3f")%(_Sp))
        #
        # ------------------------------------------
        # True Wall Axial Force
        # N = S - PiAi + PeAe
        #
        # Internal Pressure (N)
        _PiAi = self.Pi*_Ai
        #
        # External Pressure (N)
        _PeAe = self.Po * _Ae
        #
        # print ("Pi, Pe",_PiAi, _PeAe )
        #
        #_k = self.Do / (self.Do - 2*_tnomcorr)
        #print ("k = ", _k, (_k**2 + 1))
        #
        # Restrained
        if self.PipeRestraint == "YES":
            # 
            _SL = (self.Stemp + _Sp)
        #
        # Unrestrained
        else:
            #
            _SL = (_Sp  + self.Sb)
        #
        # Summing all components of longitudinal
        # normal stress:
        #self.SigmaL = self.Sb + self.Sdl + self.Sp - self.Stemp
        #
        # Include Temperature + hoop stress + given axial load
        if self.TempIntPressureFlag == "YES" :
            self.SigmaL =  ( _SL + _PiAi - _PeAe + self.Fx )/_Anomfab
            #self.Sa = self.SigmaL
            self.Fx = abs(min(( _SL + _PiAi - _PeAe + self.Fx ),0))
        #
        else:
            self.SigmaL =  self.Fx /_Anomfab
            self.Fx = abs(min(self.Fx, 0))
            #self.Sa = self.SigmaL
        #
        #print (("Temperature Long Stress = "+"%2.3f")%(self.Stemp))
        #print (("Axial Load = "+"%2.3f")%((_S + _PiAi - _PeAe)))
        print (("Longitudinal Stress = "+"%2.3f")%(self.SigmaL))
    #
    # 6.4 Strength
    #
    # 6.4.1 Design factors
    def DesignFactors(self):
        #
        # The design factor, fd, appropriate to the assessment
        # of allowable stress is given in Table 2. Alternatively,
        # the acceptability of construction loads may be assessed
        # on an allowable strain basis.
        #
        # *NOTE BS EN 14161 allows a design factor of up to 0.83
        #       to be used. The design factors recommended for UK
        #       use are shown in Table 2. Higher design factors may
        #       be used for all or part of a pipeline system, provided
        #       that an equivalent level of safety is achieved throughout 
        #       the system under consideration, and across all relevant
        #       limit states. A full risk assessment (see Annex D) is 
        #       recommended if higher design factors are used, and might 
        #       be subject to regulatory review.
        #
        # Where the application of a reliability-based limit state design 
        # method leads to a reduction in the wall thickness that is 
        # necessary to meet the recommendations for pressure containment, 
        # particular attention should be paid to installation and operability 
        # considerations, i.e. it should be demonstrated that the selected 
        # wall thickness is appropriate for all the load conditions and 
        # combinations that can reasonably be expected throughout the life 
        # of the pipeline.
        #
        # Account should be taken of the variation of strength with 
        # temperature on the basis of verifiable test data appropriate to 
        # the material under consideration.
        #
        # Table 2 - Design factor, fd
        #
        #
        # Equivalent stresses arising from
        # construction or hydrotest loads
        if self.DesignCondition == "HYDROTEST":
            #
            # Seabed including tie-in
            # & Riser/landfall
            self.fa = 1.0
            #
            # Hoop stress
            self.fa_hs = 1.0
            #
        #
        #
        # Equivalent stresses resulting from
        # functional and environmental or
        # accidental loads
        else:
            #
            # Riser/landfall
            if self.PipeType == 'RISER':
                self.fa = 0.72 
                # Hoop stress
                self.fa_hs = 0.60
            #
            # Seabed including tie-in 
            else: 
                self.fa = 0.96
                # Hoop stress
                self.fa_hs = 0.72
            #
        #
        #
        print (" ")
        print ("Design Factors")
        print ("Test Data  = ",self.DesignCondition)
        print ("Pipe Type  = ",self.PipeType)
        #
        print (("fa  = "+"%2.3f")%(self.fa))
        print (("fa hs  = "+"%2.3f")%(self.fa_hs))
        #
        #
    #
    #
    # 6.4.2 Stress-based design
    # -------------------------
    #
    # 6.4.2.1 Allowable stress
    def AllowableStress(self):
        #
        # Stress in the pipeline system should meet the 
        # inequality shown in equation (2).
        # where SigmaA is determined in accordance with 
        # 6.4.2.2 or 6.4.2.4 as appropriate.
        #
        self.SigmaAeq = self.fa * self.Sigmay
        #
        self.SigmaAhoop = self.fa_hs * self.Sigmay 
        #
        print (" ")
        print ("Allowable Stress")
        #
        self.URhoop = (self.Sigmah / self.SigmaAhoop )
        print (("URhoop : " +"%2.3f")%(self.URhoop))
        #
        #
        if self.DesignMethod == "STRESS":
            #
            self.UReq = ( self.Sigmae/ self.SigmaAeq)
            print (("UReq: " +"%2.3f")%(self.UReq))
            #print (("Sigmae: " +"%2.3f")%(self.Sigmae))
            #
        #
        else:
            #
            if self.Epsilonp > 0.0010:
                print (("Fail  Epsilonp > 0.0010:" +"%2.3f")%(self.Epsilonp))
            #
            else:
                print (("Pass  Epsilonp < 0.0010:" +"%2.3f")%(self.Epsilonp))
            #
        #
    #
    #
    # 6.4.2.2 Hoop stress
    def HoopStress(self, thoop):
        #
        #
        _thoop = float(thoop)
        _DiHoop = (self.Do - 2*_thoop)
        #
        # NOTE For clad or lined pipelines, the strength contribution
        # of the cladding or lining is usually not taken into account,
        # unless it is necessary to contribute to the structural integrity.
        #
        # For all other stress checks in this section, tnom should
        # be used in the calculation of component stresses.
        #
        #
        if self.Do/_thoop > 20:
            # Hoop stress should be calculated using equation (3)
            # (thin wall) when the ratio of Do/tmin is greater than 20.
            # (3)
            self.Sigmah = (self.Pi - self.Po)*(self.Do/(2 * _thoop))
            #
        #
        #
        else:
            # Equation (5) (thick wall) should be used when the ratio
            # of Do/tmin is less than or equal to 20.
            # (5)
            self.Sigmah = ((self.Pi - self.Po)*
                           ((self.Do**2 + _DiHoop**2) / 
                            (self.Do**2 - _DiHoop**2)))
            #
        #
        #
        print (" ")
        print ("Hoop Stress", _thoop)
        print (("Sigmah  = "+"%2.3f")%(self.Sigmah))
        #
        #
        #self.SFhoop = (self.fa_hs * self.Sigmay / self.Sigmah)
        #print (("Hoop Stress Usage Factor: " +"%2.3f")%(self.SFhoop))
        #
        #
        #
    #
    #
    # 6.4.2.3 Expansion and flexibility
    def ExpansionFlexibility(self):
        #
        # Pipelines and piping should be designed with sufficient
        # flexibility to prevent expansion or contraction causing
        # excessive forces or stresses in pipe material, joints, 
        # equipment, anchors or supports.
        #
        # Expansion calculations should be carried out on pipelines
        # where flexibility is in doubt, and where temperature changes
        # are expected. Thermal and pressure expansion or contraction
        # can cause movement at termination points, changes in direction 
        # or changes in size. The necessary flexibility should be provided
        # if such movements are unrestrained. Account should be taken of 
        # buckling forces that can be imposed on pipelines (see 6.4.4).
        #
        # The effect of restraints, such as support friction, branch 
        # connections and lateral interferences should be taken into account.
        # Calculations should take into account stress intensification 
        # factors found to be present in components other than plain 
        # straight pipe. Account should be taken of any extra flexibility
        # of such components.
        #
        # NOTE In the absence of more directly applicable data, the 
        # flexibility factors and stress intensification factors given in
        # BS EN 13480 or ASME B31.3 may be used.
        #
        # Pipelines can be restrained so that the longitudinal movement 
        # owing to thermal and pressure changes is absorbed by direct 
        # axial compression or tension of the pipe. In such cases expansion
        # calculations should be carried out taking into account all the 
        # forces acting on the pipeline. Consideration should be given to 
        # elastic instability due to longitudinal compressive forces.
        #
        # Where movement is restrained, flexibility should be provided by
        # means of loops, offsets or special fittings. The total operating 
        # temperature range should be taken as the difference between the
        # maximum and minimum metal temperatures for the operating cycle 
        # under consideration and should be used in calculating stresses in 
        # loops, bends and offsets.
        #
        # The temperature range used in the calculation of reactions on 
        # anchors and equipment should be taken as the difference between
        # the maximum or minimum metal temperatures and the installation 
        # temperature, whichever gives the greater reaction.
        #
        # Where there is a likelihood of repeated stress changes (including
        # thermal stress) giving rise to fatigue conditions, the stress 
        # range and allowable number of cycles should be calculated in 
        # accordance with 6.4.6. Nominal pipe wall thickness (including any
        # corrosion allowance) and nominal outside diameter should be
        # used for expansion and flexibility calculations.
        #
        #
        if self.FSFactorsCode == "ASME_B31.3":
            ASME.AppendixD(self)
        #
        # BS EN 13480
        elif self.FSFactorsCode == "BS_EN_13480":
            #
            # Bend Flexibility
            if self.SIFs_Flag == 'BEND':
                #
                print ("No implement yet")
            #
            # Tee Flexibility
            elif self.SIFs_Flag == 'TEE':
                #
                print ("No implement yet")
            #
            # Unknow
            else:
                print ("No implement yet")
            #
            # 
            # Flanges
            if self.Flanges == 0 or self.Flanges > 2:
                self.C1 = 1.0
            #
            # Rest
            else:
                print ("No implement yet")
            #
            #
        #
        #
        # USER Defined
        elif self.FSFactorsCode == "USER":
            pass
        #
        #
        # No flexibility & Stress Factors
        else:
            #
            #print ('N/A')
            # Flexibility Characteristic
            self.h = 1.0
            # Flexibility Factor
            self.k = 1.00
            # Stress Intensification
            # Out-of-Plane
            self.lo = 1.0
            # In-Plane
            self.li = 1.0
            #
            self.C1 = 1.0
            #
            #
        #
        #
        # Recalculating M
        self.M = math.sqrt((self.My * self.li)**2 +
                           (self.Mz * self.lo)**2)
        #
        print (" ")
        print ("Expansion Flexibility ")
        print ("Based on ",self.FSFactorsCode)
        print ("Member Description: ", self.PipeDescription)
        print ("Member Type: ",self.SIFs_Flag)
        print (("h  = "+"%2.3f")%(self.h))
        print (("C1 = "+"%2.3f")%(self.C1))
        print (("k  = "+"%2.3f")%(self.k))
        print (("lo = "+"%2.3f")%(self.lo))
        print (("li = "+"%2.3f")%(self.li))
        #
    #
    #
    # 6.4.2.4 Equivalent stress
    def EquivalentStress(self):
        #
        # NOTE Nominal wall thickness may be used in the evaluation.
        #
        print (" ")
        print ("Equivalent Stress")
        #
        # The total component longitudinal stress should be the sum
        # of the longitudinal stresses arising from pressure, bending,
        # temperature, weight (force), other sustained loadings and 
        # occasional loadings.
        #
        # Accidental loads should be taken into account as indicated
        # in F.7. Account should be taken of the variation in axial
        # restraint throughout the pipeline.
        #
        # A pipeline is deemed to be totally restrained when axial
        # movement and bending resulting from temperature or pressure
        # change is totally prevented.
        #
        # The shear stress should be calculated from the torque and
        # shear force applied to the pipeline using equation (7).
        # (7)
        #
        #   Elastic Modulus (Tnom - Tcorr thickness)
        #   --------------------------------------
        #
        # set Tnom - Tcorr thickness 
        _tnomcorr = self.tnom - self.tcorr
        #
        _Ze = ((math.pi/64.0) * 
                   (self.Do**4 - (self.Do - 2 * _tnomcorr)**4) /
                   (self.Do / 2.0))
        #
        # Pipe section area (Tnom - Tcorr thickness)
        _Anomfab = ((math.pi / 4.)*
                (self.Do**2 -(self.Do - 2*_tnomcorr)**2))
        #
        #self.Tau = ((self.T_/(2.*self.Ze)) + (2*self.Fs/self.A))
        self.Tau = ((self.Mx/(2.*_Ze)) + (2*self.Fs/_Anomfab))
        #
        print (("Tau  = "+"%2.3f")%(self.Tau))
        # 
        #
        # Unless a strain-based design approach is adopted 
        # (see 6.4.3), equivalent stresses should be evaluated
        # using the von Mises stress criterion shown in equation(6)
        # (6)
        self.Sigmae = math.sqrt(self.Sigmah**2 + self.SigmaL**2 -
                                (self.Sigmah * self.SigmaL) + 
                                (3*self.Tau**2))
        #
        print (("Sigmae  = "+"%2.3f")%(self.Sigmae))
        #
        #self.SFeq = (self.fa * self.Sigmay / self.Sigmae)
        #print (("Equiv Stress Usage Factor: " +"%2.3f")%(self.SFeq))
        #
    #
    #
    #
    # stress based on FE analysis inlcuding Temp & Int Pressure
    def StressFEA(self):
        #
        # set Tnom - Tcorr thickness 
        _tnomcorr = self.tnom - self.tcorr
        #
        # Pipe section area (Tnom - Tcorr thickness)
        _Anomfab = ((math.pi / 4.)*
                (self.Do**2 -(self.Do - 2*_tnomcorr)**2))
        #
        # NOTE Nominal wall thickness may be used in the evaluation.
        #
        print (" ")
        print ("Equivalent Stress")
        print ("T fact:", (self.tnom/_tnomcorr))
        #
        #
        _Sigmah = self.Sigmah * (self.tnom/_tnomcorr)
        #
        self.SigmaL = self.SigmaL * (self.tnom/_tnomcorr)
        #
        self.Tau = self.Tau * (self.tnom/_tnomcorr)
        #
        #
        print (("Hoop Stress  = "+"%2.3f")%(_Sigmah))
        print (("Longitudinal Stress = "+"%2.3f")%(self.SigmaL))
        print (("Tau  = "+"%2.3f")%(self.Tau))
        # 
        #
        # Unless a strain-based design approach is adopted 
        # (see 6.4.3), equivalent stresses should be evaluated
        # using the von Mises stress criterion shown in equation(6)
        # (6)
        self.Sigmae = math.sqrt(_Sigmah**2 + self.SigmaL**2 -
                                (_Sigmah * self.SigmaL) + 
                                (3*self.Tau**2))
        #
        print (("Sigmae  = "+"%2.3f")%(self.Sigmae))
        #
        # NOTE Minimum wall thickness may be used in the evaluation
        #
        self.Sigmah = self.Sigmah * (self.tnom/self.tmin)
        #
        print (("Hoop Stress  = "+"%2.3f")%(self.Sigmah))
        #
        #
        #if self.Fx == 0.0:
            # check if SigmaL (-ve) is in compression
        #    if self.SigmaL < 0.0:
                #
        #        self.Fx = abs(self.SigmaL/_Anomfab)
                #
            #
        #
        #
        #
    #
    #
    # 6.4.3 Strain-based design
    # -------------------------
    #
    def EquivalentStrain(self):
        #
        print (" ")
        print ("Strain Design")
        #
        # The limit on equivalent stress recommended in 6.4.2.4 
        # may be replaced by a limit on allowable strain,
        # provided that all the following conditions are met:
        # a) The allowable hoop stress criterion (see 6.4.2.1 and
        #    6.4.2.2) is met.
        #
        # b) Under the maximum operating temperature and pressure,
        #    the plastic component of the equivalent strain does 
        #    not exceed 0.001 (0.1 %).
        #   The reference state for zero strain is the as-built 
        #   state (after pressure test). The plastic component 
        #   of the equivalent uniaxial tensile strain should be
        #   calculated using equation (8).
        #   This analysis can be performed conservatively by assuming
        #   a linearly elastic - perfectly plastic stress/strain curve.
        #   Other, more realistic stress/strain curves may be used. 
        #   However, it is essential that the assumed curve is validated 
        #   as being conservative by material stress/strain curves from
        #   the manufactured pipe.
        #
        # c) Any plastic deformation occurs only when the pipeline is
        #    first raised to its maximum operating pressure and temperature,
        #    but not during subsequent cycles of depressurization, reduction
        #    in temperature to the minimum operating temperature, or return 
        #    to the maximum operating pressure and temperature. This should 
        #    be determined via analytical methods or an appropriate finite 
        #    element analysis. The analysis should include an estimate of 
        #    the operational cycles that the pipeline is likely to 
        #    experience during the operational lifetime.
        # 
        # d) The Do/tnom ratio does not exceed 60.
        #
        if self.Do/self.tnom > 60:
            #
            print ("Do/tnom > 60  --> Strain Design Not Applicable")
        #
        #
        else:
            #
            self.Epsilonp = (math.sqrt((2.0/3.0) * 
                                   (self.EpsilonpL**2 +
                                    self.Epsilonph**2 +
                                    self.Epsilonpr**2)))
            #
            print (("Equivalent Uniaxial Tensile Strain :"+"%2.5f")%(self.Epsilonp))
        #
        # e) Welds have adequate fracture resistance to accept plastic
        #    deformation when determined either by direct testing or by 
        #    fracture mechanics testing and analysis. Where direct testing
        #    is employed, pipes containing maximum credible flaws located in 
        #    the weld metal and heat-affected zone (HAZ) should show that 
        #    fabrication flaws do not extend beyond acceptable limits when
        #    subjected to maximum operational loads. If design envisages 
        #    cyclic loading, this needs to be anticipated in the test. Where 
        #    fracture mechanics testing analysis is employed, testing of 
        #    representative pipe welds should be conducted in accordance with 
        #    BS 7448-1, BS 7448-2, BS 7448-4 and BS EN ISO 12737 as appropriate.
        #    The analysis procedures for fatigue and fracture should be in 
        #    accordance with BS 7910 in order to ascertain whether maximum 
        #    credible  flaws in the weld metal and HAZ extend beyond 
        #    acceptable limits.
        # 
        # f) Actual or angular misalignment at welds is maintained within
        #    defined tolerances.
        #
        # g) A fatigue analysis is carried in accordance with 6.4.6.4.
        #
        # h) A fracture analysis is carried out in accordance with 6.4.6.4.
        #
        # i) Additional limit states are analysed as follows:
        #    1) bending failure resulting from application of a moment in
        #       excess of the moment capacity of the pipe;
        #    2) ovalization - distortion of the pipe wall associated with
        #       bending to high strain levels (see 6.4.4.2 and Annex G);
        #    3) local buckling (see 6.4.4.1 and Annex G);
        #    4) global buckling - lateral or upheaval buckling due to 
        #       overall axial compression (see 6.4.4.1 and Annex G).
        # 
        # Plastic deformation reduces pipeline flexural rigidity; this effect 
        # can reduce resistance to upheaval buckling and should be checked if 
        # upheaval buckling might occur.
        #
        # The effects of strain localization should be taken into account in
        # the strain-based design. Strain localization is associated with 
        # discontinuities in stiffness of the pipeline (bending or axial) and
        # can therefore develop in the following locations:
        #   - changes in wall thickness;
        #   - buckle arrestor locations;
        #   - locally thinned regions, e.g. due to corrosion;
        #   - field joints and coatings;
        #   - welds, due to undermatching of the strength of the weld.
        #
    #
    #
    # ===================================================
    # 6.4.4 Buckling
    #
    # 6.4.4.1 General
    # 
    # The following buckling modes should be taken into account:
    #
    # a) local buckling of the pipe wall due to external pressure,
    #    axial tension or compression, bending and torsion or a 
    #    combination of these loads (see G.1);
    #
    # NOTE 1 For fabrication processes which introduce cold 
    #        deformations giving different strength in tension
    #        and compression, a fabrication factor, Mufab, should 
    #        be determined. Guidance on the selection of a 
    #        suitable fabrication factor is given in 
    #        DNV-OS-F101:2000, Section 5.
    # 
    # b) propagation buckling due to external pressure, following
    #    the formation of local buckles or localized damage (see G.2);
    # 
    # c) restrained pipe buckling due to axial compressive forces, 
    #    induced by high operating temperatures and pressures. This
    #    can take the form of horizontal snaking of pipelines, or 
    #    vertical upheaval of trenched or buried pipelines (see G.3).
    # 
    # NOTE 2 The formulae given in Annex G define one approach to 
    #        analysis. Alternative approaches are available and may
    #        be used where justified.
    # 
    # In all buckling analyses, the nominal wall thickness should be used.
    #
    #
    # 6.4.4.2 Ovality
    #
    # Ovality, or out-of-roundness, of pipes or a section of pipeline
    # that could cause buckling or interference with pigging operations 
    # should be avoided.
    #
    # NOTE 1 In some situations, where loading is dominated by bending,
    #        buckling might not occur but unacceptable levels of 
    #        ovalization can result.
    # 
    # NOTE 2 Ovalization may be calculated in accordance with G.4 in 
    #        the absence of a more rigorous evaluation.
    #
    #
    # ===================================================
    #
    # Annex G (informative)
    # Buckling
    #
    # G.1 Local buckling
    #
    # -------------------------
    # G.1.1 General
    # NOTE Local buckling of the pipe wall can be avoided
    # if the various loads to which the pipe is subjected 
    # are less than the characteristic values in G.1.2 to 
    # G.1.7. where the concrete cladding is thick enough 
    # and reinforced to provide a structural member conforming
    # to BS 6349-1 and BS 8110, it may be used to provide 
    # support against buckling provided that appropriate
    # documentation is given.
    #
    # G.1.2
    def ExternalPressure(self):
        #
        # The characteristic value, Pc, that causes collapse
        # when the external pressure is acting alone, can be
        # calculated using equations (G.1) to (G.4).
        # NOTE 1 See also Murphy and Langner [36].
        # NOTE 2 Equations (G.1) and (G.4) have been changed
        # in this part of PD 8010 to be consistent with common
        # usage, but do not affect the result.
        #
        print (" ")
        print ("External Pressure")
        #
        # (G.4) Maximun Ovality
        # self.fo = (self.Dmax - Dmin)/self.Do
        #
        print (("Maximun Ovality (fo) = "+"%2.3f")%(self.fo))
        #
        # (G.3) Yield Pressure
        self.Py = ((2*self.Sigmay)*(self.tnom/self.Do))
        #
        print (("Yield Pressure (Py) = "+"%2.3f")%(self.Py))
        #
        # (G.2) Elastic Critical Pressure
        self.Pe = (((2*self.E)/(1.0 - self.Poisson**2)) * 
                   (self.tnom/self.Do)**3)
        #
        print (("Elastic Critical Pressure (Pe) = "+"%2.3f")%(self.Pe))
        #
        #
        print (("Po max  = "+"%2.3f")%(self.Po))
        #
        # (G.1)
        # Define Funtion 
        def f(x): return (((x/self.Pe) - 1.0)*
                          ((x/self.Py)**2 - 1.0) - 
                          2*(x/self.Py)*(self.fo*self.Do/self.tnom))
        #
        # Find first root (Note that it may not be the minimum
        # use 'full' to find all roots, but process is slow)
        #self.Search = 'FAST'
        # Find Pc
        self.Pc = GoalSeeker(f, self.Sigmau*10, self.RootSearch)
        #
        print (("Pc  = "+"%2.3f")%(self.Pc))
        print (("URhp  = "+"%2.3f")%(abs(self.Po/self.Pc)))
        #
        #
    #
    # G.1.3
    def AxialCompression(self):
        #
        # If D/tnom is less than 60, local buckling under
        # axial compression does not occur until the mean 
        # axial compression load, Fxc, reaches the yield
        # load, Fy, i.e. as shown in equation (G.5).
        #
        _Fy = math.pi*(self.Do - self.tnom)*self.tnom*self.Sigmay
        #
        self.Fxc = _Fy
        #
        print (" ")
        print ("Axial Compresion")
        print (("Fxc  = "+"%2.3f")%(self.Fxc))
        print (("URaxial  = "+"%2.3f")%(self.Fx/self.Fxc))
        #
    #
    # G.1.4
    def Bending(self):
        #
        # The characteristic bending moment value, Mc, 
        # required to cause buckling when bending moments
        # are acting alone, can be obtained using equations
        # (G.6) and (G.7).
        #
        # (G.7)
        _Mp = ((self.Do - self.tnom)**2 * 
               (self.tnom * self.Sigmay))
        #
        # (G.6)
        self.Mc = ((1.0 - 0.0024*(self.Do/self.tnom)) * _Mp)
        #
        print (" ")
        print ("Bending")
        print (("Mc  = "+"%2.3f")%(self.Mc))
        print (("URbm  = "+"%2.3f")%(self.M/self.Mc))
        #print (("URBMop  = "+"%2.3f")%(self.Mz/self.Mc))
        #
        # The characteristic bending strain, Epsilon_bc, at  
        # which buckling due to bending moments acting alone 
        # occurs, can be obtained using equation (G.8).
        #
        # (G.8)
        self.Epsilonbc = 15.0 * (self.tnom/self.Do)**2
        #
        print (("Epsilon bc  = "+"%2.3f")%(self.Epsilonbc))
        #
        #
    #
    # G.1.5
    def Torsion(self):
        #
        # The characteristic value, Tauc, that causes buckling
        # when torsion is acting alone, can be obtained using
        # equations (G.9) to (G.13).
        # Equation (G.9) is used when Alphat < 1.5; equation (G.10)
        # is used when Alphat is >=1.5 and <=9; and equation (G.11)
        # is used when Alphat > 9.
        #
        print(" ")
        print("Torsion")
        #
        # (G.12)
        _Tauy = self.Sigmay / math.sqrt(3.0)
        #
        print (("Tau y  = "+"%2.3f")%(_Tauy))
        #
        # (G.13)
        _Alphat = (self.E/ _Tauy)*(self.tnom/self.Do)**(3.0/2.0)
        #
        print (("Alpha t = "+"%2.3f")%(_Alphat))
        #
        # 
        if _Alphat < 1.5:
            # (G.9)
            self.Tauc = (0.542 * _Alphat)*_Tauy
            #
        #
        elif _Alphat > 9.0:
            # (G.11)
            self.Tauc = _Tauy
            #
        #
        else:
            # (G.10)
            self.Tauc = (0.813 + 
                         0.068 * math.sqrt(_Alphat - 1.50))
            #
            #
        #
        print (("Tau c = "+"%2.3f")%(self.Tauc))
        #
        _fvt = abs((self.Mx * self.Do)/(2 * self.Ip))
        print (("Torsion  = "+"%2.3f")%(_fvt))
        print (("URt  = "+"%2.3f")%(_fvt/self.Tauc))
        #
        #
    #
    # G.1.6
    def LoadCombination(self):
        #
        # The maximum external overpressure, P, in the 
        # presence of compressive axial force, Fx, and/or
        # bending moment, M, when fo is less than 0.05 (5%),
        # can be calculated using equation (G.14), where:
        # - Gamma is calculated using equation (G.15);
        # - Sigmahb is calculated using equation (G.16);
        # - Sigmahcr is calculated using equation (G.17) 
        #   or equation (G.18) as appropriate.
        #
        print (" ")
        print ("Load Combination")
        #
        if self.fo <= 0.05:
            #
            _SigmahE = self.E * (self.tnom/(self.Do - self.tnom))**2
            #
            print (("Sigma hE  = "+"%2.3f")%(_SigmahE))
            #
            # (G.16)
            _Sigmahb = (self.Po * self.Do)/ (2*self.tnom)
            #
            print (("Sigma hb  = "+"%2.3f")%(_Sigmahb))
            #
            # (G.17)
            if _SigmahE <= ((2.0/3.0) * self.Sigmay) :
                #
                _Sigmahcr = _SigmahE
                #
            #
            # (G.18)
            else:
                # 
                _Sigmahcr = (self.Sigmay * 
                             (1 - 
                              ((1.0/3.0)*(2*self.Sigmay/(3*_SigmahE)))))
            #
            # (G.15)
            _Gamma = (1 + 300.0 * 
                      (self.tnom/self.Do)*(_Sigmahb/_Sigmahcr))
            #
            print (("Gamma  = "+"%2.3f")%(_Gamma))
            #
            self.URlc = (((abs(self.M/self.Mc) + 
                           abs(self.Fx/self.Fxc))**_Gamma) +
                         abs(self.Po/self.Pc))
            #
            print (("UR load comb  = "+"%2.3f")%(self.URlc))
            #
        #
        else:
            print ("fo > 0.05 (5%)")
            print ("Section not applicable")
        #
    #
    # G.1.7
    def StrainCriteria(self):
        #
        # The bending strain, Epsilonb, required to cause
        # buckling, in the presence of external overpressure,
        # P, can be calculated using equation (G.19).
        # (G.19)
        self.Epsilonb = (1 - (self.P/self.Pc))*self.Epsilonbc
        #
        print (("Epsilon b = "+"%2.3f")%(self.Epsilonb))
        #
        # Values for Epsilonbc and Pc can be obtained from 
        # equations (G.8) and (G.1) respectively.
        #
    # 
    # -------------------------
    # G.2 
    def PropagationBuckling(self):
        #
        #
        print(" ")
        print("Propagation Buckling")
        #
        # The potential for a pipeline to propagate local 
        # buckles is dependent on the external overpressure, 
        # P, and its relationship with the propagation pressure Pp.
        #
        # The propagation pressure, Pp, can be calculated using 
        # equation (G.21).
        # (G.21)
        self.Pp = (10.70 * self.Sigmay * (self.tnom/self.Do)**2.25)
        #
        print (("Pp = "+"%2.3f")%(self.Pp))
        #
        #
        # The external overpressure, P, can be calculated using
        # equation (G.20).
        self.P = self.Po - self.Pi
        # Check if external pressure is not significant
        if self.P < 0:
            #
            self.P = self.Po
        #
        print (("P  = "+"%2.3f")%(self.P))
        #
        # If P is less than Pp, then, even though it is possible
        # for the pipe to develop a local buckle, the buckle will
        # not propagate.
        #
        # If P is greater than or equal to Pp and a local buckle
        # or local damage has occurred, then the pipeline is likely
        # to undergo propagation buckling. It can be advisable to 
        # provide buckle arresters
        #
        print (("URpp  = "+"%2.3f")%(abs(self.P/self.Pp)))
        #
    #
    # -------------------------
    # G.3 
    def UpheavalBuckling(self):
        # No yet Included
        print ("No yet Included")
        #
    #
    # -------------------------
    # G.4 
    def Ovalization(self):
        #
        # The total ovalization, f, of a pipe due to
        # the combined effects of unidirectional bending
        # and external pressure can be calculated 
        # using equations (G.25) to (G.27).
        #
        # Values for Pe and fo can be obtained from 
        # equations (G.2) and (G.4) respectively.
        #
        # NOTE If cyclic or reversed bending is applied,
        # the resulting ovalization can be considerably 
        # greater than that predicted by the equation.
        #
        # (G.27)
        self.Cf = 0.120*(1 + self.Do/(120*self.tnom))
        #
        print (" ")
        print("Ovalizacion")
        print (("Cf  = "+"%2.3f")%(self.Cf))
        #
        # (G.26)
        self.Cp = 1.0 / (1.0 - self.Po/self.Pe)
        #
        print (("Cp  = "+"%2.3f")%(self.Cp))
        #
        # (G.25)
        self.f = (self.Cp * ((self.Cf * 
                              (self.Epsilonb*self.Do/self.tnom)**2) +
                             self.fo))
        #
        print (("f  = "+"%2.3f")%(self.f))
        #
    #
    #
#
#
#
