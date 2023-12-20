# Copyright (c) 2015-2023 steelpy

# Python stdlib imports
from __future__ import annotations
import math
import datetime
#from typing import NamedTuple, Dict, List, Tuple, Union, Iterator


# package imports
from steelpy.utils.units.main import Units
from steelpy.utils.math.rootsearch import GoalSeeker
from steelpy.design.codes.piping.ASME_B313 import ASME
from steelpy.design.codes.piping.DNV_F101 import DNV
from steelpy.design.codes.piping.process import MainPipe, pipe_section
#
#
# ===================================================
#    PD8010-1:2004 Part 1 : Steel Pipelines on Land
# ===================================================
#
#
class PD8010_2015_1(MainPipe):
    """
    PD 8010-1 & 2: 2015
    """
    #
    def __init__(self):
        """
        """
        super().__init__()
        self.a = 0.72
    # 
    def substances_categorization(self, category:str):
        """
        category : A - Typically non-flammable water-based fluids
                   B - Flammable and/or toxic fluids that are liquids
                       at ambient temperature and at atmospheric
                       pressure conditions
                   C - Non-flammable fluids that are non-toxic
                       gases at ambient temperature and
                       atmospheric pressure conditions
                   D - Non-toxic, single-phase natural gas
                   E - Flammable and/or toxic fluids that are gases
                       at ambient temperature and atmospheric
                       pressure conditions and are conveyed as
                       gases and/or liquids
                       Mixtures of petroleum or chemical
                       substances, having a Reid vapour pressure
                       greater than 31 kPa absolute
        
        
        5.2 Categorization of fluids
        
        The substances to be transported should be categorized, with respect to
        hazard potential in respect of public safety, into one of the five categories given
        in Table 1.
        
        NOTE 1 - Attention is drawn to the Pipelines Safety Regulations 1996 [15] for the
        definition and classification of dangerous fluids (hazardous substances).
        Gases or liquids not specifically included by name should be classified in the
        category containing substances most closely similar in hazard potential to those
        quoted. If the category is not clear, the more hazardous category should be
        assumed.
        
        NOTE 2 - Guidance is given in a number of publications including
        HSE publication L82 [22] and ICE publication Nomenclature for hazard and risk
        assessment [23].
        
        NOTE 3 - Additional guidance on CO2 pipelines is given in DNV-RP-J202.
        """
        a = 0.72
        if category == "C":
            a = 0.30
        return a
    #
    def substance_factor(self, substance:str, Q=None):
        """
        substance : ammonia
                    carbon dioxide dense/gas phase
                    ethylene
                    hydrogen
                    liquid petroleum gas
                    natural gas liquid
                    user
        
        Q         : Substance Factor
        
        Table 3 Substance factors
        """
        if 'ammonia' in substance:
            self.Q = 2.50
        
        elif 'carbon' in substance:
            self.Q = 2.0
            if 'gas' in substance:
                self.Q = 1.0
        
        elif 'ethylene' in substance:
            self.Q = 0.80
        
        elif 'hydrogen' in substance:
            self.Q = 0.45
        
        elif 'petroleum' in substance:
            self.Q = 1.0
    
        elif 'natural' in substance:
            self.Q = 1.25
        
        else:
            self.Q = Q
    #
    def minimum_routeing_distance(self):
        """
        c) For pipelines having a design factor not exceeding 0.72, the minimum
           distance for routeing purposes between the pipeline and occupied
           buildings, Y, should be determined using equation (1).
        """
        self.Y = (self.Q * 
                  ((self.Do**2 / 32000.0) + (self.Do / 160.0) + 11.0) 
                  * (self.p / 3.20 + 1.40))
    #
    def bends(self, n):
        """
        n :
        
        6.2.2.3 Bends
        Changes in direction may be made by bending pipe or installing factory-made
        bends or elbows. All bends should be free from buckling, cracks or other
        evidence of mechanical damage. The nominal internal diameter of a bend
        should not be reduced in ovality by more than 2.5% at any point around the
        bend. Sufficient tangent lengths should be left at each end of a bend to ensure
        good alignment and to facilitate welding. Pipes bent cold should not contain a
        girth weld within the bent section.
        """
        self.thin = 50.0 / (n + 1)
    #
    # 
    def limits_calculated_stress(self, sigma_h:Units, Sah:Units, 
                                 sigma_e:Units, Sae:Units, 
                                 design_method:str):
        """
        sigma_h : hoop stress
        Sah     : allowable hoop stress
        sigma_e : equivalent stress
        Sae     : allowable equivalent stress
        
        6.4.3 Limits of calculated stress
        """
        #
        print(" ")
        print("Allowable Stress")
        #
        URhoop = sigma_h.value / Sah.value
        
        if 'stress' in design_method:
            UReq = sigma_e.value / Sae.value
        else:
            UReq = self.epsilon_p / 0.0050
            
            if self.epsilon_p > 0.0050:
                print (f"Fail  Epsilonp > 0.0010 : {self.epsilon_p:2.3f}")
            else:
                print (f"Pass  Epsilonp < 0.0010 : {self.epsilon_p:2.3f}")            
        #
        print("UR_hoop = {:2.3f}".format(URhoop))
        print("UR_eq   = {:2.3f}".format(UReq))
        return URhoop, UReq
    #
    def allowable_hoop_stress(self, a:float, pipe_history=False):
        """
        a : 
        pipe_history : unknown = False
                         known = True
        
        
        6.4.3.1 Allowable hoop stress
        
        The allowable hoop stress (Sah) should be 
        calculated using equation (11).
        
        The weld joint factor, e, should be 1.0 for pipe conforming to
        BS EN ISO 3183:2012 and/or API 5L:2012 when supplied as seamless,
        longitudinally welded or spirally welded pipe. If the pipe history is unknown,
        the weld joint factor e should not exceed 0.60 for pipe of 0.114 m outside
        diameter or smaller, or 0.80 for pipe larger than 0.114 m outside diameter.
        
        NOTE - The effect of temperature de-rating on the SMYS of carbon steel is included
        in the design factors for temperatures up to 120C.
        """
        e = 1.0
        if not pipe_history :
            if self.Do.value > 0.114:
                e = 0.80
            else:
                e = 0.60
        #
        Sah = a * e * self.sigma_y
        fd_hs = a * e
        print (f"Allowable hoop stress Sah = {Sah.convert('megapascal').value} MPa")
        return Sah, fd_hs
    #
    def allowable_equivalent_stress(self):
        """
        sigma_y : 
        
        6.4.3.2 Allowable Equivalent Stress
        
        NOTE Further guidance is given in BS EN 13480, ASME B31.3 and ASME B31.8.
        """
        Sae = 0.90 * self.sigma_y
        fd =  0.90
        print ("Allowable equivalent stress Sah = {:} MPa".format(Sae.convert('megapascal').value))
        return Sae, fd
    #
    def anchor_blocks(self):
        """
        6.17 Anchor blocks
        
        The design of anchor blocks to prevent axial movement of a pipeline should
        take into account the pipeline expansion force and any pipe-to-soil
        friction-preventing movement. The axial compressive force necessary to restrain
        a pipeline should be calculated using equation (14) for thin wall or
        equation (15) for thick wall.
        """
        # Thin wall
        self.F = (A * (self.E * self.alpha * (self.T2 - self.T1) 
                       + 0.50*self.sigma_hl 
                       - self.Poisson * self.sigma_hl))
        # Thick wall
        self.F = (A * (self.E * self.alpha * (self.T2 - self.T1) 
                       + (self.sigma_hl / (K**2 + 1))
                       - self.Poisson * (self.sigma_hl - P)))
    #
    #
    def hydrostatic_test(self, D, t, Tf):
        """
        D  : pipe diametre
        t  : wall thickness
        Tf : Temperature factor change
        
        
        11.7.2 Method of assessment for hydrostatic test
        
        The relationship between pressure and temperature should be calculated in
        accordance with equation (20).
        """
        self.delta_p = 0.10 * (264.70 * Tf / (D/t + 100))
    #
    def hoop_stress(self, P:Units,
                    Do:Units|None=None, tmin:Units|None=None):
        """
        Do   :
        tmin : minimimum wall thickness (m)
        P : External Overpressure

        6.4.2.1 Hoop stress

        The wall thickness used for hoop stress calculation should be the minimum value
        allowing for permitted wall thickness variations, such as fabrication tolerances,
        and subtracting any corrosion allowance, i.e. as shown in equation (4).
        tnom = tmin + tfab + tcorr    (4)
        """
        #
        if not tmin:
            tmin = self.tmin

        if not Do:
            Do = self.Do

        Dih = Do - 2*tmin
        #
        # NOTE For clad or lined pipelines, the strength contribution
        # of the cladding or lining is usually not taken into account,
        # unless it is necessary to contribute to the structural integrity.
        #
        # For all other stress checks in this section, tnom should
        # be used in the calculation of component stresses.
        #
        # Thin wall
        if Do.value/tmin.value > 20:
            # Hoop stress should be calculated using equation (3)
            # (thin wall) when the ratio of Do/tmin is greater than 20.
            sigma_h = P * Do /(2 * tmin)
        else:
            # Equation (5) (thick wall) should be used when the ratio
            # of Do/tmin is less than or equal to 20.
            sigma_h = P * (Do**2 + Dih**2) / (Do**2 - Dih**2)
        #
        print(" ")
        #print(f"Hoop Stress = {tmin.convert('megapascal').value:2.3f}")
        print(f"Sigmah = {sigma_h.convert('megapascal').value:1.3f} MPa")
        #
        return sigma_h
    #
    #
    def shear_stress(self, tcheck, T, Fs, output=False):
        """
        T  : torque
        Fs : Shear Force
        
        6.4.2.4 Shear stree
        The shear stress, tau, should be calculated from the torque and shear force applied
        to the pipeline using equation (9)(7).
        """
        Ai, Ae, CSA, Anomfab, I, Ze, C =  pipe_section(self.Do, self.tnom, tcheck)
        tau = T / (2.0 * C) + (2 * Fs / Anomfab)
        if output:
            print("")
            print("Shear stress")
            print("Tau  = {:2.3f} MPa".format(tau.convert('megapascal').value))
        return tau
    #    
    def longitudinal_stress(self, tcheck, 
                            sigma_h:Units, delta_T:Units, 
                            P:Units, Fx:Units, Mb:Units, 
                            restrained=False, output=False):
        
        """
        restrained : False/True
        temperature_pressure : False/True

        6.4.2.3 Longitudinal stress

        The total longitudinal stress should be the sum of the longitudinal stress arising
        from pressure, bending, temperature, mass, other sustained loadings and
        occasional loadings (see Annex G).

        NOTE A - pipeline is deemed to be totally restrained when axial movement and
        bending resulting from temperature or pressure change is totally prevented.
        """
        #
        #   --------------------------------------
        Ai, Ae, CSA, Anomfab, I, Ze, C =  pipe_section(self.Do, self.tnom, tcheck)
        sigma_b = Mb / Ze
        sigma_x = Fx / Anomfab
        try:
            1/ delta_T.value
            temp_delta = (delta_T.value - 273.15)   # from K to C
        except ZeroDivisionError:
            temp_delta = 0
        alpha = (self.alpha.value - 1/273.15) # from K to C
        #
        print(" ")
        print("Longitudinal Stress Calculation")
        print("Pressure    = {: 1.3e} bar"
              .format(P.convert('megapascal').value*10))
        print("Temperature = {: 1.3e} C".format(temp_delta))
        print("Fx          = {: 1.3e} kN"
              .format(Fx.convert('kilonewton').value))
        print("Mb          = {: 1.3e} kN-m"
              .format(Mb.convert('kilonewton*metre').value))        
        print("-----------------------------")
        print("Sigma_b     = {: 1.3e} MPa"
              .format(sigma_b.convert('megapascal').value))
        print("Sigma_x     = {: 1.3e} MPa"
              .format(sigma_x.convert('megapascal').value))        
        #
        # ------------------------------------------
        # Hoop Stress Component of von Mises Stress
        # S = H -DeltaPi*Ai*(1-2*poisson) - As*E*AlphaT*DeltaT
        # ------------------------------------------
        #
        # Restrained
        if restrained:
            # Temperature induced part of Force
            # - As*E*AlphaT*DeltaT
            Stemp = 0 * self.units.MPa
            #if delta_T:
            Stemp += self.E * alpha * temp_delta
            #
            print("Sigma_t     = {: 1.3e} MPa"
                  .format(Stemp.convert('megapascal').value))
            #
            # Pressure induced part of Force
            Sp = 0 * self.units.MPa
            #if P:
            v = self.Poisson
            # thin wall
            if self.Do.value/self.tnom.value > 20:
                Sp += sigma_h * v 
            else:
                Sp += (sigma_h - P) * v 
            #
            print("Sigma_p     = {: 1.3e} MPa"
                   .format(Sp.convert('megapascal').value))
            #       
            sigma_L = (Sp + Stemp)
            #print("SL = {:2.3f}".format(SL))
        else:
            # Unrestrained
            # Bending Stress due to temperature, weight
            # of pipe contents, insulation, snow and ice,
            # wind or earthquake is calculated by the 
            # following equation:
            #
            # thin wall
            if self.Do.value / self.tnom.value > 20:
                k = self.Do.value / (self.Do.value - 2 * tcheck.value)
            else:
                k = 1.0
            #
            print('k  = {:1.3e}'.format(k))
            sigma_L = sigma_h/(k**2 + 1) + sigma_b
        #
        # ------------------------------------------      
        # Summing all components of longitudinal
        #
        sigma_L += sigma_x
        print("-----------------------------")
        print (("Sigma_L     = {: 1.3e} MPa")
               .format(sigma_L.convert('megapascal').value))
        return sigma_L, sigma_b
    # 
    def equivalent_stress(self, sigma_h, sigma_L, tau, output=False):
        """
        sigma_h
        sigma_L
        tau
        
        6.4.2.5 Equivalent stress
        
        Unless a strain-based design approach is adopted (see 6.4.3), equivalent stresses
        should be evaluated using the von Mises stress criterion shown in equation (6).
        
        NOTE 1 - Nominal wall thickness may be used in the evaluation.
        The total component longitudinal stress should be the sum of the longitudinal
        stresses arising from pressure, bending, temperature, weight (force), other
        sustained loadings and occasional loadings. Accidental loads should be taken
        into account as indicated in F.7. Account should be taken of the variation in
        axial restraint throughout the pipeline.
        
        NOTE 2 - A pipeline is deemed to be totally restrained when axial movement and
        bending resulting from temperature or pressure change is totally prevented.
        
        """
        # 
        print (" ")
        print ("Equivalent Stress")
        #
        #sigma_h =  hoop_stress(self, tnom)
        #
        # Unless a strain-based design approach is adopted 
        # (see 6.4.3), equivalent stresses should be evaluated
        # using the von Mises stress criterion shown in equation(6)
        # (6)
        sigma_e = (sigma_h**2 + sigma_L**2 - sigma_h * sigma_L + 3*tau**2)**0.50
        #
        if output:
            print (" ")
            print ("Functional Stress Calculation")             
            print (("sigmae  = {:2.3f}")
                   .format(sigma_e.convert('megapascal').value))
        #
        #self.SFeq = (self.fd * self.sigma_y / self.sigma_e)
        #print (("Equiv Stress Usage Factor: " +"%2.3f")%(self.SFeq))
        #
        return sigma_e
    #
    #
    def equivalent_strain(self, epsilon_pL, epsilon_ph, epsilon_pr, output=False):
        """
        epsilon_pL : Principal longitudinal plastic strain
        epsilon_ph : Principal circunferential (hoop) strain
        epsilon_pr : Radial plastic strain
        
        6.4.3 Strain-based design
        
        The limit on equivalent stress recommended in 6.4.2.4 
        may be replaced by a limit on allowable strain,
        provided that all the following conditions are met:
        
        a) The allowable hoop stress criterion (see 6.4.2.1 and
           6.4.2.2) is met.
        
        b) Under the maximum operating temperature and pressure,
           the plastic component of the equivalent strain does 
           not exceed 0.005 (0.5 %).
           
        c) The reference state for zero strain is the as-built 
          state (after pressure test). The plastic component 
          of the equivalent uniaxial tensile strain should be
          calculated using equation (8).
          This analysis can be performed conservatively by assuming
          a linearly elastic - perfectly plastic stress/strain curve.
          Other, more realistic stress/strain curves may be used. 
          However, it is essential that the assumed curve is validated 
          as being conservative by material stress/strain curves from
          the manufactured pipe.
        
        d) Any plastic deformation occurs only when the pipeline is
           first raised to its maximum operating pressure and temperature,
           but not during subsequent cycles of depressurization, reduction
           in temperature to the minimum operating temperature, or return 
           to the maximum operating pressure and temperature. This should 
           be determined via analytical methods or an appropriate finite 
           element analysis. The analysis should include an estimate of 
           the operational cycles that the pipeline is likely to 
           experience during the operational lifetime.
        
        e) The Do/tnom ratio does not exceed 60.
        
        f) Axial or angular misalignment at welds is maintained within defined
           tolerances.
        
        g) A fracture analysis is carried out in accordance with 6.4.5.
        
        h) A fatigue analysis is carried in accordance with 6.4.6
        
        i) The weld metal yield stress matches or overmatches the longitudinal yield
           stress of the pipe.
           
        j) For welds where allowable defect sizes are based on an ECA,
           UT supplements radiographic testing, unless automated ultrasonic
           testing (AUT) is performed.
        
        k) Additional limit states are analysed as follows:
           1) bending failure resulting from application of a moment in
              excess of the moment capacity of the pipe;
           2) ovalization - distortion of the pipe wall associated with
              bending to high strain levels (see 6.4.4.2 and Annex G);
           3) local buckling (see 6.4.4.1 and Annex G);
           4) global buckling - lateral or upheaval buckling due to 
              overall axial compression (see 6.4.4.1 and Annex G).
        
        Plastic deformation reduces pipeline flexural rigidity; this effect can reduce
        resistance to upheaval buckling and should be checked if upheaval buckling
        might occur. The effects of strain localization should be taken into account in
        the strain-based design.
        
        NOTE Strain localization is associated with discontinuities in stiffness of the pipeline
            (bending or axial) and can therefore develop in the following locations:
          - changes in wall thickness;
          - buckle arrestor locations;
          - locally thinned regions, e.g. due to corrosion;
          - field joints and coatings;
          - welds, due to undermatching of the strength of the weld.
        
        """
        #
        if self.Do.value/self.tnom.value > 60:
            print ("Do/tnom > 60  --> Strain Design Not Applicable")
            sys.exit()
        else:
            epsilon_P = (2.0/3.0 * (epsilon_pL**2 + epsilon_ph**2 + epsilon_pr**2))**0.50
        #
        if output:
            print (" ")
            print ("Strain Design")
            print ("Equivalent Uniaxial Tensile Strain : {:2.5f}"
                   .format(epsilon_P))
        return epsilon_P
    #    
    #
    def expansion_flexibility(self, FS_code, SIFs_type, pipe_description,
                              flanges=0, T_= 0, r2=0, R1S=0, theta=0):
        """ 
        FS_code : flexibility & Stress Code
        SIFs_type :
        pipe_description : 

        Return :
        h  : Flexibility Characteristic
        k  : Flexibility Factor
        lo : Stress Intensification Out-Plane
        li : Stress Intensification In-Plane
        C1 : Flanges

        6.4.2.3 Expansion and flexibility

        Pipelines and piping should be designed with sufficient
        flexibility to prevent expansion or contraction causing
        excessive forces or stresses in pipe material, joints, 
        equipment, anchors or supports.

        Expansion calculations should be carried out on pipelines
        where flexibility is in doubt, and where temperature changes
        are expected. Thermal and pressure expansion or contraction
        can cause movement at termination points, changes in direction 
        or changes in size. The necessary flexibility should be provided
        if such movements are unrestrained. Account should be taken of 
        buckling forces that can be imposed on pipelines (see 6.4.4).

        The effect of restraints, such as support friction, branch 
        connections and lateral interferences should be taken into account.
        Calculations should take into account stress intensification 
        factors found to be present in components other than plain 
        straight pipe. Account should be taken of any extra flexibility
        of such components.

        NOTE In the absence of more directly applicable data, the 
        flexibility factors and stress intensification factors given in
        BS EN 13480 or ASME B31.3 may be used.

        Pipelines can be restrained so that the longitudinal movement 
        owing to thermal and pressure changes is absorbed by direct 
        axial compression or tension of the pipe. In such cases expansion
        calculations should be carried out taking into account all the 
        forces acting on the pipeline. Consideration should be given to 
        elastic instability due to longitudinal compressive forces.

        Where movement is restrained, flexibility should be provided by
        means of loops, offsets or special fittings. The total operating 
        temperature range should be taken as the difference between the
        maximum and minimum metal temperatures for the operating cycle 
        under consideration and should be used in calculating stresses in 
        loops, bends and offsets.

        The temperature range used in the calculation of reactions on 
        anchors and equipment should be taken as the difference between
        the maximum or minimum metal temperatures and the installation 
        temperature, whichever gives the greater reaction.

        Where there is a likelihood of repeated stress changes (including
        thermal stress) giving rise to fatigue conditions, the stress 
        range and allowable number of cycles should be calculated in 
        accordance with 6.4.6. Nominal pipe wall thickness (including any
        corrosion allowance) and nominal outside diameter should be
        used for expansion and flexibility calculations.
        """
        #
        if FS_code == "ASME_B31.3":
            asme = ASME()
            asme.bend_data(pipe_description,
                           flanges, 
                           T_, 
                           r2, 
                           R1S, 
                           theta)
            h, k, lo, li, C1 = asme.AppendixD()
        #
        # BS EN 13480
        elif FS_code == "BS_EN_13480":
            #
            # Bend Flexibility
            if SIFs_type == 'BEND':
                #
                print ("No implement yet")
            # Tee Flexibility
            elif SIFs_type == 'TEE':
                #
                print ("No implement yet")
            # Unknow
            else:
                print ("No implement yet")
            #
            # 
            # Flanges
            if self.Flanges == 0 or self.Flanges > 2:
                self.C1 = 1.0
            # Rest
            else:
                print ("No implement yet")        
        #
        # No flexibility & Stress Factors
        else:
            # Flexibility Characteristic
            h = 1.0
            # Flexibility Factor
            k = 1.00
            # Stress Intensification
            # Out-of-Plane
            lo = 1.0
            # In-Plane
            li = 1.0
            #
            C1 = 1.0
        #
        #
        print(" ")
        print("Expansion Flexibility ")
        print("Based on ",FS_code)
        print("Member Description: ", pipe_description)
        print("Member Type: ",SIFs_type)
        print(("h  = "+"%2.3f")%(h))
        print(("k  = "+"%2.3f")%(k))
        print(("lo = "+"%2.3f")%(lo))
        print(("li = "+"%2.3f")%(li))
        print(("C1 = "+"%2.3f")%(C1))
        #
        return h, k, lo, li, C1
#    
#     

