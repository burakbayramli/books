# Copyright (c) 2015-2023 steelpy
#
# Python stdlib imports
from __future__ import annotations
import math
import datetime

# package imports
from steelpy.utils.units.main import Units
from steelpy.design.codes.piping.process import MainPipe, pipe_section
#
#
#
# ===================================================
#      PD8010-2:2004 Part 2 : Subsea Pipelines
# ===================================================
class PD8010_2015_2(MainPipe):
    """
    PD 8010-1 & 2: 2015
    """
    #
    def __init__(self):
        """
        """
        super().__init__()
        # Prop height and natural half-length
        h = [0.05, 0.10, 0.20, 0.30, 0.40, 0.50]
        self.h = [hi*1000.0 for hi in h]
    #    
    # 6.4 Strength
    def design_factors(self, design_condition, pipe_type, output=False):
        """
        fd   : Design factor
        fd_h : Hoop stress desigh factor
        
        6.4.1 Design factors
        
        The design factors, fd, should be used for the stress-based design
        method in 6.4.2. The design factors for strain-based design method 
        are given in 6.4.3.
        
        *NOTE BS EN 14161 allows a design factor of up to 0.83
              to be used. The design factors recommended for UK
              use are shown in Table 2. Higher design factors may
              be used for all or part of a pipeline system, provided
              that an equivalent level of safety is achieved throughout 
              the system under consideration, and across all relevant
              limit states. A full risk assessment (see Annex D) is 
              recommended if higher design factors are used, and might 
              be subject to regulatory review.
        
        Where the application of a reliability-based limit state design 
        method leads to a reduction in the wall thickness that is 
        necessary to meet the recommendations for pressure containment, 
        particular attention should be paid to installation and operability 
        considerations, i.e. it should be demonstrated that the selected 
        wall thickness is appropriate for all the load conditions and 
        combinations that can reasonably be expected throughout the life 
        of the pipeline.
        
        Account should be taken of the variation of strength with 
        temperature on the basis of verifiable test data appropriate to 
        the material under consideration.
        """
        #
        # Table 2 - Design factor, fd
        #
        # Equivalent stresses arising from
        # construction or hydrotest loads
        if "hydrotest" in design_condition:
            # Seabed including tie-in
            # & Riser/landfall
            self.fd = 1.0
            self.fd_hs = 1.0 # Hoop stress
        #
        # Equivalent stresses resulting from
        # functional and environmental or
        # accidental loads
        else:
            # Riser/landfall
            if 'riser' in pipe_type.lower():
                self.fd = 0.72 
                self.fd_hs = 0.60 # Hoop stress
            # Seabed including tie-in
            else: 
                self.fd = 0.96
                self.fd_hs = 0.72 # Hoop stress
        #
        if output :
            print ("")
            print ("Design Factors")
            print ("Test Data  = {:}".format(design_condition))
            print ("Pipe Type  = {:}".format(pipe_type))
            print ("fd         = {:2.3f}".format(self.fd))
            print ("fd hs      = {:2.3f}".format(self.fd_hs))
        #
        return self.fd, self.fd_hs
    #
    # 6.4.2 Stress-based design
    # -------------------------
    def allowable_stress(self, sigma_h, sigma_e, fd, fd_hs,
                         design_method, output=False):
        """
        6.4.2.1 Allowable stress
        
        Stress in the pipeline system should meet the 
        inequality shown in equation (2).
        where SigmaA is determined in accordance with 
        6.4.2.2 or 6.4.2.4 as appropriate.
        The effect of temperature re-rating on the SMYS should be included.
        
        NOTE In the absence of more directly applicable data, the derating data
        in DNV-OS-F101 may be used.
        """
        #
        URhoop = sigma_h.value / (fd_hs * self.sigma_y.value)
        # 
        if 'stress' in design_method:
            UReq = sigma_e.value / (fd * self.sigma_y.value)
            checkout = ("UR eq: {:2.3f}".format(UReq))
        else:
            UReq = self.epsilon_p / 0.0050
            
            if self.epsilon_p > 0.0050:
                checkout = ("Fail  Epsilonp > 0.0010: {:2.3f}"
                            .format(self.epsilon_p))  
            else:
                checkout = ("Pass  Epsilonp < 0.0010: {:2.3f}"
                            .format(self.epsilon_p))
        #
        if output:
            print("")
            print("Allowable Stress")
            print("UR hoop : {:2.3f}".format(URhoop))
            print("{:}".format(checkout))
        return URhoop, UReq
    #
    def hoop_stress(self, P:Units, Do:Units|None=None, 
                    tmin:Units|None=None, output=False):
        """
        Do   :
        tmin : minimimum wall thickness (m)
        P : External Overpressure
        
        6.4.2.2 Hoop stress
        
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
        if output:
            print("")
            print("External Overpressure P : {:2.3f}"
                  .format(P.convert('megapascal').value))
            #print("Hoop Stress, tnom : {:2.3f}"
            #      .format(tmin))
            print("Sigma h  = {:2.3f}"
                  .format(sigma_h.convert('megapascal').value))
        #
        # self.sigma_h = sigma_h
        #
        return sigma_h
    #
    #
    def equivalent_stress(self, sigma_h, sigma_L, tau=0, output=False):
        """
        sigma_h
        sigma_L
        tau
        
        6.4.2.4 Equivalent stress
        
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
            print ("Von Mises Equivalent Stress = {:2.3f}"
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
    def longitudinal_stress(self, tcheck, 
                            sigma_h:Units, delta_T:Units, 
                            P:Units, Fx:Units, Mb:Units, 
                            restrained=False, output=False):
        """
        restrained : False/True
        temperature_pressure : False/True

        6.4.2.4 Equivalent stress

        The total longitudinal stress should be the sum of the longitudinal stress arising
        from pressure, bending, temperature, mass, other sustained loadings and
        occasional loadings (see Annex G).

        NOTE A - pipeline is deemed to be totally restrained when axial movement and
        bending resulting from temperature or pressure change is totally prevented.
        """
        #
        #   --------------------------------------
        Ai, Ae, CSA, Anomfab, I, Ze, Ip =  pipe_section(self.Do, self.tnom, tcheck)
        # 
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
        #checkout = []
        #checkout.append("Hoop Stress = {:2.3f}".format(sigma_h))        
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
            sigma_L = Sp + Stemp
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
        print ("Sigma_L     = {: 1.3e} MPa"
               .format(sigma_L.convert('megapascal').value))
        return sigma_L, sigma_b
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
        Ai, Ae, CSA, Anomfab, I, Ze, Ip =  pipe_section(self.Do, self.tnom, tcheck)
        #
        tau = T / (2.0 * Ze) + (2 * Fs / Anomfab)
        if output:
            print("")
            print("Shear stress")
            print("Tau  = {:2.3f}".format(tau.convert('megapascal').value))
        #
        return tau
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
    # NOTE 1 Local buckling of the pipe wall can be avoided if the various loads to
    # which the pipe is subjected are less than the characteristic values in G.1.2 to G.1.7.
    #
    # NOTE 2 Guidance on buckling is given in DNV-OS-F101.
    # Where the concrete cladding is thick enough and reinforced to provide a
    # structural member conforming to BS 6349-1-4 and BS EN 1992-1-1, it may be
    # used to provide support against buckling provided that appropriate justification
    # is given.
    #
     # -------------------------
    # 
        
    # -------------------------
    #  G.3 Upheaval buckling
    def upheaval_buckling(self, output=False):
        """
        G.3 Upheaval Buckling
        
        Two major factors contribute towards the upheaval of subsea pipelines: the
        effective axial driving force, arising from the internal pressure and temperature,
        and the presence of vertical out-of-straightness (OOS) in the seabed profile.
        """
        # No yet Included
        print ("No yet Included")
    #
    #
    def ovality_bending_effect(self, D, t, j, output=False):
        """
        D 
        t
        j
        """
        #
        f = [(i+1)/100.0 for i in range(j)]
        # 
        I = [ellipse_section(D, t, fo) for fo in f]
        
        EI = [self.E * Ii for Ii in I]
        
        if output:
            print('EIo = {:1.4e} N mm^2'.format(EI[0]))
        
        return EI
    #
    def download_response(self, F, Lr, EI, hr, w_op, rho_sub, f, Dc):
        """
        F : Buckling design force
        Lr : Inflection points height
        EIj : 
        """
        Phi_L = []
        for i in range(len(Lr)):
            Phi_L.append([])
            for j in range(len(EI)):
                Phi_L[i].append(Lr[i] * math.sqrt(F / EI[j]))
        
        Phi_W = []
        for i in range(len(Lr)):
            Phi_W.append([])
            for j in range(len(EI)):
                
                if Phi_L[i][j] < 2.75:
                    Phi_W[i].append(0.180)
                
                else:
                    if Phi_L[i][j] < 10.0:
                        calc = (2.402 / (Phi_L[i][j])**2) - (7.874 / (Phi_L[i][j])**4)
                        Phi_W[i].append(calc)
                    
                    else:
                        Phi_W[i].append(0.023)
        # Required total download
        Wreq = []
        for i in range(len(Lr)):
            Wreq.append([])
            for j in range(len(EI)):
                Wreq[i].append(Phi_W[i][j] * F**2 * hr[i] / EI[j])
        # require cover download
        Wcover = []
        for i in range(len(Lr)):
            Wcover.append([])
            for j in range(len(EI)):
                Wcover[i].append(Wreq[i][j] - w_op)
        # require cover height
        Hreq = []
        for i in range(len(Lr)):
            Hreq.append([])
            for j in range(len(EI)):
                calc = ((math.sqrt(rho_sub**2 * Dc**2 
                                   + 4 * rho_sub * f * Wcover[i][j] ) 
                         - rho_sub * Dc) / (2.0 * rho_sub * f))
                Hreq[i].append(calc)
        #
        return Hreq
    #
    # -------------------------
    # G.4 Ovalization
    def ovalization(self, P, fo, epsilon_b, tnom=None, output=False):
        """
        G.4  Ovalization
        
        The total ovalization, f, of a pipe due to
        the combined effects of unidirectional bending
        and external pressure can be calculated 
        using equations (G.25) to (G.27).
        
        Values for Pe and fo can be obtained from 
        equations (G.2) and (G.4) respectively.
        
        NOTE If cyclic or reversed bending is applied,
        the resulting ovalization can be considerably 
        greater than that predicted by the equation.
        """
        #
        t = self.tnom
        if tnom:
            t = tnom
        # (G.27)
        self.Cf = 0.120 * (1 + self.Do/(120 * t))
        
        # (G.26)
        self.Cp = 1.0 / (1.0 - P/self.Pe)
        
        # (G.25)
        self.f = (self.Cp 
                  * (self.Cf * (epsilon_b * self.Do /t)**2 + fo))
        
        if output:
            print("")
            print("G.4 Ovalizacion")
            print("Cp  = {:2.3f}".format(self.Cp))
            print("Cf  = {:2.3f}".format(self.Cf))            
            print("f   = {:2.3f}".format(self.f))
        #
        return self.Cf, self.Cp, self.f
#
#
# -------------------------
#    
#
