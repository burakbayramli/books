# Copyright (c) 2015-2019 steelpy

# Python stdlib imports
import math
#import datetime

# package imports

#-------------------------------------------------
#
#-------------------------------------------------
#
class ISO19902:
    """
    """
    def __init__(self, units, material):
        """
        """
        self._units = units
        self._material = material
    #
    @property
    def PSI(self):
        """
        Shear Force (to prevent slip)
        """
        if self.add_on_flag:
            return self._shear_force_addition_of_member()
        else:
            return self._shear_force_end_to_end_connection()
    
    def _shear_force_end_to_end_connection(self):
        """
        In addition, the substrate member tends to slip 
        relative to the clamp. The total slip force may be
        taken as the vectorial sum of the slip forces in 
        the longitudinal and circumferential directions. 
        The slip force associated with the longitudinal shear 
        force induced by the bending moment (M) in the 
        substrate member may be ignored.
        Hence, the total acting slip force is according to
        Equation (A.15.3-5) where:
        PSl is the slip force between the substrate member
            and the clamp;
        Pax is the axial force on the substrate member;
        T is the torsional moment on the substrate member;
        D is the outside diameter of the substrate member 
          (see Figure A.15.3-2)
        """
        _substrate = self.substrate.section
        _actions = self.substrate.actions
        # (A.15.3-5)
        return (_actions.Fx**2 + (2 * _actions.Mx / _substrate.D)**2)**0.50
    
    def _shear_force_addition_of_member(self):
        """
        The total acting slip force can be taken as (A.15.3-10) where:
        PSl is the slip force between the substrate member and
            the clamp;
        Vip is the in-plane shear force on the added member;
        Vop is the out-of-plane shear force on the added member;
        D is the outside diameter of the substrate member.
        """
        _theta = self._add_on.theta.value
        _substrate = self.substrate.section
        _actions = self.substrate.actions
        # (A.15.3-10)
        return ((_actions.Fx * math.cos(_theta) + 
                     _actions.Fz * math.sin(_theta))**2 +
                    (((2*(_actions.Mx * math.cos(_theta) + 
                          _actions.Mz * math.sin(_theta)))/ 
                      _substrate.D) + _actions.Fy)**2)**0.50
        
    #
    @property
    def M(self):
        """
        prying_moment
        """
        if self.add_on_flag:
            return self._prying_moment_addition_of_member()
        else:
            return self._prying_moment_end_to_end_connection()
    
    def _prying_moment_end_to_end_connection(self):
        """
        The resultants of the in-plane and out-of-plane 
        bending moments and shears can be taken as:
        (A.15.3-1)
        """
        _actions = self.substrate.actions
        return (_actions.My**2 + _actions.Mz**2)**0.50        
    
    def _prying_moment_addition_of_member(self):
        """
        (A.15.3-7)
        """
        _theta = self._add_on.theta.value
        _actions = self._add_on.actions
        return (((_actions.Mz * math.cos(_theta)) +
                 (_actions.Mx * math.sin(_theta)))**2 +
                _actions.My**2)**0.50       
    #
    @property
    def PBM(self):
        """
        PBM, total force on end bolt due to prying moment M
        ref figures A.15.3.2 & A.15.3.3
        """
        # inflection moment point
        a = self.shell.Lc / 2
        # firts bolt from edge of clamp
        b = self.shell.stiffener.e + self.shell.stiffener.Bsp / 2.0
        # moment at first bolt
        w1 = self.wi(a, b)
        print('--------------------------------')
        print('Maximum force per unit length due to bending moment')
        print("w  : {:1.2f} kN/m".format(self.w.convert('kilonewton/metre').value))
        print("w1 : {:1.2f} kN/m".format(w1.convert('kilonewton/metre').value))
        #
        return ((w1 * b) + (self.w - w1) * b/2.0)
    
    @property
    def w(self):
        """
        w : maximum force per unit length due to bending moment
        """
        return 6 * self.M / self.shell.Lc**2        
    
    def wi(self, a, b):
        """
        w : maximum force per unit length due to bending moment
        """      
        return self.w /a * (a - b)
    #
    @property
    def PBP(self):
        """
        PBP is the bolt force induced by other forces parallel to
        the bolt axes and acting on the clamp, if present.
        (A.15.3-9)
        """
        _actions = self.substrate.actions
        if self.add_on_flag:
            _theta = self._add_on.theta.value
            return ((_actions.Fx * math.sin(_theta) - 
                         self.V * math.cos(_theta)) / self.shell.Nbp)
        else:
            # TODO: confirm this assumtion
            return 0 * _actions.Fx
    #
    @property
    def PBV(self):
        """
        """
        # where, further to previous definitions,
        # a = Lc/2;
        _a = self.shell.Lc / 2.0
        #
        # b is the larger of the two distances s and (s/2 + e);
        # s is the bolt spacing
        _b = max(self.shell.stiffener.Bsp.value, 
                 self.shell.stiffener.Bsp.value/2.0 
                 + self.shell.stiffener.e.value) * self.units.m
        #
        # (A.15.3-4) 
        return self.V / (2* _a) * _b
    #
    @property
    def V(self):
        """
        V is the resultant shear force on the substrate 
        member assumed acting as if in-plane
        (A.15.3-2)
        """
        _actions = self.substrate.actions
        # 
        _V = (_actions.Fy**2 + _actions.Fz**2)**0.50
        print("Resultant Shear Force")
        print("V : {:1.3f} KN".format(_V.convert("kilonewton").value))
        return _V
    #
    @property
    def sigma_P(self):
        """
        sigma_p = The interface transfer stress acting on the clamp
        where:
        S : in the slip force
        Ls : is the length of the clamp over which the slip force is assumed to transfer
        """
        _substrate = self.substrate.section
        if self.add_on_flag:
            # A.15.3.4.3.3 Addition-of-member clamps
            # (A-15.3-11)
            return self.PSI / (2.0 * math.pi * _substrate.D * self.Ls)
        else:
            # A.15.3.4.3.2 End-to-end connection clamps
            # (A-15.3-6)
            return self.PSI / (math.pi * _substrate.D * self.Ls)
    #
    @property
    def PBb(self):
        """
        Bolt force required to prevent lift off
        where :
        PBb is the total force on the end bolt 
            (assuming two parallel rows of bolts);
        PBM is the force on the end bolt induced by the resultant 
            bending moment, M;
        PBV is the force on the end bolt induced by the resultant 
            shear force, V;
        PBP is the bolt force induced by other forces parallel to
            the bolt axes and acting on the clamp, if present.
        """
        if self.add_on_flag:
            # (A.15.3-3)
            return (self.PBM + self.PBV + self.PBP)/2.0            
        else:
            # (A.15.3-8)
            return (self.PBM + self.PBP)/2.0
    #
    def get_contact_force(self):
        """
        Calculate contact force to prevent slit
        """
        print("Prying Moment")
        print("M : {:1.3f} kN*m".format(self.M.convert('kilonewton * metre').value))
        #
        print('Bolt force induced by forces parallel to the bolt axes')
        print("PBP : {:1.3f} KN".format(self.PBP.convert('kilonewton').value))
        #
        print('Total force on end bolt induced by the resultant bending moment')
        print("PBM : {:1.3f} kN".format(self.PBM.convert('kilonewton').value))        
        #
        if self.add_on_flag:
            print("The force on the end bolt induced by the resultant shear force V")
            print("PBV : {:1.3f} kN".format(self.PBV.convert('kilonewton').value))
        #
        self.PBb_lift = self.PBb
        print('Bolt force required to prevent lift off')
        print("PBb Lift : {:1.3f} kN".format(self.PBb_lift.convert('kilonewton').value))
        
    #
    def get_shear_force(self):
        """
        Shear force to prevent slip
        """
        print('--------------------------------')
        print('Shear Force to prevent slip')
        print('The total acting slip force')
        print("PSI  : {:1.3f} kN".format(self.PSI.convert('kilonewton').value))
        print('Interface transfer stress (resisted by front shell only)')
        print("SigmaP : {:1.3f} MPa".format(self.sigma_P.convert('megapascal').value))        
        #print("")
    #
    #-------------------------------------------------
    #  A.15.3 Clamps for strengthening and repair
    #-------------------------------------------------
    #
    def clamp_properties(self):
        """
        """
        #
        _substrate = self.substrate.section
        #print("tc =", self.tc)
        # Outside diameter (OD)
        self.shell.Dc = self.shell.Dci + 2 * self.shell.tc
        print ('')
        print('Clamp Properties')        
        print("OD/Dc : {:1.3f} mm".format(self.shell.Dc.convert('millimetre').value))
        #
        self.alpha = math.atan(2 * self.shell.Hgap.value / self.shell.Dc.value)
        print("alpha : {:1.3f} degrees".format(math.degrees(self.alpha)))
        #
        self.thetaC = (180 - 2 * math.degrees(self.alpha))
        print("-------------------------------")
        print("Clamp shell enclosed angle (ThetaC) :  {:1.3f} degrees".format(self.thetaC))
        #
        # Clamp Arc Length
        self.arc_clamp_length = math.pi * _substrate.D * (self.thetaC / 360.0)
        # Contact area of one clamp shell
        self.As = math.pi * (_substrate.D/2.0) * self.shell.Lc
        #
        #print(" ")
        #print("Clamp shell enclosed angle:", self.thetaC )
        print("Clamp Arc Length : {:1.3f} mm"
              .format(self.arc_clamp_length.convert('millimetre').value))
        print("Contact Area of One Clamp : {:1.0f} mm^2"
              .format(self.As.convert('millimetre^2').value))
        #print("As =", self.As)
    #    
    #
    # A.15.3.4.3 Bolt forces
    # A.15.3.4.3.1 General
    #
    # Figures A.15.3-2, A.15.3-3 and A.15.3-4 illustrate 
    # typical applications of clamps for strengthening or
    # repair of structures for, respectively:
    #   a) end-to-end connections;
    #   b) addition of members, and
    #   c) tubular joints.
    #
    # The capability of prestressed clamps to transfer 
    # forces is determined by the bolt prestress. Bolts
    # should be prestressed so that the clamp does not 
    # open or slip due to the forces acting on the clamp.
    # Shear forces and bending moments from the substrate 
    # member  tend to pry or separate the clamp, whereas
    # axial force and torsion tend to cause the clamp 
    # to slip.
    # The prying action on the clamp arising from the 
    # shear forces and bending moments is directly resisted
    # by extension in the bolts. However, the forces that 
    # tend to pull out or slip the substrate member from the
    # clamp are resisted by the friction developed at the 
    # clamp-member interface. This frictional resistance is 
    # induced by the contact pressure developed as the bolts 
    # are tightened or prestressed. Because the prying action
    # tends to relieve the contact pressure and, thus, decrease
    # the resistance against slippage, the bolt prestress should
    # correspond to the greater of the following two bolt forces:
    #   - bolt forces required to resist the prying 
    #    (without separation);
    #   - bolt forces required to prevent slippage.
    #
    # When calculating the bolt forces in clamps intended to 
    # strengthen or repair joints between tubulars, advantage
    # may be taken of the sharing of forces between the clamp 
    # and the substrate member, provided that such load sharing 
    # of forces is demonstrated by analysis or tests. Otherwise,
    # it is conservative to assume that no sharing of forces takes 
    # place and that the clamp fully transfers the substrate forces
    # across the joint.
    #
    # Methods for calculating the required prestressing forces in
    # the bolts to prevent prying and slipping at the clamp-member 
    # interface for each of the three clamp applications are 
    # presented in A.15.3.4.3.2 to A.15.3.4.3.4,
    # ignoring sharing of forces between the clamp and the 
    # substrate member.
    #
    # A.15.3.4.3.2 End-to-end connection clamps
    def end_to_end_connection(self):
        """
        """
        #
        # where:
        # M is the resultant bending moment on the 
        #   substrate member assumed acting as if in-plane;
        # Mipb is the in-plane bending moment on the 
        #      substrate member;
        # Mopb is the out-of-plane bending moment on the 
        #      substrate member;

        # Vip is the in-plane shear force on the substrate
        #     member;
        # Vop is the out-of-plane shear force on the 
        #     substrate member.
        #
        # The force on the end bolt induced by the resultant
        # shear force is equal to the average shear force over
        # the contact length (V/2a) times the distance, b.
        #
        #
        #
        #
        #
        #
        # Tests of an end-to-end joint have shown that in the
        # case of grouted clamps with the bolts oriented as 
        # shown in Figure A.15.3-2, only a fraction of the 
        # applied  out-of-plane bending moment (Mopb) results
        # in bolt force variation so long as no separation of
        # the clamp halves occurs. In absence of pertinent data,
        # one half of the acting Mopb can be substituted in the
        # above equation when calculating the total bending 
        # moment, M. However, for neoprene-lined clamps, tests
        # show that the full Mopb should be used.
        #
        # The overall separation action on the clamp results 
        # in a contact pressure distribution at the clamp-member
        # interface that is maximum at the free end of the clamp
        # and decreases toward the centre of the clamp. The
        # distribution of this pressure and the length over which 
        # it acts is not well understood. In the absence of a
        # detailed analytical or experimental evaluation of such 
        # pressure, the pressure distribution arising from the
        # resulting bending moment (M) may be assumed to decrease
        # over a distance, 2a, from the free end, where it is
        # maximum, as indicated in Figure A.15.3-2. The distance 
        # 2a can be taken as equal to the contact length, Lc.
        #
        # The pressure arising from the resulting shear force (V)
        # may be assumed to be uniform over the same length
        # (2a) over which the bending moment was assumed to have
        # dissipated.
        #
        # In general, the force resisted by the end bolts due to 
        # prying action should include the effect of the resultant
        # bending moment and resultant shear force and, if applicable,
        # any other components of forces acting directly on the clamp 
        # (e.g. add-on members) that are parallel to the bolt axes. 
        # Thus, the bolt force, PB,b, assuming two rows of bolts parallel 
        # with the axis of the substrate members (one on either side) 
        # as shown in Figure A.15.3-2, can in general be given by
        # 
        #
        # (A.15.3-3)
        #self.PBb_lift = (self.PBM + self.PBV + self.PBP)/2.0
        self.PBb_lift = self.PBb
        print('Bolt force required to prevent lift off')
        print("PBb Lift : {:1.3f} kN".format(self.PBb_lift.convert('kilonewton').value))        
        #
        # For clamps installed over end-to-end connections PBP = 0,
        # therefore, only the bending moments and shear forces on 
        # the substrate member contribute to the bolt force through 
        # PBM and PBV. As indicated in Figure A.15.3-2 by the shaded 
        # area, these forces can generally be obtained from the 
        # corresponding pressure distributions brought about by the
        # bending moment and shear force resultants acting over a 
        # distance equal to one half of the bolt spacing on each 
        # side of the bolt.
        self.Ls = self.shell.Lc
        #
        #ISO19902.shear_force(self)
        print('--------------------------------')
        print('Shear Force (to prevent slip)')
        print('The total acting slip force')
        print("PSI  : {:1.3f} kN".format(self.PSI.convert('kilonewton').value))         
        #
        # The minimum bolt force required to prevent slip of
        # the clamp can be obtained by comparing the acting 
        # slip or interface transfer stress to the corresponding
        # strength as given in A.15.3.5.5.
        # The interface transfer stress Sigmap acting on the 
        # clamp is according to Equation (A.15.3-6) where:
        # S is the slip force;
        # Ls is the length of the clamp over which the slip 
        #    force is assumed to transfer.
        #
        # For clamps used in end-to-end joints, Ls should be 
        # taken as the shortest length from the joint to either 
        # end of the clamp, i.e. the shortest contact length Lc, 
        # if the clamp is not placed symmetrically relative to 
        # the joint. Both halves of the clamp are assumed to 
        # contribute to the slip resistance.
        #
        #_substrate = self.substrate.section
        # (A.15.3-6)
        #self.sigma_P = self.PSI / (math.pi * _substrate.D * self.shell.Lc)
        print('Interface transfer stress (resisted by front shell only)')
        print("SigmaP : {:1.3f} MPa".format(self.sigma_P.convert('megapascal').value))        
    #
    # A.15.3.4.3.3 Addition-of-member clamps
    def addition_of_member(self):
        """
        Figure A.15.3-3 illustrates the use of clamps to 
        add a member. In this instance, the substrate member
        only supports the clamp and the forces acting on the
        add-on member tend to pry and slide the clamp relative
        to the substrate member.
        The prying moment,M, may be taken as (A.15.3-7) where:
        M is the total prying moment on the clamp;
        Mipb is the in-plane bending moment on the added member;
        Mopb is the out-of-plane bending moment on the added member;
        T is the torsional moment on the added member;
        Theta is the included angle between the added member and 
        the substrate member (see Figure A.15.3-3).
        """
        print("")
        print("Addition-of-member clamps")
        print("Contact Force (to prevent lift off)")
        #
        # Both Mipb and Mopb are calculated about the intersection
        # of the add-on member axis and the axis of the substrate 
        # member (the work point).
        # The bolt force, PB,b, is analogous to Equation (A.15.3-3),
        # without a contribution from shear forces in the substrate 
        # member (A.15.3-8) where:
        # PBb is the total force on the end bolt (assuming two 
        #     parallel rows of bolts);
        # PBM is the force on the end bolt pair induced by the total
        #     prying moment, M; PB,M is calculated as for end-to-end 
        #     clamps (see A.15.3.4.3.2);
        # PBP is the force on the end bolt pair parallel to the bolt
        #     axes, induced by the forces in the added member where:
        # P is the axial force on the added brace;
        # V is the resultant shear force on the added brace;
        # Nbp is the number of bolt pairs.
        #
        # (A.15.3-8)
        #self.PBb_lift = (self.PBM + self.PBP)/2.0
        self.PBb_lift = self.PBb
        print('Bolt force required to prevent lift off')
        print("PBb Lift : {:1.3f} kN".format(self.PBb_lift.convert('kilonewton').value))
        #
        #ISO19902.shear_force(self)
        print('--------------------------------')
        print('Shear Force (to prevent slip)')
        print('The total acting slip force')
        print("PSI  : {:1.3f} kN".format(self.PSI.convert('kilonewton').value))         
        #
        # The minimum bolt prestress force required to prevent 
        # slip of the clamp can be obtained by comparing the
        # acting slip or interface transfer stress to the 
        # corresponding strength as given in A.15.3.5.5.
        # The interface transfer stress is assumed resisted by 
        # that half of the clamp to which the add-on member is
        # attached, and can be determined as (A.15.3-11) where:
        # SigmaP is the interface transfer stress;
        # Ls is the length of the clamp over which the slip force
        # is assumed to transfer.
        #
        self.Ls = self.shell.Lc
        #
        # (A.15.3-11)
        #_substrate = self.substrate.section
        #self.sigma_P = self.PSI / (2.0 * math.pi * _substrate.D * self.Ls)
        print('Interface transfer stress (resisted by front shell only)')
        print("SigmaP : {:1.3f} MPa".format(self.sigma_P.convert('megapascal').value))
        #
        # The above slip force is assumed to act only on the clamps
        # half to which the add-on member attaches. The other half 
        # of the clamp is not active due to the inability of the 
        # bolts to transfer shear. However, both halves of
        # the clamp can be made to act simultaneously by providing shear transfer between the two clamp halves using
        # rigidly engaged longitudinal hinges, placed on each side of the clamp.
        #
    #
    # A.15.3.5.5 Interface transfer strength of prestressed clamps
    def prestressed_clamp(self):
        """
        The slip strength equations for prestressed clamps 
        given below are based on a relatively limited experimental
        database [A.15.3-1], primarily with respect to the clamp 
        configuration and distribution of bolts. In using this
        equation, designers should be aware of the limitations of 
        the database. The single most important characteristic 
        of the clamp cross-section is that it has a continuous top
        plate and is very rigid compared to the substrate member 
        (see Figure A.15.3-5).
        
        The database corresponds to pull tests on a single clamp
        geometry having a cross-section as shown in Figure A.15.3-2 
        (end-to-end connection). Three bolt sizes M20, M32, and 
        M40 of constant length and at a constant spacing and 
        prestress are included in the database. The clamp 
        length-to-member diameter ratio varies from 0,5 to 2,0 
        and the member diameter-to-thickness ratios from 20 to 50.
        The steel surfaces were shot blasted, and the grout strength 
        was greater than 50 MPa at 28 days for all tests.
        """
        print('--------------------------------')
        print("Interface transfer strength of prestressed clamps")
        #
        self.gamma_Rg = 2.0
        _substrate = self.substrate.section
        #self.tubular.Dp = self.tubular.D
        #_PBn = self.Nbp * self.PBb
        # Prestressed grouted clamps,
        if "GROUTED" in self.clamp_type.upper():
            #
            # For prestressed grouted clamps, the acting interface 
            # transfer stress (SigmaP) due to factored actions should 
            # satisfy condition (A.15.3-12) where:
            
            # Cp is the scale factor for the diameter of the substrate
            #    member as defined in 15.1.5.1;
            
            # Km is the (modified) radial stiffness factor (see below);
            # fcu is the specified unconfined strength of the grout as 
            #     defined in 15.1.5.1;
            # The radial stiffness factor, Km, is a modification of that
            # used in grouted connections (see 15.1.5.1) to account
            # for the # reduction in hoop stiffness due to the presence
            # of the bolts, and is given Equation (A.15.3-14):
            
            # where
            # Dp is the outside diameter of the substrate member;
            # tp is the wall thickness of the substrate member;
            # sn is the bolt spacing;
            # Ln is the stressed length of the bolt;
            # An is the cross-sectional area of the bolt;
            # m is the ratio of elastic moduli of steel and grout = Es/Eg;
            _m = self.Es/self.Eg
            # Dg is the outside diameter of the grout annulus;
            # tg is the thickness of the grout annulus.
            
            # (A.15.3-14)
            _Km = ((math.pow(((_substrate.D /_substrate.t) + 
                              self.Sn*self.Ln/self.An),-1.0)) + 
                   ((1.0/_m)*math.pow(self.Dg/self.tg, -1.0)))
            
            # Rhoc is a representative friction factor at the grout-steel
            #      interface (see below);
            # The representative friction factor at the grout-steel 
            # interface (not the classical coefficient of friction) 
            # can be determined from
            # (A.15.3-15)
            _Rhoc = 0.15 * (1 + 30*_Km**0.60)
            #
            # fg is the representative interface transfer strength, 
            #    in stress units, given by
            # (A.15.3-13)
            #self.fg = ((2*self.Cp*_Km**0.60 *self.fcu**0.30) + 
            #           (_Rhoc * (_PBn/(_Dp * self.Ls * math.pi/2.0))))
            #
            # The representative interface transfer strength Equation 
            # (A.15.3-13) includes the contributions from:
            # a) the plain member grouted connection as given in 15.1.6
            #    for h/s = 0, but with a modified radial stiffness
            #    factor, Km, and
            # b) the friction developed at the grout-member interface 
            #    due to the prestress in the bolts.
            #
            #
            # Combining equations A.15.3-12 & A.15.3-13 and re-arranging
            # to find PBb gives:
            #
            # Where:
            # self.Nbp : No Bolts Provided
            # self.PBb : Bolt Prestress
            # self.gamma_Rg : Partial resistance factor for interface
            #                transfer for prestressed clamps
            #
            fg = self.gamma_Rg * self.sigma_P
            self.PBb_slip = ((math.pi * _substrate.D * self.Ls * fg 
                              - (2 * self.Cp * _Km**0.60 * self.fcu**0.30)) 
                             / (2 * _Rhoc * self.Nbp))
        # Prestressed lined clamps,
        elif "LINED" in self.clamp_type.upper():
            #
            # For prestressed lined clamps, representative interface 
            # strength values should be determined by tests. In the
            # absence of any such test data, the acting interface 
            # transfer stress, SigmaP, due to factored actions should satisfy
            # the condition (A.15.3-20) with the representative interface 
            # transfer strength given by:
            #
            # (A.15.3-20)
            # self.sigma_P = self.fg/self.GammaRg
            #
            # (A.15.3-21)
            # PBn = self.Nbp * self.PBb
            # self.fg = self.mu* (_PBn/(_Dp * self.Ls * math.pi/2.0))
            #
            #
            # where Mu is a generic coefficient of friction.
            # A value for Mu between 0,1 and 0,2 may be used in the absence
            # of specific data [A.15.3-2]; however, the use of elastomer 
            # lined clamps is not recommended, see 15.3.3 c) and 15.3.6.3.
            #
            # Combining equations A.15.3-20 & A.15.3-21 and re-arranging
            # to find PBb gives:
            #
            # Where:
            # self.Nbp : No Bolts Provided
            # self.PBb : Bolt Prestress
            # self.mu  : Co-efficent of friction between steel
            #            and neoprene linear. This can be taken
            #            between 0.10 & 0.20.
            # self.gamma_Rg : Partial resistance factor for interface
            #                transfer for prestressed clamps
            #
            fg = self.gamma_Rg * self.sigma_P
            self.PBb_slip = ((math.pi * _substrate.D * self.Ls * fg) 
                             / (2 * self.neoprene.Mu * 2 * self.shell.Nbp))
        # Prestressed mechanical clamps,
        else:
            # For prestressed mechanical clamps, the acting interface
            # transfer stress, SigmaP, due to factored actions should
            # also satisfy the condition (A.15.3-16) with:
            #
            _Sn = self.shell.stiffener.Bsp
            _Ln = self.bolt.Lsb
            _An = self.bolt.bolts.tensile_area
            # (A.15.3-19)
            _Km = (math.pow(((_substrate.D.value / _substrate.t.value) 
                             + _Sn.value * _Ln.value / _An.value), -1.0))
            #
            # (A.15.3-18)
            _Rhoc = 0.13 * (1 + 30*_Km**0.60)
            #
            # with the representative interface transfer strength given by
            # (A.15.3-17)
            #self.fg = _Rhoc * (_PBn/(_Dp * self.Ls * math.pi/2.0))
            #
            # In this case, only the steel to steel friction contributes 
            # to the interface transfer strength of the clamp, which is in
            # turn a function of the substrate member and bolt stiffnesses.
            # As in the case of grouted clamps, the transfer strength equation
            # for mechanical clamps is based on limited data. 
            # The restrictions imposed on the database for the grouted clamps 
            # also apply to the mechanical clamps, with the caveat that 
            # mechanical clamps are much more sensitive to fit-up than grouted
            # clamps are. Hence, in designing mechanical clamps, the obtaining 
            # of the actual dimensions of the structure upon which the clamp is
            # to be installed is strongly recommended.
            #
            # Combining equations A.15.3-16 & A.15.3-17 and re-arranging
            # to find PBb gives:
            #
            # Where:
            # self.Nbp : No Bolts Provided
            # self.PBb : Bolt Prestress
            # self.gamma_Rg : Partial resistance factor for interface
            #                transfer for prestressed clamps
            #
            fg = self.gamma_Rg * self.sigma_P  # (A.15.3-16)
            self.PBb_slip = (math.pi * _substrate.D * self.Ls * fg
                             / (2 * _Rhoc * 2 * self.shell.Nbp))
        #
        # For prestressed clamps, the acting interface transfer stress 
        # (SigmaP) due to factored actions should satisfy the 
        # following condition:
        #
        print('Bolt force required to prevent slip')
        print ("PBb Slip = {:1.3f} kN"
               .format(self.PBb_slip.convert('kilonewton').value))
        
    # A.15.3.4.3.1 General 
    def maximum_bolt_tension(self):
        """
        Maximum bolt prestress should correspond to the
        greater of the bolt forces required to prevent
        lift off and slip (i.e max(PBb_slip, PBb_lift))
        """
        # B) Bolt Load Factor of Safety
        # In absence of any other guidance, the factors of
        # safety against slip and lift off in line with
        # JIRRP (OTH88283) guidance can be used
        #
        # Therefore, the factored bolts forces are as
        # follows :
        self.Tmax = max(self.PBb_lift*self.gamma_lift,
                        self.PBb_slip*self.gamma_friction)
        print("-------------------------------------")
        print("Maximum Bolt Tension Required")
        print("Tmax : {:1.3f} kN".format(self.Tmax.convert('kilonewton').value))
    #
    def bolt_selection(self):
        """
        C) Select Suitable Bolts
        
        According to ISO19902 the yield strength of
        the fasteners provided should not be greater
        than 725 N/mm2 (CL 15.2.8.2) or less than
        600 N/mm2 (CL 15.2.8.3) to avoid potential
        embrittlement from exposure to cathodic 
        protecction and to ensure a controled
        preload.
        Good practice also reccommends that the bolt
        hardness shall be limited to < 300 HB due to
        low service temperatures.
        """
        #
        print("-------------------------------------")
        print("Select Suitable Bolts")
        print("Bolt ID : {:}".format(self.bolt.name))
        print("Bolt tensile stress area : {:1.2f} mm2"
              .format(self.bolt.bolts.tensile_area.convert('millimetre^2').value))
        print("Bolt Yield Strength      : {:1.2f} MPa"
              .format(self.bolt.bolts.material.Fy.convert('megapascal').value))
        
        # 1) Allowable Bolt Tension
        _Ft = (0.60 * self.bolt.bolts.material.Fy 
               * self.bolt.bolts.tensile_area * self.Fy_factor )
        print("-------------------------------------")
        print("Bolt Allowable Tension   : {:1.2f} kN".format(_Ft.convert('kilonewton').value))
        #
        _URbt = self.Tmax.value / _Ft.value
        _URbt_flag = "PASS"
        if _URbt > 1.0:
            _URbt_flag = "FAIL"
            print("Bolt Tension UC : {:1.3}".format(_URbt))
            raise Warning('Bolt {:} no suitable'.format(self.bolt.name))
        print("Bolt Tension Unity : {:1.3}".format(_URbt))
        print("Status: {:}".format(_URbt_flag))
        # 2) Bolt Pretension
        # In order to prevent bolt fatigue, the
        # long stud bolts are to be pretensioned.
        # 
        # Max bolt Pretension
        # From ISO19902 clause A.15.2.8.3 Fpt < Fyb
        # (two standard deviations from calibration
        # test). For hydroulic tensioner 10% reduction
        # will be used ==>
        _Fptmax = (0.90 * self.bolt.bolts.material.Fy 
                   * self.bolt.bolts.tensile_area)
        print("-------------------------------------")
        print("Bolt Pretension")
        print("Bolt maximum pretension : {:1.3f} kN"
              .format(_Fptmax.convert('kilonewton').value))
        
        # Min Bolt Pretension
        # Fpt min > 0.60*Fyb + allownce for tension loss
        # Tension loss in bolts occurs initially during
        # the pretensioning and also over the long term
        # through bolt relaxation and creep of liner.
        # Initial losses can be calculated using ISO19902
        # eq A.15.2-22
        _DeltaPmax = self.bolt.bolts.d / self.bolt.Lsb
        print("Delta Pmax : {:1.3f} %".format(_DeltaPmax.value*100))
        
        if _DeltaPmax.value < 0.10:
            _DeltaPmax = 0.10
        print("Minimum Long term losses : {:1.3f} %".format(_DeltaPmax*100))
        print("Initial losses : {:1.3f} %".format(5.0))
        
        _DeltaPmax = _DeltaPmax + 0.050
        print("Total Allow losses : {:1.3f} %".format(_DeltaPmax*100))
        
        if self.bolt.total_Pmax < _DeltaPmax:
            self.total_Pmax = _DeltaPmax
        print("Addopt Allow losses : {:1.3f} %".format(self.total_Pmax*100))
        
        # Therefore the minimum bolt pretension:
        _Fptmin = (0.60 * self.bolt.bolts.material.Fy 
                   * self.bolt.bolts.tensile_area * (1.0 + self.total_Pmax))
        print("Bolt minimum pretension : {:1.3f} kN"
              .format(_Fptmin.convert('kilonewton').value))
        #
        # Pretension range is therefore
        # _Fptmin > Fpt < _Fptmax
        #
        # NOTE - Pretension values above 0.85Fyb can
        # lead to problems with excessive liner strain
        # when ribbed profiles are adopted.
        #
        # The bolt pretension will conservatively
        # taken as the following:
        self.Fpt = (self.bolt.fp  * self.bolt.bolts.material.Fy 
                    * self.bolt.bolts.tensile_area)
        print("Pretension factor fp : {:1.3f}".format(self.bolt.fp))
        print("Fpt : {:1.3f} kN".format(self.Fpt.convert('kilonewton').value))
        # Bolt tension unity for pretension
        _URbpt = self.Fpt.value /_Fptmax.value
        print("Bolt tension unity for pretension : {:1.3f}".format(_URbpt))
        # Note
        # Bolt pretension after loses
        self.Fptal = self.Fpt / (1.0 + self.total_Pmax)
        print("Bolt pretension after loses : {:1.3f} kN"
              .format(self.Fptal.convert('kilonewton').value))
        #print("xxx")
    #
    def neoprene_liner_checks(self):
        """
        Two checks are provided for the neoprene liner.
        The first check is a secondary check on the clamp
        slip capacity based on HSE guidance.
        The second check assess the neoprene strain
        
        Note - The benefit of using a neoprene liner is
        that it ptovides tolerance agains potential lack
        of fit up between the clamp shell and the tubular
        member. This is particulary pevelant offshore
        where clamps are used at locations difficult to
        access. The noeprene liner is bonded to the inner
        face of the clamp shell during fabrication.
        
        Liner can be plain or ribbed. Ribbed liner is
        used on longer/larger clamps to accomodate potentially
        large lack of fit tolerance and permit cathodic
        protection to extend into the leg clamped area.
        """
        _substrate = self.substrate.section
        _actions = self.substrate.actions
        print(" ")
        print("Checks on Neoprene Liner")
        # Plain Neoprene
        if "PLAIN" in self.neoprene.neoprene_type.upper():
            print("Plain Neoprene")
            # 1) Slip Capacity Check
            # Clamp pressure q (considering load after losses)
            _q = (2 * self.shell.Nbp * self.Fptal 
                  / (self.shell.Lc*_substrate.D))
            print("Clamp pressure q = {:1.3f} MPa".format(_q.convert('megapascal').value))
            
            # Slip Capacity of one clamp half under axial load
            # IRHD = Rubber hardness. Typically for neoprene used
            # with offshore clamps IRHD = 65 is used.
            _alpha = (math.sin(math.radians(self.neoprene.IRHD.convert('megapascal').value)) 
                      / 3.0) * self.units.MPa
            #_alpha = math.degrees(_alpha)
            print("Alpha = {:1.3f} MPa".format(_alpha.convert('megapascal').value))
            #
            #self.rruber = 1.0
            if _q.convert('megapascal').value <= 3.0 :
                _Pc = ((_alpha * _substrate.D * self.neoprene.Ln / self.neoprene.gamma) 
                       * (2 * _q.convert('megapascal').value / 3.0 
                          - _q.convert('megapascal').value**2 / 9.0))
            elif _q.convert('megapascal').value <= 8.50 :
                _Pc = _alpha * _substrate.D * self.neoprene.Ln / self.neoprene.gamma
            else:
                raise Warning(' q value ')
            print('Slip capacity of one clamp half under axial load')
            print("Pc = {:1.3f} kN".format(_Pc.convert('kilonewton').value))
            
            # Slip Capacity of clamp under torsional moment
            _Mc = _Pc * _substrate.D
            print('Slip capacity of clamp under torsional moment')
            print("Mc = {:1.3f} kN*m".format(_Mc.convert('kilonewton*metre').value))
            # Note - For HSE check both clamp half shell are
            #        used to resist torsion.
            #
            # Combined Axial and Torsion unity Check
            _UCr = _actions.Fx.value / _Pc.value + _actions.Mx.value / _Mc.value
            print('Combined axial and torsional unity check')
            print("UC = {:1.3f}".format(_UCr))
            
            # 2) Check Neoprene Compressive Strain
            # Shape factor for neoprene, Sn
            _Bn = 0.50 * math.pi * _substrate.D
            _Sn = (self.neoprene.Ln * _Bn
                   / (2*self.neoprene.tn * (self.neoprene.Ln + _Bn)))
            print('')
            print('Check Neoprene compressive strain')
            print("Shape factor for neoprene, Sn = {:1.3f}".format(_Sn.value))
            
            # Compression Modulus, Ecm
            _Ecm = self.neoprene.material.E*( 1.0 + 2.0 * self.neoprene.Kn * _Sn**2)
            print("Compression Modulus, Ecm = {:1.3f} MPa".format(_Ecm.convert('megapascal').value))
            
            if _Ecm.value / self.neoprene.E_alpha.value > 0.10:
                # Use a modified value of Ecm
                _Ecm1 = _Ecm / (1 + _Ecm.value / self.neoprene.E_alpha.value)
            else:
                # Use the unmodified compression modulus
                _Ecm1 = _Ecm
            print("Ecm1 = {:1.3f} MPa".format(_Ecm1.convert('megapascal').value))
            
            # Deflection of neoprene linear due to intial
            # pretensioning clamp pressure q; (before bolt losses)
            _q1 = ((2 * self.shell.Nbp * self.Fpt)/(self.shell.Lc*_substrate.D))
            print("Clamp pressure (before bolt losses) q = {:1.3f} MPa".format(_q1.convert('megapascal').value))
            
            # Then, deflexion on neoprene
            _deltan = _q1 * self.neoprene.tn / _Ecm1
            print("Deflexion on neoprene : {:1.3f} mm".format(_deltan.convert('millimetre').value))
            
            # Compressive Strain in Neoprene
            _USr = _deltan.value / self.neoprene.tn.value
            print("Compressive Strain in Neoprene = {:1.3f} %".format(_USr*100))
            #
            if _USr < self.neoprene.strain_limit:
                print("OK < 10%")
            else:
                raise Warning("FAIL > 10% ==> Increase Neoprene Thickness")
        
        else:
            print("Ribbed Neoprene")
            print("No implemented yet")
    #
    def clamp_shell_design(self):
        """
        """
        print(" ")
        print("Clamp Shell Design")
        _substrate = self.substrate.section
        #
        # 1) Clamp Shell Design
        # Try 20 mm thick clamp shell
        print('Try shell thickness : {:1.3f} mm'
              .format(self.shell.tc.convert('millimetre').value))
        
        # Bolt pretension before losses
        print("Bolt pretension before losses Fpt: {:1.3f} kN"
              .format(self.Fpt.convert('kilonewton').value))
        
        # Hoop Stress
        _fhs = (2 * self.shell.Nbp * self.Fpt)/(2 * self.shell.tc * self.shell.Lc)
        print("Hoop Stress fhs = {:1.3f} MPa".format(_fhs.convert('megapascal').value))
        #
        # Allowable hoop stress
        _Fhs = 0.60 * self.shell.material.Fy
        
        # Unity Check
        self.UCc = _fhs.value / _Fhs.value
        print("Unity Check UC = {:1.3f}".format(self.UCc))
        #if self.UCc > 1.0:
        #    raise Warning("Clamp Shell UC [{:1.3f}] > 1".format(self.UCc))
        #
        # Note - As the clamp stud bolts are offset
        # from the ideal circle of membrane tension
        # causing a bending moment, the shell requires
        # to be partially stiffened to prevent the
        # moment lifting the shell from the substrata.
        # Also, due to the simplistic assumtion taken
        # for hand design, a low UC ratio for clamp
        # shell design should be aimed for as good
        # practice.
        
        # When determining the shell internal diameter
        # (ID) and angular gap of between 1-3 % of the
        # substrata member should be provided to assist
        # fit up of the clamp.
        
        _anular_gap = _substrate.D * self.shell.annular_gap
        print("AnularGap = {:1.2f} mm"
              .format(_anular_gap.convert('millimetre').value))
        _Dci = _substrate.D + _anular_gap
        # Lined
        if self.clamp_type == "LINED":
            _Dci += 2 * self.neoprene.tn
        # grauted
        elif self.clamp_type == "GROUTED":
            _Dci += 2 * self.tgrout
        # Mechanical
        #else:
        #    _Dci = _substrate.D + _anular_gap
        #
        # TODO: below should be fixed in clamp.py
        # Clamp Check
        # Check if Internal Diameter of clamp
        # was given
        try:
            if self.shell.Dci.value < _Dci.value:
                self.shell.Dci = _Dci            
        except AttributeError:
            self.shell.Dci = _Dci
        print("Clamp Shell ID/Dci = {:1.0f} mm"
              .format(self.shell.Dci.convert('millimetre').value))
        print("----------------")
        print("Uniform clamping pressure on both shells")
        # Uniform clamping pressure on both shells
        # (Baxter Brown p. 253):
        if (self.shell.Nbp *self.Fpt / (_substrate.D * self.shell.Lc)) < (self.Fpt/(_substrate.D*self.Lst)):
            self.q = 2.0 * (self.Fpt/(_substrate.D*self.Lst))
        else:
            self.q = 2.0 * (self.Fpt/(_substrate.D*self.Lst))
        print ("q = {:1.3f} MPa".format(self.q.convert('megapascal').value))
#
#