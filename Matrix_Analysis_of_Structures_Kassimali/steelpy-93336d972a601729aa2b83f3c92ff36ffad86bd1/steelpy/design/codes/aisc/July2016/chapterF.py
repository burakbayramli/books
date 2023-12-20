# 
# Copyright (c) 2019 iLift
#
# Python stdlib imports
from math import pi

# package imports
from iLift.codes.process.process import ChapterResults
#

#
def Chapter_F(self, section, material):
    """
    DESIGN OF MEMBERS FOR FLEXURE\n
    This chapter applies to members subject to simple bending about one principal axis. For
    simple bending, the member is loaded in a plane parallel to a principal axis that passes
    through the shear center or is restrained against twisting at load points and supports.\n
    The chapter is organized as follows:
    F1. General Provisions
    F2. Doubly Symmetric Compact I-Shaped Members and Channels Bent About Their Major Axis
    F3. Doubly Symmetric I-Shaped Members with Compact Webs and Noncompact or Slender Flanges Bent About Their Major Axis
    F4. Other I-Shaped Members With Compact or Noncompact Webs Bent About Their Major Axis
    F5. Doubly Symmetric and Singly Symmetric I-Shaped Members With Slender Webs Bent About Their Major Axis
    F6. I-Shaped Members and Channels Bent About Their Minor Axis
    F7. Square and Rectangular HSS and Box-Shaped Members
    F8. Round HSS
    F9. Tees and Double Angles Loaded in the Plane of Symmetry
    F10. Single Angles
    F11. Rectangular Bars and Rounds
    F12. Unsymmetrical Shapes
    F13. Proportions of Beams and Girders
    """
    #  
    #print("")        
    #print("-----------------------------")
    #print("     Chapter F - Flexion     ")
    #print("")
    #
    # F1 General Provisions
    #
    # The design flexural strength and the allowable
    # flexural strength shal be determined as follows:
    #
    # (1) For all provisions in this chapter
    self.Phi_b = 1.0
    self.Omega_b = 1.0
    # ASD
    if 'asd' in self.design_method.lower():
        self.Omega_b = 1.67
    # User defined
    elif 'user_defined' in self.design_method.lower():
        self.Omega_b = self.user_factor_B
    # LRFD
    else:
        self.Phi_b = 0.9
    # 
    # (2) The provision in this chapter are based on
    # the assumtion that points of support for
    # beams and girders are restrained agains
    # rotation about their longitudinal axis
    # 
    # (3) For singly symmetric members in single
    #     curvature and all doubly symmetric members  
    #            
    # Cb = Lateral-Torsional Buckling Factor for 
    #      nonuniform moment diagrams when both ends
    #      are braced    
    #print("Cb = {:4.3f}".format(self.Cb))
    #
    #
    # F2 to F11 Symmetric Shapes 
    #
    if not 'unsymmetric' in section.type.lower() :
        if "i section" in section.type.lower() or "channel" in section.type.lower():
            # F2 to F5 I-shaped members bent about their major axis                                        
            # @hami2230 - the logic in section F has been updated due to errors in the previous version of the Python code.  
            if section.type.lower() == "symmetrical i section" and self.compactness.web.flexure.compactness  == "compact":   
                # F2 & F3 Doubly symmetric I-shaped members bent about 
                # their major axis with compact webs
                
                # This section applies to F2 i.e. doubly symmetric I beams with
                # compact web and flanges
                #                        
                # (F2-5) Limiting Laterally Unbraced Length Lp
                #        for Full Plastic Flexural Stregth
                self.Lp = 1.76 * section.rz * (material.E / material.Fy)**0.50 
                self.Lp_flag = '(F2-5)'
                #                
                # (F2-7) Efective Radius of Gyration rts for
                #        Mayor Axis Bending of Doubly 
                #        Simetric Compact I Beam
                _rts1 = (section.bft 
                         / (12 * (1 + (1 / 6) * (section.d * section.tw
                                                 / (section.bft * section.tft)))) **0.50)
                
                _rts2 = (((section.Cw * section.Iz)**0.50) / section.Zey)**0.50
                
                _rts = min(_rts1, _rts2)
                _rts_flag = '(F2-7)'
                #      
                # (F2-8) Coefficient c is determined:
                #                
                # a) For doubly symmetric I-Shapes
                _c = 1
                _c_flag ='(F2-8a)'
                #                
                # (F2-6) Limiting Laterally Unbraced Length 
                #        Lr for Inelastic Lateral-Torsional
                #        Buckling
                self.Lr = (1.95 * _rts * (material.E / (0.7 * material.Fy)) 
                           * ((section.J * _c / (section.Zey * section.ho)) 
                              + ((section.J * _c / (section.Zey * section.ho))**2 
                                 + (6.76 * (0.7 * material.Fy / material.E)**2))**0.50)**0.50)
                #
                self.Lr_flag = '(F2-6)'
                #
                # Lc -> Maximum Unbraced Length of the compression flange
                #       at which the allowable bending stress is 0.66Fy
                #Lc = max(Lr,Lp)
                #
                # Checking Length of the Beam
                #               
                # (F2-4) Critical Stress Fcr
                self.Fcrx = (((self.Cb * material.E * pi**2) / (self.Lb / _rts)**2) 
                             * (1 + 0.078 * (section.J * _c / (section.Zey * section.ho)) 
                                * (self.Lb / _rts)**2)**0.50)
                
                self.Fcrx_flag ='(F2-4)'
                #
                # F2.1 Yielding
                _Mpx = material.Fy * section.Zpy
                _Mn_F2_1 = _Mpx
                _Mn_F2_1_flag = '(F2-1)'
                #  
                # F2.2 Lateral-Torsional Buckling
                #
                # a) When Lb < Lp the limit state of lateral-torsional
                #    buckling does not apply
                if self.Lb <= self.Lp:
                    # F2.1 Yielding
                    _Mn_F2_2 = _Mn_F2_1
                    _Mn_F2_2_flag = '(F2.2a)'
                else:
                    # c) When Lb > Lr
                    if self.Lb > self.Lr:
                        _Mn_F2_2 = (self.Fcrx * section.Zey)
                        _Mn_F2_2 = min(_Mn_F2_2, _Mpx)
                        _Mn_F2_2_flag = '(F2.2c)'
                    # b) When Lp < Lb < = Lr
                    else:
                        _Mn_F2_2 = (self.Cb * (_Mpx - (_Mpx - 0.7 * material.Fy * section.Zey) 
                                               * ((self.Lb - self.Lp) / (self.Lr - self.Lp))))
                        _Mn_F2_2 = min(_Mn_F2_2, _Mpx)
                        _Mn_F2_2_flag = '(F2.2b)' 
                #-------
                #
                # F2 Doubly symmetric compact I-shaped members bent 
                #    about their major axis                             
                # This section applies to doubly symmetric I-shaped
                # and channel bent aout their major axis, having
                # compact flanges as defined in Sec B4.1 for flexure
                #        
                # The nominal  flexural strength, Mn shall be the
                # lower value obtained according to the limit states
                # of yielding (platic moment) and lateral torsional
                # buckling
                if self.compactness.flange.flexure.compactness == "compact":
                    #
                    self.Mnx = min(_Mn_F2_1, _Mn_F2_2)
                    # flag selection                
                    if self.Mnx == _Mn_F2_1: 
                        self.Mnx_flag = _Mn_F2_1_flag
                    else: 
                        self.Mnx_flag = _Mn_F2_2_flag
                #
                else: 
                    # F3 Doubly Symmetric I_Shaped Members with Compact Webs
                    # and NonCompact or Slenders Flanges Bent about their 
                    # Major Axis
                    # This section applies to doubly symmetric I-shaped
                    # members bent about their major axis having compact
                    # webs and noncompact or slender flanges as defined
                    # in Sec B4.1 for flexure
                    #
                    # -------------------------------
                    # 1.- Lateral-Torsional Buckling
                    # F3.1 
                    _Mn_F3_1 = min(_Mn_F2_1,_Mn_F2_2)
                    _Mn_F3_1_flag = '(F3.1)'
                    # print 'Mnx Section F.2: ',Mn_F2/1000000
                    #
                    # 2.- Compression Flange Local Buckling
                    # F3.2 
                    # Compression Flange
                    _lambda_F3_2 = section.bft / (2 * section.tft)
                    #
                    # Flange Limiting Slenderness
                    _lambda_pf = self._lambda_p_flange
                    _lambda_rf = self._lambda_r_flange_b
                    #
                    # a) For Sections With NonCompact Flanges
                    if "noncompact" in self.compactness.flexure:                   
                        _Mn_F3_2 = (_Mpx - (_Mpx - 0.7 * material.Fy * section.Zey) *
                                    ((_lambda_F3_2 - _lambda_pf) /
                                     (_lambda_rf - _lambda_pf)))      
                        _Mn_F3_2_flag = '(F3.2a)'
                    # b) For Sections With Slender Flanges
                    else:             
                        _Mn_F3_2 = (0.9 * material.E * self.kc * section.Zey / _lambda_F3_2**2)
                        _Mn_F3_2_flag = '(F3.2b)'
                    #
                    # The nominal flexural strength, Mn shall be the
                    # lower value obtained accordingly to the limit
                    # states of lateral-torsional buckling and 
                    # compression flange local buckling
                    #
                    self.Mnx = min(_Mn_F3_1, _Mn_F3_2)
                    # Select flag
                    if self.Mnx == _Mn_F3_1: 
                        self.Mnx_flag = _Mn_F3_1_flag
                    else: 
                        self.Mnx_flag = _Mn_F3_2_flag
            #
            elif (section.type.lower() == "symmetrical i section" and self._class_web_flexure == "noncompact") or (section.type.lower() == "asymmetrical i section" and self._class_web_flexure != "slender") or (self._class_web_flexure == "slender"):
                #
                # This section calculates parameters common to section F4 and F5
                #
                # aw --> Radio of two times the web area in compression due 
                #        to application of major axis bending moment alone 
                #        to the area of the compression flange components
                # (4-12)
                _aw = ((self.hc*section.tw)/(section.bft*section.tft))
                #
                # (4-11)
                #
                # rt --> The effective radius of gyration for lateral-torsional 
                #        buckling
                #
                # i) For I-shapes with a rectangular compression flange
                _rt_1 = (section.bft / (12 * (1 + (_aw / 6.0)))**0.50)         # @hami2230 - this equation was wrong and has been updated
                #
                # ii) For I-shapes with a channel cap or a cover plate attached
                #     to the compression flange
                #
                #_rt_ 2 radius of gyration of the flange components in flexural 
                # compression plus one-third of the web area in compression due 
                # to application of major axis bending moment alone, in. (mm)
                #
                # depth of one-third of web area in compression                # @hami2230 - modified working for _rt_2
                #_hw_ca = 0.3 * (section.Zc - section.tft)
                #
                # 2nd moment of one-third of web area in compression
                #_Iy_hw_ca = ((section.tw * _hw_ca**3) / 12 
                #             + (_hw_ca * section.tw) * (section.Zc - _hw_ca / 2)**2)
                #
                #_rt_2 = ((section._Iy_ft / (section.bft * section.tft))**0.5 
                #         + (_Iy_hw_ca / (_hw_ca * section.tw))**0.5)
                #
                _rt =_rt_1                                                     # @hami2230 - only rectangular compression flanges active
                
                #
                # flag
                if _rt == _rt_1:
                    _rt_flag='(F4-10i)'
                else:
                    _rt_flag='(F4-10ii)'
                #
                # (F4-7)
                # Lp --> The limiting laterally unbraced length for the 
                # limit stated of yielding, Lp, is determined as:
                self.Lp = 1.1 * _rt * (material.E/material.Fy)**0.50
                self.Lp_flag ='(F4-7)'
                #
                # Elastic Section Modulus
                _Sxc = section.Iy /  section.Zc
                _Sxt = section.Iy / (section.d - section.Zc)
                #
                #
                if self._class_web_flexure == "slender" :
                    #
                    # --------------------------------------------------------
                    #
                    # F5
                    #
                    # This section applies to doubly and single symmetric 
                    # I-shaped members with slender webs attached to the
                    # mid-width of the flanges and bent about their major
                    # axis as defined in Sec B4.1 for flexure
                    #
                    # aw is defined by Ecu F4-12 but shall not exceed 10
                    #
                    # hc --> Elastic Centroid to the Compression Flange
                    # hc =2*(section.Zc-section.tft)        
                    #
                    # aw --> Radio of two times the web area in compression 
                    #        due to application of major axis bending moment alone 
                    #        to the area of the compression flange components
                    _aw = min(10.0, _aw)
                    #print ( 'aw ---->:', _aw)
                    #               
                    # rt is the effective radious of gyration for lateral
                    # buckling as defined in section F4
                    #
                     #
                    # Limiting Slenderness of Flange
                    #
                    _lambda_F5 = (section.bft/(2*section.tft))
                    _lambda_pf = self._lambda_p_flange
                    _lambda_rf = self._lambda_r_flange_b
                    #   
                    #
                    # (F5-6) Rpg is the bending strength reduction factor
                    #
                    _Rpg1 = (1 - (_aw/(1200.0 + 300 * _aw))*
                             ((self.hc / section.tw) - 5.70 * (material.E / material.Fy)**0.50))
                    #
                    _Rpg = min(1.0, _Rpg1)
                    #
                    #
                    # -------------------------------------
                    # 1. Compression Flange Yielding
                    # F5-1 
                    #
                    _Mn_F5_1 = _Rpg * material.Fy * _Sxc
                    _Mn_F5_1_flag = '(F5-1)'
                    #
                    # (F4-10) The effective radious of gyration,rt for lateral
                    #         torsional buckling
                    #        
                    # Lp is defined by Equ F4-7  
                    #
                    # (F5-5)
                    self.Lr = pi * _rt * (material.E / (0.70 * material.Fy))**0.50
                    self.Lr_flag = '(F5-5)'
                    #
                    # ------------------------------------
                    # 2. Lateral-Torsional Buckling
                    # F5-2
                    # a) The limit state of lateral-torsional buckling
                    #    does not apply
                    if self.Lb <= self.Lp :
                        _Fcrx_5_2 = material.Fy                                # @hami2230 - typo corrected 
                        _Fcrx_flag_5_2 = '(F5-1)'
                    # (F5-3 b) & (F5-4 c) 
                    else:
                        # (F5-4 c) 
                        if self.Lb > self.Lr:
                            _Fcrx1 = ((self.Cb*material.E*pi**2)/(self.Lb / _rt)**2)
                            _Fcrx_5_2 = min(material.Fy, _Fcrx1)
                            _Fcrx_flag_5_2 = '(F5-4)'
                        # (F5-3 b)
                        else:                          
                            _Fcrx1 = (self.Cb * (material.Fy - (0.30*material.Fy) 
                                                 * ((self.Lb - self.Lp) / (self.Lr - self.Lp))))
                            _Fcrx_5_2 = min(material.Fy, _Fcrx1)
                            _Fcrx_flag_5_2 = '(F5-3)'
                    #     
                    # F5-2
                    _Mn_F5_2 = _Rpg * _Fcrx_5_2 * _Sxc
                    _Mn_F5_2_flag = '(F5-2)'
                    #
                    #
                    # -----------------------------------------------
                    # 3. Compression Flange Local Buckling
                    # F5-3 
                    #
                    # a) For sections with compact flanges, the limit
                    #    state of compresion flange local buckling
                    #    does not apply
                    if "compact" == self._class_flange_flexure:
                        _Fcrx_5_3 = material.Fy
                        _Fcrx_flag_5_3 = '(5-1)'
                    # b) For section with noncompact flanges
                    elif "noncompact" in self._class_flange_flexure:
                        _Fcrx_5_3 = (material.Fy-(0.30*material.Fy)*
                                     ((_lambda_F5 - _lambda_pf) /
                                      (_lambda_rf - _lambda_pf)))
                        
                        _Fcrx_flag_5_3 = '(F5-8)'
                    #
                    # c) For section with slender flanges
                    else:
                        _Fcrx_5_3 = (0.90 * material.E * self.kc
                                     / (section.bft / (2*section.tft))**2)
                        _Fcrx_flag_5_3 = '(F5-9)'
                    #
                    # F5-3                 
                    _Mn_F5_3 = _Rpg * _Fcrx_5_3 * _Sxc
                    _Mn_F5_3_flag = '(F5-7)'
                    #
                    # -------------------------------
                    # 4.- Tension Flange yielding
                    # (F5-4)
                    if _Sxt >= _Sxc:
                        _Fcrx_5_4 = material.Fy
                        _Fcrx_flag_5_4 = '(F5-1)'
                    # (F5-4b)
                    else:
                        _Fcrx_5_4 = material.Fy
                        _Fcrx_flag_5_4 = '(F5-10b)'
                    
                    # (F5-4)
                    _Mn_F5_4 = _Fcrx_5_4 * _Sxt
                    _Mn_F5_4_flag = '(F5-10)'        
                    #
                    #
                    # The nominal flexural strength, Mn shall be the
                    # lowest value obtained according to the limit 
                    # states of compression flange yielding, lateral
                    # torsional buckling, compression flange local
                    # buckling and tension flange yielding
                    #
                    self.Mnx = min(_Mn_F5_1, _Mn_F5_2, _Mn_F5_3, _Mn_F5_4)
                    #
                    if self.Mnx == _Mn_F5_1: 
                        self.Mnx_flag = _Mn_F5_1_flag
                        self.Fcrx = material.Fy
                        self.Fcrx_flag = '(F5-1)'
                    
                    elif self.Mnx == _Mn_F5_2: 
                        self.Mnx_flag = _Mn_F5_2_flag
                        self.Fcrx = _Fcrx_5_2
                        self.Fcrx_flag = _Fcrx_flag_5_2
                    
                    elif self.Mnx == _Mn_F5_3: 
                        self.Mnx_flag = _Mn_F5_3_flag
                        self.Fcrx = _Fcrx_5_3
                        self.Fcrx_flag = _Fcrx_flag_5_3
                    
                    else: 
                        self.Mnx_flag = _Mn_F5_4_flag
                        self.Fcrx = _Fcrx_5_4
                        self.Fcrx_flag = _Fcrx_flag_5_4
                #
                # -----------------------------------------------------------
                #
                # F4 Other I-Shaped members with compact or noncompact
                #    webs bent about their major axis
                else:
                    # This section applies to:
                    # a) Doubly symmetric I-shaped memebers bent about
                    #    their major axis with noncompact webs.
                    # b) Singly symmetric I-shaped memebrs with webs
                    #    attached to the mid-width of the flanges, bent
                    #    about their major axis, with compact or noncompact
                    #    webs, as defined in Sec B4.1 for flexure.
                    #
                    # Moment of inertia of the compression flange
                    #
                    # about weak axis y
                    _Iyc = (section.tft*section.bft**3)/12.0
                    #
                    # Slenderness Parameter
                    _lambda_F4_9 = self.hc/section.tw
                    #
                    # Limiting Slenderness of Compression Web
                    _lambda_pw = self._lambda_p_web                            # @hami2230 - error fixed here                             
                    _lambda_rw = self._lambda_r_web_b
                    
                    # print("lambda: ", _lambda_F4_9.value)
                    # print("lambda p: ", _lambda_pw.value)
                    # print("lambda r: ", _lambda_rw.value)
                    
                    #
                    # Myc = Yield moment in the compression flange
                    # (F4-4)
                    _Myc = material.Fy * _Sxc                                  # @hami2230 - error fixed here
                    #
                    # Plastic Moment
                    _Mpx = min(material.Fy * section.Zpy, 1.6 * section.Zey * material.Fy)  # @hami2230 - error fixed here. Plastic modulus was about minor axis instead of major.
                    
                    # print("Myc :", _Myc.convert('kilonewton * metre').value)
                    # print("Mp :", _Mpx.convert('kilonewton * metre').value)
                    #
                    # FL --> Nominal Flexural Strength
                    # The stress, FL, is determined as follows:
                    #
                    # (F4-6b)(ii)
                    if (_Sxt / _Sxc) < 0.70:
                        self.FL = max(material.Fy * (_Sxt /_Sxc), 0.5 * material.Fy)
                        _FL_flag ='(F4-6b-ii)'                
                    # (F4-6a)(i)
                    else:
                        self.FL = 0.7 * material.Fy
                        _FL_flag ='(F4-6b-i)'
                    #    
                    # print("FL :", self.FL.convert('megapascal').value)
                    # print("FL flag:", _FL_flag)
                    #                                            
                    # The web plastification factor Rpc, shall be
                    # determined as follows:
                    # J shall be taken as zero if:
                    if _Iyc / section.Iz <= 0.23:
                        _J = 0
                        # Rpc --> The web plastification factor
                        _Rpc = 1.0
                        _Rpc_flag = '(Iyc/Iy <= 0.23)'
                    else:
                        _J = section.J
                        #
                        # Rpc --> The web plastification factor
                        #         (F4-9b)(ii)
                        if self.hc / section.tw > _lambda_pw:
                            _Rpc = ((_Mpx/_Myc)-
                                   ((_Mpx/_Myc)-1)*
                                   ((_lambda_F4_9 - _lambda_pw )/
                                    (_lambda_rw - _lambda_pw )))
                            #
                            _Rpc = min(_Rpc, (_Mpx/_Myc))
                            _Rpc_flag = '(F4-9b)'                 
                        # (F4-9a)(i)
                        else:
                            _Rpc = (_Mpx/_Myc)
                            _Rpc_flag = '(F4-9a)'
                    #
                    # print("Rpc :", _Rpc)
                    # print("Rpc flag :", _Rpc_flag)
                    #
                    # (F4-8)
                    # rts -> The limiting unbraced lenght for the limit
                    #         state of inelastic lateral-torsional buckling
                    self.Lr = (1.95*_rt*(material.E / self.FL) *
                               ((_J / (_Sxc * section.ho)) +
                                ((section.J / (_Sxc * section.ho))**2 +
                                 (6.76*(self.FL / material.E)**2))**0.50)**0.50)
                    #
                    self.Lr_flag ='(F4-8)'
                    # print ('Lr = ', self.Lr, self.Lr_flag)
                    #
                    # Lc -> Maximum Unbraced Length of the compression flange
                    #       at which the allowable bending stress is 0.66Fy
                    Lc = min(self.Lr, self.Lp)                                 # @hami2230 - Lc can be deleted? Cant see where this is used.
                    #
                    # Checking Length of the Beam
                    #
                    # (F4-5) Critical Stress Fcr
                    self.Fcrx = (((self.Cb*material.E*pi**2)/(self.Lb/_rt)**2)*
                                 (1 + 0.078*(_J/(_Sxc*section.ho))*
                                  (self.Lb/_rt)**2)**0.50)
                    
                    self.Fcrx_flag = '(F4-5)' 
                    #
                    # 1.- Compression Flange Yielding                          
                    # -------------------------------
                    # (F4-1)
                    _Mn_F4_1 = _Rpc * _Myc
                    _Mn_F4_1_flag = '(F4-1)'
                    #print ('Mn_F4_1',_Mn_F4_1)
                    #
                    # 2.- Lateral-Torsional Buckling
                    # ------------------------------
                    #
                    # (F4-2a)
                    if self.Lb <= self.Lp:
                        _Mn_F4_2 = _Mn_F4_1
                        _Mn_F4_2_flag = _Mn_F4_1_flag
                    # (F4-2c & 2b)
                    else:
                        # (F4-2c)
                        if self.Lb > self.Lr:
                            _Mn_F4_2 = min((self.Fcrx * _Sxc),(_Rpc * _Myc))
                            _Mn_F4_2_flag = '(F4-3)'
                        # (F4-2b)
                        else:
                            _Mn = self.Cb*(_Rpc * _Myc -
                                          (_Rpc * _Myc - self.FL * _Sxc) *
                                          ((self.Lb - self.Lp)/(self.Lr - self.Lp)))
                    #
                            _Mn_F4_2 = min(_Mn,(_Rpc * _Myc))
                            _Mn_F4_2_flag = '(F4-2)'
                    #
                    # 3.- Compression Flange Local Buckling                    
                    # -------------------------------------
                    #
                    _lambda_F4_12 = section.bft/(2 * section.tft)              
                    #
                    if self._class_flange_flexure == "noncompact":             # @hami2230 - mistake corrected here
                        _lambda_pf = self._lambda_p_flange
                        _lambda_rf = self._lambda_r_flange_b
                        
                        _Mn_F4_12 = (_Rpc * _Myc - 
                                    ((_Rpc * _Myc - self.FL * _Sxc) * 
                                     ((_lambda_F4_12 - _lambda_pf)/(_lambda_rf - _lambda_pf))))
                        
                        _Mn_F4_12_flag = '(F4-13)'                             # @hami2230 - changed to F4-13                      
                    
                    elif self._class_flange_flexure == 'slender':              # @hami2230 - mistake corrected here
                        self.kc = 4/(section.d / section.tw)**0.5
                        
                        _Mn_F4_12 = (0.90 * material.E * self.kc * _Sxc)/ _lambda_F4_12**2
                        _Mn_F4_12_flag = '(F4-14)'                             # @hami2230 - changed to F4-14 
                    else:
                        _Mn_F4_12 = _Mn_F4_1
                        _Mn_F4_12_flag = _Mn_F4_1_flag
                    #
                    #
                    # 4.- Tension Flange Yielding
                    #--------------------------------------
                    #
                    # b) When
                    if _Sxt < _Sxc:
                        _Myt = material.Fy * _Sxt
                        _lambda_F4_15 = self.hc / section.tw
                        # (F4-(b))
                        if _Iyc / section.Iz > 0.23:                           # @hami2230 - added if statement here for Iyc/Iy > 0.23                        
                            if self.hc/section.tw > _lambda_pw :
                                _Rpt1 = (_Mpx / _Myt - ((_Mpx / _Myt) - 1) 
                                         * ((_lambda_F4_15 - _lambda_pw) / (_lambda_rw - _lambda_pw)))
                                
                                _Rpt = min(_Rpt1, (_Mpx/_Myt))
                                _Rpt_flag = '(F4-16b)'                         # @hami2230 - changed to F4-16
                            else:
                                _Rpt = _Mpx / _Myt
                                _Rpt_flag = '(F4-16a)'                         # @hami2230 - changed to F4-16
                        else:
                            _Rpt = 1
                            
                        _Mn_F4_15 = _Rpt * _Myt
                        _Mn_F4_15_flag = '(F4-15)'                             
                    # (F4-(a))
                    else:
                        _Mn_F4_15 = _Mn_F4_1
                        _Mn_F4_15_flag = _Mn_F4_1_flag
                    #
                    #
                    # -------------------------------------
                    #              SUMMARY
                    # -------------------------------------
                    #
                    self.Mnx = min(_Mn_F4_1,_Mn_F4_2, _Mn_F4_12, _Mn_F4_15)
                    
                    # print("F4.1 :", _Mn_F4_1.convert('kilonewton * metre').value, _Mn_F4_1_flag)
                    # print ("F4.2 :", _Mn_F4_2.convert('kilonewton * metre').value, _Mn_F4_2_flag)
                    # print("F4.3 :", _Mn_F4_12.convert('kilonewton * metre').value, _Mn_F4_12_flag)
                    # print("F4.4 :", _Mn_F4_15.convert('kilonewton * metre').value, _Mn_F4_15_flag)                    
                    #
                    #print ('min(_Mn_F4_1,_Mn_F4_2)', _Mn_F4_1, _Mn_F4_2)
                    #               flag   
                    if self.Mnx == _Mn_F4_1:
                        self.Mnx_flag = _Mn_F4_1_flag
                    #
                    elif self.Mnx == _Mn_F4_2:
                        self.Mnx_flag = _Mn_F4_2_flag
                    #
                    elif self.Mnx == _Mn_F4_12:
                        self.Mnx_flag = _Mn_F4_12_flag
                    #
                    else:
                        self.Mnx_flag = _Mn_F4_15_flag
                    #
                    #
                    # print ('self.Mnx' , self.Mnx, self.Mnx_flag)
                    #
            # -----------------------------------------------------------------
            # F6 I-Shaped Members and Channels Bent About their
            #    Minor Axis
            #            
            #    This section applies to I-Shaped members and
            #    channel bent about their minor axis
            #
            #    The nominal flexural strength, Mn shall be the
            #    lower value obtained according to the limit
            #    states of yielding (plastic moment) and flange
            #    local buckling
            #
            # -------------
            # 1.- Yielding
            # F6.1 
            _Mpy = min( material.Fy * section.Zpz, 1.6 * material.Fy * section.Zez)
            _Mn_F6_1 = _Mpy
            _Mn_F6_1_flag = '(F6-1)'
            #
            # --------------------------
            # 2.- Flange Local Buckling
            # F6.2 
            #
            # Limiting Slenderness of Flange
            #
            _lambda_F6 = 0.5 * section.bft / section.tft                       # @hami2230 - error corrected (*2 on denominator)
            _lambda_pf = self._lambda_p_flange
            _lambda_rf = self._lambda_r_flange_b
            #
            # 
            # (F6-4)
            _tf = min(section.tft, section.tfb)
            #
            self.Fcry = (0.69 * material.E) / (0.5 * section.bft / _tf)**2     # @hami2230 - error corrected (*0.5 on denominator)
            #
            self.Fcry_flag = '(F6-4)'
            #
            #
            # (F6.2a) For Sections with Compact Flanges
            if self.compactness.flange.flexure.compactness == "compact":                        # @hami2230 - error updated
                _Mn_F6_2 = _Mn_F6_1
                _Mn_F6_2_flag = '(F6.1)'
            # F6.2b For Sections with Non Compact Flanges 
            elif self.compactness.flange.flexure.compactness == "noncompact":                   # @hami2230 - error updated
                _Mn_F6_2 = (_Mpy - (_Mpy - 0.70 * material.Fy * section.Zez) *
                            ((_lambda_F6 - _lambda_pf) /
                             (_lambda_rf - _lambda_pf)))
                
                _Mn_F6_2_flag = '(F6.2)'
            # F6.2c For Sections with Slender Flanges
            else:        
                _Mn_F6_2 = self.Fcry * section.Zez
                _Mn_F6_2_flag = '(F6.3)'
            #         
            # Summary Weak Axis
            self.Mny = min(_Mn_F6_1, _Mn_F6_2)
            # Flag            
            if self.Mny == _Mn_F6_1:
                self.Mny_flag = _Mn_F6_1_flag
            else:
                self.Mny_flag = _Mn_F6_2_flag
        #               
        # F11 Rectangular bars and rounds   
        elif 'rectangular bar' in section.type.lower():
            #
            # This section applies to rectangular bars bent about either geometric axis and rounds.
            # The nominal flexural strength, Mn , shall be the lower value obtained according to the
            # limit states of yielding (plastic moment) and lateral-torsional buckling.
            #
            # 1.- Yielding
            if self.Lb * section.d / section.t**2 > 0.08 * material.E / material.Fy :
                if self.Lb * section.t / section.d**2 > 0.08 * material.E / material.Fy:
                    raise ValueError(' F11 Rectangular bars and rounds not applicable')
            
            # My is the moment at yielding of the extreme fiber
            My = material.Fy * section.Zey * 1.60
            Mpy = material.Fy * section.Zpy
            Mnx_1 = min(Mpy, My)
            #
            Mx = material.Fy * section.Zez * 1.60
            Mpx = material.Fy * section.Zpz
            Mny_1 = min(Mpx,  Mx)
            #
            # 2.- Lateral Torsional Buckling                                   # @hami2230 - not assessed for I section local check.
            # major axis
            #if (self.Lb * section.d / section.t**2 < 0.08 * material.E / material.Fy): #@hami2230 - error changed. 1.9 to 0.08
            #    Mnx_2  = Mnx_1
            #    self.Mnx_flag = '(11.1)'
            #elif (self.Lb * section.d / section.t**2 > 1.9 * material.E / material.Fy):
            #    Fcr = 1.9 * material.E * self.Cb / ((self.Lb * section.d) / section.t**2) #@hami2230 - error changed. 1.9 to 0.08
            #    Mnx_2 = Fcr * section.Zey                                         # @hami2230 - error updated
            #    self.Mnx_flag = '(11.3)'
            #else:
            #    Mnx_2 = min(self.Cb * (1.52 - 0.274 * 
            #                           (self.Lb * section.d / section.t**2) 
            #                        * material.Fy / material.E) * My , Mpy)
            #    self.Mnx_flag = '(11.2)'
                
            self.Mnx = Mnx_1
            self.Mnx_flag = '(F11.1)'
            #            
            self.Mny = Mny_1
            self.Mny_flag = '(F11.1)'
        #
        # F13 Proportions of beam and girders  
        else:
            raise Exception('section {:} not yet implemented'.format(section.typ))
    # F12 Unsymmetrical shapes
    else:
        print ('F12 Unsymmetrical No defined yet')
    #
    #print("L  = {:1.2f} mm".format(self.L.convert('millimetre').value))
    #
    #try:
    #    print("Lp = {:1.2f} mm {:}".format(self.Lp.convert('millimetre').value,
    #                                       self.Lp_flag ))
    #    print("Lr = {:1.2f} mm {:}".format(self.Lr.convert('millimetre').value,
    #                                       self.Lr_flag ))
    #except AttributeError: 
    #    pass
    #
    #print("Lb = {:1.2f} mm".format(self.Lb.convert('millimetre').value))
    # 
    self.Mcx = self.Mnx * self.Phi_b / self.Omega_b
    self.Fbx = self.Mcx / section.Zey   
    #
    #print (" ")
    #print ("Major Axis ")
    #try:
    #    print("Fcr = {: 1.3f} N/mm2 {:}".format(self.Fcrx.convert('megapascal').value,
    #                                            self.Fcrx_flag))
    #except AttributeError: 
    #    pass 
    #
    #print('--->', max([abs(_item.value) for _item in self.stress.sigma_y]))
    #
    #print("Mcx = {: 1.3f} kN-m  {:}".format(self.Mcx.convert('kilonewton * metre').value,
    #                                        self.Mnx_flag))
    #print("Fbx = {: 1.3f} N/mm2".format(self.Fbx.convert('megapascal').value))
    ur_outy = max([abs(_item.value/self.Fbx.value) for _item in self.stress.sigma_y])
    #print("Sy (MPa) : ", max([abs(_item.convert('megapascal').value) for _item in self.stress.sigma_y]))
    #print("TauZ (MPa) : ", max([abs(_item.convert('megapascal').value) for _item in self.stress.tau_z]))
    #print("UR Major Axis flexion = {:1.3f}".format(ur_outy))
    #
    # Print Minor Axis Summary
    self.Mcy = self.Mny * self.Phi_b / self.Omega_b 
    self.Fby = self.Mcy / section.Zez
    #
    #print (" ")
    #print ("Minor Axis ")
    #try:
    #    print("Fcr = {: 2.3f} N/mm2 {:}".format(self.Fcry.convert('megapascal').value, 
    #                                            self.Fcry_flag))
    #except AttributeError: 
    #    pass
    #   
    # following line changed so that it prints Mny and Mny_flag, previously it reprinted Mnx and Mnx_flag
    #print("Mcy = {: 1.3f} kN-m  {:}".format(self.Mcy.convert('kilonewton * metre').value,
    #                                        self.Mny_flag))
    #print("Fby = {: 1.3f} N/mm2".format(self.Fby.convert('megapascal').value))
    #print('--->', max([abs(_item.value) for _item in self.stress.sigma_z]))
    ur_outz = max([abs(_item.value/self.Fby.value) for _item in self.stress.sigma_z])
    #print("UR Minor Axis flexion = {:1.3f}".format(ur_outz))
    self.ChaperF_results = ChapterResults(ur_outy, ur_outz, self.Mnx_flag,
                                          self.Fbx, self.Fby)
    #print('-->')
#