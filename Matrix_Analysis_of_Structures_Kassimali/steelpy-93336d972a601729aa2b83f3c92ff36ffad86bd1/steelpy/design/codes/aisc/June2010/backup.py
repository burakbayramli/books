def print_section_compacness(SectionName):
    '''
    Print section compacness
    '''
    
    global SectionType, Build, SecComp
    global _case, _case_ratio, _case_flag
    global _ratio, _lambda_r, _class
    #
    global _ratio_flange, _lambda_p_flange, _lambda_r_flange
    global _case_flange, _case_flange_ratio, _class_flange
    global _ratio_web, _lambda_p_web, _lambda_r_web
    global _case_web, _case_web_ratio, _class_web
    
    # Memeber Compacness Section
    
    print(" ")
    print("_______________________________________________________________________________________")
    print(" ")
    print("                                  SECTION COMPACTNESS")
    print("")
    print("Section Name {:>3} .....Table B4.1a......  .....Table B4.1b......  .....Table B4.1b......"
          .format(''))
    print("Section Type     Ratio{:>5s}  Element     Ratio{:>5s}  Element     Ratio{:>5s}  Element"
          .format(_case_ratio,_case_flange_ratio, _case_web_ratio))
    print("Section Build {:>14} Case No     lambda p    Case No     lambda p    Case No"
          .format(''))
    print("Compactness      Lambda r    Limiting    Lambda r    Limiting    Lambda r    Limiting"
          .format(''))            
    #print("                 Class                   Class                 Class"
    #      .format(_Units,'',''))
    print(".......................................................................................")
    print(" ")
    print(("{:15s}  [Member in  AxialComp]  [Member in  Flexure 1]  [Member in  Flexure 2]".
           format(SectionName)))
    print(("{:15s}  {:1.4E}  {:10s}  {:1.4E}  UnStiffend  {:1.4E}  Stiffened".
           format(SectionType, _ratio, _case_flag,_ratio_flange, _ratio_web)))
    print(("{:15s}  {:>11} {:10s}  {:1.4E}  {:10s}  {:1.4E}  {:10s}".
           format(Build, '',_case,  _lambda_p_flange, _case_flange, 
                  _lambda_p_web, _case_web)))
    print(("{:15s}  {:1.4E}  {:10s}  {:1.4E}  {:10s}  {:1.4E}  {:10s}".
           format(SecComp, _lambda_r, _class, _lambda_r_flange, 
                  _class_flange, _lambda_r_web, _class_web)))    
    print(" ")
    #
#
    #-------------------------------------------------
    #
    #            ++++++ Chapter D  ++++++ 
    #
    def ChapterD(self):
        print (' ')
        print ('-----------------------------')
        print ('     Chapter D - Tension')
        print (' ')
        # self.L=L
        #
        #if self.Fu == 0:
        #    self.Fu = material.Fy/0.75        
        #
        # D1 Slender Limitation
        _L_rx = self.Lx / self.rx
        _L_ry = self.Ly / self.ry
        #
        self.L_r = max(_L_rx, _L_ry)
        #        
        if self.L_r < 300 : print (("L/r ("+"%6.2f"+" )< 300")%(self.L_r))
        #        
        else: print ('FAIL')
        #        
        # D3 Effective Net Area
        _U_D3 = 1.0
        _Ae_D3_1 = self.An * _U_D3
        #        print 'Ae_D3_1 =',Ae_D3_1
        #
        # D2 Tensile Strength
        # 
        if self.design_method == 'ASD':
            # a)
            _ASD_D2a = 1.67
            _LRFD_D2a = 1.0
            # b)
            _ASD_D2b = 2.0
            _LRFD_D2b = 1.0
            #
        #
        elif self.design_method == 'USER_DEFINED':
            # a)
            _ASD_D2a = self.user_factor_t
            _LRFD_D2a = 1.0
            # b)
            _ASD_D2b = self.user_factor_t
            _LRFD_D2b = 1.0 
            #
        #
        else:
            # a)
            _ASD_D2a = 1.0
            _LRFD_D2a = 0.9
            # b)
            _ASD_D2b = 1.0
            _LRFD_D2b = 0.75
            #
        #
        # a) For tensile yielding i the gross section
        _Pn_D2a = (material.Fy * self.A)
        _Pn_D2a_Flag ='(D2-1)'
        #print 'a)',Pn_D2a, Pn_D2a_Flag 
        #
        #
        # b) For tensile rupture in the net section       
        _Pn_D2b = (self.Fu * _Ae_D3_1)
        _Pn_D2b_Flag ='(D2-2)'
        # print 'b)',Pn_D2b, Pn_D2b_Flag
        #
        # Take the lower value
        if (_Pn_D2a*(_LRFD_D2a / _ASD_D2a)) < (_Pn_D2b *(_LRFD_D2b / _ASD_D2b)) :
            #
            self.Pn_D = _Pn_D2a
            self.Omega_t = _ASD_D2a
            self.Phi_t = _LRFD_D2a 
            self.Pn_D_Flag = _Pn_D2a_Flag
        #
        else:
            self.Pn_D = _Pn_D2b
            self.Omega_t = _ASD_D2b
            self.Phi_t = _LRFD_D2b
            self.Pn_D_Flag = _Pn_D2b_Flag
            #
        # print ('LRFD :',LRFD, '  ASD:',ASD)
        print (("Pn ="+ "%8.2f"+" kN "+ "%-6s" )%(self.Pn_D/1000, self.Pn_D_Flag))
        print (' ')
    #        return self.Pn_D
    #      
    #
    #-------------------------------------------------
    #
    #             ++++++ Chapter E  ++++++ 
    #
    def ChapterE(self):
        print (' ')        
        print ('-----------------------------')
        print ('   Chapter E - Compression')
        print (' ')
        #
        # Stability Factors
        # Kx = Kx
        # Ky = Ky       
        # Kz = Kz
        print (("Kx = "+"%2.3f")%(self.Kx))
        print (("Ky = "+"%2.3f")%(self.Ky))
        print (("Kz = "+"%2.3f")%(self.Kz))
        #
        #        
        #_Fcr_E = 0.0
        #_Fcr_E_flag = 'N/A'
        #
        # E1 General Previsions
        if self.design_method == 'ASD' :
            self.Omega_c = 1.67
            self.Phi_c = 1.0
        #
        elif self.design_method == 'USER_DEFINED' :
            self.Omega_c = self.user_factor_c
            self.Phi_c = 1.0
        #
        else:
            self.Omega_c=1.0
            self.Phi_c = 0.90
        #
        #       E2 Efective Length
        #
        _kLrx = self.Kx*self.Lx/self.rx
        _kLry = self.Ky*self.Ly/self.ry
        #
        if _kLrx > _kLry: self.KLr = _kLrx
        #        
        else: self.KLr = _kLry
        #
        if self.KLr > 200: print (("WARNING KLr ("+"%2.3f"+") > 200")%(self.KLr))
        #        
        else: print (("KLr ("+"%2.3f"+") < 200")%(self.KLr))
        #
        #       --------------------------------------------------------
        #                               CONSTANTS
        #       --------------------------------------------------------
        #            
        # Coordinate of Shear Centre with respect to
        # the centroid (_xo & yo)
        _xo = self.SCx
        _yo = self.SCy
        #
        # Polar radius of gyration about the shear centre
        _ro2 = (_xo**2 + _yo**2 + ((section.Iy + section.Iz)/section.Ag))
        #
        # (E4-10) Flexural Constant H
        _H_E4_10 = 1.0 - ((_xo**2 + _yo**2)/_ro2)
        #
        # (E4-7) Elastic Flexural Buckling Stress Major Axis
        self.Fex_E4_7 = (material.E*math.pi**2)/(_kLrx)**2
        #
        # (E4-8) Elastic Flexural Buckling Stress Minor Axis
        self.Fey_E4_8 = (material.E*math.pi**2)/(_kLry)**2
        #        
        # (E4-9) Elastic Torsional Buckling
        self.Fez_E4_9 = ((((section.Cw*material.E*math.pi**2)/(self.Kz*self.Lzt)**2)+
                     (material.G*section.J))/(section.Ag*_ro2))
        #       Start Q
        self.Q =1.0
        #
        # Elastic Buckling Stress Fe
        # --------------------------        
        #
        # (E3-4)
        #  Fe determined according to Ecu E3-4, as especified
        #  in Appendix 7.2.3(b), or through an elastic buckling
        #  analysis as applicable.
        _Fe_E3_4 = (material.E*math.pi**2)/(self.KLr)**2        
        #
        # E-4 the critical stres Fcr is determined as follows:
        #        
        # a) For double angle and tee-shaped compression members
        # (E4-2) No yet implemented
        # (E4-3) No yet implemented
        #        
        # b) For all other cases, Fcr shal be determined according
        #    to Ecu E3-2 or E3-3, using the torsional or flexural-
        #    torsional elastic buckling stress Fe as follows:
        #        
        # (i) For doubly symmetric members
        # (E4_4)
        _Fe_E4_4 = ((((section.Cw*material.E*math.pi**2)/(self.Kz*self.Lzt)**2)+
                    (material.G*section.J))*(1.0/(section.Iy + section.Iz)))        
        #
        # (ii) For singly symmetric members where y is the axis of
        #      symmetry
        # (E4_5)
        _Fe_E4_5 = (((self.Fey_E4_8 + self.Fez_E4_9)/(2*_H_E4_10))*
                 (1.0-(1-((4.0*self.Fey_E4_8*self.Fez_E4_9*_H_E4_10)/
                          (self.Fey_E4_8 + self.Fez_E4_9)**2))**0.5))
        #
        #
        # --------------------------------------------------------
        #                    END CONSTANTS
        # --------------------------------------------------------
        #
        #
        # E3 & E4 Members Without Slender Elements        
        if self.ClassFlangeE != 'SLENDER' and self.ClassWebE != 'SLENDER':
            #
            # E5 Single Angle Compression Members
            if section.type =='Single-Angle':
                print ('Channel Members not implemented yet')
                # 
            #
            #       E6 Built-Up Members
            elif section.type =='Built-Up':
                print ('Built-Up Members not implemented yet')
                #
            # E3 & E4 Chapters
            else:
                # E3 Flexural Buckling of Members Without slender
                #    Elements and Lz < Lb
                if self.Lzt <= self.Lb:
                    #                
                    # This section applies to compression members without
                    # slender elements as defined in Sec B4.1
                    #                
                    # USER NOTE : When the torsional unbraced length is
                    #             larger than the lateral unbraced length, 
                    #             Section E4 may control the design of wide 
                    #             flange and similarly shaped columns.
                    #
                    # The Critical Stress,Fcr is determined as follows:
                    #                
                    # b) (E3-3)
                    if self.KLr > 4.71*(material.E/material.Fy) and (material.Fy/_Fe_E3_4) > 2.25:
                        _Fcr_E3 = 0.877*_Fe_E3_4
                        _Fcr_E3_flag = '(E3-3)'
                        self.Fe = _Fe_E3_4
                        self.Fe_flag ='(E3-4)'
                        #    
                    #
                    # a) (E3-2)
                    else:
                        _Fcr_E3 = (0.658**(material.Fy/_Fe_E3_4))*material.Fy
                        _Fcr_E3_flag = '(E3-2)'
                        self.Fe = _Fe_E3_4
                        self.Fe_flag = '(E3-4)'                        
                        #
                    #
                    #(E3-4) Elastic Buckling Stress Fe               
                    # _Fe_E3_4_flag = '(E3-4)'
                    # print 'Fe :',_Fe_E3_4, ' ',_Fe_E3_4_flag
                    #
                    # print 'Fcr :',_Fcr_E3,' ',_Fcr_E3_flag
                    #
                    # The Nominal Compressive Strength, Pn shal be determined
                    # based on the limit state of flexural buckling
                    # E3-1  
                    self.Fcr_E = _Fcr_E3
                    self.Fcr_E_flag = _Fcr_E3_flag
                    self.Pn_E = _Fcr_E3*section.Ag
                    self.Pn_E_flag = '(E3)'
                    # print 'Pn Max=',self.Pn_E, self.Pn_E_flag
                #
                # E4 Torsional and Flexural-torsional Buckling of
                #    Members without Slender Elements and Lz > Lb            
                else:
                    # This section applies to singly symmetric and
                    # unsymmeric members and certain doubly symmetric
                    # members, such as cruciform or buil-up columns
                    # without slender elements.
                    # 
                    # The critical stress, Fcr is determined as follows:
                    #                
                    # (E4-3a) For double Angle and Tee-Shaped Members
                    if section.type =='Double-Angle' or section.type =='T':
                        print ('Double-Anle & T Members not implemented yet')
                        #
                    #
                    # (E4-3b) For All Other Cases
                    else:
                        # Fcr shall be determined according to Ecu
                        # E3-2 or E3-3, using the torsional or 
                        # flexural-torsional elastic buckling stress
                        # Fe determine as follows:    
                        #                 
                        # i) For Doubly Symmetric Members
                        if section.type == 'DOUBLY':
                            #                        
                            _Fe_E4 = _Fe_E4_4
                            #_Fe_E4_flag = '(E4-4)'
                            self.Fe = _Fe_E4_4
                            self.Fe_flag ='(E4-4)'                                     
                            #
                        #
                        # ii) For Syngle Symmetric Members where y is
                        #     the axis of symmetry
                        elif section.type == 'SINGLY':
                            #                        
                            _Fe_E4 = _Fe_E4_5
                            #_Fe_E4_flag = '(E4-5)'
                            self.Fe = _Fe_E4_5
                            self.Fe_flag ='(E4-5)'    
                            #
                        #
                        # iii) For Unsymmetric Members, Fc is the lowest 
                        #      root of the cubic equation 
                        else: print ('Unsymmetric MembersNo Yet Defined')
                        #
                        #  The Critical Stress,Fcr is determined as follows:
                        #                
                        #  b) (E3-3)
                        if self.KLr > 4.71*(material.E/material.Fy) and (material.Fy/_Fe_E4) > 2.25:
                            _Fcr_E4 = 0.877*_Fe_E4
                            _Fcr_E4_flag = '(E3-3)'
                        #                    
                        #  a) (E3-2)
                        else:
                            _Fcr_E4 = (0.658**(material.Fy/_Fe_E4))*material.Fy
                            _Fcr_E4_flag = '(E3-2)'       
                            #
                    #-------
                    # The Nominal Compressive Strength, Pn shall be
                    # determined based on the limit states of flexural
                    # torsional and torsional buckling.
                    # E4-1    
                    self.Fcr_E = _Fcr_E4
                    self.Fcr_E_flag = _Fcr_E4_flag    
                    self.Pn_E = _Fcr_E4*section.Ag
                    self.Pn_E_flag = '(E4-1)'
                    # print 'Pn max=',self.Pn_E, self.Pn_E_flag
        #                
        # E7 Members with Slender Elements
        else:
            # This section applies to compression members with 
            # slender elements, as defined in Sec B4.1.
            #
            _Qa = 1.0
            _Qs = 1.0
            #            
            _bt = (max((section.a/section.ta),(section.b/section.tb)))
            #
            # (E7.1) Slender Unstiffened Elements _Qs            
            # if Self.Stiffned == 'NO':
            if not self.stiffned_element:  
                # The reduction fator _Qs for slender unstiffened elements
                # is defined as follows:
                # 
                # (E7.1c) For Single Angles
                if section.type =='ANGLE':
                    #                    
                    # i)
                    if _bt <= 0.45*(material.E/material.Fy)**0.5:
                        _Qs = 1.0
                        _Qs_Flag = '(E7-10)'
                    #
                    # ii)
                    elif _bt > (0.45*(material.E/material.Fy)**0.5) and _bt < (0.91*(material.E/material.Fy)**0.5):
                        _Qs = (1.34-(0.76*_bt*(material.Fy/material.E)**0.5))
                        _Qs_Flag = '(E7-11)'
                    #
                    # iii )
                    else:
                        _Qs = (0.53*material.E)/(material.Fy*_bt**2)
                        _Qs_Flag = '(E7-12)'
                #
                # (E7.1d) For Stems of Tees
                elif section.type =='T':
                    #
                    # i)
                    if _bt <= 0.75*(material.E/material.Fy)**0.5:
                        _Qs = 1.0
                        _Qs_Flag = '(E7-13)'
                    #
                    # ii)
                    elif _bt > (0.75*(material.E/material.Fy)**0.5) and _bt < (1.03*(material.E/material.Fy)**0.5):
                        _Qs = (1.908-(1.22*(section.d/section.tw)*(material.Fy/material.E)**0.5))
                        _Qs_Flag = '(E7-14)'
                    #
                    # iii )
                    else:
                        _Qs = (0.69*material.E)/(material.Fy*(section.d/section.tw)**2)
                        _Qs_Flag = '(E7-12)'
                #                
                # (E7.1a) & (E7.1b) For other compression members
                else :
                    # (E7.1a) For flanges, angles and Plates projecting from 
                    # rolled coulms or other compresion members                    
                    if section.build == "rolled":
                        #                    
                        # i)
                        if _bt <= 0.56*(material.E/material.Fy)**0.5:
                            _Qs = 1.0
                            _Qs_Flag = '(E7-4)'
                        #
                        # ii)
                        elif _bt > (0.56*(material.E/material.Fy)**0.5) and _bt < (1.03*(material.E/material.Fy)**0.5):
                            # print 'here',_bt,material.Fy,material.E,(material.Fy/material.E)**0.5
                            _Qs = (1.415-(0.74*_bt*(material.Fy/material.E)**0.5))
                            _Qs_Flag = '(E7-5)'
                        #
                        # iii )
                        else:
                            _Qs = (0.69*material.E)/(material.Fy*_bt**2)
                            _Qs_Flag = '(E7-6)'
                            #
                    #                        
                    # (E7.1b) For flanges, angles and plates projecting from build-up
                    # columns or other compression memebers (welded)
                    else:
                        #
                        # i)   (E7-7)
                        if _bt <= 0.64*(self.Kc*material.E/material.Fy)**0.5:
                            _Qs = 1.0
                            _Qs_Flag = '(E7-7)'
                        #                        
                        # ii)  (E7-8)
                        elif _bt > (0.64*(self.Kc*material.E/material.Fy)**0.5) and _bt < (1.17*(self.Kc*material.E/material.Fy)**0.5):
                            _Qs = (1.415-(0.65*_bt*(material.Fy/(material.E*self.Kc))**0.5))
                            _Qs_Flag = '(E7-8)'
                        #                        
                        # iii ) (E7-9)
                        else:
                            _Qs = (0.90*material.E*self.Kc)/(material.Fy*_bt**2)
                            _Qs_Flag = '(E7-9)'
            #-----------
            # 
            # (E7.2) Slender Stiffened Elements _Qa
            else:
                # The reduction factor _Qa, for slender stiffened
                # elements is defined as follows:                
                # 
                # Where:  
                # f = Pn/Ae                    
                _f = material.Fy                
                #                
                # (E7.2b) For flanges of square and rectangular sections
                #         of uniform thickness
                if section.type =='BOX':                    
                    # 
                    # USER NOTE: In lieu of calculating f = Pn/Ae, which
                    # requires iteration, f may be taken as Fy, this will 
                    # result slightly conservative estimate of column
                    # capacity
                    #                    
                    # Where:  
                    # f = Pn/Ae                    
                    #_f = material.Fy
                    _tw = min(self.TL, self.TR)
                    _tf = min(self.TT, self.TB)
                    #  
                    if (section.d/_tw) > 1.40*(material.E/_f) and (self.B/_tf) > 1.40*(material.E/_f):
                        # (E7-18)
                        # Webs
                        _be_tw = ((1.92*_tw*(material.E/_f)**0.5)*
                                  (1-(0.38/(section.d/_tw))*(material.E/_f)**0.5))
                        #
                        _be_H = min(_be_tw, section.d)
                        #
                        # Flanges
                        _be_tf = ((1.92*_tf*(material.E/_f)**0.5)*
                                  (1-(0.38/(self.B/_tf))*(material.E/_f)**0.5))
                        #
                        _be_B = min(_be_tf, (self.B-self.TL-self.TR))
                        #
                        # (1-(0.38/_bt)*(material.E/f)**0.5))
                        # be_E7 = min (be_E7,section.a)
                        #print ('Box section no defined yet')
                        _Ae_E7 = 2*(_be_H*_tw)+2*(_be_B*_tf)
                        #_Ae_flag = '(E7-18)'
                    #
                    else:
                        print ('*******************************')
                        print ('(E7-18)' )
                        if (section.d/_tw) < 1.40*(material.E/_f):
                            print ('ERROR H-b/t (',(section.d/_tw),') < 1.40*E/f (',1.49*(material.E/f),')')
                            #
                        else:
                            print ('ERROR B-b/t (',(self.B/_tf),') < 1.40*E/f (',1.49*(material.E/f),')')
                            #
                        print ('*******************************')                        
                #---
                #
                # (E7.2c) For axial loaded circular section
                elif section.type =='CIRCULAR':
                    #
                    if 0.11*(material.E/material.Fy) < (self.od/self.wt) and (self.od/self.wt) < 0.45*(material.E/material.Fy):
                        #
                        _Q_circular = ((0.38*material.E)/(material.Fy*(self.od/self.wt))) + (2.0/3.0)
                        _Ae_E7 = self.A
                        #_Ae_flag = '(E7-19)'
                        #
                    #
                    else:
                        print ('*******************************')
                        print ('(E7-19)' )
                        if (self.od/self.wt) < 0.45*(material.E/material.Fy):
                            print ('WARNING D/t (',(self.od/self.wt),') > 0.45*E/Fy (',0.45*(material.E/material.Fy),')')
                        else:
                            print ('WARNING D/t (',(self.od/self.wt),') < 0.11*E/Fy (',0.11*(material.E/material.Fy),')')
                        print ('*******************************')                        
                #---
                #
                # (E7.2a) For uniformly compressed slender elements except 
                # flanges of square and rectangular sections of 
                # uniform thickness
                else:
                    #
                    #
                    # f is taken as Fcr with Fcr based on Q = 1.0    
                    #                    
                    # The critical stress, Fcr shall be determined as follows:
                    # b)
                    if self.KLr > 4.71*(material.E/(material.Fy))**0.5 and (material.Fy/_Fe_E7) > 2.25:
                        _f = 0.877*_Fe_E7
                    #
                    # a)
                    else: _f = (0.658**(material.Fy/_Fe_E7))*material.Fy
                    # 
                    #
                    #
                    if _bt >= 1.49*(material.E/_f):
                        #
                        # Compression flange
                        _bt = (section.a/section.ta)                    
                        _be_bfc = ((1.92*section.ta*(material.E/_f)**0.5)*
                                 (1-(0.34/_bt)*(material.E/_f)**0.5))
                        _be_bfc = min (_be_bfc,section.a)
                        #
                        # Tension flange
                        _bt = (section.b/section.tb)
                        _be_bft = ((1.92*section.tb*(material.E/_f)**0.5)*
                                 (1-(0.34/_bt)*(material.E/_f)**0.5))
                        _be_bft = min (_be_bft,section.b)
                        # 
                        # Web
                        _bt = (self.hw/section.tw)
                        _be_hw = ((1.92*section.tw*(material.E/_f)**0.5)*
                                 (1-(0.34/_bt)*(material.E/_f)**0.5))
                        _be_hw = min (_be_hw,self.hw)
                        # 
                        _Ae_E7 = (_be_bfc*section.ta)+(_be_bft*section.tb)+(_be_hw*section.tw)
                        #_Ae_flag = '(E7-17)'              
                    #
                    else:
                        print ('*******************************')
                        print ('(E7-17)' )
                        print ('ERROR _bt (',_bt,') < 1.49*E/f (',1.49*(material.E/_f),')')
                        print ('*******************************')
                #
                #                    
                # _Qa --> Reduction Factor
                _Qa = _Ae_E7/section.Ag
            # print '_Qa = ',_Qa
            #   
            #
            if section.type == 'CIRCULAR':
                self.Q  = _Q_circular
                #
            # Net Reduction Factor Accounting for all Slender compression elements                
            else:
                self.Q = _Qs*_Qa
                #
            #
            # Q=0.89
            if section.type =='CIRCULAR':
                #
                print ("Circular Section")
                print (("Q =  "+"%2.3f"+"%8s")%(self.Q, _Qs_Flag))
                #
            #
            else:
                print (("_Qa = "+"%2.3f")%(_Qa))
                print (("_Qs = "+"%2.3f")%(_Qs))
                print (("Q =  "+"%2.3f"+"%8s")%(self.Q, _Qs_Flag))
            #print ("------------------")
            #print ("section.type", section.type)
            #print ( self.Lzt," <= ",self.Lb )
            #print (_Fe_E3_4, _Fe_E4_4, _Fe_E4_5 )
            #
            # Select elastic buckling stress Fe using Equations:
            # Ecu E3-4 and E4-4 for doubly symmetric members with Lz < Lb      
            if section.type == 'DOUBLY' and self.Lzt <= self.Lb:
                _Fe_E7 = _Fe_E3_4
                _Fe_E7_flag = '(E3-4)'
                self.Fe = _Fe_E3_4
                self.Fe_flag ='(E3-4)'                
            #
            # Ecu E3-4, E4-5 for Singly and E4-6 for unsimmetric members
            else:
                # 
                # i) For Doubly Symmetric Members
                #    elastic buckling stress, calculated using Equations 
                #    E3-4 and E4-4 for doubly symmetric members                  
                if section.type == 'DOUBLY':         
                    #
                    _Fe_E7 = min(_Fe_E3_4, _Fe_E4_4)
                    #
                    if _Fe_E7 == _Fe_E3_4:
                        _Fe_E7_flag= '(E3-4)'
                    #
                    else:
                        _Fe_E7_flag = '(E4-4)'
                    #
                    self.Fe = min(_Fe_E3_4,_Fe_E4_4)
                    #
                    if self.Fe == _Fe_E3_4:
                        self.Fe_flag = '(E3-4)'
                    #
                    else:
                        self.Fe_flag ='(E4-4)'  
                        #
                #---
                # ii) For Syngle Symmetric Members where y is the axis of symmetry
                #     Equations E3-4 and E4-5 for singly symmetric members
                elif section.type == 'SINGLY':
                    #
                    _Fe_E7 = min(_Fe_E3_4, _Fe_E4_5)
                    if _Fe_E7 == _Fe_E3_4:
                        _Fe_E7_flag = '(E3-4)'
                    #
                    else:
                        _Fe_E7_flag = '(E4-5)'
                    #
                    self.Fe = min(_Fe_E3_4, _Fe_E4_5)
                    if self.Fe == _Fe_E3_4:
                        self.Fe_flag ='(E3-4)'
                    #
                    else:
                        self.Fe_flag ='(E4-5)'
                        #
                # 
                # iii) For Unsymmetric Members, Fc is the lowest root of the
                #      cubic equation 
                else: print ('Unsymmetric Members No Yet Defined')
                #
            #
            # The critical stress, Fcr shall be determined as follows:
            # b)
            if self.KLr > 4.71*(material.E/(self.Q*material.Fy))**0.5 and (self.Q*material.Fy/_Fe_E7) > 2.25:
                _Fcr_E7 = 0.877*_Fe_E7
                _Fe_E7_flag = '(E7-3)'
                #                print '_Fcr_E7 ',_Fcr_E7,' Ecu', _Fe_E7_flag 
            #
            # a)
            else:
                _Fcr_E7 = self.Q*(0.658**(self.Q*material.Fy/_Fe_E7))*material.Fy
                _Fe_E7_flag = '(E7-2)'
                #                print '_Fcr_E7 ',_Fcr_E7,' Ecu', _Fe_E7_flag 
            #
            # The Nominal Compressive Strength,Pn shall be the lowest
            # value based on the applicable limit states of flexural
            # buckling (E-3), torsional and flexural-torsional buckling (E-4)
            #     
            self.Fcr_E = _Fcr_E7
            self.Fcr_E_flag = _Fe_E7_flag
            self.Pn_E = _Fcr_E7*section.Ag
            self.Pn_E_flag = '(E-7)'
            # print 'Pn =', Pn_E,_Fe_E7_flag
            # print 'Fcr =',_Fcr_E7,_Fe_E7_flag
            # print 'Pn max =',self.Pn_E
        #
        #
        #
        #self.Pn_E = self.Pn_E*(self.Phi_c/self.Omega_c)
        print (("Fe  = "+"%-6.0f"+" N/mm2 "+"%-8s")%(self.Fe, self.Fe_flag))
        print (("Fcr = "+"%-6.0f"+" N/mm2 "+"%-8s")%(self.Fcr_E,self.Fcr_E_flag))
        # print ('Pn max=',self.Pn_E, self.Pn_E_flag)
        print (("Pn  = "+"%-8.3f"+"  kN "+"%-8s")%((self.Pn_E/1000), self.Pn_E_flag))
        print (" ")
    #
    #   
    #
    #-------------------------------------------------
    #
    #             ++++++ Chapter F  ++++++ 
    #
    def ChapterF(self):
        print (" ")        
        print ("-----------------------------")
        print ("     Chapter F - Flexion ")
        print (" ") 
        #
        # ho=(section.d-0.5*section.ta-0.5*section.tb)
        # print 'ho = ',ho
        #
        # F1 General Provisions
        #
        # The design flexural strength and the allowable
        # flexural strength shal be determined as follows:
        #
        # (1) For all provisions in this chapter
        if self.design_method == 'ASD':
            self.Omega_b=1.67
            self.Phi_b = 1.0
        #
        elif self.design_method == 'USER_DEFINED':
            self.Omega_b = self.user_factor_B
            self.Phi_b = 1.0 
        #
        else:
            self.Omega_b=1.0
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
        print (("Cb = "+"%-4.3f")%(self.Cb))
        #
        #
        # F2 to F11 Symmetric Shapes 
        if section.type != 'UNSYMMETRIC':
            # 
            if section.type =='I' or section.type =='CHANNEL':   
                #
                # F2 Doubly symmetric compact channel members bent
                #    about their major axis 
                if section.type == 'CHANNEL': 
                    print ('Channel Section On')                     
                    #
                    # This section applies to doubly symmetric I-shaped
                    # and channel bent aout their major axis, having
                    # compact flanges as defined in Sec B4.1 for flexure
                    #                               
                    # (F2-5) Limiting Laterally Unbraced Length Lp
                    #        for Full Plastic Flexural Stregth
                    self.Lp = 1.76*self.ry*(material.E/material.Fy)**0.5
                    self.Lp_flag ='(F2-5)'
                    #                
                    # (F2-7) Efective Radius of Gyration rts for
                    #        Mayor Axis Bending of Doubly 
                    #        Simetric Compact I Beam
                    _rts1=(section.a/
                         (12*(1+(1/6)*(self.hw*section.tw/
                                       (section.a*section.ta))))**0.5)
                    #
                    _rts2=(((section.Cw*section.Iz)**0.5)/section.Zey)**0.5
                    #
                    _rts = min(_rts1,_rts2)
                    # print 'rts =',_rts
                    #
                    #               
                    # (F2-8) Coefficient c is determined:
                    #                
                    # b) For Channels
                    _c = (ho/2)*(section.Iz/Cw)
                    # print 'c =',_c
                    #
                    # F2.2 Lateral-Torsional Buckling
                    #                
                    # (F2-6) Limiting Laterally Unbraced Length 
                    #        Lr for Inelastic Lateral-Torsional
                    #        Buckling
                    self.Lr=(1.95*_rts*(material.E/(0.7*material.Fy))*
                        ((section.J*_c/(section.Zey*section.ho))+
                         ((section.J*_c/(section.Zey*section.ho))**2+
                          (6.76*(0.7*material.Fy/material.E)**2))**0.5)**0.5)
                    #                
                    self.Lr_flag = '(F2-6)'
                    #
                    # Lc -> Maximum Unbraced Length of the compression flange
                    #       at which the allowable bending stress is 0.66Fy
                    Lc=min(self.Lr,self.Lp)
                    #
                    # Checking Length of the Beam
                    #
                    if self.Lx == 0: self.Lx = Lc
                    #
                    if self.Ly == 0: self.Ly = self.Lx 
                    #
                    if self.Lb == 0: self.Lb = max(self.Lx, self.Ly)
                    #  
                    #
                    # (F2-4)
                    # Critical Stress Fcr
                    _Fcrx = (((self.Cb*material.E*math.pi**2)/(self.Lb/_rts)**2)*
                            (1+0.078*(section.J*_c/(section.Zey*section.ho))*
                             (self.Lb/_rts)**2)**0.5)
                    # print 'Fcr =',_Fcrx
                    # 
                    #
                    # F2.1 Yielding
                    _Mn_F2_1 = material.Fy*section.Zpz
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
                    #
                    # b) When Lp < Lb < = Lr
                    elif  self.Lp < self.Lb and self.Lb <= self.Lr:
                        _Mn_F2_2 = (self.Cb*(self.Mpx-(self.Mpx-0.7*material.Fy*section.Zey)*
                                       ((self.Lb-self.Lp)/(self.Lr-self.Lp))))
                        _Mn_F2_2_flag = '(F2.2b)'
                    #                
                    # c) When Lb > Lr
                    else:
                        _Mn_F2_2 = (_Fcrx*section.Zey)
                        _Mn_F2_2_flag = '(F2.2b)'
                    #
                    #
                    # F2 The nominal  flexural strength, Mn shall be the
                    #    lower value obtained according to the limit states
                    #    of yielding (platic moment) and lateral torsional
                    #    buckling
                    #
                    self.Mnx = min(_Mn_F2_1,_Mn_F2_2)
                    # flag selection                
                    if self.Mnx == _Mn_F2_1: self.Mnx_flag = _Mn_F2_1_flag
                    #                
                    else:self.Mnx_flag = _Mn_F2_2_flag
                #
                #
                # F2 to F5 I-shaped members bent about their major axis                                        
                else:
                    #                    
                    # F2 & F3 Doubly symmetric I-shaped members bent about 
                    #         their major axis 
                    if section.type == 'DOUBLY' and self.ClassWebF == "compact" :        
                        #
                        # This section applies to F2 & F3
                        #                        
                        # (F2-5) Limiting Laterally Unbraced Length Lp
                        #        for Full Plastic Flexural Stregth
                        self.Lp = 1.76 * self.ry * math.sqrt(material.E/material.Fy)
                        #
                        self.Lp_flag = '(F2-5)'
                        #                
                        # (F2-7) Efective Radius of Gyration rts for
                        #        Mayor Axis Bending of Doubly 
                        #        Simetric Compact I Beam
                        _rts1 = (section.a/
                                 math.sqrt(12*(1+(1/6)*(self.hw*section.tw/
                                               (section.a*section.ta)))))
                        #
                        _rts2 = math.sqrt((math.sqrt(section.Cw * section.Iz)) / section.Zey)
                        #
                        _rts = min(_rts1, _rts2)
                        #
                        _rts_flag = '(F2-7)'
                        #print 'rts =',_rts, rts_flag
                        #      
                        # (F2-8) Coefficient c is determined:
                        #                
                        # a) For doubly symmetric I-Shapes
                        _c = 1
                        #
                        _c_flag ='(F2-8a)'
                        # print 'c =',_c,c_flag
                        #                
                        # (F2-6) Limiting Laterally Unbraced Length 
                        #        Lr for Inelastic Lateral-Torsional
                        #        Buckling
                        self.Lr = (1.95 * _rts * (material.E / (0.7 * material.Fy)) *
                                   math.sqrt((section.J * _c / (section.Zey * section.ho)) +
                                             math.sqrt((section.J * _c / (section.Zey * section.ho))**2 +
                                                       (6.76 * (0.7 * material.Fy / material.E)**2))))
                        #                
                        self.Lr_flag = '(F2-6)'
                        #
                        # Lc -> Maximum Unbraced Length of the compression flange
                        #       at which the allowable bending stress is 0.66Fy
                        #Lc = max(Lr,Lp)
                        #
                        # Checking Length of the Beam
                        #
                        #if self.Lx == 0: self.Lx = Lc
                        #                            
                        #if self.Ly == 0: self.Ly = Lc
                        #                            
                        #if self.Lzt == 0: self.Lzt = Lc  
                        #                            
                        #if self.Lb == 0: self.Lb = Lc 
                        #
                        #print ('Lb =====>', self.Lb)
                        #               
                        # (F2-4) Critical Stress Fcr
                        self.Fcrx = (((self.Cb * material.E * math.pi**2) / (self.Lb / _rts)**2) *
                                     math.sqrt(1 + 0.078 * (section.J * _c / (section.Zey * section.ho)) *
                                               (self.Lb / _rts)**2))
                        #
                        self.Fcrx_flag ='(F2-4)'
                        # print 'Fcr =',_Fcrx,Fcrx_flag
                        #
                        # F2.1 Yielding
                        _Mpx = material.Fy * section.Zpz
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
                            #
                        #
                        else:
                            #
                            # c) When Lb > Lr
                            if self.Lb > self.Lr:
                                _Mn_F2_2 = (self.Fcrx * section.Zey)
                                #
                                _Mn_F2_2 = min(_Mn_F2_2, _Mpx)
                                #
                                _Mn_F2_2_flag = '(F2.2b)'
                                #
                            #
                            # b) When Lp < Lb < = Lr
                            else:
                                _Mn_F2_2 = (self.Cb * (_Mpx - (_Mpx - 0.7 * material.Fy * section.Zey) *
                                                       ((self.Lb - self.Lp) / (self.Lr - self.Lp))))
                                #
                                _Mn_F2_2 = min(_Mn_F2_2, _Mpx)
                                #
                                _Mn_F2_2_flag = '(F2.2b)'
                                #                
                        #-------
                        #
                        #
                        # F2 Doubly symmetric compact I-shaped members bent 
                        #    about their major axis                             
                        if  self.compacness_flexure == "compact":
                            # This section applies to doubly symmetric I-shaped
                            # and channel bent aout their major axis, having
                            # compact flanges as defined in Sec B4.1 for flexure
                            #        
                            # The nominal  flexural strength, Mn shall be the
                            # lower value obtained according to the limit states
                            # of yielding (platic moment) and lateral torsional
                            # buckling
                            #
                            self.Mnx = min(_Mn_F2_1, _Mn_F2_2)
                            # flag selection                
                            if self.Mnx == _Mn_F2_1: 
                                self.Mnx_flag = _Mn_F2_1_flag
                            #
                            else: 
                                self.Mnx_flag = _Mn_F2_2_flag
                            #                
                        #
                        # F3 Doubly Symmetric I_Shaped Members with Compact Webs
                        #    and NonCompact or Slenders Flanges Bent about their 
                        #    Major Axis
                        else:
                            # This section applies to doubly symmetric I-shaped
                            # members bent about their major axis having compact
                            # webs and noncompact or slender flanges as defined
                            # in Sec B4.1 for flexure
                            #
                            # -------------------------------
                            # 1.- Lateral-Torsional Buckling
                            # F3.1 
                            _Mn_F3_1 = min(_Mn_F2_1, _Mn_F2_2)
                            _Mn_F3_1_flag = '(F3.1)'
                            # print 'Mnx Section F.2: ',Mn_F2/1000000
                            #
                            # 2.- Compression Flange Local Buckling
                            # F3.2 
                            # Compression Flange
                            _lambda_F3_2 = section.a / (2 * section.ta)
                            #
                            # Flange Limiting Slenderness
                            _lambda_pf = self.lambda_p_fF
                            _lambda_rf = self.lambda_r_fF
                            #
                            # a) For Sections With NonCompact Flanges
                            #
                            if self.compacness_flexure == "noncompact":
                                #                        
                                _Mn_F3_2 = (_Mpx - (_Mpx - 0.7 * material.Fy * section.Zey) *
                                            ((_lambda_F3_2 - _lambda_pf) /
                                             (_lambda_rf - _lambda_pf)))
                                #                        
                                _Mn_F3_2_flag = '(F3.2a)'
                                #
                            #
                            # b) For Sections With Slender Flanges
                            #
                            else:
                                #                        
                                _Mn_F3_2 = (0.9 * material.E * self.Kc * section.Zey / _lambda_F3_2**2)
                                #
                                _Mn_F3_2_flag = '(F3.2b)'
                                #
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
                            #
                            else: 
                                self.Mnx_flag = _Mn_F3_2_flag
                            #
                    #
                    # F4 & F5 Doubly and singly I-shaped members with
                    # compact, noncompact and slender webs bent
                    # about their major axis.
                    else :
                        #
                        # F5 Dobly and singly symmetric I-Shaped Members 
                        # With slender Webs Bent About Their Major Axis
                        #
                        # hc --> twice the distance from the centroid to the following:
                        #         --The inside face of the compression flange less the 
                        #           fil1 let or corner radius, 
                        #         --For rolled shapes; the nearest line of fasteners at 
                        #           the compression flange or the inside faces of the 
                        #           compression flange when welds are used, for 
                        #           built-up sections, in. (mm)
                        #  
                        _hc = 2*(section.Zc - section.ta)
                        #
                        # aw --> Radio of two times the web area in compression due 
                        #        to application of major axis bending moment alone 
                        #        to the area of the compression flange components
                        # (4-11)
                        _aw = ((_hc*section.tw)/(section.a*section.ta))
                        # print ('aw :',aw)
                        #
                        #
                        # (F4-10)
                        # rt --> The effective radious of gyration for lateral-torsional 
                        #        buckling
                        #
                        # i) For I-shapes with a rectangular compression flange
                        _rt_1 = (section.a/math.sqrt(12*((section.ho / section.d) +
                                                        ((_aw/6.0)*
                                                         (self.hw**2/(section.ho*section.d))))))
                        #
                        # ii) For I-shapes with a channel cap or a cover plate attached
                        #     to the compression flange
                        _rt_2 = (section.a / (math.sqrt(12*(1 + (_aw / 6.0)))))
                        #
                        _rt = min(_rt_1, _rt_2)
                        #
                        # flag
                        if _rt == _rt_1:
                            _rt_flag='(F4-10i)'
                        #
                        else:
                            _rt_flag='(F4-10ii)'
                        #             
                        #print ('rt :', _rt, _rt_flag)
                        #
                        #                
                        # (F4-7)
                        # Lp --> The limiting laterally unbraced length for the 
                        # limit stated of yielding, Lp, is determined as:
                        self.Lp = 1.1 * _rt * math.sqrt(material.E/material.Fy)
                        self.Lp_flag ='(F4-7)'
                        # print 'Lp :',Lp
                        #
                        #
                        # Elastic Section Modulus
                        _Sxc = section.Iy /  section.Zc
                        _Sxt = section.Iy / (section.d - section.Zc)
                        #print ('Sx needs to be checked ',_Sxc, _Sxt)
                        #
                        #
                        #
                        if self.ClassWebF == 'SLENDER' :
                        #if self.ClassWebF == "noncompact" :
                            #
                            # This section applies to doubly and single symmetric 
                            # I-shaped members with slender webs attached to the
                            # mid-width of the flanges and bent about their major
                            # axis as defined in Sec B4.1 for flexure
                            #
                            # aw is defined by Ecu F4-11 but shall not exceed 10
                            #
                            # hc --> Elastic Centroid to the Compression Flange
                            # hc =2*(section.Zc-section.ta)        
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
                            _lambda_F5 = (section.a/(2*section.ta))
                            _lambda_pf = self.lambda_p_fF
                            _lambda_rf = self.lambda_r_fF
                            #   
                            #
                            # (F5-6) Rpg is the bending strength reduction factor
                            #
                            _Rpg1 = (1 - (_aw/(1200.0+300*_aw))*
                                     ((_hc / section.tw) - 5.70*math.sqrt(material.E / material.Fy)))
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
                            #        
                            # Lp is defined by Equ F4-7  
                            #
                            # (F5-5)
                            self.Lr = math.pi * _rt * math.sqrt(material.E / (0.70*material.Fy))
                            self.Lr_flag = '(F5-5)'
                            #
                            #
                            # ------------------------------------
                            # 2. Lateral-Torsional Buckling
                            # F5-2
                            # a) The limit state of lateral-torsional buckling
                            #    does not apply
                            if self.Lb <= self.Lp :
                                _Fcrx_5_2 = slef.Fy 
                                _Fcrx_flag_5_2 = '(F5-1)'
                                #
                            #
                            # (F5-3 b) & (F5-4 c) 
                            else:
                                # (F5-4 c) 
                                if self.Lb > self.Lr:
                                    #
                                    _Fcrx1 = ((self.Cb*material.E*math.pi**2)/(self.Lb / _rt)**2)
                                    #
                                    _Fcrx_5_2 = min(material.Fy, _Fcrx1)
                                    #
                                    _Fcrx_flag_5_2 = '(F5-4)'
                                    #
                                # (F5-3 b)
                                else:
                                    #
                                    _Fcrx1 = (self.Cb*(material.Fy - (0.30*material.Fy) * 
                                                       ((self.Lb - self.Lp) / (self.Lr - self.Lp))))
                                    #
                                    _Fcrx_5_2 = min(material.Fy, _Fcrx1)
                                    #
                                    _Fcrx_flag_5_2 = '(F5-3)'
                                    #
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
                            if self.compacness_flexure == "compact":
                                _Fcrx_5_3 = material.Fy
                                #
                                _Fcrx_flag_5_3 = '(5-1)'
                                #
                            #
                            # b) For section with noncompact flanges
                            elif self.compacness_flexure == "noncompact":
                                _Fcrx_5_3 = (material.Fy-(0.30*material.Fy)*
                                             ((_lambda_F5 - _lambda_pf) /
                                              (_lambda_rf - lambda_pf)))
                                #
                                _Fcrx_flag_5_3 = '(F5-8)'
                                #
                            #
                            # c) For section with slender flanges
                            else:
                                _Fcrx_5_3 = ((0.90 * material.E * self.Kc) / 
                                         (section.a / (2*section.ta))**2)
                                #
                                _Fcrx_flag_5_3 = '(F5-9)'
                                #
                            #
                            # F5-3                 
                            _Mn_F5_3 = _Rpg * _Fcrx_5_3 * _Sxc
                            _Mn_F5_3_flag = '(F5-7)'
                            #
                            #
                            # -------------------------------
                            # 4.- Tension Flange yielding
                            # (F5-4)
                            if _Sxt >= _Sxc:
                                #
                                _Fcrx_5_4 = material.Fy
                                #
                                _Fcrx_flag_5_4 = '(F5-1)'
                                #
                            #
                            # (F5-4b)
                            else:
                                #
                                _Fcrx_5_4 = material.Fy
                                #
                                _Fcrx_flag_5_4 = '(F5-10b)'
                                #
                            # 
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
                            #                        
                            elif self.Mnx == _Mn_F5_2: 
                                self.Mnx_flag = _Mn_F5_2_flag
                                self.Fcrx = _Fcrx_5_2
                                self.Fcrx_flag = _Fcrx_flag_5_2
                            #
                            elif self.Mnx == _Mn_F5_3: 
                                self.Mnx_flag = _Mn_F5_3_flag
                                self.Fcrx = _Fcrx_5_3
                                self.Fcrx_flag = _Fcrx_flag_5_3
                            #                        
                            else: 
                                self.Mnx_flag = _Mn_F5_4_flag
                                self.Fcrx = _Fcrx_5_4
                                self.Fcrx_flag = _Fcrx_flag_5_4
                            #
                            #
                        #----
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
                            #
                            #
                            # Moment of inertia of the compression flange
                            # about main axis x
                            _Ixc = (section.a*section.ta**3)/12.0
                            # about weak axis y
                            _Iyc = (section.ta*section.a**3)/12.0
                            # print 'Iyc =',Iyc
                            # print 'Ixc =',Ixc
                            #
                            #
                            # Slenderness Parameter
                            _lambda_F4_9 = _hc/section.tw
                            #
                            # Limiting Slenderness of Compression Web
                            _lambda_pw = self.lambda_p_wF
                            _lambda_rw = self.lambda_r_wF
                            #
                            #
                            # FL --> Nominal Flexural Strength
                            #
                            #
                            # Myc = Yield moment in the compression flange
                            # (F4-4)
                            _Myc = material.Fy*_Sxc
                            #print ('Myc Needs to be corrected:',_Myc)
                            #
                            # Plastic Moment
                            _Mpx = min(material.Fy*section.Zpz, 1.6*_Sxc*material.Fy)
                            #print ('Mp :',_Mpx)
                            #
                            #
                            # The stress, FL, is determined as follows:
                            #
                            # (F4-6b)(ii)
                            if (_Sxt/_Sxc) < 0.70:
                                _FL = max(material.Fy*(_Sxt/_Sxc),0.5*material.Fy)
                                _FL_flag ='(F4-6b-ii)'
                            #                    
                            # (F4-6a)(i)
                            else:
                                _FL = 0.7*material.Fy
                                _FL_flag ='(F4-6b-i)'
                            #                    
                            #print ('FL :', _FL, _FL_flag)
                            #
                            #                         
                            # The web plastification factor Rpc, shall be
                            # determined as follows:
                            # J shall be taken as zero if:
                            if (_Iyc/section.Iz) <= 0.23:
                                _J = 0
                                # Rpc --> The web plastification factor
                                _Rpc = 1.0
                                _Rpc_flag = '(Iyc/Iy <= 0.23)'
                            #
                            else:
                                #
                                _J = section.J
                                #
                                # Rpc --> The web plastification factor
                                #         (F4-9b)(ii)
                                if (_hc/section.tw) > _lambda_pw:
                                    #
                                    _Rpc = ((_Mpx/_Myc)-
                                           ((_Mpx/_Myc)-1)*
                                           ((_lambda_F4_9 - _lambda_pw )/
                                            (_lambda_rw - _lambda_pw )))
                                    #
                                    _Rpc = min(_Rpc, (_Mpx/_Myc))
                                    _Rpc_flag = '(F4-9b-ii)'
                                #                    
                                # (F4-9a)(i)
                                else:
                                    #
                                    _Rpc = (_Mpx/_Myc)
                                    _Rpc_flag = '(F4-9b-i)'
                                #
                            #
                            #print ('Rpc =', _Rpc, _Rpc_flag)
                            #
                            #
                            # (F4-8)
                            # rts -> The limiting unbraced lenght for the limit
                            #         state of inelastic lateral-torsional buckling
                            self.Lr = (1.95*_rt*(material.E / _FL) *
                                       math.sqrt((_J / (_Sxc * section.ho)) +
                                                 math.sqrt((section.J / (_Sxc * section.ho))**2 +
                                                           (6.76*(_FL / material.E)**2))))
                            #
                            self.Lr_flag ='(F4-8)'
                            #
                            print ('Lr = ', self.Lr, self.Lr_flag)
                            #
                            # Lc -> Maximum Unbraced Length of the compression flange
                            #       at which the allowable bending stress is 0.66Fy
                            Lc = min(self.Lr, self.Lp)
                            #
                            #
                            # Checking Length of the Beam
                            #
                            #if self.Lx == 0: self.Lx = self.Lr
                            #                            
                            #if self.Ly == 0: self.Ly = self.Lr
                            #                            
                            #if self.Lzt == 0: self.Lzt = self.Lr
                            #                            
                            #if self.Lb == 0: self.Lb = self.Lr 
                            #
                            #
                            # (F4-5) Critical Stress Fcr
                            self.Fcrx = (((self.Cb*material.E*math.pi**2)/(self.Lb/_rt)**2)*
                                         math.sqrt(1 + 0.078*(_J/(_Sxc*section.ho))*
                                                   (self.Lb/_rt)**2))
                            #                
                            self.Fcrx_flag = '(F4-5)'
                            #print ('Fcr =',Fcrx)
                            # 
                            #
                            # 1.- Compression Flange Yielding 
                            # -------------------------------
                            # (F4-1)
                            _Mn_F4_1 = _Rpc * _Myc
                            _Mn_F4_1_flag = '(F4-1)'
                            #print ('Mn_F4_1',_Mn_F4_1)
                            #
                            #
                            # 2.- Lateral-Torsional Buckling
                            # ------------------------------
                            #
                            # (F4-2a)
                            if self.Lb <= self.Lp:
                                _Mn_F4_2 = _Mn_F4_1
                                _Mn_F4_2_flag = '(F4-2a)'
                            #
                            # (F4-2c & 2b)
                            else:
                                #
                                # (F4-2c)
                                if self.Lb > self.Lr:
                                    #
                                    _Mn_F4_2 = min((self.Fcrx * _Sxc),(_Rpc * _Myc))
                                    _Mn_F4_2_flag = '(F4-3)'
                                #
                                # (F4-2b)
                                else:
                                    #
                                    _Mn = self.Cb*(_Rpc * _Myc -
                                                  (_Rpc * _Myc - _FL * _Sxc) *
                                                  ((self.Lb - self.Lp)/(self.Lr - self.Lp)))
                                    #
                                    _Mn_F4_2 = min(_Mn,(_Rpc * _Myc))
                                    _Mn_F4_2_flag = '(F4-2)'
                                    #
                            #
                            #
                            # 3.- Compression Flange Local Buckling
                            # -------------------------------------
                            #
                            _lambda_F4_12 = section.a/(2 * section.ta)
                            #
                            if self.compacness_flexure == "noncompact":
                                #
                                _lambda_pf = self.lambda_p_fF
                                _lambda_rf = self.lambda_r_fF
                                #
                                _Mn_F4_12 = (_Rpc * _Myc - 
                                            ((_Rpc * _Myc - _FL * _Sxc) * 
                                             ((_lambda_F4_12 - _lambda_pf)/(_lambda_rf - _lambda_pf))))
                                #
                                _Mn_F4_12_flag = '(F4-12)'
                                #
                            #
                            #
                            elif self.compacness_flexure == 'SLENDER':
                                #
                                _Kc = 4/math.sqrt(section.d / section.tw)
                                #
                                _Mn_F4_12 = (0.90 * material.E * _Kc * _Sxc)/ _lambda_F4_12**2
                                _Mn_F4_12_flag = '(F4-13)'
                                #
                            #
                            #
                            else:
                                _Mn_F4_12 = _Mn_F4_1
                                _Mn_F4_12_flag = _Mn_F4_1_flag
                            #
                            #
                            # 4.- Tension Flange Yielding
                            #
                            # b) When
                            if _Sxt < _Sxc:
                                #
                                _Myt = material.Fy * _Sxt
                                _lambda_F4_15 = _hc / section.tw
                                #
                                # (F4-15b)
                                if (_hc/section.tw) > _lambda_pw :
                                    #
                                    _Rpt1 = ((_Mpx/_Myt) - 
                                             ((_Mpx/_Myt)-1) * 
                                             ((_lambda_F4_15 - _lambda_pw)/(_lambda_rw - _lambda_pw)))
                                    #
                                    _Rpt = min(_Rpt1, (_Mpx/_Myt))
                                    #
                                    _Rpt_flag = '(F4-15b)'
                                    #
                                #
                                # (F4-15a)
                                else:
                                    #
                                    _Rpt = _Mpx / _Myt
                                    #
                                    _Rpt_flag = '(F4-15a)'
                                    #
                                #
                                _Mn_F4_15 = _Rpt * _Myt
                                _Mn_F4_15_flag = '(F4-14)'
                            #
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
                            #print ('self.Mnx' , self.Mnx, self.Mnx_flag)
                            #
                #-----------
                #                              
                #
                #
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
                _lambda_F6 = (section.a / (2.0*section.ta))
                _lambda_pf = self.lambda_p_fF
                _lambda_rf = self.lambda_r_fF
                #
                # 
                # (F6-4)
                _tf = min(section.ta, section.tb)
                #
                self.Fcry = (0.69 * material.E) / (section.a / _tf)**2
                #
                self.Fcry_flag = '(F6-4)'
                #
                #
                # (F6.2a) For Sections with Compact Flanges
                if self.compacness_flexure == "compact":
                    #
                    _Mn_F6_2 = _Mn_F6_1
                    _Mn_F6_2_flag = '(F6.1)'
                    #
                #
                # F6.2b For Sections with Non Compact Flanges 
                elif self.compacness_flexure == "noncompact":
                    #            
                    _Mn_F6_2 = (_Mpy - (_Mpy - 0.70 * material.Fy * section.Zez) *
                                ((_lambda_F6 - _lambda_pf) /
                                 (_lambda_rf - _lambda_pf)))
                    #
                    _Mn_F6_2_flag = '(F6.2)'            
                    #
                #
                # F6.2c For Sections with Slender Flanges
                else:
                    #            
                    _Mn_F6_2 = self.Fcry * section.Zez
                    _Mn_F6_2_flag = '(F6.3)'
                    #
                #           
                # Summary Weak Axis
                self.Mny = min(_Mn_F6_1, _Mn_F6_2)
                # Flag            
                if self.Mny == _Mn_F6_1:
                    self.Mny_flag = _Mn_F6_1_flag
                #
                else:
                    self.Mny_flag = _Mn_F6_2_flag
            #               
            #
            # F7 Square and rectangular HSS and box-shaped members       
            if section.type =='BOX':
                #
                # This section applies to square and rectangular HSS, and doubly
                # symmetric box-shaped members bent about either axis, having
                # compact or noncompact webs and compact, noncompact or slender
                # flanges as defined in Section B4 for flexure.
                #
                #
                # The nominal flexural strength, Mn, shall be the lowest value obtained
                # according to the limit states of yielding (plastic moment), flange local
                # buckling and web local buckling under pure flexure.
                #
                # F7.1. Yielding
                #
                _Mpx = material.Fy*section.Zpz
                #
                _Mn_F7_1 = _Mpx
                _Mn_F7_1_flag = '(F7-1)'
                #
                #
                # F7.2. Flange Local Buckling
                #
                # (F7.2a) For Sections with Compact Flanges
                if self.compacness_flexure == "compact": 
                    #
                    _Mn_F7_2 = _Mn_F7_1
                    _Mn_F7_2_flag = _Mn_F7_1_flag 
                    # 
                #
                # F7.2b For Sections with Non Compact Flanges 
                elif self.compacness_flexure == "noncompact":
                    #
                    _Mn_F7_2 = (_MPx - (_MPx - material.Fy * section.Zey) * 
                                (3.57 * (section.a/section.ta) * math.sqrt(material.Fy/material.E) - 4.0))
                    #
                    _Mn_F7_2_flag = '(F7.2)'
                    #
                #
                # F7.2c For Sections with Slender Flanges
                else:
                    # (F7.4)
                    # Effective section modulus determined with the 
                    # effective width, be of the compression flange
                    # taken as:
                    _be = (1.92 * section.ta * math.sqrt(material.E/material.Fy) * 
                           (1.0 - (0.38 * section.ta/section.a)) * math.sqrt(material.E/material.Fy))
                    #
                    print ('be :',_be)
                    #
                    if _be > section.a:
                        #
                        _hi = section.d - 2 * section.ta
                        _bi = _be - 2 * section.tw
                        #
                        _Se = ((_be  * section.d**3 - _bi * _hi**3) / (6.0 * section.d))
                        #
                    #
                    else:
                        _Se = section.Zey
                    #
                    #
                    print('Sx :',section.Zey)
                    #
                    #
                    _Mn_F7_2 = material.Fy * _Se
                    _Mn_F7_2_flag = '(F7.3)'
                    #
                #
                # F7.3 Web Local Buckling
                #
                # F7.3.a For compact sections
                if self.ClassWebF == "compact":
                    # The limit state of web local buckling 
                    # does not apply
                    _Mn_F7_3 = _Mn_F7_1
                    _Mn_F7_3_flag = _Mn_F7_1_flag 
                #
                # F7.3.b For sections with noncompact webs
                else:
                    _Mn_F7_3 = (_MPx - (_MPx - material.Fy * section.Zey) * 
                                (0.305 * (self.hw/section.tw) * math.sqrt(material.Fy/material.E) - 0.738))
                    #
                    _Mn_F7_3_flag = '(F7.5)'
                #
                #
                self.Mnx = max(_Mn_F7_1, _Mn_F7_2, _Mn_F7_3)
                #
                if self.Mnx == _Mn_F7_1:
                    self.Mnx_flag = _Mn_F7_1_flag
                #
                elif self.Mnx == _Mn_F7_2:
                    self.Mnx_flag = _Mn_F7_2_flag
                #
                else: self.Mnx_flag = _Mn_F7_3_flag
                #
                #
                # ------------------
                #     Week Axis
                # ------------------
                #
                _Mpy = material.Fy*section.Zpz
                _Mn_F7_1 = _Mpy
                _Mn_F7_1_flag = '(F7-1)'
                #
                #
                # F7.2. Flange Local Buckling
                #
                # (F7.2a) For Sections with Compact Flanges
                if self.ClassWebF == "compact": 
                    #
                    _Mn_F7_2 = _Mn_F7_1
                    _Mn_F7_2_flag = _Mn_F7_1_flag 
                    # 
                #
                # F7.2b For Sections with Non Compact Flanges 
                elif self.ClassWebF == "noncompact":
                    #
                    _Mn_F7_2 = (_MPy - (_MPy - material.Fy * section.Zez) * 
                                (3.57 * (section.d/self.hw) * math.sqrt(material.Fy/material.E) - 4.0))
                    #
                    _Mn_F7_2_flag = '(F7.2)'
                    #
                #
                # F7.2c For Sections with Slender Flanges
                else:
                    # (F7.4)
                    # Effective section modulus determined with the 
                    # effective width, be of the compression flange
                    # taken as:
                    _be = (1.92 * section.tw * math.sqrt(material.E/material.Fy) * 
                           (1.0 - (0.38 * section.tw/section.d)) * math.sqrt(material.E/material.Fy))
                    #
                    print ('be :',_be)
                    #
                    if _be > section.a:
                        #
                        _hi = section.d - 2 * section.ta
                        _bi = _be - 2 * section.tw
                        #
                        _Se = ((_be**3  * section.d - _bi**3 * _hi) / (6.0 * section.a))
                        #
                        #
                    #
                    else:
                        _Se = section.Zez
                    #
                    #
                    print('Sy :',section.Zez)
                    #
                    #
                    _Mn_F7_2 = material.Fy * _Se
                    _Mn_F7_2_flag = '(F7.3)'
                    #
                #
                # F7.3 Web Local Buckling
                #
                # F7.3.a For compact sections
                if self.compacness_flexure == "compact":
                    # The limit state of web local buckling 
                    # does not apply
                    _Mn_F7_3 = _Mn_F7_1
                    _Mn_F7_3_flag = _Mn_F7_1_flag 
                #
                # F7.3.b For sections with noncompact webs
                else:
                    _Mn_F7_3 = (_MPy - (_MPy - material.Fy * section.Zez) * 
                                (0.305 * ((section.a - 2*section.tw)/section.ta) *
                                 math.sqrt(material.Fy/material.E) - 0.738))
                    #
                    _Mn_F7_3_flag = '(F7.5)'
                #
                #
                self.Mny = max(_Mn_F7_1, _Mn_F7_2, _Mn_F7_3)
                #
                if self.Mny == _Mn_F7_1:
                    self.Mny_flag = _Mn_F7_1_flag
                #
                elif self.Mny == _Mn_F7_2:
                    self.Mny_flag = _Mn_F7_2_flag
                #
                else: self.Mny_flag = _Mn_F7_3_flag
                #
                #
                #
            #
            # F8 Round HSS       
            elif section.type =='HSS':
                print ('HSS (F8) No defined yet')
            #
            # F9 Tees and double angles loaded in the plane of     
            elif section.type =='Te':
                print ('TEE (F9) No defined yet')
            #
            # F10 Single angles    
            elif section.type =='ANGLE':
                print ('Angle (F10) No defined yet')
            #
            # F11 Rectangular bars and rounds   
            elif section.type =='BAR' or section.type =='Round':
                print ('Bar & Round (F11) No defined yet')
            #
            # F13 Proportions of beam and girders  
            elif section.type =='HOLE':
                print ('F13 No defined yet')
        #
        # F12 Unsymmetrical shapes
        else:
            print ('F12 Unsymmetrical No defined yet')
        #
        #
        #               Print Major Axis Summary
        #
        print (("Lx = "+"%-4.3f"+" m")%(self.Lx/1000.0))
        print (("Ly = "+"%-4.3f"+" m")%(self.Ly/1000.0))
        print (("Lz = "+"%-4.3f"+" m")%(self.Lzt/1000.0))
        #
        try:
            print (("Lp = "+"%-4.3f"+" m "+"%-8s")%(self.Lp/1000.0, self.Lp_flag ))
            print (("Lr = "+"%-4.3f"+" m "+"%-8s")%(self.Lr/1000.0, self.Lr_flag ))
        except : pass
        #
        print (("Lb = "+"%-4.3f"+" m")%(self.Lb/1000.0))
        #
        print (" ")
        print ("      Major Axis ")
        try:
            print (("rts = "+"%-4.3f"+" "+"%-8s")%(_rts,rts_flag))
            print (("c   = "+"%-4.4f"+" "+"%-8s")%(c,c_flag))
        #
        except:
            pass
        #
        try:
            print (("Fcr = "+"% 6.0f"+" N/mm2 "+"%-8s")%(self.Fcrx, self.Fcrx_flag))
             #self.MnxMax = self.Mnx
        #
        except : pass  
        #
        # print ('Mnx Max =', (self.MnxMax/1000000.0))
        #self.Mcx = self.Mnx*(self.Phi_b/self.Omega_b)
        #print ('self.Mnx*(LRFD/ASD)',self.Mnx, self.Phi_b, self.Omega_b, (self.Phi_b/self.Omega_b))
        print (("Mn  = "+"% 6.0f"+" kNm "+"%-8s")%((self.Mnx/1000000.0),self.Mnx_flag))  
        #
        # Print Minor Axis Summary
        #
        print (" ")
        print ("      Minor Axis ")
        try:
            print (("Fcr  = "+"% 6.0f"+" N/mm2 "+"%-8s")%(self.Fcry, self.Fcry_flag))
        #
        except : pass
        #
        #self.MnyMax = self.Mny
        # print ('Mny Max =', (self.MnyMax/1000000))
        #self.Mcy = self.Mny*(self.Phi_b/self.Omega_b)
        # following line changed so that it prints Mny and Mny_flag, previously it reprinted Mnx and Mnx_flag
        print (("Mn   = "+"% 6.0f"+" kNm "+"%-8s")%((self.Mny/1000000.0),self.Mny_flag))  
    #
    #
    #
    #-------------------------------------------------
    #
    #            ++++++ Chapter G  ++++++ 
    #
    def ChapterG(self):
        #
        print (" ")        
        print ("-----------------------------")
        print ("     Chapter G - Shear ")
        print (" ")
        print ("Shear Stress Calc :", self.shear_stress)
        print (" ")
        print ("        Main Axis ")        
        #
        # (1) For all provisions in this chapter
        if self.design_method == 'ASD' :
            # if self.shear_stress == "maximum" or self.shear_stress == 'MAX':
            #  ASD=1.50
            #  else: ASD=1.67
            #
            self.Omega_shear = 1.67
            self.Phi_shear = 1.0
        # 
        elif self.design_method == 'USER_DEFINED' :
            self.Omega_shear = self.user_factor_V
            self.Phi_shear = 1.0 
        #            
        else:
            self.Omega_shear = 1.0
            self.Phi_shear = 0.9
        #            
        # ShearFv = 1000.0
        #  print 'ShearFv =',self.Vy 
        #
        #
        # This is temporary will need to be fixed   
        #if self.shear_stress == "maximum":
        self.VryMax = (self.tau_y*self.Aw)
        self.VrxMax = (self.tau_x*self.Af)
        #        self.Aw=section.d*section.tw
        #        
        #       Rolled sections
        if section.build == "rolled":
            _h = self.hw
        #
        #       Tee Sections
        elif section.build == 'T':
            print ('TEE No yet defined')
        #
        #       Welded + rest
        else :
            _h = self.hw
        #
        #
        #
        #       G4 Single Angles
        #        
        if section.type =='ANGLE':
            print ('Angle Section No implemented yet')
        #
        #
        #       G5 Rectangular HSS and Box Members
        #
        elif section.type =='BOX':
            #
            # The nominal shear strength, Vn of rectangular
            # HSS and box members shall be determined using
            # the provisions of section G2.1 with Aw = 2ht
            # where:
            # h = width rsisting the shear force, taken as
            #     the clear distance between the flanges
            #     less the inside corner raious of each side
            #
            # If the corner radious is not know, h shall be
            # taken as the corresponding outside dimension
            # minus 3 times the thickness
            #
            _h = (section.d - 1.5 * section.ta - 1.5 * section.tb)
            #
            # t = design wall thickness, equal to 0.93 times
            #     the nominal thickness for SAW HSS
            #
            _tw = 0.93 * section.tw
            #
            _k = 5.0
            #
        #
        #
        #        print 'h :',_h
        #
        self.htw_Gx = _h/section.tw
        print (("h/tw = "+"%-3.4f")%(self.htw_Gx))
        #
        #       Clear distance between transverse stiffeners
        self.aMaxLength = round(_h*min( 3.0,(260/(_h/section.tw))**2 ))
        #
        #
        #
        #
        # G6 Round HSS
        #
        if section.type =='HSS':
            print ('Circular HSS Section No implemented yet')
        #
        #
        # G2 Members with Unstiffened or Stiffened Webs
        #
        else:
            #
            # G2.2 Transverse Stiffeners
            #      Transverse stiffeners are not required where
            #      h/tw <= 2.46(E/Fy)^0.5 , or where the required
            #      shear strength is less than or equal to the
            #      available shear stregth in accordance with 
            #      Section G2.1 for Kv = 5.0
            #
            self.Kv = 5.0
            #            
            # a) For webs of rolled I-Shaped members with:
            #    h/tw <= 2.24(E/Fy)^0.5            
            if (section.type =='I' and section.type == 'DOUBLY' and
                section.build == "rolled" and self.htw_Gx <= (2.24*(material.E/material.Fy)**0.5)):          
            #
                #
                if self.design_method == 'ASD':
                    self.Omega_shear=1.50
                    self.Phi_shear = 1.0
                #
                elif self.design_method == 'USER_DEFINED':
                    self.Omega_shear = self.user_factor_V
                    self.Phi_shear = 1.0
                #
                else:
                    self.Omega_shear=1.0
                    self.Phi_shear = 1.0   
                #
                self.Cv = 1.0
            #
            #
            #  b) For webs of all other shapes, except round HSS.
            else:
                #          
                # i)
                if self.htw_Gx <= (1.10*(self.Kv*material.E/material.Fy)**0.5):
                    #
                    self.Cv = 1.0
                #
                elif (self.htw_Gx > (1.10*(self.Kv*material.E/material.Fy)**0.5) and
                      self.htw_Gx <= (1.37*(self.Kv*material.E/material.Fy)**0.5)):
                    #
                    self.Cv = ((1.10*(self.Kv*material.E/material.Fy)**0.5)/(self.htw_Gx))
                #
                else:
                    #
                    self.Cv = ((1.51*material.E*self.Kv)/(material.Fy*self.htw_Gx**2))
            #
            # Shear Strength
            self.Vny = (0.6*material.Fy*self.Aw*self.Cv)
            #
            #
            # Check if transverse stifenners are required   
            # print 'h/tw (',self.htw_Gx, ') < 2.24',(2.24*(material.E/material.Fy)**0.5)
            # print 'h/tw (',self.htw_Gx, ') < 2.46',(2.46*(material.E/material.Fy)**0.5)
            # 
            if self.htw_Gx > (2.46*(material.E/material.Fy)**0.5) or self.Vy > self.Vny:
                self.TransvStiffeners= 'Required'               
                print ("Transverse Stiffeners : Requiered")
            #            
            else:
                print ("Transverse Stiffeners : No Requiered")
                self.TransvStiffeners= 'NoRequired'
            #
            print ("Tension Field Action : ",self.tension_field_action )
            #
            #
            # G2.1. Shear strength
            #       This section applies to webs of singly or doubly
            #       symmertric members and channels subject to shear
            #       in the plane of the web
            #
            #       The web plate shear buckling coefficient, Kv, is
            #       determined as follows:
            #
            #  i) For unstiffened webs:
            if self.a == 0.0:
                #                
                # with h/tw < 260
                if self.htw_Gx < 260.0:
                    self.Kv = 5.0
                    self.Kv_flag = 'h/tw < 260'
                #                    
                # rise an error
                else:
                    print ('***error h/tw > 260')
            #
            # ii) For stiffened webs
            else:
                #                
                #
                # G2 Members with Unstiffened Webs
                #    The moment of inertia Ist, of a transverse
                #    stiffeners used to develop the available web shear
                #    strength, as provided in Sec G2.1, about an axis
                #    in the web center for stiffener pairs or about the
                #    face in contact with the web plate for single 
                #    stiffeners, shall meet the following requirement:
                #                  
                #    Clear distance between transverse stiffeners
                #    print 'a =',self.a
                #    self.aMaxLength = round(h*min( 3.0,(260/(h/section.tw))**2 ))
                print (("a = "+"%-4.3f"+" m")%(self.a/1000.0))
                print (("a Max Length = "+"%-4.3f"+" m")%(self.aMaxLength/1000.0))
                #      
                # G2-8
                _j_G2 = max((2.5/(self.a/_h)**2)-2,0.5)
                #                
                # b is the smaller of the dimensions a and h
                _b_G2 = min(self.a, _h)
                #
                # G2-7
                self.Ist1 = _b_G2*_j_G2*section.tw**3
                #                
                # print (("j = "+"%-2.3f"+" (G2-8)")%(_j_G2))
                # print (("b = "+"%-4.3f"+" mm")%(_b_G2))
                # print (("Ist "+"%-1.4E"+" mm^4 (G2-7)")%(self.Ist1))
                #
                #
                # with a/h > 3.0
                if self.htw_Gx > 3.0 :
                    self.Kv = 5.0
                    self.Kv_flag = 'h/tw > 3.0'
                #
                #               or a/h > (260/(h/tw))^2 
                elif (a/h) > (260.0/self.htw_Gx)**2:
                    self.Kv = 5.0
                    self.Kv_flag = 'a/h > (260/(h/tw))^2'
                #
                # (G2-6)
                else:
                    self.Kv = 5.0 + (5.0/(a/h)**2)
                    self.Kv_flag = '(G2-6)'
                    #                
            #
            # print Kv
            print (("Kv = "+"%-2.3f"+ " (" +"%-8s"+")")%(self.Kv, self.Kv_flag))
            #
            #        
            # a) For webs of rolled I-Shaped members with:
            # h/tw <= 2.24(E/Fy)^0.5            
            if (section.type =='I' and section.type == 'DOUBLY' and
                section.build == "rolled" and self.htw_Gx <= (2.24*(material.E/material.Fy)**0.5)):          
                #
                self.Cv = 1.0
                self.Cv_flag = '(G2-2)'
                #                print 'a)' 
                #                print 'Cv :',self.Cv, self.Cv_flag
                #
            #
            # b) For webs of all other doubly symmetric shapes and
            #    channels, except round HSS, the web shear coefficient
            #    Cv, is determined as follows:
            else:
                # print 'b)'
                #                
                # (i) When h/tw <= 1.10*(Kv*E/Fy)^0.5
                if self.htw_Gx <= (1.10*(self.Kv*material.E/material.Fy)**0.5):
                    #                    
                    self.Cv = 1.0
                    self.Cv_flag = '(G2-3)'
                #
                # (ii) When 
                elif ((1.10*(self.Kv*material.E/material.Fy)**0.5) < self.htw_Gx <=
                      (1.37*(self.Kv*material.E/material.Fy)**0.5)):
                    #
                    self.Cv = ((1.10*(self.Kv*material.E/material.Fy)**0.5)/(self.htw_Gx))
                    self.Cv_flag = '(G2-4)'
                #
                # (iii) When h/tw > 1.37*(Kv*E/Fy)^0.5
                else:
                    #
                    self.Cv = ((1.51*material.E*self.Kv)/(material.Fy*self.htw_Gx**2))
                    self.Cv_flag = '(G2-5)'
                    #
            #-------
            # G2.1 Shear Strength
            # The nominal shear strength,Vn, of unstifened or stiffened
            # webs according to the limit states of shear yielding and
            # shear buckling:
            #                    
            self.Vny = (0.6*material.Fy*self.Aw*self.Cv)
            self.Vny_Flag = '(G2-1)'
            # print 'Vnx1 :',self.Vny , self.Vny_Flag
            self.Vc1 = self.Vny
            # print 'TensionFieldAction :', self.tension_field_action
            print (("Cv = "+"%-2.3f"+" "+"%-8s")%(self.Cv, self.Cv_flag))
            print (("Vn = "+"%-6.0f"+ " kN " +"%-8s")%(self.Vny/1000.0 , self.Vny_Flag ))
            #
            #
            #
            # G3 Tension Field Actions
            #
            # print 'xxxxx', self.Kv,self.Cv
            if self.tension_field_action != 'NO' and self.a != 0.0:
                #            
                # G3.1 Limits on the Use of Tension Field Actions
                # Consideration of tension field action is permited
                # for flanged mebers when the web plate is supported
                # on all four sides by flanges or stiffeners. 
                # Consideration of thension field actions is not permited:
                # print (" ")
                print (" -- Tension Field Action On --")
                #
                # Fyst - Specified minimum yield stress of the
                #        stiffener material        
                if self.Fyst==0: self.Fyst = material.Fy
                # print 'Fy stiffener :',self.Fyst
                #        
                # tst - Specified Thickness of the stiffener
                if self.tst == 0.0: self.tst = section.tw
                # print 'tst stiffener :',self.tst
                #            
                #            
                # Aw = (self.hw*section.tw)
                _Afc = (section.a*section.ta)
                _Aft = (section.b*section.tb)
                #
                #            
                # (a) for end panels in all members with transverse
                #     stiffeners;
                #     ??????
                #
                # (b) when a/h exceeds 3.0 or [260/(h/tw)]^2
                if self.a/_h > min( 3.0,(260/(_h/section.tw))**2 ):
                    # print (self.a/h) ,'>', min( 3.0,(260/(h/section.tw))**2 )
                    # print 'a = ' ,self.a
                    print (("a ("+"%-2.3f"+" m) should be < " +"%-2.3f" +" m")%
                           (self.a/1000.0,round(_h*min( 3.0,(260/(_h/section.tw))**2 ))/1000.0))
                    _G3_1b_flag = '(b) --> Reduce a - clear distance between transverse stiffnefer'
                    _Limit_G3_1 = 'FAIL'
                #
                # (c) when 2Aw/(Afc + Aft) > 2.5       
                elif (2*self.Aw/(_Afc + _Aft)) > 2.5 :
                    _G3_1b_flag = '(c) --> 2Aw/(Afc + Aft) > 2.5 '
                    _Limit_G3_1 = 'FAIL'
                #
                # (d) When h/bfc or h/bft > 2.5
                elif (h/section.a) > 6.0 or (h/section.b) > 6.0:
                    _G3_1b_flag = '(d) --> h/bfc or h/bft > 2.5'
                    _Limit_G3_1 = 'FAIL'
                #
                else:
                    _G3_1b_flag = ' '
                    _Limit_G3_1 = 'PASS'
                    #
                #
                # G3.2 Shear Strength with Tension field Action
                #
                if _Limit_G3_1 == 'PASS' :
                    # When tension field action is permitted accordingly to
                    # Sec G3.1, the nominal shear strength, Vn, with tension
                    # field action, accordingly to the limit state of tension
                    # field yielding, shall be:
                    #                
                    # print '(G3_1)',_Limit_G3_1
                    # print '(G3_3)',Limit_G3_3                
                    #                
                    # (b) When h/tw > 1.10*(Kv*E/Fy)^0.5
                    if (_h/section.tw) > 1.10*(self.Kv*material.E/material.Fy)**0.5:
                        #                    
                        self.VnG3 = ((0.6*material.Fy*self.Aw*
                                    (self.Cv+((1-self.Cv)/
                                              (1.15*(1+(self.a/_h)**2)**0.5))))*
                                     (self.Phi_shear/self.Omega_shear))
                        self.VnG3_Flag = '(G3-2)'
                        #
                    # (a) When h/tw <= 1.10*(Kv*E/Fy)^0.5
                    else:
                        #                    
                        self.VnG3 = 0.6*material.Fy*self.Aw*(self.Phi_shear/self.Omega_shear)
                        self.VnG3_Flag = '(G3-1)'
                        #
                    # Vn2
                    self.Vc2 = self.VnG3
                    # print 'Vnx2 G3:',self.VnG3 , self.VnG3_Flag
                    # print (("Cv  = "+"%-2.3f"+"%-8s")%(self.Cv, self.Cv_flag))
                    print (("Vn  = "+"%-6.0f"+ " kN " +"%-8s")%(self.VnG3/1000.0 , self.VnG3_Flag ))    
                    #
                    # 
                    # G3.3. Transversal Stiffeners
                    # Transverse stiffeners subject to tension field
                    # action shall meet the requirements of Sec G2.2
                    # and the following limitations:   
                    #
                    # print (' ')
                    # print ('Stiffener Section')
                    # Stiffener Section
                    _h_st = self.hw
                    # print ('h stiffener :',_h_st)
                    #            
                    self.b_st = (0.5*section.a) - (0.5*section.tw)
                    # print ('b stiffener :',self.b_st)
                    #            
                    # t_st = section.tw
                    # print ('t stiffener :',self.tst)
                    #            
                    #            
                    # Stiffener Properties
                    # Fyw = material.Fy
                    # print ('Fyw =',self.Fyst)
                    self.Ist = (self.b_st*_h_st**3)/12.0
                    # print ('Ist =', self.Ist)
                    # print ('Ist1 =',self.Ist1)
                    _pst = max((material.Fy/self.Fyst),1.0)
                    self.Ist2 = (((_h**4*float(_pst)**1.3)/40.0)*(self.Fyst/material.E)**1.50)
                    # print ('Ist2 =',self.Ist2)
                    # print ('Vr =', self.Vy)
                    # print ('Vc1 =', self.Vc1)
                    # print ('Vc2 =', self.Vc2)
                    #   
                    _Limit_G3_31 = 'PASS'
                    _Limit_G3_32 = 'PASS'
                    #                
                    # (1) (b/t)st <= 0.56*(E/Fyst)
                    if (self.b_st/self.tst) > 0.56*(material.E/self.Fyst):
                        _G3_3_flag = '(G3-3) --> (b/t)st > 0.56*(E/Fyst)'
                        _Limit_G3_31 = 'FAIL'
                    #
                    # (2) Ist >= Ist1 + (Ist2-Ist1)[(Vr-Vc1)/(Vc2-Vc1)]
                    try:
                        if self.Ist < (self.Ist1 + (self.Ist2-self.Ist1)[(abs(self.Vy)-self.Vc1)/
                                                      (self.Vc2-self.Vc1)]):
                            _G3_3_flag = '(G3-4) --> Ist < Ist1 + (Ist2-Ist1)[(Vr-Vc1)/(Vc2-Vc1)]'
                            _Limit_G3_32 = 'FAIL'                
                    #
                    # except ZeroDivisionError:
                    except :                        
                        # print 'zero division'                
                        if self.Ist < self.Ist1 :
                            _G3_3_flag = "(G3-4) --> Ist < Ist1 + (Ist2-Ist1)[(Vr-Vc1)/(Vc2-Vc1)]"
                            _Limit_G3_32 = 'FAIL'                 
                    #
                    if _Limit_G3_31 == 'PASS' and _Limit_G3_32 == 'PASS':
                        self.Limit_G3 = 'PASS'
                        self.tension_field_actionsflag='Permitted'
                    #                        
                    else : 
                        # Limit_G3_3 = 'FAIL'
                        self.tension_field_actionsflag='Not Permitted'
                        self.Limit_G3 = str(_G3_3_flag)
                        #                
                        # print self.Limit_G3,self.tension_field_actionsflag
                    #
                    #
                    #
                    if self.tension_field_actionsflag =='Permitted':
                        #                        
                        # self.tension_field_actionsflag='Permitted'
                        print ("Tension Field Actions : Permitted")
                        # print 'Vn G3 = ',self.VnG3, self.VnG3_Flag
                        #
                        if self.shear_stress == "maximum": self._Vr_TA = abs(self.VryMax)
                        #                
                        else: self._Vr_TA = abs(self.Vy)
                
                        self.URvG3 = abs(self._Vr_TA/self.VnG3)
                        #
                        if self.URvG3 > 1.0:
                            self.URvG3Status = 'FAIL'
                        #            
                        else:
                            self.URvG3Status = 'PASS'
                            #
                    #                    
                    else:
                        # self.tension_field_actionsflag='Not Permitted'
                        print ("Tension Field Actions : Not Permitted")              
                        #  print '(G3_3)',Limit_G3_3    
                #-------           
                # 
                else:
                    self.tension_field_actionsflag ='Not Permitted'
                    #  print (self.tension_field_actionsflag)
                    print ("Tension Field Actions : Not Permitted") 
                    self.Limit_G3 = 'G3.1.'+str(_G3_1b_flag)
                    print (self.Limit_G3,_Limit_G3_1)
                    #
        #-----------
        #
        #
        # G7 Weak Axis Shear in Singly and Doubly Symmetric Shapes
        #
        if section.type != 'UNSYMMETRIC':
            #            
            # For singly and doubly symmetric shapes loaded in the
            # weak axis without torsion, the nominal shear strength  
            # Vn, for shear resisting element shall be determined
            # using Equ G2-1 and Sec G2.1(b) with Aw = bf*tf,
            # h/tw = bf/tf and Kv = 1.2
            #
            print (" ")
            print ("        Weak Axis ")
            #            print ('Aw  =',self.Af)
            self.htw_Gy = max(section.a/section.ta,section.b/section.tb)
            # print ('h/tw Weak Axis=',self.htw_Gy)
            self.Kvx = 1.20
            # print ('Kv Weak Axis =',self.Kvx)
            #            
            # G2.1(b)
            #                
            # (i) When h/tw <= 1.10*(Kv*E/Fy)^0.5
            if self.htw_Gy <= (1.10*(self.Kvx*material.E/material.Fy)**0.5):
                #                    
                self.Cvx = 1.0
                self.Cvx_flag = '(G2-3)'
                #
            #
            # (ii) When 
            elif ((1.10*(self.Kvx*material.E/material.Fy)**0.5) < self.htw_Gy <=
                  (1.37*(self.Kvx*material.E/material.Fy)**0.5)):
                #
                self.Cvx = ((1.10*(self.Kvx*material.E/material.Fy)**0.5)/(self.htw_Gy))
                self.Cvx_flag = '(G2-4)'
                #
            #
            # (iii) When h/tw > 1.37*(Kv*E/Fy)^0.5
            else:
                #
                self.Cvx = ((1.51*material.E*self.Kvx)/(material.Fy*self.htw_Gy**2))
                self.Cvx_flag = '(G2-5)'
                #
            #
            # G2.1 Shear Strength
            #                    
            self.Vnx = (0.6*material.Fy*self.Af*self.Cvx)
            self.Vnx_Flag  = '(G2-7)'
            # print ('Factor ASD-LRFD', self.Omega_shear, self.Phi_shear,(0.6*material.Fy*self.Af*self.Cvx))
            print (("Cv = "+"%-2.3f"+" "+"%-8s" )%(self.Cvx, self.Cvx_flag))
            print (("Vn = "+"%-6.0f"+ " kN " +"%-8s")%(self.Vnx/1000.0 , self.Vnx_Flag ))
            print (" ")
            #
        #---
        #
        # Unsymmetric  Sections
        else:
            print ('UNSYMMETRIC Sections no implemented yet')
            #
        #
        #
        # Determine Max Shear UR
        #
        if self.shear_stress == "maximum":
        #            
            self.URvy = abs(self.VryMax/(self.Vny*(self.Phi_shear/self.Omega_shear)))
            self.URvx = abs(self.VrxMax/(self.Vnx*(self.Phi_shear/self.Omega_shear)))
            #            
            #if  self.URvy > self.URvx:
            #    self.URv = self.URvy
            #    self.URv_flag = self.Vny_Flag
            #            
            #else:
            #    self.URv = self.URvx
            #    self.URv_flag = self.Vnx_Flag   
            #                
        # 
        else:
            #   
            self.URvy = abs(self.Vy/(self.Vny*(self.Phi_shear/self.Omega_shear)))
            self.URvx = abs(self.Vx/(self.Vnx*(self.Phi_shear/self.Omega_shear)))
            # 
        #
        if  self.URvy > self.URvx :
            self.URv = self.URvy
            self.URv_flag = self.Vny_Flag
        #            
        else:
            self.URv = self.URvx 
            self.URv_flag = self.Vnx_Flag             
        #
        #
        if self.URv > 1.0:
            self.URvStatus = 'FAIL'
        #            
        else:
            self.URvStatus = 'PASS'
        #
        #        print ("ASD =",self.Omega_shear," LRFD =",self.Phi_shear)
        try:
            print (( "UR Main Axis (TFA) = "+"%-2.3f")%(self.URvG3))
        #
        except:
            print (( "UR Main Axis = "+"%-2.3f")%(self.URvy))
        #            
        print (( "UR Weak Axis = "+"%-2.3f")%(self.URvx))            
    #
    #
    #
    #                
    #-------------------------------------------------
    #
    #             ++++++ Torsion  ++++++             
    #
    def Torsion(self):
        # (a)
        _Fna = material.Fy
        #
        #(b)
        _Fnb = 0.6*material.Fy
        #
        # (c)
        _Fnc = 1
        #
    #
    #
    #
    #-------------------------------------------------
    #
    #            ++++++ Chapter H  ++++++ 
    #
    def ChapterH(self):
        #        
        print (" ")        
        print ("-----------------------------")
        print (" Chapter H - Combined Forces")
        print (" ")
        #
        # self.P = Pr
        # Pr = Pr
        # self.Mx =abs(Mrx)
        # self.My =abs(Mry)
        #      
        # Flange Moment of Inertia            
        _Iyc = (section.ta * section.a**3) / 12.0
        # Ixc = (section.ta*section.a**3)/12.0
        # print 'Iyc =',Iyc
        # print 'Ixc =',Ixc  
        _Icy_Iy = _Iyc / section.Iz
        # print 'Icy/Iy',Icy_Iy
        #
        # H1 Doubly and singly symmetric members subject
        #    to flexure and axial force
        if section.type != 'UNSYMMETRIC' and _Icy_Iy >= 0.1 and _Icy_Iy <= 0.9:
            #      
            self.Mcx = self.Mnx * (self.Phi_b/self.Omega_b)
            self.Mcy = self.Mny * (self.Phi_b/self.Omega_b)
            #
            # H1-1 Doubly and Syngly Symmetric Members
            #      in Flexure and Compression
            if self.FAxial == 'COMPRESSION':
                self.Pc = self.Pn_E * (self.Phi_c/self.Omega_c)
            #
            # if (Mrx/Mcx) < 0.05 or (Mcy/Mcy) < 0.05:
            # print '?'
            #                                   
            #
            # H1-2 Doubly and Syngly Symmetric Members
            #      n Flexure and Tension           
            else:
                self.Pc = self.Pn_D * (self.Phi_t/self.Omega_t)
            #
            # (H1-1b)
            if (abs(self.P) / self.Pc) < 0.2:
                #
                _UR = ((abs(self.P) / (2 * self.Pc)) +
                       (abs(self.Mx) / self.Mcx) + 
                       (abs(self.My)/self.Mcy))
                #
                _UR_flag = '(H1-1b)'
            #
            # (H1-1a)
            else:
                #
                _UR = ((abs(self.P) / self.Pc) +
                       (8.0 / 9.0) * ((abs(self.Mx) / self.Mcx) + 
                                      (abs(self.My) / self.Mcy)))
                #
                _UR_flag = '(H1-1a)'   
                #
        #
        # self.Pc = _Pc
        self.UR = _UR
        self.UR_flag = _UR_flag
        #        
        if self.UR > 1.0:
            self.URStatus = 'FAIL'
        #
        else:
            self.URStatus = 'PASS'
        #            
        # print (' ')
        print (("UR = "+"%-2.3f"+" "+"%-8s")%(self.UR, self.UR_flag))
    #        print self.FAxial
    #
    #     
    #
    #
    def PrintSummary(self):
        #          
        OutputFile = open(self.FileOut,'a+')
        #
        if section.type != 'I':
            OutputFile.write(" "+"\n")
            OutputFile.write("_______________________________________________________________________________________"+"\n")
            OutputFile.write(" "+"\n")
            OutputFile.write("                                YIELD Check Results"+"\n")
            OutputFile.write(" "+"\n")
            OutputFile.write("Name          URComb   UR Ax   Pr      Mrx       Mry   UR Vx    Vrx      Vry    Tr"+"\n")
            OutputFile.write("              Equ      UR Mx   Pc      Mcx       Mcy   UR Vy    Vcx      Vcy    Tc"+"\n")
            OutputFile.write("              Result   UR My                           UR T                       "+"\n")
            OutputFile.write(" "+"\n")
            OutputFile.write("_______________________________________________________________________________________"+"\n")
            OutputFile.write(" "+"\n")
            #
            # OutputFile.write(" "+"\n")
            # OutputFile.write("_______________________________________________________________________________________"+"\n")
            OutputFile.write(" "+"\n")
            OutputFile.write("                              STABILITY Check Results"+"\n")
            OutputFile.write(" "+"\n")
            OutputFile.write("Name          URComb   UR Ax     Pr        Mrx       Mry       Fex       Kx        Lx"+"\n")
            OutputFile.write("              Equ      UR Mx     Pc        Mcx       Mcy       Fey       Ky        Ly"+"\n")
            OutputFile.write("              Result   UR My               Cmx       Cmx       Cb        Kz        Lb"+"\n")
            OutputFile.write("_______________________________________________________________________________________"+"\n")
            OutputFile.write(" "+"\n")
            #
        #
        else:
            #
            # Memeber Compacness Section
            #
            OutputFile.write(" "+"\n")
            OutputFile.write("_______________________________________________________________________________________"+"\n")
            OutputFile.write(" "+"\n")
            OutputFile.write("                                  SECTION COMPACTNESS"+"\n")
            OutputFile.write(" "+"\n")
            OutputFile.write("Member ID     Type     T B4.1a    T B4.1a    T B4.1b    T B4.1b    b    [mm]  Kc "+"\n")
            OutputFile.write("              Build    Flg b/t    Web h/tw   Flg b/t    Web h/tw   t    [mm]  FL[N/mm2]"+"\n")
            OutputFile.write("              Symm     Alpha r    Alpha r    Alpha r    Alpha r    tw   [mm]  Ag  [mm2]"+"\n")
            OutputFile.write("                                             Alpha p    Alpha p    hc   [mm]  An  [mm2]"+"\n")            
            OutputFile.write("                       Class      Class      Class      Class      hp   [mm]  My [N.mm]"+"\n")
            OutputFile.write("......................................................................................."+"\n")
            OutputFile.write(" "+"\n") 
            #            
            OutputFile.write(("%-12s" +'  '+"%-6s" +"   FLANGE     WEB        FLANGE     WEB"+ 8*" "+ "%-1.3E" +
                              2*" "+ "%-1.3E" +"\n")% (self.BeamID, section.type, max(0.5*section.b,0.5*section.a),self.Kc))
            #            
            OutputFile.write((14*" "+ "%-6s" +3*" "+ "%-1.3E" +2*" "+ "%-1.3E" +2*" "+
                              "%-1.3E" +2*" "+ "%-1.3E" +2*" "+ "%-1.3E" +2*" "+ "%-1.3E" +"\n")%
                             (section.build, self.bt, self.htw, self.bt_F, self.htw, self.t,
                              self.FL))
            #
            OutputFile.write((14*" "+ "%-6s" +3*" "+ "%-1.3E" +2*" "+ "%-1.3E" + 2*" "+
                              "%-1.3E" +2*" "+ "%-1.3E" +2*" "+ "%-1.3E" +2*" "+ "%-1.3E" +"\n")%
                             (section.type, self.lambda_r_fE, self.lambda_r_wE, self.lambda_r_fF, 
                              self.lambda_r_wF, section.tw, section.Ag))
            #
            OutputFile.write((45*" "+ "%-1.3E" +2*" "+ "%-1.3E" +2*" "+ "%-1.3E" +2*" "+ "%-1.3E" +"\n")%
                             ( self.lambda_p_fF, self.lambda_p_wF,
                               self.hc, self.An))
            #
            OutputFile.write((23*" "+ "%-10s" +" "+ "%-10s"  +" "+ "%-10s" + " "+
                              "%-10s"+" "+ "%-1.3E" +2*" "+ "%-1.3E" +"\n")%
                             ( self.ClassFlangeE, self.ClassWebE, self.compacness_flexure, self.ClassWebF,
                               self.hp, self.Mym))
            #
            #
            #
            #
            # Shear and Torsion Section
            #
            #                
            OutputFile.write(" "+"\n")
            OutputFile.write("_______________________________________________________________________________________"+"\n")
            OutputFile.write(" "+"\n")
            OutputFile.write("                                  SHEAR CHECK RESULTS"+"\n")
            OutputFile.write(" "+"\n")
            OutputFile.write("Member ID     UR  Max  Vry    [N]  Vrx    [N]  h/tw_W  h/tw_F   TransvStiff  ShrStress"+"\n")
            OutputFile.write("Design to     Equ Max  Vny    [N]  Vnx    [N]  Kv_Web  Kv_Flg   a      [mm]  Maximum[N]"+"\n")
            #
            if self.design_method == 'ASD' or self.design_method == 'USER_DEFINED':
                _shear_factor = self.Omega_shear
                #
                OutputFile.write("Result        Equ Vny  OmegaV web  OmegaV flg  Cv_Web  Cv_Flg   aMax   [mm]  Average[N]"+"\n")           
                OutputFile.write("              Equ Vnx  OmV*Vr/Vn   OmV*Vr/Vn                    Comments"+"\n")
                #
            #
            else:
                _shear_factor = self.Phi_shear
                #
                OutputFile.write("Result        Equ Vny  PhiV        Vn Flng[N]  Cv_Web  Cv_Flg   aMax   [mm]  Average"+"\n")           
                OutputFile.write("              Equ Vnx  Vr/Vn*PhiV  Vr/Vn*PhiV                   Comments"+"\n")
                # 
            #
            OutputFile.write("......................................................................................."+"\n")
            OutputFile.write(" "+"\n")
            #  
            if self.shear_stress == "maximum":
                #
                _shear_stress_flag_1 = "maximum"
            #                
            else:
                #_shear_stress_flag_2 = ' '
                _shear_stress_flag_1 = "average"
                #
            #
            OutputFile.write(("%-12s" +"  "+ "%3.4f" +"   "+ "%1.4E" +"  "+ "%1.4E" +"  "+
                              "%3.3f" +"  "+"%3.3f"+"   "+ "%-12s" +2*" "+ "%-9s" +"\n")%
                             (self.BeamID, abs(self.URv), abs(self.Vy), abs(self.Vx), self.htw_Gx, 
                              self.htw_Gy, self.TransvStiffeners,_shear_stress_flag_1))
            #
            # if a was selected  
            if self.a != 0.0:                
                #
                if float(self.a) > float(self.aMaxLength): _a_comm = "Fail a>aMax"
                #
                else: _a_comm = "Ok  a<aMax"               
                #
                #
                OutputFile.write(("%-12s" +"  "+ "%6s" +3*" "+ "%1.4E" +"  "+ "%1.4E" +"  "+
                                  "%1.4f" +"  "+"%1.4f"+"   "+ "%1.4E"  + 3*" "+ "%1.4E" +"\n")%
                                 (self.design_method, self.URv_flag, self.Vny, self.Vnx, self.Kv, 
                                  self.Kvx, self.a, self.VryMax))
                #                    
                OutputFile.write(("%-12s" +"  "+ "%6s" +3*" "+ "%1.4E" +"  " + "%1.4E" +"  "+
                                  "%1.4f" +"  "+"%1.4f"+"   "+ "%1.4E" + 3*" "+ "%1.4E" +"\n")%
                                 (self.URvStatus, self.Vny_Flag, _shear_factor, _shear_factor, 
                                  self.Cv, self.Cvx, self.aMaxLength, abs(self.Vy)))
                #
                OutputFile.write((14*" "+ "%-6s" +"   "+ "%3.6f" +4*" "+ "%3.6f" +21*" "+ "%6s" + "\n")%
                                 (self.Vnx_Flag, self.URvy, self.URvx, _a_comm))
                #                
            #
            #
            else:
                #
                # Check if Transversal Stiffeners are Required                
                if self.TransvStiffeners == 'NoRequired' :
                    #                
                    #
                    OutputFile.write(("%-12s" +"  "+ "%6s" +3*" "+ "%1.4E" +"  "+ "%1.4E" +"  "+
                                      "%1.4f" +"  "+"%1.4f"+ 16*" "+ "%1.4E" + "\n")%
                                     (self.design_method, self.URv_flag, self.Vny, self.Vnx, self.Kv, 
                                      self.Kvx, self.VryMax))
                    #                    
                    OutputFile.write(("%-12s" +"  "+ "%6s" +3*" "+ "%1.4E" +"  " + "%1.4E" +"  "+ 
                                      "%1.4f" +"  "+"%1.4f" + 16*" "+ "%1.4E" +"\n")%
                                     (self.URvStatus, self.Vny_Flag, _shear_factor, _shear_factor, 
                                      self.Cv, self.Cvx, abs(self.Vy)))
                    #
                    OutputFile.write((14*" "+ "%-6s" +"   "+ "%3.6f" +4*" "+ "%3.6f" + "\n")%
                                     (self.Vnx_Flag, self.URvx, self.URvy))               
                    # 
                #
                else :
                    #                    
                    _a_comm = 'Provide a'                
                    #
                    OutputFile.write((14*" "+ "%6s" +3*" "+ "%1.4E" +"  "+ "%1.4E" +"  "+
                                 "%1.4f" +"  "+"%1.4f"+"   "+ "%1.4E"  +4*" "+ "%1.4E" +"\n")%
                                (self.URv_flag, self.VryMax, self.VrxMax, self.Kv, self.Kvx,
                                 self.a, self.VryMax))
                    #                    
                    OutputFile.write((14*" "+ "%-6s" +"   "+ "%1.4E" +"  "+ "%1.4E" +"  "+
                                 "%1.4f" +"  "+"%1.4f"+"   "+ "%1.4E" +"\n")%
                                (self.URvStatus, _shear_factor, _shear_factor, self.Cv, self.Cvx,
                                 self.aMaxLength))
                    #
                    OutputFile.write((14*" "+ "%-6s" +"   "+ "%3.4f" +6*" "+ "%3.4f" +
                                 23*" "+ "%6s" + "\n")%
                                ( self.design_method.upper(), self.URvx, self.URvy, _a_comm))      
                    #
            #
            # Check if Tension Action is selected                    
            if (self.tension_field_action != 'NO') :
                #  
                if self.a == 0.0: self.tension_field_actionsflag = 'MISSING DATA'
                #                
                OutputFile.write(" "+"\n")
                OutputFile.write("_______________________________________________________________________________________"+"\n")
                # OutputFile.write("......................................................................................."+"\n")
                OutputFile.write(" "+"\n")
                OutputFile.write("                          TENSION FIELD ACTIONS : ")
                OutputFile.write(str(self.tension_field_actionsflag)+"\n")
                #                
                if self.tension_field_actionsflag == 'Permitted' and self.URvy > 1.0:
                    #                    
                    OutputFile.write(" "+"\n")
                    OutputFile.write("Member ID     URComb   Vr Web[N]   h_st  [mm]  Fyw[N/mm2]  Ist  [mm4]      Comments"+"\n")
                    OutputFile.write("              Equ      Vn Web[N]   b_st  [mm]  Vc1    [N]  Ist1 [mm4]  "+"\n")
                    OutputFile.write("Result                 Vr/Vn Web   t_st  [mm]  Vc2    [N]  Ist2 [mm4]  "+"\n")
                    # OutputFile.write(" "+"\n")
                    OutputFile.write("......................................................................................."+"\n")
                    OutputFile.write(" "+"\n")
                    #    
                    OutputFile.write(("%-12s" +"  "+ "%3.4f" +"   "+ "%1.4E" +"  "+ "%1.4E" +"  "+
                                 "%1.4E" +"  "+ "%1.4E")%
                                (self.BeamID, self.URvG3, self._Vr_TA, self.hw, self.Fyst, self.Ist))
                    #                    
                    if self.Ist < self.Ist1 or self.Ist < self.Ist2:
                        OutputFile.write("  **Fail"+"\n")
                    #
                    else:
                        OutputFile.write("   < Ist1 & < Ist2"+"\n")
                    #                
                    OutputFile.write(("              "+  "%6s"  +"   "+ "%1.4E" +"  "+ "%1.4E" +"  "+
                                 "%1.4E" +"  "+ "%1.4E")%
                                (self.VnG3_Flag, self.VnG3, self.b_st, self.Vc1, self.Ist1))
                    #                    
                    if self.Ist < self.Ist1 :
                        OutputFile.write("  > Ist "+"\n")
                    #
                    else:
                        OutputFile.write("         ok "+"\n")                    
                    #                
                    OutputFile.write(("%-12s" +11*" "+ "%3.4f" +"      "+ "%1.4E" +"  "+
                                 "%1.4E" +"  "+ "%1.4E")%
                                (self.URvG3Status, abs(self._Vr_TA/self.VnG3), self.tst, self.Vc2, self.Ist2))
                    if self.Ist < self.Ist2 :
                        OutputFile.write("  > Ist "+"\n")
                    #
                    else:
                        OutputFile.write("         ok "+"\n")                      
                #                    
                elif self.a == 0.0:
                    OutputFile.write(" "+"\n")                     
                    OutputFile.write("  **Fail : Provide the Clear Distance (a) Between Transverse Stiffeners"+"\n")
                    OutputFile.write("_______________________________________________________________________________________"+"\n")
                    OutputFile.write(" "+"\n")       
                #
                elif self.URvy < 1.0:
                    OutputFile.write(" "+"\n")                     
                    OutputFile.write(("   But Web Shear Utilisation Vrx/Vnx ("+ "%2.4f" +") < 1.0 Therefore not required "+" "+"\n")%(self.URvy ))
                    OutputFile.write("_______________________________________________________________________________________"+"\n")
                    OutputFile.write(" "+"\n")                         
                #                    
                else:
                    OutputFile.write(" "+"\n")                     
                    OutputFile.write("  **Fail : "+str(self.Limit_G3)+" "+"\n")
                    OutputFile.write("_______________________________________________________________________________________"+"\n")
                    OutputFile.write(" "+"\n")                    
                    #
            #
            # Axial and Bending Moment Section
            #                
            if self.FAxial == 'COMPRESSION':
                #
                OutputFile.write(" "+"\n")
                OutputFile.write("_______________________________________________________________________________________"+"\n")
                OutputFile.write(" "+"\n")
                OutputFile.write("                    FLEXURE AND AXIAL COMPRESSION FORCE CHECK RESULTS"+"\n")
                OutputFile.write(" "+"\n")
                OutputFile.write("Member ID     URComb   Pr     [N]  Mrx [N/mm]  Mry [N/mm]  Cb    Q     Lx   [mm]  Kx"+"\n")
                OutputFile.write("Design to     Equ UR   Pn     [N]  Mnx [N/mm]  Mny [N/mm]  Fex[N/mm2]  Ly   [mm]  Ky"+"\n")
                #
                if self.design_method == 'ASD' or self.design_method == 'USER_DEFINED':
                    _Axial_factor = self.Omega_c
                    _BM_factor = self.Omega_b
                    #
                    OutputFile.write("Result        Equ Pn   OmegaC      OmegaBx     OmegaBy     Fey[N/mm2]  Lz   [mm]  Kz"+"\n")
                    OutputFile.write("              Equ Mn   Om*Pr/Pn    Om*Mrx/Mnx  Om*Mry/Mny  Fez[N/mm2]  Lb   [mm]  KL/r"+"\n")                
                else:
                    _Axial_factor = self.Phi_c
                    _BM_factor = self.Phi_b
                    #
                    OutputFile.write("Result        Equ Pn   PhiC        PhiCx       PhiCy       Fey[N/mm2]  Lz   [mm]  Kz"+"\n")
                    OutputFile.write("              Equ Mn   Pr/Pn*Phi   Mrx/Mn*Phi  Mry/Mn*Phi  Fez[N/mm2]  Lb   [mm]  KL/r"+"\n")
                #
                OutputFile.write("......................................................................................."+"\n")
                OutputFile.write(" "+"\n")
                #                
                OutputFile.write(("%-12s" +"  "+ "%3.4f" +"   "+ "%1.4E" +"  "+ "%1.4E" +"  "+
                             "%1.4E" +"  "+"%1.2f" +"  "+"%1.2f"+"  "+"%1.3E"+"  "+ "%1.2f" +"\n")%
                            (self.BeamID, self.UR, abs(self.P), abs(self.Mx), abs(self.My), self.Cb,
                              self.Q, self.Lx, self.Kx))
                #                
                OutputFile.write(("%-12s" +"  "+ "%6s" +"  "+ "%1.4E" +"  "+ "%1.4E" +"  "+
                             "%1.4E" +"  "+"%1.4E" +"  "+"%1.3E" +"  "+ "%1.2f" +"\n")%
                            (self.design_method, self.UR_flag, self.Pn_E, self.Mnx, self.Mny, self.Fex_E4_7, self.Ly, self.Ky))
                #                
                OutputFile.write(("%-12s" +"  "+ "%-6s" +"   "+ "%1.4E" +"  "+ "%1.4E"  + "  "+ "%1.4E"  +
                              "  "+ "%1.4E" +"  "+"%1.3E" +"  "+ "%1.2f" +"\n")%
                            (self.URStatus, self.Pn_E_flag, _Axial_factor, _BM_factor, _BM_factor, 
                             self.Fey_E4_8, self.Lzt, self.Kz))
                #                
                OutputFile.write((14*" "+ "%-6s" +"   "+ "%3.6f" + 4*" "+ "%3.6f"  + 4*" "+ "%3.6f"  +
                              4*" " + "%1.4E" +"  "+"%1.3E" +"  "+ "%3.1f" +"\n")%
                            (self.Mnx_flag, abs(self.P/self.Pc ), abs(self.Mx/self.Mcx), 
                             abs(self.My/self.Mcy), self.Fez_E4_9, self.Lb, self.KLr))
                #                
                OutputFile.write(" "+"\n")
                #OutputFile.write("_______________________________________________________________________________________"+"\n")
                OutputFile.write(" "+"\n")
                #
            #
            else:
                OutputFile.write(" "+"\n")
                OutputFile.write("_______________________________________________________________________________________"+"\n")
                OutputFile.write(" "+"\n")
                OutputFile.write("                     FLEXURE AND AXIAL TENSION FORCE CHECK RESULTS"+"\n")
                OutputFile.write(" "+"\n")
                OutputFile.write("Member ID     URComb   Pr     [N]  Mrx [N/mm]  Mry [N/mm]  Lx/rx       Lx    [mm]"+"\n")
                OutputFile.write("Design to     Equ UR   Pn     [N]  Mnx [N/mm]  Mny [N/mm]  Ly/ry       Ly    [mm]"+"\n")
                #
                if self.design_method == 'ASD' or self.design_method == 'USER_DEFINED':
                    _Axial_factor = self.Omega_t
                    _BM_factor = self.Omega_b
                    #
                    OutputFile.write("Result        Equ Pn   OmegaT      OmegaBx     OmegaBy                 Lz    [mm]"+"\n")
                    OutputFile.write("              Equ Mn   Om*Pr/Pn    Om*Mrx/Mnx  Om*Mry/Mny              Lb    [mm]"+"\n")
                #
                else:
                    _Axial_factor = self.Phi_t
                    _BM_factor = self.Phi_b
                    #
                    OutputFile.write("Result        Equ Pn   PhiT        OmegaBx     OmegaBy                 Lz    [mm]"+"\n")
                    OutputFile.write("              Equ Mn   Pr/Pn*Phi   Mrx/Mn*Phi  Mry/Mn*Phi              Lb    [mm]"+"\n")
                #
                OutputFile.write("......................................................................................."+"\n")
                OutputFile.write(" "+"\n")
                #   
                if self.L_r > 300 :
                    _Lr_flag_1 = "L/r > 300"
                    _Lr_flag_2 = "**FAIL"
                #
                else:
                    _Lr_flag_1 = "L/r < 300"
                    _Lr_flag_2 = "OK"
                #                
                OutputFile.write(("%-12s" +"  "+ "%3.4f" +"   "+ "%1.4E" +"  "+ "%1.4E" +"  "+
                             "%1.4E" +"  "+"%1.4E" +"  "+"%1.4E"+"  "+"\n")%
                            (self.BeamID, self.UR, abs(self.P), abs(self.Mx), abs(self.My), 
                             (self.Lx/self.rx), self.Lx))
                #                
                OutputFile.write(("%-12s" +"  "+ "%-6s" +"  "+ "%1.4E" +"  "+ "%1.4E" +"  "+
                             "%1.4E" +"  "+"%1.4E" +"  "+"%1.4E" +"  "+"\n")%
                            (self.design_method, self.UR_flag, self.Pn_D, self.Mnx, self.Mny, (self.Ly/self.ry), self.Ly))
                #                
                OutputFile.write(("%-12s" +"  "+ "%-6s" +"   "+ "%1.4E" + "  "+ "%1.4E"  +"  "+ "%1.4E"  +
                              "  "+ "%-6s" + 3*" " +"%1.4E" +"\n")%
                            (self.URStatus, self.Pn_D_Flag, _Axial_factor, _BM_factor, _BM_factor, 
                            _Lr_flag_1, self.Lzt))   
                #                
                OutputFile.write((14*" "+ "%-6s" +"   "+ "%3.6f" + 4*" "+ "%3.6f"  + 4*" "+ "%3.6f" +
                                  4*" " +"%-6s"+ 6*" " +"%1.4E"  +"\n")%
                                 (self.Mnx_flag, abs(self.P/self.Pc ), abs(self.Mx/self.Mcx),
                                  abs(self.My/self.Mcy), _Lr_flag_2,self.Lb))
                #                
                OutputFile.write(" "+"\n")
                #OutputFile.write("_______________________________________________________________________________________"+"\n")
                OutputFile.write(" "+"\n")             
                #
        #
        #
        OutputFile.close()
        #
        print (" ")
        print ("End Writing Out File")
    #
    #    
    #
    #
#