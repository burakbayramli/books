# 
# Copyright (c) 2019 iLift
#
# Python stdlib imports
import datetime
#

# package imports


#-------------------------------------------------
#
#
#
def PrintSummary(self):
    """
    """
    OutputFile = open(self.FileOut,'a+')
    #
    if self.section.type != 'I':
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
                          2*" "+ "%-1.3E" +"\n")% (self.BeamID, section.type, max(0.5*section.bfb,0.5*section.bft),self.kc))
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
                         ( self.ClassFlangeE, self.ClassWebE, self.compactness_flexure, self.ClassWebF,
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
            if float(self.a) > float(self.a_max_length): _a_comm = "Fail a>aMax"
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
                              self.Cv, self.Cvx, self.a_max_length, abs(self.Vy)))
            #
            OutputFile.write((14*" "+ "%-6s" +"   "+ "%3.6f" +4*" "+ "%3.6f" +21*" "+ "%6s" + "\n")%
                             (self.Vnx_Flag, self.URvy, self.URvx, _a_comm))
            #                
        #
        #
        else:
            #
            # Check if Transversal Stiffeners are Required                
            if self.TransvStiffeners == 'norequired' :
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
                             self.a_max_length))
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
#
def print_header():
    """
    """
    today = datetime.date.today()
    output = [] # open(self.FileOut,'w')
    #
    #output.append(" "+"\n")
    output.append("***************************************************************************************\n")
    output.append("*                                  CODE CHECK TOOL                                    *\n")
    output.append("*                                      AISC 16                                        *\n")
    output.append("*                     Specification for Structural Steel Buildings                    *\n")
    output.append("*                                   BETA Version                             12/12/18 *\n")            
    output.append("***************************************************************************************\n")
    output.append("DATE: {:8s} {:>59}".format(str(today), ""))
    # 
    #output=open(self.FileOut,'a+')
    return output
#
#
def print_compactness(self, section):
    """
    """
    output = []
    output.append("\n")
    output.append("{:_<87}\n".format(""))
    output.append("\n")
    output.append("                                  SECTION COMPACTNESS\n")
    output.append(" "+"\n")
    output.append("Member ID     Type     T B4.1a    T B4.1a    T B4.1b    T B4.1b    b    [mm]  Kc\n")
    output.append("              Build    Flg b/t    Web h/tw   Flg b/t    Web h/tw   t    [mm]  FL[N/mm2]\n")
    output.append("              Symm     Alpha r    Alpha r    Alpha r    Alpha r    tw   [mm]  Ag  [mm2]\n")
    output.append("                                             Alpha p    Alpha p    hc   [mm]  An  [mm2]\n")            
    output.append("                       Class      Class      Class      Class      hp   [mm]  My [N.mm]\n")
    output.append("{:.<87}\n".format(""))
    output.append("\n") 
    #
    output.append("{:12s}{:8s} FLANGE     WEB        FLANGE     WEB {:6s} {:1.3E}  {:1.3E}\n"
                  .format(self.beam_name, "", "",
                          max(0.5*section.bfb.convert('millimetre').value, 0.5*section.bft.convert('millimetre').value), 
                          self.kc))                                             # @hami2230 - removed the .value from kc. This value is specified as a float in some places and has has no .value
    #
    htw = (section.hw.value / section.tw.value)
    output.append("{:20s} {:1.3E}  {:1.3E}  {:1.3E}  {:1.3E}  {:1.3E}  {:1.3E}\n"
                  .format(section.build, (0.50 * section.bf.value / section.tf.value),
                          htw, (section.bf.value / section.tf.value), htw, 
                          section.tw.convert('millimetre').value, self.FL.convert('megapascal').value))
    #
    output.append("{:20s} {:1.3E}  {:1.3E}  {:1.3E}  {:1.3E}  {:1.3E}  {:1.3E}\n"
                   .format("",
                           self._lambda_r_flange_a.value, self._lambda_r_web_a.value,
                           self._lambda_r_flange_b.value, self._lambda_r_web_b.value,
                           section.tw.convert('millimetre').value, section.Ag.convert('millimetre^2').value))
    #
    output.append("{:42s} {:1.3E}  {:1.3E}  {:1.3E}  {:1.3E}\n"
                  .format( "", self._lambda_p_flange.value, self._lambda_p_web.value,
                           self.hc.convert('millimetre').value, section.An.convert('millimetre^2').value))
    #
    output.append("{:20s} {:10s} {:10s} {:10s} {:10s} {:1.3E}  {:1.3E}\n"
                  .format("", self.compactness.flange.compression.compactness, 
                          self.compactness.web.compression.compactness, #@ hami2230 - renamed compactness names as above
                          self.compactness.flange.flexure.compactness, 
                          self.compactness.web.flexure.compactness,
                          self.hp.convert('millimetre').value, 
                          self.Mym.convert('newton * millimetre').value))
    return output
#
#
def printChaperG(self, section, material):
    """
    """
    # Shear and Torsion Section
    output = []        
    output.append(" "+"\n")
    output.append("_______________________________________________________________________________________"+"\n")
    output.append(" "+"\n")
    output.append("                                  SHEAR CHECK RESULTS"+"\n")
    output.append(" "+"\n")
    output.append("Member ID     UR  Max  Vry    [N]  Vrx    [N]  h/tw_W  h/tw_F   TransvStiff  ShrStress"+"\n")
    output.append("Design to     Equ Max  Vny    [N]  Vnx    [N]  Kv_Web  Kv_Flg   a      [mm]  Maximum[N]"+"\n")
    #
    if 'asd' in self.design_method.lower() or 'user' in self.design_method.lower():
        _shear_factor = self.Omega_shear
        output.append("Result        Equ Vny  OmegaV web  OmegaV flg  Cv_Web  Cv_Flg   aMax   [mm]  Average[N]"+"\n")           
        output.append("              Equ Vnx  OmV*Vr/Vn   OmV*Vr/Vn                    Comments"+"\n")
    else:
        _shear_factor = self.Phi_shear
        output.append("Result        Equ Vny  PhiV        Vn Flng[N]  Cv_Web  Cv_Flg   aMax   [mm]  Average"+"\n")           
        output.append("              Equ Vnx  Vr/Vn*PhiV  Vr/Vn*PhiV                   Comments"+"\n")
    # 
    output.append("......................................................................................."+"\n")
    output.append(" "+"\n")
    #  
    if 'maximum' in self.shear_stress.lower():
        _shear_stress_flag_1 = "maximum"           
    else:
        _shear_stress_flag_1 = "average"
    #
    UR = max(self.ChaperG_results.URy, self.ChaperG_results.URz)
    UR_status = 'Pass'
    if UR > 1:
        UR_status = 'Fail'
    
    output.append("{:12s} {:3.4f} {:1.4E} {:1.4E} {:3.3f} {:3.3f} {:16s} {:9s}\n"
                  .format(self.beam_name, UR, 
                          abs(self.actions.Fy.convert('kilonewton').value), 
                          abs(self.actions.Fz.convert('kilonewton').value), 
                          self.htw_Gx.value, self.htw_Gy.value,                            
                          self.TransvStiffeners, _shear_stress_flag_1))
    #
    # if a was selected  
    if self.a:
        print ("Transverse stiffeners not implemented")                        # @hami2230 - transverse stiffeners not implemented
        #if self.a > self.a_max_length: 
        #    _a_comm = "Fail a>aMax"
        #else: 
        #    _a_comm = "Ok  a<aMax"               
        #
        #output.append(("%-12s" +"  "+ "%6s" +3*" "+ "%1.4E" +"  "+ "%1.4E" +"  "+
        #                  "%1.4f" +"  "+"%1.4f"+"   "+ "%1.4E"  + 3*" "+ "%1.4E" +"\n")%
        #                 (self.design_method, self.ChaperG_results.UR_flag, 
        #                  self.Vny.convert('kilonewton').value, 
        #                  self.Vnx.convert('kilonewton').value, 
        #                  self.Kv, self.Kvx, self.a.convert('metre').value, self.VryMax.value))
        #                    
        #output.append(("%-12s" +"  "+ "%6s" +3*" "+ "%1.4E" +"  " + "%1.4E" +"  "+
        #                  "%1.4f" +"  "+"%1.4f"+"   "+ "%1.4E" + 3*" "+ "%1.4E" +"\n")%
        #                 (UR_status, self.Vny_Flag, _shear_factor, _shear_factor, 
        #                  self.Cv, self.Cvx, self.a_max_length.convert('metre').value, 
        #                  abs(self.actions.Fy.convert('kilonewton').value)))
        #
        #
        #output.append((14*" "+ "%-6s" +"   "+ "%3.6f" +4*" "+ "%3.6f" +21*" "+ "%6s" + "\n")%
        #                 (self.Vnx_Flag, self.ChaperG_results.URy, self.ChaperG_results.URz, _a_comm))
        #
    else:
        # Check if Transversal Stiffeners are Required               
        if self.TransvStiffeners.lower() == 'norequired' :
            output.append(("{:12s} {:6s} {:1.4E} {:1.4E} {:1.4f} {:1.4f} {:1.4E}\n")
                          .format(self.design_method, self.ChaperG_results.UR_flag, 
                                  self.Vny.convert('kilonewton').value, 
                                  self.Vnx.convert('kilonewton').value, 
                                  self.Kv, self.Kvx, self.VryMax.value))
            #
            output.append(("{:12s} {:6s} {:1.4E} {:1.4E} {:1.4f} {:1.4f} {:1.4E}\n")
                          .format(UR_status, self.Vny_Flag, _shear_factor, _shear_factor, 
                                  self.Cvy,  self.Cvx, abs(self.actions.Fy.convert('kilonewton').value))) #@hami2230 - changed self.Cv to self.Cvy. Also removed .values as Cv is now a float.
            #
            output.append(("{:6s} {:3.6f} {:3.6f}\n")
                          .format(self.Vnx_Flag, 
                                  self.ChaperG_results.URy, self.ChaperG_results.URz))               
        else :
            _a_comm = 'Provide a'
            self.a = self.L
            self.a_max_length = self.L                                         # @hami2230 - fudge to run
            output.append((14*" "+ "%6s" +3*" "+ "%1.4E" +"  "+ "%1.4E" +"  "+
                         "%1.4f" +"  "+"%1.4f"+"   "+ "%1.4E"  +4*" "+ "%1.4E" +"\n")%
                        (self.ChaperG_results.UR_flag, self.VryMax.value, self.VrzMax.value, 
                         self.Kv, self.Kvx, self.L.convert('metre').value, self.VryMax.value)) # @hami2230 - changed self.a to self.L
                                
            #output.append((14*" "+ "%-6s" +"   "+ "%1.4E" +"  "+ "%1.4E" +"  "+
            #             "%1.4f" +"  "+"%1.4f"+"   "+ "%1.4E" +"\n")%
            #            (UR_status, _shear_factor, _shear_factor, self.Cvy, self.Cvx,   # @hami2230 - removed the .value from Cv so can print when assigned as integer and float.
            #             self.a_max_length))
            
            output.append((14*" "+ "%-6s" +"   "+ "%3.4f" +6*" "+ "%3.4f" +
                         23*" "+ "%6s" + "\n")%
                        ( self.design_method.upper(), 
                          self.ChaperG_results.URy, self.ChaperG_results.URz, _a_comm))      
    #
    #
    return output
#
#
def printChapterD(self, section):
    """
    """
    output = []
    output.append(" "+"\n")
    output.append("_______________________________________________________________________________________"+"\n")
    output.append(" "+"\n")
    output.append("                     FLEXURE AND AXIAL TENSION FORCE CHECK RESULTS"+"\n")
    output.append(" "+"\n")
    output.append("Member ID     URComb   Pr     [N]  Mrx [N/mm]  Mry [N/mm]  Lx/rx       Lx    [mm]"+"\n")
    output.append("Design to     Equ UR   Pn     [N]  Mnx [N/mm]  Mny [N/mm]  Ly/ry       Ly    [mm]"+"\n")
    #
    if 'lrfd' in self.design_method.lower() or 'user_defined' in self.design_method.lower():
        _Axial_factor = self.Omega_t
        _BM_factor = self.Omega_b
        #
        output.append("Result        Equ Pn   OmegaT      OmegaBx     OmegaBy                 Lz    [mm]"+"\n")
        output.append("              Equ Mn   Om*Pr/Pn    Om*Mrx/Mnx  Om*Mry/Mny              Lb    [mm]"+"\n")
    else:
        _Axial_factor = self.Phi_t
        _BM_factor = self.Phi_b
        #
        output.append("Result        Equ Pn   PhiT        OmegaBx     OmegaBy                 Lz    [mm]"+"\n")
        output.append("              Equ Mn   Pr/Pn*Phi   Mrx/Mn*Phi  Mry/Mn*Phi              Lb    [mm]"+"\n")
    #
    output.append("......................................................................................."+"\n")
    output.append(" "+"\n")
    #   
    if self.L_r > 300 :
        _Lr_flag_1 = "L/r > 300"
        _Lr_flag_2 = "**FAIL"
    else:
        _Lr_flag_1 = "L/r < 300"
        _Lr_flag_2 = "OK"
    #                
    output.append(("{:12s} {:3.4f} {:1.4E} {:1.4E} {:1.4E} {:1.4E} {:1.4E}\n")
                  .format(self.beam_name, self.ChapterH_results.UR, 
                          abs(self.actions.Fx.convert('newton').value), 
                          abs(self.actions.Mx.convert('newton*metre').value), 
                          abs(self.actions.My.convert('newton*metre').value), 
                          (self.L.value/section.ry.value), 
                          self.L.convert('metre').value))
    #                
    output.append(("{:12s} {:12s} {:1.4E} {:1.4E} {:1.4E} {:1.4E} {:1.4E}\n")
                  .format(self.design_method, self.ChapterH_results.UR_flag, 
                          self.Pn_D.convert('kilonewton').value,
                          self.Mnx.convert('newton*metre').value, 
                          self.Mny.convert('newton*metre').value, 
                          (self.L.value/section.ry.value), 
                          self.L.convert('metre').value))
    #                
    output.append(("{:12s} {:6s} {:1.4E} {:1.4E} {:1.4E} {:6s}{:1.4E}\n")
                  .format(self.ChapterH_results.status, self.Pn_D_Flag, _Axial_factor, 
                          _BM_factor, _BM_factor, 
                          _Lr_flag_1, self.L.convert('metre').value))
    #                
    output.append(("{:6s} {:3.6f} {:3.6f} {:3.6f} {:6s} {:1.4E}\n")
                  .format(self.Mnx_flag, abs(self.actions.Fx.value/self.Pc.value), 
                          abs(self.actions.My.value/self.Mcx.value),
                          abs(self.actions.Mz.value/self.Mcy.value), 
                          _Lr_flag_2, self.Lb.convert('metre').value))
    #                
    #output.append("\n")
    #output.append("_______________________________________________________________________________________"+"\n")
    output.append("\n")             
    return output
#
#
def printChapterE(self, section):
    """
    """
    output = []
    #if self.FAxial == 'COMPRESSION':
    #
    output.append(" "+"\n")
    output.append("_______________________________________________________________________________________"+"\n")
    output.append(" "+"\n")
    output.append("                    FLEXURE AND AXIAL COMPRESSION FORCE CHECK RESULTS"+"\n")
    output.append(" "+"\n")
    output.append("Member ID     URComb   Pr     [N]  Mrx [N/mm]  Mry [N/mm]  Cb    Q     Lx   [mm]  Kx"+"\n")
    output.append("Design to     Equ UR   Pn     [N]  Mnx [N/mm]  Mny [N/mm]  Fex[N/mm2]  Ly   [mm]  Ky"+"\n")
    #
    if self.design_method == 'ASD' or self.design_method == 'USER_DEFINED':
        _Axial_factor = self.Omega_c
        _BM_factor = self.Omega_b
        #
        output.append("Result        Equ Pn   OmegaC      OmegaBx     OmegaBy     Fey[N/mm2]  Lz   [mm]  Kz"+"\n")
        output.append("              Equ Mn   Om*Pr/Pn    Om*Mrx/Mnx  Om*Mry/Mny  Fez[N/mm2]  Lb   [mm]  KL/r"+"\n")                
    else:
        _Axial_factor = self.Phi_c
        _BM_factor = self.Phi_b
        #
        output.append("Result        Equ Pn   PhiC        PhiCx       PhiCy       Fey[N/mm2]  Lz   [mm]  Kz"+"\n")
        output.append("              Equ Mn   Pr/Pn*Phi   Mrx/Mn*Phi  Mry/Mn*Phi  Fez[N/mm2]  Lb   [mm]  KL/r"+"\n")
    #
    output.append("......................................................................................."+"\n")
    output.append(" "+"\n")
    #                
    self.Q = 1                                                                 # @hami2230 - added as a fudge. To be corrected during reporting.
    output.append("{:12s} {:3.4f} {:1.4E} {:1.4E} {:1.4E} {:1.2f} {:1.2f} {:1.3E} {:1.2f}\n"
                  .format(self.beam_name, self.ChapterH_results.UR, 
                          abs(self.actions.Fx.convert('newton').value), 
                          abs(self.actions.My.convert('newton*metre').value), 
                          abs(self.actions.Mz.convert('newton*metre').value),
                          self.Cb, self.Q, self.L.convert('metre').value, self.Kx))
    #  
    self.Fex_E4_7 = 1 
    self.Fey_E4_8 = 1
    self.Fez_E4_9 = 1                                                            # @hami2230 -added as a fudge. To be corrected during reporting.    
    output.append("{:12s} {:6s} {:1.4E} {:1.4E} {:1.2f} {:1.2f} {:1.3E} {:1.2f}\n"
                  .format(self.design_method, self.ChapterH_results.UR_flag, 
                          self.Pn_E.convert('kilonewton').value, 
                          self.Mnx.convert('newton*metre').value, 
                          self.Mny.convert('newton*metre').value, 
                          self.Fex_E4_7,
                          self.L.convert('metre').value, self.Ky))
    #                
    output.append("{:12s} {:6s} {:1.4E} {:1.4E} {:1.4E} {:1.4E} {:1.3E} {:1.2f}\n"
                  .format(self.ChapterH_results.status, self.Pn_E_flag, _Axial_factor, 
                          _BM_factor, _BM_factor, 
                          self.Fey_E4_8, 
                          self.L.convert('metre').value, self.Kz))
    #                
    self.Pc = 1
    output.append("{:6s} {:3.6f} {:3.6f} {:3.6f} {:1.4E} {:1.3E} {:3.1f}\n"
                  .format(self.Mnx_flag, abs(self.actions.Fx.value/self.Pc), 
                          abs(self.actions.My.value/self.Mcx.value), 
                          abs(self.actions.Mz.value/self.Mcy.value), 
                          self.Fez_E4_9, 
                          self.Lb.convert('metre').value, self.KLr.value))
    #                
    output.append(" "+"\n")
    #output.append("_______________________________________________________________________________________"+"\n")
    #output.append(" "+"\n")
    return output
#
#