# Copyright (c) 2015-2023 steelpy

# Python stdlib imports
import datetime

# package imports


#-------------------------------------------------
#                   Print Section
#-------------------------------------------------
#
#
def header(self):
    """
    """
    today = datetime.date.today()
    #
    output = []
    output.append("\n")
    output.append("***************************************************************************************\n")
    output.append("*                                  CODE CHECK TOOL                                    *\n")
    output.append("*                               Strength Of Pipelines                                 *\n")
    output.append("*                                  ALPHA Version                             01/06/15 *\n")            
    output.append("***************************************************************************************\n")
    output.append(("DATE: {:} {:} UNITS [N-mm]\n").format(today, 57*" "))
    output.append("\n")
    output.append("{:}\n".format(88*"_"))
    output.append("\n")
    output.append("PIPING CODE: {:55s} CALCULATION: {:3.0f}\n"
                  .format(self.code_name, self.header))    
    #
    return output
    #        
#
def pipe_geometry(self):
    """
    """
    #
    output = []
    output.append("{:}\n".format(88*"_"))
    output.append("\n")
    output.append("                                     GEOMETRY DATA\n")
    output.append("\n")
    output.append("Pipe ID      PipelineType  Do    [mm]  Tnom  [mm]  Tcorr [mm]  tolfab [%]  Lc    [mm]\n")
    output.append("Pipe Wall         Do/Tmin  Di    [mm]  Tmin  [mm]  Tfab  [mm]  fo     [%]\n")
    output.append("\n")
    output.append("{:}\n".format(88*"."))
    output.append("\n")
    #
    if self.Do/self.tmin > 20:
        _PipeWallFlag = "THIN"
    
    else:
        _PipeWallFlag = "THICK"
    
    output.append("{:14s}{:11s} {: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E}\n"
                  .format(self.pipe_name, self.pipe_type, self.Do,
                          self.tnom, self.tcorr, self.tol*100, self.Lc))
    
    output.append("{:14s}{:11.2f} {: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E}\n"
                  .format(_PipeWallFlag, self.Do/self.tmin, self.Di,
                          self.tmin, self.tfab, self.fo*100))
    #
    return output
#
def section_properties(self):
    """
    """
    #
    output = []
    output.append("\n")
    output.append("{:}\n".format(88*"_"))
    output.append("\n")
    output.append("                            SECTION DERIVED PROPERTIES (Tmin)"+"\n")
    output.append("\n")
    output.append("Pipe ID        Area[mm^2]  I   [mm^4]  Ze  [mm^3]  Zp  [mm^3]  ShapeFctor  r    [mm]\n")
    output.append("                           Ip  [mm^4]  J   [mm^4]\n")
    output.append("{:}\n".format(88*"."))
    output.append("\n")
    output.append("{:14s}{: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E}\n"
                  .format(self.pipe_name, self.section.area, self.section.Iy, self.section.Zey,
                   self.section.Zpy , (self.section.Zpy/self.section.Zey), self.section.ry))
    
    output.append("{:}{: 1.4E} {: 1.4E}\n"
                  .format(26*" ", self.section.Ip, self.section.J))
    #
    return output 
#
def material_properties(self):
    """
    """
    #
    output = []
    output.append("{:}\n".format(88*"_"))
    output.append("\n")
    output.append("                                  MATERIAL PROPERTIES\n")
    output.append("\n")
    output.append("Pipe ID        Fy [N/mm2]  Fu [N/mm2]  E  [N/mm2]  G  [N/mm2]  Poisson     Alpha[C-1]\n")
    if self.material_derating:
        output.append("MaterialType   Fy DeRated  Fu DeRated  Tmax[degC]  CodeMethod\n")
        #output.append("Pipe ID        Sy [N/mm2]  Su [N/mm2]  "+"\n")
    output.append("\n")
    output.append("{:}\n".format(88*"."))
    output.append("\n")        
    output.append("{:14s}{: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E}\n"
                  .format(self.pipe_name, self.SMYS, self.SMTS, 
                          self.E, self.G, self.Poisson, self.alpha_T))
    
    if self.material_derating:
        output.append("{:14s}{: 1.4E} {: 1.4E} {: 1.4E}  {:12s}\n"
                      .format(self.material_type, self.sigma_y, 
                              self.sigma_u, self.Tmax, self.derate_method))
    #
    return output
    #
#
def design_data(self):
    """
    """
    #
    output = []
    output.append("\n")
    output.append("{:}\n".format(88*"_"))
    output.append("\n")
    output.append("                                   PIPING DESIGN DATA\n")
    output.append("\n")
    output.append("Pipe ID        Pi [N/mm2]  Po [N/mm2]  T1  [degC]  T2  [degC]  Delta[degC]\n")
    #output.append("               Pi [N/mm2]  T2  [degC]              "+"\n")
    #output.append("               Se [N/mm2]  Tau[N/mm2]  Fsz    [N]  Mz [N/mm2]  (Mz*lo*C1)  Alpha[C-1] "+"\n")
    output.append("\n")
    output.append("{:}\n".format(88*"."))
    output.append("\n")
    for x in range(len(self.delta_T)):
        output.append("{:14s}{: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E}\n"
                      .format(self.pipe_name, self.Pi, self.Po, 
                              self.T1, self.T2, self.delta_T[x]))
    #
    return output
    #
#
def hydrostatic_pressure(self):
    #
    output = []
    output.append("\n")
    output.append("{:}\n".format(88*"_"))
    output.append("\n")
    output.append("                           HYDROSTATIC PRESSURE CALCULATIONS\n")
    output.append("\n")
    output.append("Pipe ID        Po [N/mm2]  H     [mm]  d     [mm]  L     [mm]  k\n")
    output.append("                           T    [sec]  z     [mm]  Hz    [mm]  WaveTheory\n")
    output.append("{:}\n".format(88*"."))
    output.append("\n")
    output.append("{:14s}{: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E}\n"
                    .format(self.pipe_name, self.Po, self.Hw, self.d,
                            self.wave_length, self.k))
    
    output.append("{:}{: 1.4E} {: 1.4E} {: 1.4E}  Stokes 5\n"
                  .format(26*" ", self.T, self.z, self.Hz))
    #
    return output
    #
#
def flexibility_stress_factors(self):
    """
    """
    #  
    output = []
    output.append("{:}\n".format(88*"_"))
    output.append("\n")
    output.append("                      FLEXIBILITY & STRESS INTENSIFICATION FACTORS\n")
    output.append("\n")
    output.append("Pipe ID       Description  r2    [mm]  T     [mm]  Db    [mm]  h           C1\n")
    output.append("Code SIFs                  rx    [mm]  Tc    [mm]  S     [mm]  k           li\n")
    output.append("                           R1    [mm]  Tr    [mm]  Theta[deg]  NoFlangles  lo\n")
    output.append("\n")
    output.append("{:}\n".format(88*"."))
    output.append("\n")
    output.append("{:14s} {:10s} {: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E}\n" 
                  .format(self.pipe_name, self.pipe_description, 
                          self.r2, self.T_, self.T_, self.h, self.C1))
    
    output.append("{:14s}{:}{: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E}\n"
                  .format(self.FS_code, 12*" ", self.rx, self.Tc,
                          self.S, self.k, self.li/self.C1))
    
    output.append("{:}{: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E}\n"
                  .format(26*" ", self.R1, self.Tr, self.theta,
                          self.flanges, self.lo/self.C1))
    #
    return output
    #
#
def stress_calculation(self):
    """
    """
    #
    output = []
    output.append("\n")
    output.append("{:}\n".format(88*"_"))
    output.append("\n")
    output.append("                                 STRESS CALCULATIONS\n")
    output.append("\n")
    output.append("Pipe ID        Sh [N/mm2]  SL [N/mm2]  Fx     [N]  Mx  [N.mm]   SIF TYPE   C1\n")
    output.append("PipeBoundCond  Se [N/mm2]  Sb [N/mm2]  Fsy    [N]  My  [N.mm]  (My*li*C1)  li\n")
    output.append("LongStresCalc              Tau[N/mm2]  Fsz    [N]  Mz  [N.mm]  (Mz*lo*C1)  lo\n")
    #output.append("               Se [N/mm2]  St [N/mm2]  Fs     [N]              M  [N/mm2]  Alpha[C-1]\n")
    output.append("\n")
    output.append("{:}\n".format(88*"."))
    output.append("\n")
    #
    _sigmaL = "INCLUDED"
    if self.pressure :
        _sigmaL = "EXCLUDED"
    #
    _pipe_restrain = "UNRESTRAINED"
    if self.pipe_restrained :
        _pipe_restrain = "RESTRAINED"
    #
    if 'fea' in self.load_type.lower():
        _pipe_restrain = "FEA DEFINED"
        _sigmaL = "EXCLUDED"
    #
    output.append("{:14s}{: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E}  {:10s} {: 1.4E}\n"
                  .format(self.pipe_name, self.sigma_h, self.sigma_L,
                          self.Fx, self.Mb, self.SIFs_type, self.C1))
    
    output.append("{:14s}{: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E}\n"
                  .format( _pipe_restrain, self.sigma_e, self.Sb, self.Fs, self.Mi, 
                              self.Mi*self.li, self.li/self.C1))
    
    output.append("{:14s}{:}{: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E}\n"
                  .format( _sigmaL, 12*" ", self.tau, self.Fs, 
                           self.T, self.T*self.lo, self.lo/self.C1))
    #
    return output
    #
#
def allowable_stress(self):
    """
    """
    #
    output = []
    output.append("\n")
    output.append("{:}\n".format(88*"_"))
    output.append("\n")
    output.append("                                  ALLOWABLE STRESS\n")
    output.append("\n")
    output.append("Pipe ID       UR max   Se [N/mm2]   Sh [N/mm2]   Sy [N/mm2]   Design Condition\n")
    output.append("              Equ      fa*Sy        fah*Sy       fa [Equiv]   Pipe Type\n")
    output.append("Result                 Se/(fa*Sy)   Sh/(fa*Sy)   fah [Hoop]\n")
    output.append("\n")
    output.append("{:}\n".format(88*"."))
    output.append("\n")
    #
    UR_max = max(self.UR_h, self.UR_eq)
    _UmEqu = "6.4.2.4"
    if UR_max == self.UR_h: 
        _UmEqu = "6.4.2.2"
    #
    UR_max_flag = 'PASS'
    if UR_max > 1.0:
        UR_max_flag = 'FAIL'
    #
    output.append(("{:14s}{:3.4f}  {: 1.4E}  {: 1.4E}  {: 1.4E}   {:12s}\n")
                  .format(self.pipe_name, UR_max, self.sigma_e, 
                          self.sigma_h, self.sigma_y, self.design_condition))
    
    output.append(("{:}{:6s} {: 1.4E}  {: 1.4E}  {: 1.4E}   {:12s}\n")
                  .format(14*" ",_UmEqu, self.fd*self.sigma_y,
                           self.fd_hs*self.sigma_y, self.fd, self.pipe_type))
    
    output.append(("{:14s}{:}{: 1.4E}  {: 1.4E}  {: 1.4E}\n")
                  .format(UR_max_flag, 8*" ", self.UR_eq,
                          self.UR_h, self.fd_hs))
    #
    return output
    #
#
def buckling(self):
    """
    """
    output = []
    output.append("\n")
    output.append("{:}\n".format(88*"_"))
    output.append("\n")
    output.append("                                   BUCKLING (Annex G1)\n")
    output.append("\n")
    output.append("Pipe ID       UR max   Po [N/mm2]   Fx     [N]   M   [N/mm]   Tau [N/mm]   Epsilon\n")
    output.append("              URcomb   Pc [N/mm2]   Fxc    [N]   Mc  [N/mm]   Tauc[N/mm]   Epsilonbc\n")
    output.append("Result                 Po/Pc        Fx/Fxc       M/Mc         Tau/Tauc     E/Ebc\n")
    output.append("\n")
    output.append("{:}\n".format(88*"."))
    output.append("\n")
    #
    UR_max = max(self.UR_lc, self.epsilon_b/self.epsilon_bc)
    #
    UR_max_flag = "PASS"
    if UR_max > 1.0:
        UR_max_flag = "FAIL"
    #
    output.append("{:14s} {:3.4f} {: 1.4E}  {: 1.4E}  {: 1.4E}  {: 1.4E}  {: 1.4E}\n"
                  .format(self.pipe_name, self.UR_lc, self.Po, self.Fx, 
                          self.Mb, self.tau, self.epsilon_b))
    #
    output.append("{:} {:3.4f} {: 1.4E}  {: 1.4E}  {: 1.4E}  {: 1.4E}  {: 1.4E}\n"
                  .format(14*" ", self.UR_lc, self.Pc, self.Fxc, 
                          self.Mc, self.tau_c, self.epsilon_bc))
    #
    output.append("{:14s}{:}{: 1.4E}  {: 1.4E}  {: 1.4E}  {: 1.4E}  {: 1.4E}\n"
                  .format(UR_max_flag, 8*" ", self.Po/self.Pc, 
                          abs(self.Fx)/self.Fxc, self.Mb/self.Mc,
                          self.tau/self.tau_c, self.epsilon_b/self.epsilon_bc))
    #
    return output
    #
#
#
def propagation_upheaval(self):
    """
    """
    #
    output = []
    output.append("\n")
    output.append("{:}\n".format(88*"_"))
    output.append("\n")
    output.append("                         PROPAGATION & OVALIZATION (Annex G2 & G4)\n")
    output.append("\n")
    output.append("Pipe ID                P  [N/mm2]   f\n")
    output.append("                       Pp [N/mm2]   Cp\n")
    output.append("                       P/Pp         Cf\n")
    output.append("\n")
    output.append("{:}\n".format(88*"."))
    output.append("\n")
    #
    UR_max = 1.0
    UR_max_flag ="FAIL"
    #
    output.append("{:14s}{:} {: 1.4E}  {: 1.4E}\n"
                  .format(self.pipe_name, 7*" ", self.P, self.f))
    
    output.append("{:}{: 1.4E}  {: 1.4E}\n"
                  .format(22*" ", self.Pp, self.Cp))
    
    output.append("{:}{: 1.4E}  {: 1.4E}\n"
                  .format(22*" ", self.P/self.Pp, self.Cf))
    #
    return output
    #
#
#