# Copyright (c) 2015-2023 steelpy
#
# Python stdlib imports
from __future__ import annotations
import math
#import datetime
#
# package imports
from steelpy.utils.units.main import Units
from steelpy.utils.math.rootsearch import GoalSeeker
#
#
def ellipse_section(D, t, fo):
    """
    D 
    t
    fo
    
    Calculation of elastic moment of inertia for an ellipse
    """
    Dmin = (1.0 - 0.50 * fo) * D
    Dmax = (1.0 + 0.50 * fo) * D
    
    a = (Dmin - t)/ 2.0
    b = (Dmax - t) / 2.0
    Area, Zc, Yc, Iy, Zey, Iz, Zez = hollow_ellipse(a, b, t)
    return min(Iy, Iy)
#
#
def pipe_section(Do, tnom, tcheck):
    """
    """
    Dinom = (Do - 2 * tnom)
    # Internal Area
    Ai = math.pi / 4.0 * Dinom**2
    # External Area
    Ae = math.pi / 4.0 * Do**2    
    # Pipe section area (nominal thickness)
    CSA = math.pi / 4.0 * (Do**2 -(Do - 2*tnom)**2)
    # Pipe section area (Tnom - Tcorr thickness)
    Anomfab = math.pi / 4.0 * (Do**2 -(Do - 2 * tcheck)**2)
    I = math.pi / 64.0 * (Do**4 - (Do - 2 * tcheck)**4)
    Ze = I / (Do / 2.0)
    # 
    # St. Venant torsional constant
    J = math.pi/32.0 * (Do**4 - (Do - 2*tcheck)**4)
    C = 2*J/Do
    # Stelco 1981
    #Q = tcheck/6 * (3*Do**2 - 6*Do*tcheck + 4*tcheck**2)
    #Crt = 2*tcheck*I / Q
    #
    return Ai, Ae, CSA, Anomfab, I, Ze, C
#
#
#
def get_PD8010_upheaval(self, PD8010):
    """
    Two major factors contribute towards the upheaval of pipelines: the effective
    axial driving force, arising from the internal pressure and temperature, and the
    presence of vertical out-of-straightness (OOS) in the profile. The resistance to
    upheaval is provided by the weight of the pipeline, plus any overburden, if
    present.
    """
    t_check = self.tnom
    self.F = PD8010.buckling_force(t_check,
                                   self.Pi, self.Do,
                                   self.delta_T, output=True)

    self.Lr, self.hr = PD8010.imperfection_shape(t_check, w_ins=t_check)

    if self.ovality:
        self.EI = PD8010.ovality_bending_effect(self.Do, self.tnom,
                                                self.ovality)
    else:
        self.EI = [self.E * PD8010.I for i in self.F]

    Dc = self.Do - self.coating
    self.Hreq = {}
    for x in range(len(self.delta_T)):
        _name = round(self.delta_T[x], 2)
        self.Hreq[_name] = PD8010.download_response(self.F[x], self.Lr, 
                                                    self.EI, self.hr,
                                                    self.w_operation, 
                                                    self.rho_sub, 
                                                    self.f, Dc)    
#
def get_PD8010_strain(self):
    """
    """
    # 6.4 Strength Check
    # 6.4.3.a
    #
    if "load" in self.load_type:
        t_check = self.tnom
        self.sigma_h = PD8010.hoop_stress(self, t_check)
    #
    elif "fea_stress" in self.load_type:
        PD8010.allowable_stress(self.sigma_h,
                                self.sigma_e,
                                self.design_method)
    #
    # 6.4.3.b & 6.4.3.d
    PD8010.equivalent_strain(self)
    #
    # 6.4.3.c (?)
    #
    # # 6.4.3.i.1.- Bending Failure (?)
    #
    #    
#
#
def PD8010_buckling(self, PD8010, output):
    """
    G.1.1 General
    NOTE 1 Local buckling of the pipe wall can be avoided if the various loads to
    which the pipe is subjected are less than the characteristic values in G.1.2 to G.1.7.
    NOTE 2 Guidance on buckling is given in DNV-OS-F101.
    """
    if output:
        print("")
        print("6.4.4 Buckling Check (Annex H)")
    # 6.4.4.1.a
    # G.1.2 External pressure
    if self._pressure[1]:
    #if self.Po.value > 0:       
        # 6.4.4.1.b
        # G.2 Propagation buckling
        P = max(self.pressure, self._pressure[0])
        self.P, self.Pp = PD8010.propagation_buckling(P, self.tnom, output=output)
        #
        self.Pc = PD8010.external_pressure(self.P, self.fo,
                                           self.sigma_u,
                                           self.root_search,
                                           output=output)
    else:
        self.Pc = 0 * self.units.MPa
        self.P = 0 * self.units.MPa
    #
    #
    try:
        # 6.4.4.1.c
        #PD8010.upheaval_buckling(self)
        # G.1.7 Strain criteria
        self.epsilon_b = PD8010.strain_criteria(self.P, self.Pc, self.epsilon_bc)
        # 6.4.4.2 Ovality
        # G.4 Ovalization
        self.Cf, self.Cp, self.f = PD8010.ovalization(self.P, self.fo, 
                                                      self.epsilon_b)                
    except AttributeError:
        #if self.load_type:
        #if "load" in self.load_type :
        # G.1.3 Axial compression
        Px = self._env_load[0]
        Fa = self.functional_load[0]
        self.Fxc = PD8010.axial_compression(Px + Fa, 
                                            self.Do, self.tnom,
                                            output=output)
        #else:
        #    self.Fxc = 0
        # G.1.4 Bending
        Mb = self.functional_load[2]
        BMip = self._env_load[3]
        BMop = self._env_load[4]
        M = (BMip**2 + BMop**2)**0.50
        self.Mc, self.epsilon_bc = PD8010.bending(Mb + M, 
                                                  self.Do, self.tnom,
                                                  output=output)
        # G.1.5 Torsion
        T  = self.functional_load[3]
        BMt = self._env_load[5]
        self.tau_c = PD8010.torsion(T + BMt, 
                                    self.Do, self.tnom,
                                    output=output)
        # G.1.6 Load Combination
        Px = self._env_load[0]
        Mb = self.functional_load[2]
        self.UR_lc = PD8010.load_combination(Mb = Mb + M, 
                                             Mc = self.Mc, 
                                             Fx = Px + Fa, 
                                             Fxc = self.Fxc,
                                             fo = self.fo, 
                                             P = self.P, 
                                             Pc = self.Pc,
                                             output=output)
    #
    # upheaval buckling
    if 'upheaval' in self.design_method:
        get_PD8010_upheaval(self, PD8010)
    #print('--')
#       
#
def get_DNV(self):
    """
    """
    # DNV Section
    #
    #
    self.code_name = "DNV-OS-F101 (2007) Submarine Pipeline Systems"
    print("NO YET IMPLEMENT")
    exit()
#
def get_ASME(self):
    """
    """
    # ASME Section
    self.code_name = "ASME B31.3-2006 Process Piping"
    print("NO YET IMPLEMENT")
    exit()
#
def expansion_flexibility(self, output):
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
    if self.FS_code == "ASME_B31.3":
        asme = ASME()
        asme.bend_data(self.pipe_description,
                       self.flanges, 
                       self.T_, 
                       self.r2, 
                       self.R1, 
                       self.theta)

        h, k, lo, li, C1 = asme.AppendixD()
    #
    # BS EN 13480
    elif self.FS_code == "BS_EN_13480":
        #
        # Bend Flexibility
        if self.SIFs_type == 'BEND':
            #
            print ("No implement yet")
        # Tee Flexibility
        elif self.SIFs_type == 'TEE':
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
    if output:
        print ("")
        print ("Expansion Flexibility ")
        print ("Based on : {:}".format(self.FS_code))
        print ("Member Description: {:}".format(self.pipe_description))
        print ("Member Type: {:}".format(self.SIFs_type))
        print ("h  = {:2.3f}".format(h))
        print ("k  = {:2.3f}".format(k))
        print ("lo = {:2.3f}".format(lo))
        print ("li = {:2.3f}".format(li))
        print ("C1 = {:2.3f}".format(C1))
    #
    return h, k, lo, li, C1
#
#
def material_derating(self):
    """
    """
    if self.derate_method == "DNV":
        derate = DNV()
        derate.temperature(self.Tmax)
        derate.material(self.SMYS, self.SMTS)
        Fy, Fu = derate.characteristic_material_properties(self.design_condition, 
                                                           self.material_type)
        #self.sigma_y = Fy
        #self.sigma_u = Fu
    return Fy, Fu 
#
#
def get_external_pressure(self, output):
    """
    """
    if not self.wave_length:
        wave = Stoke5()
        wave.Data(self.Hw/1000., self.Tw, self.d/1000.)
        #print ('Wave Length =',Wave.WaveLength)
        self.wave_length = wave.WaveLength * 1000.0
    #
    self.k = (2*math.pi)/self.wave_length
    
    # Maximum External Pressure
    self.Hz = (-self.z + (self.Hw/2)*
               ((math.cosh(self.k * (self.d + self.z)))/
                (math.cosh(self.k * self.d))))
    
    # (13.2-20)
    Po = (self.rho_w * self.g * self.Hz)
    #
    if output:
        print("")
        print("External Pressure")
        print('Wave Length               = {:2.3f}'.format(wave.WaveLength))
        print("Maximum External Pressure = {:2.3f}".format(self.Hz))
        print("                      Po  = {:2.3f}".format(Po))
    #
    return Po
#
#
class MainPipe:
    
    def __init__(self):
        """
        """
        self.units = Units()
        self.g = 9.810 * self.units.gravity
        self.rho_w = 0.0000010250 * self.units.kg/self.units.mm**2
    #   
    # ===================================================
    # Section Properties
    # ===================================================
    #     
    def section_input(self, Do:Units, tnom:Units, 
                      tcorr:Units, tol:float, output:bool=False): 
        """
        Do    : outside diameter of a pipe
        tnom  : nominal wall thickness
        tcorr : corrosion allowance
        tol   : Manufacturing Tolerance (-ve) (include percentage sign)
        """
        self.Do = Do
        self.tnom = tnom
        self.tcorr = tcorr 
        self.tol = tol
        
        self.tfab = self.tnom * self.tol
        self.tmin = self.tnom - self.tfab - self.tcorr
        self.Di = (self.Do - 2 * self.tmin)
        #
        #-------------------------------------------------
        #   Cross-Sectional Area
        #
        self.A = (math.pi / 4.0) * (self.Do**2 -(self.Do - 2*self.tmin)**2)
        #
        #-------------------------------------------------
        #               Section Properties
        #-------------------------------------------------
        #   Second Moment of Area about Mayor Axis
        #   --------------------------------------
        #
        self.I = (math.pi / 64.0) * (self.Do**4 - (self.Do - 2*self.tmin)**4)
        # 
        self.Ze = self.I / (Do / 2.0)
        #-------------------------------------------------
        #   Plastic Modulus about Mayor Axis
        #    def Zx(self):
        #
        self.Zp = (self.Do**3 - (self.Do - 2 * self.tmin)**3) / 6.0 
        #
        #-------------------------------------------------
        #   Radius of gyration about Mayor Axis
        #
        self.r = (self.I / self.A)**0.50
        #
        #-------------------------------------------------
        #   Torsional Constant
        #
        self.J = 2 * self.I
        # print ('Torsional Constant',self.J)
        # 
        #-------------------------------------------------
        #   Polar Moment of Inertia
        self.Ip = (math.pi / 32.0) * (self.Do**4 - (self.Do - 2*self.tmin)**4)
        #
        #-------------------------------------------------
        #  Mass
        #self.mass = (self.A * self.Rhos)/1000**2
        #
        if output:
            print ('')
            print ('Section Properties')
            print ('Do = {:1.3f} mm'.format(self.Do.convert('millimetre').value))
            print ('t nom = {:1.3f} mm'.format(self.tnom.convert('millimetre').value))
            print ('t corr = {:1.3f} mm'.format(self.tcorr.convert('millimetre').value))
            print ('t min = {:1.3f} mm'.format(self.tmin.convert('millimetre').value))
            #print (' ')        
        #
        return self.tmin, self.Di
    #
    def material_input(self, sigma_y:Units, sigma_u:Units, 
                       E:Units, Poisson:float, alpha_T:float, output=False):
        """
        """
        self.sigma_y = sigma_y
        self.E = E
        self.Poisson = Poisson
        self.sigma_u = sigma_u
        self.alpha = alpha_T
        
        if output:
            print ('')
            print ('Material Properties')
            print ('Fy = {: 1.3e} MPa'.format(self.sigma_y.convert('megapascal').value))
            print ('Fu = {: 1.3e} MPa'.format(self.sigma_u.convert('megapascal').value))
            print ('E =  {: 1.3e} MPa'.format(self.E.convert('megapascal').value))
            print ('Coefficient of Thermal Expansion = {: 1.3e} C^-1'
                   .format(self.alpha.value - 1/273.15))
            #print('-->')
    #
    def enviromental_stress(self, Px, Vip, Vop, BMip, BMop, BMt,
                            tcheck=None, output=False):
        """
        """
        if not tcheck:
            tcheck = self.tnom
        
        Ai, Ae, CSA, Anomfab, I, Ze, C =  pipe_section(self.Do, self.tnom, tcheck)
        
        M = (BMip**2 + BMop**2)**0.50
        
        sigma_I = abs(Px / Anomfab) + (M / Ze)
        
        sigma_II = 0 * self.units.MPa
        
        V = (Vip**2 + Vop**2)**0.50
        
        sigma_tau =  BMt / (2*C) + 2*V / CSA
        
        sigma_e = (sigma_I**2 - sigma_I * sigma_II 
                   + sigma_II**2 + sigma_tau**2)**0.50
        
        if output:
            print("")
            print("Enviromental Stress Calculation")
            print("Equivalent Stress = {:1.3e} MPa"
                  .format(sigma_e.convert('megapascal').value))
        
        return sigma_e
    #
    # 
    def stressFEA(self, tnom=None, output=False):
        """
        tnom : Nominal wall thickness (m)
        
        Stress based on FE analysis inlcuding Temp & Int Pressure
        """
        #
        if tnom:
            tnomcorr = float(tnom)
        else:
            tnomcorr = self.tnom - self.tcorr
        #
        # NOTE Nominal wall thickness may be used in the evaluation.
        #
        Sigmah = self.sigma_h * (self.tnom/tnomcorr)
        #
        self.sigma_L = self.sigma_L * (self.tnom/tnomcorr)
        #
        self.tau = self.tau * (self.tnom/tnomcorr)
        #
        #
        # Unless a strain-based design approach is adopted 
        # (see 6.4.3), equivalent stresses should be evaluated
        # using the von Mises stress criterion shown in equation(6)
        # (6)
        self.sigma_e = math.sqrt(Sigmah**2 + self.sigma_L**2 -
                                (Sigmah * self.sigma_L) + 
                                (3*self.tau**2))
        #
        # NOTE Minimum wall thickness may be used in the evaluation
        #
        self.sigma_h = self.sigma_h * (self.tnom/self.tmin)
        #
        if output:
            print (" ")
            print ("Equivalent Stress")
            print ("T fact: {:}".format(self.tnom/tnomcorr))
            print ("Hoop Stress  = {:2.3f}".format(Sigmah))
            print ("Longitudinal Stress = {:2.3f}".format(self.sigma_L))
            print ("Tau  = {:2.3f}".format(self.tau))
            print ("Sigma e  = {:2.3f}".format(self.sigma_e))
            print ("Hoop Stress  = {:2.3f}".format(self.sigma_h))
        #
        return self.sigma_e
    #
    # G.1 Local Buckling
    def external_pressure(self, Po, fo, sigma_u,
                          root_search:str='FAST', 
                          factor:float=2.0,
                          output=False):
        """
        factor = 2.0 (Legacy from BS8010-3-1993)
        
        G.1.2 External Pressure
        
        The characteristic value, Pc, that causes collapse
        when the external pressure is acting alone, can be
        calculated using equations (G.1) to (G.4).
        """
        # (G.4) Maximun Ovality
        # self.fo = (self.Dmax - Dmin)/self.Do
        #
        # (G.3) Yield Pressure
        self.Py = 2*self.sigma_y * self.tnom / self.Do
        #
        # (G.2) Elastic Critical Pressure
        self.Pe = (2*self.E / (1.0 - self.Poisson**2) * (self.tnom/self.Do)**3)
        #        
        #
        # (G.1)
        # Define Funtion 
        # TODO : Check why 2?
        foDt = fo * self.Do.value / self.tnom.value
        Pe = self.Pe.convert('megapascal').value
        Py = self.Py.convert('megapascal').value
        def f(x): return ((x / Pe - 1.0)  * ((x / Py)**2 - 1.0) 
                          - factor * (x / Py) *  foDt)
        #
        # Find first root (Note that it may not be the minimum
        # use 'full' to find all roots, but process is slow)
        #self.Search = 'FAST'
        # Find Pc
        sigmaP = sigma_u.convert('megapascal').value*10
        Pc = GoalSeeker(f, sigmaP, root_search) * self.units.MPa
        #
        if output:
            print("")
            print("H.1.2 External Pressure")            
            print("Maximun Ovality (fo)           = {:1.3e}".format(fo))
            print("Yield Pressure (Py)            = {:1.3e} MPa".format(Py))
            print("Elastic Critical Pressure [Pe] = {:1.3e} MPa".format(Pe))
            #print("Po max  = {:2.3f}".format(Po))
            print("Charact Ext Pressure     [Pc'] = {:1.3e} MPa"
                  .format(Pc.convert('megapascal').value))
            print("External Pressure         [Po] = {:1.3e} MPa"
                  .format(Po.convert('megapascal').value))            
            print("URhp  = {:2.3f}".format(abs(Po.value/Pc.value)))
        #
        return Pc
    #
    # -------------------------
    # G.2  Propagation buckling
    def propagation_buckling(self, P, t, output=False):
        """
        H.2  Propagation Buckling
        
        The potential for a pipeline to propagate local 
        buckles is dependent on the external overpressure, 
        P, and its relationship with the propagation pressure Pp.
        """
        #
        # The propagation pressure, Pp, can be calculated using 
        # equation (G.21).
        # (H.21)
        Pp = 10.70 * self.sigma_y * (t/self.Do)**2.25
        #
        # The external overpressure, P, can be calculated using
        # equation (H.20).
        #P = Po - Pi
        # Check if external pressure is not significant
        #if P.value < 0:
        #    P = Po
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
        if output:
            print("")
            print("H.2 Propagation Buckling")
            print("Pp    = {:1.3e}".format(Pp.convert('megapascal').value))
            print("P     = {:1.3e}".format(P.convert('megapascal').value))
            print("URpp  = {:2.3f}".format(P.value / Pp.value))
            if P < Pp:
                print("Buckle will not propagate")
            else:
                print("Local buckle or local damage occurr")
        #
        return P, Pp
    #
    def buckling_force(self, tcheck:Units, Pi:Units, 
                       d:Units, delta_T:list[Units], 
                       SF=1.2, output=False):
        """
        """
        Ai, Ae, CSA, Anomfab, I, Ze, C =  pipe_section(self.Do, self.tnom, tcheck)
        # pressure difference
        L = 1 * self.units.m
        delta_P = Pi - (self.rho_w * d * math.pi * self.g / L)
        # fully build-in in Force
        F = []
        for x, _deltaT in enumerate(delta_T):
            F.append((self.E * self.alpha_T * CSA * _deltaT 
                           + Ai * (1.0 - 2.0 * self.Poisson) * delta_P) * SF)
        #
        # factored temperature difference
        self.delta_Tfactor = []
        for _F in F:
            self.delta_Tfactor.append((_F - Ai * (1.0 - 2.0 * self.Poisson) * delta_P) 
                                      / (self.E * self.alpha_T * CSA))
        #
        if output:
            for x, _F in enumerate(F):
                print(f"F[{x}] = {F[x].convert('kilonewton').value:1.3e} kN")
                print("Factored tempe diff = {: 1.3e} C"
                      .format(self.delta_Tfactor[x].value - 273.15))
        #
        return F
    #
    def imperfection_shape(self, tcheck, w_ins):
        """
        """
        Ai, Ae, CSA, Anomfab, I, Ze, C =  pipe_section(self.Do, self.tnom, tcheck)
        
        L = [(hi * 72.0 * self.E * I / w_ins)**0.25 for hi in self.h]
        
        # Define height and half length between inflection points
        hr = [hi/2.4545 for hi in self.h]
        Lr = [Li/3.0 for Li in L]
        #print(' Lr : ', self.Lr)
        
        return Lr, hr
    #
    #
    def axial_compression(self, Fx, D, t, output=False):
        """
        G.1.3 Axial Compression
        
        If D/tnom is less than 60, local buckling under
        axial compression does not occur until the mean 
        axial compression load, Fxc, reaches the yield
        load, Fy, i.e. as shown in equation (G.5).
        """
        if D.value/t.value < 60:
            Fy = math.pi*(D - t) * t * self.sigma_y
            Fxc = Fy
        else:
            print('D/t > 60')
            1/0
        #
        if output:
            print ("")
            print ("G.1.3 Axial Compresion")
            print ("Fx       = {: 1.3e} KN".format(Fx.convert('kilonewton').value))
            print ("Fxc      = {: 1.3e} kN".format(Fxc.convert('kilonewton').value))
            print ("URaxial  = {:2.3f}".format(Fx.value/Fxc.value))
        #
        return Fxc
    #
    def bending(self, Mb, D, t, output=False):
        """
        D
        t
        sigma_y :
        
        H.1.4 Bending
        The characteristic bending moment value, Mc, 
        required to cause buckling when bending moments
        are acting alone, can be obtained using equations
        (H.6) and (H.7).
        """
        # (H.7)
        Mp = (D - t)**2 * t * self.sigma_y
        # (H.6)
        Mc = (1.0 - 0.0024 * (D.value/t.value)) * Mp
        # The characteristic bending strain, Epsilon_bc, at  
        # which buckling due to bending moments acting alone 
        # occurs, can be obtained using equation (H.8).
        # (H.8)
        epsilon_bc = 15.0 * (t.value/D.value)**2
        #
        if output:
            print ("")
            print ("H.1.4 Bending")
            print ("Epsilon bc = {:2.3f}".format(epsilon_bc))
            print ("Mb         = {:1.3e} KM-m"
                   .format(Mb.convert('kilonewton*metre').value))
            print ("Mc         = {:1.3e} KM-m"
                   .format(Mc.convert('kilonewton*metre').value))
            print ("URbm       = {:2.3f}".format(Mb.value/Mc.value))          
        #
        return Mc, epsilon_bc
    #
    def torsion(self, T,  D, t, output=False):
        """
        H.1.5 Torsion
        
        The characteristic value, Tauc, that causes buckling
        when torsion is acting alone, can be obtained using
        equations (H.9) to (H.13).
        """
        # (H.12)
        tau_y = self.sigma_y / math.sqrt(3.0)
        # (H.13)
        alpha_t = self.E.value / tau_y.value * (t.value/D.value)**(3.0/2.0)
        # (H.9)
        if alpha_t < 1.5:
            tau_c = (0.542 * alpha_t) * tau_y
        # (H.11)
        elif alpha_t > 9.0:
            tau_c = tau_y
        # (H.10)
        else:
            tau_c = (0.813 + 0.068 * math.sqrt(alpha_t - 1.50)) * tau_y
        #
        fvt = abs((T * D)/(2 * self.Ip))
        #
        if output:
            print("")
            print("H.1.5 Torsion")
            print("Tau y   = {: 1.3e} MPa".format(tau_y.convert('megapascal').value))
            print("Alpha t = {: 1.3e}".format(alpha_t))
            print("Tau c   = {: 1.3e} MPa".format(tau_c.convert('megapascal').value))
            print("Torsion = {: 1.3e} MPa".format(fvt.convert('megapascal').value))
            print("URt     = {: 1.3e}".format(fvt.value/tau_c.value))
        #
        return tau_c
    #
    def load_combination(self, Mb, Mc, Fx, Fxc, fo, P, Pc, output=False):
        """
        G.1.6 Load Combinations
        
        The maximum external overpressure, P, in the 
        presence of compressive axial force, Fx, and/or
        bending moment, M, when fo is less than 0.05 (5%),
        can be calculated using equation (G.14), where:
        - Gamma is calculated using equation (G.15);
        - Sigmahb is calculated using equation (G.16);
        - Sigmahcr is calculated using equation (G.17) 
          or equation (G.18) as appropriate.
        """
        #
        if fo <= 0.05:
            sigma_hE = self.E * (self.tnom.value / (self.Do.value - self.tnom.value))**2
            # (H.16)
            sigma_hb = P * self.Do.value / (2*self.tnom.value)
            # (H.17)
            if sigma_hE.value <= 2.0/3.0 * self.sigma_y.value :
                sigma_hcr = sigma_hE
            # (H.18)
            else:
                sigma_hcr = (self.sigma_y 
                             * (1 - (1.0/3.0 * (2*self.sigma_y.value 
                                                / (3*sigma_hE.value))**2)))
            # (H.15)
            gamma = (1 + 300.0 * self.tnom.value/self.Do.value
                     * sigma_hb.value/sigma_hcr.value)
            #
            try:
                URp = abs(P.value / Pc.value)
            except ZeroDivisionError:
                URp = 0
            
            if Fx:
                axial = abs(Fx.value / Fxc.value)
            else:
                axial = 0
            
            try:
                bending = abs(Mb.value / Mc.value)
            except ZeroDivisionError:
                bending = 0
            #
            URf = (bending + axial)**gamma
            UR_lc = URf + URp
        else:
            print ("fo > 0.05 (5%)")
            raise Warning("Section not applicable")
        #
        if output:
            print("")
            print("GH.1.6 Load Combination")
            print("Sigma hE  = {:1.3e} MPa".format(sigma_hE.convert('megapascal').value))
            print("Sigma hb  = {:1.3e} MPa".format(sigma_hb.convert('megapascal').value))
            print("Gamma     = {:1.3e}".format(gamma))
            print(f"URcomb [{URf:1.3f}] + [{URp:1.3f}] = {UR_lc:1.3f}")
        #
        return UR_lc
    # 
    def strain_criteria(self, P, Pc, epsilon_bc, output=False):
        """
        G.1.7 Strain criteria
        
        The bending strain, Epsilonb, required to cause
        buckling, in the presence of external overpressure,
        P, can be calculated using equation (G.19).
        """
        #
        # Values for Epsilonbc and Pc can be obtained from 
        # equations (G.8) and (G.1) respectively.        
        # (G.19)
        epsilon_b = (1 - (P/Pc))*epsilon_bc
        #
        if output:
            print("")
            print("G.1.7 Strain criteria")
            print("Epsilon b = {:2.3f}".format(epsilon_b))
        #
        return epsilon_b
    #     