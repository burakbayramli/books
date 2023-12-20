'''
Created on Mar 24, 2011

@author: ATIA1555
'''

import math as m

class Cone_Check:
    '''
    classdocs
    '''


    def __init__(self):
        '''
        Constructor
        '''
        self.dmin = 0.0
        self.dmax = 0.0
        self.h = 0.0
        self.tmin = 0.0
        self.tmax = 0.0
        self.tc = 0.0
        self.pmin = 0.0
        self.mmin = 0.0
        self.pmax = 0.0
        self.mmax = 0.0
        self.fy_c = 0.0
        self.fy_t_min = 0.0
        self.fy_t_max = 0.0
        self.E = 0.0
        self.g = 9.810
        self.rho_w = 1.025
        self.z = 0.0
        self.h = 0.0
        self.d = 0.0
        self.l = 0.0
        self.k = 0.0
        self.design_condition = 'O'
        self.period = 0.0
        self.hydro = 'OFF'
        self.units_mm = 'Y'
        self.stiff_max = 'N'
        self.stiff_min = 'N'
        #
        self.alpha = 0.0
        #
        # eq_dia:         Equivalent Diameter (dmax / cos(alpha)
        # eq_stress:    Equivalent Stress
        # sat:          is the axial stress in the tubular section at the junction due to global
        #               axial forces from factored actions (_min for start and _max for end)
        # sbt :         is the bending stress in the tubular section at the junction due to global
        #               bending moments from factored actions (_min for start and _max for end)
        # sbjt:         is the local bending stress at the tubular side of the junction
        # sbjc:         is the local bending stress at the cone side of the junction
        # sht :         is the hoop stress at the tubular side of the junction
        # shc :         is the hoop stress at the cone side of the junction
        # fyc :         is the local buckling strength of the cone
        # grc :         us the partial resistance factor for axial compression
        #
        self.eq_dia = 0.0
        self.eq_stress_min = 0.0
        self.eq_stress_max = 0.0
        self.sat_min = 0.0
        self.sbt_min = 0.0
        self.sat_max = 0.0
        self.sbt_max = 0.0
        self.sbjt_min = 0.0
        self.sbjt_max = 0.0
        self.sht_min = 0.0
        self.sht_max = 0.0
        self.shc_min = 0.0
        self.shc_max = 0.0
        self.fyc_min = 0.0
        self.fyc_max = 0.0
        self.sh_hydro = 0.0
        #
        # section 13.2.3.1, 13.2.2
        #
        self.grc = 1.18
        self.grt = 1.05
        self.grh = 1.25
        self.grb = 1.05
        #
        self.warning = ''
        self.error = ''
        #
        self.um_13_6_10_min = 0.0
        self.um_13_6_10_max = 0.0
        #
        self.u = 0.0
        self.stiff_max_status = ''
        self.stiff_min_status = ''
        #
        self.info_prepared = 'NO'
        #
        self.stf_ht_min = 0.0
        self.stf_thk_min = 0.0
        self.stf_L1_min = 0.0
        self.stf_Lc_min = 0.0
        self.stf_ht_max = 0.0
        self.stf_thk_max = 0.0
        self.stf_L1_max = 0.0
        self.stf_Lc_max = 0.0
        
    def reset(self):
        '''
        Constructor
        '''
        self.dmin = 0.0
        self.dmax = 0.0
        self.h = 0.0
        self.tmin = 0.0
        self.tmax = 0.0
        self.tc = 0.0
        self.pmin = 0.0
        self.mmin = 0.0
        self.pmax = 0.0
        self.mmax = 0.0
        self.fy_c = 0.0
        self.fy_t_min = 0.0
        self.fy_t_max = 0.0
        self.E = 0.0
        self.g = 9.810
        self.rho_w = 1.025
        self.z = 0.0
        self.wh = 0.0
        self.d = 0.0
        self.l = 0.0
        self.k = 0.0
        self.period = 0.0
        self.hydro = 'OFF'
        self.stiff_max = 'N'
        self.stiff_min = 'N'
        #
        self.alpha = 0.0
        #
        # eq_dia:         Equivalent Diameter (dmax / cos(alpha)
        # eq_stress:    Equivalent Stress
        # sat:          is the axial stress in the tubular section at the junction due to global
        #               axial forces from factored actions (_min for start and _max for end)
        # sbt :         is the bending stress in the tubular section at the junction due to global
        #               bending moments from factored actions (_min for start and _max for end)
        # sbjt:         is the local bending stress at the tubular side of the junction
        # sbjc:         is the local bending stress at the cone side of the junction
        # sht :         is the hoop stress at the tubular side of the junction
        # shc :         is the hoop stress at the cone side of the junction
        # fyc :         is the local buckling strength of the cone
        # grc :         us the partial resistance factor for axial compression
        #
        self.eq_dia = 0.0
        self.eq_stress_min = 0.0
        self.eq_stress_max = 0.0
        self.sat_min = 0.0
        self.sbt_min = 0.0
        self.sat_max = 0.0
        self.sbt_max = 0.0
        self.sbjt_min = 0.0
        self.sbjt_max = 0.0
        self.sht_min = 0.0
        self.sht_max = 0.0
        self.shc_min = 0.0
        self.shc_max = 0.0
        self.fyc_min = 0.0
        self.fyc_max = 0.0
        self.sh_hydro = 0.0
        #
        # section 13.2.3.1, 13.2.2
        #
        self.design_condition = 'O'
        self.grc = 1.18
        self.grt = 1.05
        self.grh = 1.25
        self.grb = 1.05
        #
        self.warning = ''
        self.error = ''
        #
        self.um_13_6_10_min = 0.0
        self.um_13_6_10_max = 0.0
        #
        self.u = 0.0
        self.stiff_max_status = ''
        self.stiff_min_status = ''
        #
        self.info_prepared = 'NO'
        #
        self.stf_ht_min = 0.0
        self.stf_thk_min = 0.0
        self.stf_L1_min = 0.0
        self.stf_Lc_min = 0.0
        self.stf_ht_max = 0.0
        self.stf_thk_max = 0.0
        self.stf_L1_max = 0.0
        self.stf_Lc_max = 0.0
        
    def prepare_info(self):
        #
        # Get cone slope from geometry
        #
        slope = (self.dmax - self.dmin)/ 2.0 / self.h
        self.alpha = m.atan(slope)
        #
        self.area_min = m.pi / 4.0 * (self.dmin**2 - (self.dmin - 2 * self.tmin)**2)
        self.area_max = m.pi / 4.0 * (self.dmax**2 - (self.dmax - 2 * self.tmax)**2)
        #
        self.inertia_min = m.pi / 64.0 * (self.dmin**4 - (self.dmin - 2 * self.tmin)**4)
        self.inertia_max = m.pi / 64.0 * (self.dmax**4 - (self.dmax - 2 * self.tmax)**4)
        #
        self.modulus_min = self.inertia_min / (self.dmin / 2.0)
        self.modulus_max = self.inertia_max / (self.dmax / 2.0)
        #
        self.sat_min = self.pmin / self.area_min
        self.sat_max = self.pmax / self.area_max
        #
        #
        if self.sat_min < 0.0:
            self.sbt_min = -1.0 * abs(self.sbt_min)
        if self.sat_max < 0.0:
            self.sbt_max = -1.0 * abs(self.sbt_max)
        ##
        self.sbt_min = self.mmin / self.modulus_min
        self.sbt_max = self.mmax / self.modulus_max
        #
        self.eq_dia = self.dmax / m.cos(self.alpha)
        #
        if self.design_condition == 'E':
            self.gfg1 = 1.10
        else:
            self.gfg1 = 1.30
        #
        if self.units_mm == 'Y':
            self.g = 9810.0
            self.rho_w = 1.025e-09
        else:
            self.g = 9.810
            self.rho_w = 1.025
        #
        self.info_prepared = 'YES'
        #
        
    def equ_stress(self):
        #
        # (13.6.2.1) Equivalent Stress
        #
        if self.info_prepared != 'YES': self.prepare_info()
        # print self.info_prepared
        #
        # x is the position from the smaller diameter
        # i.e. position of bigger diameter would be at x = h
        # sa : is the axial stress at section
        # sb : is the bending stress at section
        #
        #
        #
        # (13.6-2)
        #
        sa_min = self.pmin / (m.pi * self.tc * (self.dmin - self.tc * m.cos(self.alpha)))
        sa_max = self.pmax / (m.pi * self.tc * (self.dmax - self.tc * m.cos(self.alpha)))
        #
        # (13.6-3)
        sb_min = self.mmin * 4.0 / (m.pi * self.tc * (self.dmin - self.tc * m.cos(self.alpha))**2)
        sb_max = self.mmax * 4.0 / (m.pi * self.tc * (self.dmax - self.tc * m.cos(self.alpha))**2)
        #
        # (13.6-1)
        #
        self.sac_min = sa_min
        self.sac_max = sa_max
        self.sbc_min = sb_min
        self.sbc_max = sb_max
        #
        self.eq_stress_min = (sa_min + sb_min) / m.cos(self.alpha)
        self.eq_stress_max = (sa_max + sb_max) / m.cos(self.alpha)
        if self.hydro == 'ON':
            self.sh_hydro_t_min = self.hydro_pressure() * self.dmin / (2.0 * self.tmin)
            self.sh_hydro_c_min = self.hydro_pressure() * self.dmax / (2.0 * self.tc)
            print "Hydro is on"
            self.sh_hydro_c_max = self.hydro_pressure() * self.dmin / (2.0 * self.tc)
            self.sh_hydro_t_max = self.hydro_pressure() * self.dmax / (2.0 * self.tc)
            #print 'Hydro_t_min', self.sh_hydro_t_min
            #print 'Hydro_c_min', self.sh_hydro_c_min
            #print 'Hydro_c_max', self.sh_hydro_c_max
            #print 'Hydro_t_max', self.sh_hydro_t_max
        #
    
    
    def Bending_stress(self):
        #
        # (13.6.2.2.2) Bending stress
        #
        if self.info_prepared != 'YES': self.prepare_info()
        #
        # sbjt : is the local bending stress at the tubular side of the junction
        # sbjc : is the local bending stress at the cone side of the junction
        #
        # (13.6-4)
        #
        self.sbjt_min = 0.6 * self.tmin * m.sqrt(self.dmin * (self.tmin + self.tc)) / self.tmin**2  * \
                        (self.sat_min + self.sbt_min) * m.tan(self.alpha) 
        #
        self.sbjt_max = 0.6 * self.tmax * m.sqrt(self.dmax * (self.tmax + self.tc)) / self.tmax**2  * \
                        (self.sat_max + self.sbt_max) * m.tan(self.alpha) 
        #
        # (13.6-5)
        #
        self.sbjc_min = 0.6 * self.tmin * m.sqrt(self.dmin * (self.tmin + self.tc)) / self.tc**2  * \
                        (self.sat_min + self.sbt_min) * m.tan(self.alpha)
        #
        self.sbjc_max = 0.6 * self.tmax * m.sqrt(self.dmax * (self.tmax + self.tc)) / self.tc**2  * \
                        (self.sat_max + self.sbt_max) * m.tan(self.alpha)
        #
    
    def hoop_stresses(self):
        #
        # (13.6.2.2.3) Hoop Stresses
        #
        # (13.6-6)
        #
        self.sht_min = 0.45 * m.sqrt(self.dmin / self.tmin) * (self.sat_min + self.sbt_min) * m.tan(self.alpha)
        self.sht_max = 0.45 * m.sqrt(self.dmax / self.tmax) * (self.sat_max + self.sbt_max) * m.tan(self.alpha)
        #
        # (13.6-7)
        #
        if self.hydro == 'ON':
            self.shc_min = 0.45 * m.sqrt(self.eq_dia / self.tmin) * (self.tmin / self.tc) * \
                            (self.sat_min + self.sbt_min) * m.tan(self.alpha)
            self.shc_max = 0.45 * m.sqrt(self.eq_dia / self.tmax) * (self.tmax / self.tc) * \
                            (self.sat_max + self.sbt_max) * m.tan(self.alpha)
        else:
            self.shc_min = 0.45 * m.sqrt(self.dmin / self.tmin) * (self.tmin / self.tc) * \
                            (self.sat_min + self.sbt_min) * m.tan(self.alpha)
            self.shc_max = 0.45 * m.sqrt(self.dmax / self.tmax) * (self.tmax / self.tc) * \
                            (self.sat_max + self.sbt_max) * m.tan(self.alpha)
        #
        if self.hydro == 'ON':
            self.sht_min = self.sht_min - self.sh_hydro_t_min
            self.sht_max = self.sht_max - self.sh_hydro_t_max
            self.shc_min = self.shc_min - self.sh_hydro_c_min
            self.shc_max = self.shc_max - self.sh_hydro_c_max
            
        #
        if (self.sat_min + self.sbt_min) < 0.0:
            #
            self.sht_min = -1.0 * abs(self.sht_min)
            self.shc_min = -1.0 * abs(self.shc_min)
            #
        else:
            #
            self.sht_min = abs(self.sht_min)
            self.shc_min = abs(self.shc_min)
            #
            
        if (self.sat_max + self.sbt_max)< 0.0:
            #
            self.sht_max = abs(self.sht_max)
            self.shc_max = abs(self.shc_max)
            #
        else:
            #
            self.sht_max = -1.0 * abs(self.sht_max)
            self.shc_max = -1.0 * abs(self.shc_max)
            #
        
    
    def local_buckling_strength(self):
        #
        # (13.2.3.3) Local Buckling
        #
        #
        # The theoretical value of Cx for an ideal tubular is 0,6. However, a reduced value of Cx = 0,3 should be used in
        # Equation (13.2-10) to account for the effect of initial geometric imperfections within the tolerance limits given in
        # Clause 21. A reduced value of Cx = 0,3 is implicit in the value of fxe used in Equations (13.2-8) and (13.2-9).
        #
        cx = 0.3
        #
        # (13.2-10)
        #
        fxe_min = 2.0 * cx * self.E * self.tc / (self.dmin / m.cos(self.alpha))
        fxe_max = 2.0 * cx * self.E * self.tc / (self.dmax / m.cos(self.alpha))
        #
        if self.fy_t_min / fxe_min <= 0.170:
            #
            # (13.2-8)
            #
            fyc_min = self.fy_t_min
            #
        else:
            #
            # (13.2-9)
            #
            fyc_min = (1.047 - 0.274 * self.fy_t_min / fxe_min) * self.fy_t_min
            #
        #
        #
        if self.fy_t_max / fxe_max <= 0.170:
            #
            # (13.2-8)
            #
            fyc_max = self.fy_t_max
            #
        else:
            #
            # (13.2-9)
            #
            fyc_max = (1.047 - 0.274 * self.fy_t_max / fxe_max) * self.fy_t_max
            #
        #
        self.fyc_min = fyc_min
        self.fyc_max = fyc_max
        
        
    def local_buckling_conical_transition(self):
        #
        # (13.6.3.2)
        #
        self.um_13_6_10_min = abs(self.eq_stress_min) * self.grc / self.fyc_min
        self.um_13_6_10_max = abs(self.eq_stress_max) * self.grc / self.fyc_max
        #
        
    def junction_yielding(self):
        #
        # junction yielding is only applied where the hoop stresses are tensile
        #
        if self.sht_min > 0.0:
            #
            st_maximum = self.sat_min + self.sbt_min + self.sbjt_min
            #
            if st_maximum > 0.0:
                self.um_13_6_13_min_t = self.grt / self.fy_t_min * \
                                        m.sqrt(st_maximum**2 + self.sht_min**2 - self.sht_min * st_maximum)
            else:
                self.um_13_6_13_min_t = self.grt / self.fy_t_min * \
                                        m.sqrt(st_maximum**2 + self.sht_min**2 + self.sht_min * abs(st_maximum))
            #
        else:
            #
            self.um_13_6_13_min_t = 0.0
            #
            #
            
        if self.shc_min > 0.0:
            #
            sc_maximum = (self.sac_min + self.sbc_min) / m.cos(self.alpha) + self.sbjc_min
            #
            if sc_maximum > 0.0:
                self.um_13_6_13_min_c = self.grt / self.fy_c * \
                                        m.sqrt(sc_maximum**2 + self.shc_min**2 - self.shc_min * sc_maximum)
            else:
                self.um_13_6_13_min_c = self.grt / self.fy_c * \
                                        m.sqrt(sc_maximum**2 + self.shc_min**2 + self.shc_min * abs(sc_maximum))
            #
        else:
            #
            self.um_13_6_13_min_c = 0.0
            #
        
        if self.sht_max > 0.0:
            #
            st_maximum = self.sat_max + self.sbt_max + self.sbjt_max
            #
            if st_maximum > 0.0:
                self.um_13_6_13_max_t = self.grt / self.fy_t_max * \
                                        m.sqrt(st_maximum**2 + self.sht_max**2 - self.sht_max * st_maximum)
            else:
                self.um_13_6_13_max_t = self.grt / self.fy_t_max * \
                                        m.sqrt(st_maximum**2 + self.sht_max**2 + self.sht_max * abs(st_maximum))
            #
        else:
            #
            self.um_13_6_13_max_t = 0.0
            #
            #
            
        if self.shc_max > 0.0:
            #
            sc_maximum = (self.sac_max + self.sbc_max) / m.cos(self.alpha) + self.sbjc_max
            #
            if sc_maximum > 0.0:
                self.um_13_6_13_max_c = self.grt / self.fy_c * \
                                        m.sqrt(sc_maximum**2 + self.shc_max**2 - self.shc_max * sc_maximum)
            else:
                self.um_13_6_13_max_c = self.grt / self.fy_c * \
                                        m.sqrt(sc_maximum**2 + self.shc_max**2 + self.shc_max * abs(sc_maximum))
            #
        else:
            #
            self.um_13_6_13_max_c = 0.0
            #
            
            
            
    def fh(self,t,d,fy):
        #
        # section 13.2.6.2 Hoop Buckling
        #
        fhe = 0.4 * self.E * t / d
        #
        if fhe > 2.44 * fy:
            #
            fh_r = fy
            #
            
        elif fhe > 0.55 * fy and fhe <= 2.44 * fy:
            #
            fh_r = 0.7 * (fhe / fy)**0.4 * fy
            #
            if fh_r > fy: fh_r = fy
            #
            
        else:
            #
            fh_r = fhe
            #
        
        return fh_r
    
    
    
            
    def junction_buckling(self):
        #
        # junction buckling is only applied where the hoop stresses are compressive
        #
        if self.sht_min < 0.0:
            #
            st_maximum = self.sat_min + self.sbt_min + self.sbjt_min
            #
            fh_v = self.fh(self.tmin,self.dmin,self.fy_t_min)
            #
            if st_maximum > 0.0:
                #
                A = self.grt * st_maximum / self.fy_t_min
                B = self.grh * abs(self.sht_min) / fh_v
                eta = 5.0 - 4.0 * fh_v / self.fy_t_min
                v = 0.3
                self.um_13_6_18_t_min = A**2 + B**(2.0*eta) + 2*v*A*B
                #
                self.um_13_6_21_t_min = 0.0
                self.um_13_6_22_t_min = 0.0
            else:
                self.um_13_6_18_t_min = 0.0
                #
                self.um_13_6_21_t_min = abs(st_maximum) / (self.fy_t_min / self.grc)
                self.um_13_6_22_t_min = abs(self.sht_min) / (fh_v / self.grh)
                #
        
        else:
            self.um_13_6_18_t_min = 0.0
            self.um_13_6_21_t_min = 0.0
            self.um_13_6_22_t_min = 0.0
        #
        #
        #
        
        if self.shc_min < 0.0:
            #
            sc_maximum = (self.sac_min + self.sbc_min) / m.cos(self.alpha) + self.sbjc_min
            #
            fh_v = self.fh(self.tc,self.dmin,self.fy_c)
            #
            if sc_maximum > 0.0:
                #
                A = self.grt * sc_maximum / self.fy_c
                B = self.grh * abs(self.shc_min) / fh_v
                eta = 5.0 - 4.0 * fh_v / self.fy_c
                v = 0.3
                self.um_13_6_18_c_min = A**2 + B**(2.0*eta) + 2*v*A*B
                #
                self.um_13_6_21_c_min = 0.0
                self.um_13_6_22_c_min = 0.0
            else:
                self.um_13_6_18_c_min = 0.0
                #
                self.um_13_6_21_c_min = abs(sc_maximum) / (self.fyc_min / self.grc)
                self.um_13_6_22_c_min = abs(self.shc_min) / (fh_v / self.grh)
                #
        
        else:
            self.um_13_6_18_c_min = 0.0
            self.um_13_6_21_c_min = 0.0
            self.um_13_6_22_c_min = 0.0
        
        #
        #
        #
        if self.sht_max < 0.0:
            #
            st_maximum = self.sat_max + self.sbt_max + self.sbjt_max
            #
            fh_v = self.fh(self.tmax,self.dmax,self.fy_t_max)
            #
            if st_maximum > 0.0:
                #
                A = self.grt * st_maximum / self.fy_t_max
                B = self.grh * abs(self.sht_max) / fh_v
                eta = 5.0 - 4.0 * fh_v / self.fy_t_max
                v = 0.3
                self.um_13_6_18_t_max = A**2 + B**(2.0*eta) + 2*v*A*B
                #
                self.um_13_6_21_t_max = 0.0
                self.um_13_6_22_t_max = 0.0
            else:
                self.um_13_6_18_t_max = 0.0
                #
                self.um_13_6_21_t_max = abs(st_maximum) / (self.fy_t_max / self.grc)
                self.um_13_6_22_t_max = abs(self.sht_max) / (fh_v / self.grh)
                #
        
        else:
            self.um_13_6_18_t_max = 0.0
            self.um_13_6_21_t_max = 0.0
            self.um_13_6_22_t_max = 0.0
        #
        #
        #
        
        if self.shc_max < 0.0:
            #
            sc_maximum = (self.sac_max + self.sbc_max) / m.cos(self.alpha) + self.sbjc_max
            #
            fh_v = self.fh(self.tc,self.dmax,self.fy_c)
            #
            if sc_maximum > 0.0:
                #
                A = self.grt * sc_maximum / self.fy_c
                B = self.grh * abs(self.shc_max) / fh_v
                eta = 5.0 - 4.0 * fh_v / self.fy_c
                v = 0.3
                self.um_13_6_18_c_max = A**2 + B**(2.0*eta) + 2*v*A*B
                #
                self.um_13_6_21_c_max = 0.0
                self.um_13_6_22_c_max = 0.0
            else:
                self.um_13_6_18_c_max = 0.0
                #
                self.um_13_6_21_c_max = abs(sc_maximum) / (self.fy_c / self.grc)
                self.um_13_6_22_c_max = abs(self.shc_max) / (fh_v / self.grh)
                #
        
        else:
            self.um_13_6_18_c_max = 0.0
            self.um_13_6_21_c_max = 0.0
            self.um_13_6_22_c_max = 0.0
            
    def hydro_pressure(self):
        #
        # Section 13.2.6.1 Calculation of hydrostatic pressure
        #
        if self.units_mm == 'Y':
            wave_length = 1.56 * self.period**2 * 1000.0
        else:
            wave_length = 1.56 * self.period**2
        #
        k = 2 * m.pi / wave_length
        #
        self.hz = -1.0 * self.z + self.wh / 2.0 * (m.cosh(k*(self.d + self.z))/m.cosh(k * self.d))
        #print 'factor', (m.cosh(k*(self.d + self.z))/m.cosh(k * self.d))
        #
        h_p = self.gfg1 * self.rho_w * self.g * self.hz
        #
        return h_p
    
    
    
    def hoop_buckling_hydro(self):
        #
        # Section 13.6.4.1 Hoop Buckling (external hydrostatic pressure)
        #
        #
        fh = self.fh(self.tc, self.eq_dia, self.fy_c)
        #
        self.um_13_2_31 = self.sh_hydro * self.grh / fh
        #
        B = min(self.sh_hydro * self.grh / fh, 1.0)
        #
        eta = 5.0 - 4.0 * fh / self.fy_c
        #
        # Section 13.4.2 Axial tension, bending and hydrostatic pressure
        #
        d2 = self.eq_dia
        d1 = self.eq_dia - 2 * self.tc
        #
        zp = (d2**3-d1**3) / 6.0
        ze = (m.pi * (d2**4-d1**2))/ 32.0 / d2
        #
        #
        ratio = self.fyc_max *  self.eq_dia / self.E / self.tc
        #
        if ratio <= 0.0517:
            #
            fb = (zp / ze) * self.fy_c
            #
        elif ratio > 0.0517 and ratio <=0.1034:
            #
            fb = (1.13 - 2.58 * ratio) * (zp / ze) * self.fy_c
            #
        else:
            #
            fb = (0.94 - 0.76 * ratio) * (zp / ze) * self.fy_c
        #
        # 13.4-9
        fbh = fb * (m.sqrt(1.0 + 0.09 * B**2 - B**(2*eta))-0.3 * B)
        #
        if self.sac_max > 0.0:
            #
            # 13.4 - 8
            fth = self.fy_c * (m.sqrt(1.0 + 0.09 * B**2 - B**(2*eta))-0.3 * B)
            #
            # 13.4-12
            self.um_13_4_12_max = self.grt * self.sac_max / fth + self.grb * self.sbc_max / fbh
            self.um_13_4_12_min = self.grt * self.sac_min / fth + self.grb * self.sbc_min / fbh
            #
        else:
            I = m.pi / 64.0 * (d2**4 - d1**4)
            A = m.pi / 4.0 * (d2**2 - d1**2)
            r = m.sqrt(I / A)
            #
            lamb_max = self.k * self.l / m.pi / r * m.sqrt(self.fyc_max/self.E)
            lamb_min = self.k * self.l / m.pi / r * m.sqrt(self.fyc_min/self.E)
            #
            fq = 0.5 * fh
            #
            if lamb_max <= 1.34 * m.sqrt((1.0 - 2.0 * fq / self.fyc_max)**-1.0):
                #
                fch_max = 0.5 * self.fyc_max * ((1.0 - 0.278 * lamb_max**2) - 2.0 * fq / self.fyc_max + \
                                                m.sqrt((1.0 - 0.278 * lamb_max**2)**2 + 1.12 * lamb_max**2 * fq / self.fyc_max))
                #
            else:
                #
                fch_max = 0.9 / lamb_max**2 * self.fyc_max
                #
            #
            if lamb_min <= 1.34 * m.sqrt((1.0 - 2.0 * fq / self.fyc_min)**-1.0):
                #
                fch_min = 0.5 * self.fyc_min * ((1.0 - 0.278 * lamb_min**2) - 2.0 * fq / self.fyc_min + \
                                                m.sqrt((1.0 - 0.278 * lamb_min**2)**2 + 1.12 * lamb_min**2 * fq / self.fyc_min))
                #
            else:
                #
                fch_min = 0.9 / lamb_min**2 * self.fyc_min
                #
            #
            fhe = 0.4 * self.E * self.tc / self.eq_dia
            fxe = 2.0 * 0.3 * self.E * self.tc / self.eq_dia
            #
            if self.eq_stress_max > 0.5 * fhe / self.grh and fxe / self.grc > 0.5 * fhe / self.grh:
                #
                self.um_13_4_21_max = (self.eq_stress_max - 0.5 * fhe / self.grh) / \
                                      (fxe / self.grc - 0.5 * fhe / self.grh) + \
                                       (self.grh * fh  / fhe)**2
                #
                self.um_13_4_21_min = (self.eq_stress_min - 0.5 * fhe / self.grh) / \
                                      (fxe / self.grc - 0.5 * fhe / self.grh) + \
                                       (self.grh * fh  / fhe)**2
                #
            #
            self.um_13_4_19_max = self.grc * self.sac_max / self.fyc_max + self.grb * self.sbc_max / fbh
            self.um_13_4_19_min = self.grc * self.sac_min / self.fyc_min + self.grb * self.sbc_min / fbh
            #
            fe = m.pi**2 * self.E / (self.k * self.l / r)**2
            #
            self.um_13_40_20_max = self.grc * self.sac_max / fch_max + self.grb / fbh * (0.85 * self.sbc_max / (1 - self.sac_max / fe))**2
            self.um_13_40_20_min = self.grc * self.sac_min / fch_min + self.grb / fbh * (0.85 * self.sbc_min / (1 - self.sac_max / fe))**2
            #
#################################################      
    def fhe_stif(self,stf_L1,d,t,E):
        mu = stf_L1 / d * (2*d/t)**0.5
        #
        if mu > 1.6 * d / t:
            #
            ch = 0.44 * t/d
            #
            
        elif mu >= 0.825 * d / t and mu < 1.6 * d / t:
            #
            ch = 0.44 * t/d + 0.21 * (d / t)**3 * mu**4
            #
        elif mu >= 1.5 and mu < 0.825 * d / t:
            #
            ch = 0.737 / (mu - 0.579)
            # 
        else:
            #
            ch = 0.8
            #
        
        fhe = 2 * ch * E * t / d
        return fhe
      
    def ring(self,d,t,tc,stf_ht,stf_thk,stf_L1,stf_Lc,fy_t,alpha,sat,sac,sbt,sbc,E): 
        fhe = self.fhe_stif(stf_L1, d, t, self.E)
        fhec = self.fhe_stif(stf_L1, d/m.cos(alpha), t, self.E)
        be = 0.55 * (m.sqrt(d * t) + m.sqrt(d * tc))
        stf_area = stf_ht * stf_thk + be * min (t,tc)
        #print 'area  ' + str(stf_area) 
        xbar = (be * min (t, tc)**2 + stf_thk * stf_ht * (2 * min (t, tc) + stf_ht))/ \
                     (2 * (min (t, tc) * be + stf_thk * stf_ht))            
        stf_MI = be/3* ( stf_ht + min (t, tc))** 3 - stf_ht** 3 / 3 *  (be - stf_thk) \
                    - stf_area * ( stf_ht + min (t, tc) - xbar) ** 2  
        #print 'Moment of inertia  ' + str(stf_MI)             
        Ac = t*d/ fy_t *m.tan(alpha)*(max(abs(sat),abs(sac)) + max(abs(sbt),abs(sbc)))
        #print 'required area  ' + str(Ac)  
        Ic = t*d * d**2 / 8 / E *m.tan(alpha)*(max(abs(sat),abs(sac)) + max(abs(sbt),abs(sbc))) 
        #print 'required IC  ' + str(Ic)  
        Ich = (d**2/16.0/ E)*(t*stf_L1* fhe +t*stf_Lc* fhec /(m.cos(alpha)**2))
        #print 'required ICH  ' + str(Ich)  
        IcT = Ic + Ich
        if self.hydro == "ON":
            if stf_area > Ac and stf_MI > IcT:
                R = "PASS"
            else:
                R = "FAIL"
        else:
            if stf_area > Ac and stf_MI > Ic:
                R = "PASS"
            else:
                R = "FAIL"
        return R
        

    def ring_min(self):
        return self.ring (self.dmin,self.tmin,self.tc,self.stf_ht_min,self.stf_thk_min,self.stf_L1_min,self.stf_Lc_min,self.fy_t_min,self.alpha,self.sat_min,self.sac_min,self.sbt_min,self.sbc_min,self.E)

    def ring_max(self):
        return self.ring (self.dmax,self.tmax,self.tc,self.stf_ht_max,self.stf_thk_max,self.stf_L1_max,self.stf_Lc_max,self.fy_t_max,self.alpha,self.sat_max,self.sac_max,self.sbt_max,self.sbc_max,self.E)

                   
    def max_u(self):
        #
        # find maximum utilization
        #
        self.prepare_info()
        self.equ_stress()
        self.Bending_stress()
        self.hoop_stresses()
        self.local_buckling_strength()
        self.local_buckling_conical_transition()
        #if self.hydro == 'ON':
        #    self.hoop_buckling_hydro()
        self.junction_yielding()
        self.junction_buckling()
        #print 'Min Diameter'
        if self.stiff_min != 'N': self.stiff_min_status = self.ring_min()
        #self.ring_chk_min = self.ring (self.dmin,self.tmin,self.tc,self.stf_ht_min,self.stf_thk_min,self.stf_L1_min,self.stf_Lc_min,self.fy_t_min,self.alpha,self.sat_min,self.sac_min,self.sbt_min,self.sbc_min,self.E)
        #print 'Max Diameter'
        if self.stiff_max != 'N': self.stiff_max_status = self.ring_max()
        #self.ring_chk_max = self.ring (self.dmax,self.tmax,self.tc,self.stf_ht_max,self.stf_thk_max,self.stf_L1_max,self.stf_Lc_max,self.fy_t_max,self.alpha,self.sat_max,self.sac_max,self.sbt_max,self.sbc_max,self.E)
        #
        # Create array
        um = []
        um.append([round(self.um_13_6_10_max,4),'um_13_6_10_max'])
        um.append([round(self.um_13_6_10_min,4),'um_13_6_10_min'])
        um.append([round(self.um_13_6_13_max_c,4),'um_13_6_13_max_c'])
        um.append([round(self.um_13_6_13_max_t,4),'um_13_6_13_max_t'])
        um.append([round(self.um_13_6_13_min_c,4),'um_13_6_13_min_c'])
        um.append([round(self.um_13_6_13_min_t,4),'um_13_6_13_min_t'])
        um.append([round(self.um_13_6_18_c_max,4),'um_13_6_18_c_max'])
        um.append([round(self.um_13_6_18_c_min,4),'um_13_6_18_c_min'])
        um.append([round(self.um_13_6_18_t_max,4),'um_13_6_18_t_max'])
        um.append([round(self.um_13_6_18_t_min,4),'um_13_6_18_t_min'])
        um.append([round(self.um_13_6_21_c_max,4),'um_13_6_21_c_max'])
        um.append([round(self.um_13_6_21_c_min,4),'um_13_6_21_c_min'])
        um.append([round(self.um_13_6_21_t_max,4),'um_13_6_21_t_max'])
        um.append([round(self.um_13_6_21_t_min,4),'um_13_6_21_t_min'])
        um.append([round(self.um_13_6_22_c_max,4),'um_13_6_22_c_max'])
        um.append([round(self.um_13_6_22_c_min,4),'um_13_6_22_c_min'])
        um.append([round(self.um_13_6_22_c_max,4),'um_13_6_22_c_max'])
        um.append([round(self.um_13_6_22_t_max,4),'um_13_6_22_t_min'])
        
        try:
            um.append([round(self.um_13_2_31,4),'um_13_2_31'])
            um.append([round(self.um_13_4_12_max,4),'um_13_4_12_max'])
            um.append([round(self.um_13_4_12_min,4),'um_13_4_12_min'])
        except:
            pass
        
        try:
            um.append([round(self.um_13_4_21_max,4),'um_13_4_21_max'])
            um.append([round(self.um_13_4_21_min,4),'um_13_4_21_min'])
            um.append([round(self.um_13_4_19_max,4),'um_13_4_19_max'])
            um.append([round(self.um_13_4_19_min,4),'um_13_4_19_min'])
            um.append([round(self.um_13_40_20_max,4),'um_13_40_20_max'])
            um.append([round(self.um_13_40_20_min,4),'um_13_40_20_min'])
        except:
            pass
        
        
        #
        u = max(self.um_13_6_10_max, self.um_13_6_10_min, self.um_13_6_13_max_c, self.um_13_6_13_max_t,\
                  self.um_13_6_13_min_c, self.um_13_6_13_min_t, self.um_13_6_18_c_max,self.um_13_6_18_c_min,\
                  self.um_13_6_18_t_max, self.um_13_6_18_t_min, self.um_13_6_21_c_max, self.um_13_6_21_c_min,\
                  self.um_13_6_21_t_max, self.um_13_6_21_t_min, self.um_13_6_22_c_max, self.um_13_6_22_c_min,\
                  self.um_13_6_22_t_max, self.um_13_6_22_t_min)
        self.u = u
        return u, um
        
    def find_utilization(self):
        u1 = self.max_u()
        u1_stiff_min = self.stiff_min_status
        u1_stiff_max = self.stiff_max_status
        u1, self.um1 = self.max_u()
        self.mmin = -1.0 * self.mmin
        self.mmax = -1.0 * self.mmax
        u2, self.um2 = self.max_u()
        u2_stiff_min = self.stiff_min_status
        u2_stiff_max = self.stiff_max_status
        self.u = max(u1,u2)
        
        if u1_stiff_min == 'FAIL' or u2_stiff_min == 'FAIL': 
            self.stiff_min_status = 'FAIL'
        elif u1_stiff_min == 'PASS' and u2_stiff_min == 'PASS':
            self.stiff_min_status = 'PASS'
        else:
            pass
        
        if u1_stiff_max == 'FAIL' or u2_stiff_max == 'FAIL': 
            self.stiff_max_status = 'FAIL'
        elif u1_stiff_max == 'PASS' and u2_stiff_max == 'PASS':
            self.stiff_max_status = 'PASS'
        else:
            pass
            