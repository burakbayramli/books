# 
# Copyright (c) 2019 iLift
#
# Python stdlib imports

# package imports
from iLift.codes.process.process import SummaryResults
#

#-------------------------------------------------
#
#
def Chapter_H(self, section, material):
    #        
    print (" ")        
    print ("-----------------------------")
    print (" Chapter H - Combined Forces")
    print (" ")
    #
    #
    if 'bar' in section.type:
        # H2 UNSYMMETRIC AND OTHER MEMBERS SUBJECT TO FLEXURE
        # AND AXIAL FORCE
        #
        #if "compression" in self.FAxial.lower() :
        #    self.Pc = self.Pn_E * self.Phi_c / self.Omega_c
        #else:
        #    self.Pc = self.Pn_D * self.Phi_t / self.Omega_t
        #
        #self.Mcx = self.Mnx * self.Phi_b / self.Omega_b
        #self.Mcy = self.Mny * self.Phi_b / self.Omega_b
        
        _UR = (abs(self.P) / self.Pc
               + abs(self.Mx) / self.Mcx
               + abs(self.My) / self.Mcy)
        _UR_flag = '(H2-1)'
    else:
        # Flange Moment of Inertia            
        _Iyc = (section.tft * section.bft**3) / 12.0 
        _Icy_Iy = _Iyc / section.Iz
        #
        # H1 Doubly and singly symmetric members subject
        #    to flexure and axial force
        if ("symmetric" in section.type.lower() 
            and _Icy_Iy >= 0.1 
            and _Icy_Iy <= 0.9):
            #self.Mcx = self.Mnx * self.Phi_b / self.Omega_b
            #self.Mcy = self.Mny * self.Phi_b / self.Omega_b
            #
            # H1-1 Doubly and Syngly Symmetric Members
            #      in Flexure and Compression
            #if "compression" in self.FAxial.lower() :
            #    self.Pc = self.Pn_E * self.Phi_c / self.Omega_c
            # H1-2 Doubly and Syngly Symmetric Members
            #      n Flexure and Tension
            #else:
            #    self.Pc = self.Pn_D * self.Phi_t / self.Omega_t
            #
            # (H1-1b)
            print(self.Pc)
            if abs(self.actions.Fx) / self.Pc < 0.2:
                _UR = (abs(self.actions.Fx) / (2 * self.Pc) 
                       + abs(self.actions.Mx) / self.Mcx
                       + abs(self.actions.My) / self.Mcy)
                _UR_flag = '(H1-1b)'
            # (H1-1a)
            else:
                _UR = (abs(self.actions.Fx) / self.Pc 
                       + (8.0 / 9.0) * (abs(self.actions.Mx) / self.Mcx 
                                        + abs(self.actions.My) / self.Mcy))
                _UR_flag = '(H1-1a)'
        else:
            print('fix this unsymmetrical')
            1/0
        #
        # 3. Non-HSS Members Subject to Torsion and Combined Stress
        #print('Fcr =', self.Fcr_E)
        #Fnt = min(material.Fy, self.Fcr_E)
    #
    _URStatus = 'PASS'
    if _UR > 1.0:
        _URStatus = 'FAIL'

    self.ChapterH_results = SummaryResults(_UR, _URStatus, _UR_flag)
    #
    #self.UR = _UR
    #self.UR_flag = _UR_flag
    #        
    #if self.UR > 1.0:
    #    self.URStatus = 'FAIL'
    #else:
    #    self.URStatus = 'PASS'
    #
    #print ("UR = {:2.3f}  {:}".format(self.UR, self.UR_flag))
#
#
def Chapter_H_stress(self, section):
    """
    """
    #print("")        
    #print("-----------------------------")
    #print(" Chapter H - Combined Forces")
    #print("")
    
    fa = max([abs(_item.value) for _item in self.stress.sigma_x])
    fbx = max([abs(_item.value) for _item in self.stress.sigma_y])
    fby = max([abs(_item.value) for _item in self.stress.sigma_z])   
    
    if 'bar' in section.type:
        # H2 UNSYMMETRIC AND OTHER MEMBERS SUBJECT TO FLEXURE
        # AND AXIAL FORCE
        _UR = fa / self.Fa.value + fbx / self.Fbx.value + fby / self.Fby.value
        _UR_flag = '(H2-1)'
    else:
        # Flange Moment of Inertia            
        # H1 Doubly and singly symmetric members subject
        # to flexure and axial force
        if "symmetric" in section.type.lower():
            # (H1-1b)
            if fa / self.Fa < 0.2:
                _UR = fa / (2 * self.Fa.value)  + fbx / self.Fbx.value + fby / self.Fby.value
                _UR_flag = '(H1-1b)'
            # (H1-1a)
            else:
                _UR = fa / self.Fa.value  + 8/9.0 * (fbx / self.Fbx.value + fby / self.Fby.value)
                _UR_flag = '(H1-1a)'
        else:
            raise ValueError('fix this unsymmetrical')
        #
        # 3. Non-HSS Members Subject to Torsion and Combined Stress
        #print('Fcr =', self.Fcr_E)
        #Fnt = min(material.Fy, self.Fcr_E)
    #
    #
    #self.UR = _UR
    #self.UR_flag = _UR_flag
    #
    _URStatus = 'PASS'
    if _UR > 1.0:
        _URStatus = 'FAIL'
    #else:
    #    self.URStatus = 'PASS'
    #
    self.ChapterH_results = SummaryResults(_UR, _URStatus, _UR_flag)
    #print ("UR = {:2.3f}  {:}".format(self.UR, self.UR_flag))
#