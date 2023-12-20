# Copyright (c) 2015-2023 steelpy

# Python stdlib imports

# package imports


#
#-------------------------------------------------
#                   ASME Section
#-------------------------------------------------
#
class ASME:
    #
    def __init__(self):
        pass
    #
    def bend_data(self, pipe_description, 
                  flanges=0, T_= 0, r2=0, R1S=0, theta=0, rx=0):
        """
        """
        self.pipe_description = pipe_description.lower()
        self.flanges = flanges
        self.T_ = T_
        self.r2 = r2
        self.rx = rx
        
        if 'pipe-bend' in self.pipe_description:
            self.R1 = R1S
        #
        else:
            self.S = R1S
            self.theta = theta
            #
        self.SIFs_type = 'bend'
    #
    # ASME B31.3
    def AppendixD(self):
        #
        #
        # Bend Flexibility
        if 'bend' in self.SIFs_type.lower():
            #
            # Welding elbow or pipe bend
            if 'pipe-bend' in self.pipe_description.lower():
                #
                # Flexibility Characteristic
                self.h = (self.T_ * self.R1)/self.r2**2
                #
                # Flexibility Factor
                self.k = 1.65/self.h
                #
                # Stress Intensification
                #
                # Out-of-Plane
                self.lo = 0.75/self.h**(2.0/3.0)
                #
                # In-Plane
                self.li = 0.90/self.h**(2.0/3.0)
                #
                #
            #
            # Miter Bend
            else:
                #
                # Closely spaced 
                # Miter bend
                if self.S < self.r2*(1 + math.tan(self.Theta)):
                    #
                    # Flexibility Characteristic
                    self.h = (0.50*(math.atan(self.Theta)) *
                              ((self.T_ * self.S)/self.r2**2))
                    #
                    # Flexibility Factor
                    self.k = 1.52/self.h**(5.0/6.0)
                    #
                    # Stress Intensification
                    #
                    # Out-of-Plane
                    self.lo = 0.90/self.h**(2.0/3.0)
                    #
                    # In-Plane
                    self.li = 0.90/self.h**(2.0/3.0)
                    #
                # 
                # Single miter bend or widely spaced
                # Miter bend
                else:
                    #
                    # Flexibility Characteristic
                    self.h = (0.50*(1.0 + math.atan(self.Theta)) *
                              (self.T_/self.r2))
                    #
                    # Flexibility Factor
                    self.k = 1.52/self.h**(5.0/6.0)
                    #
                    # Stress Intensification
                    #
                    # Out-of-Plane
                    self.lo = 0.90/self.h**(2.0/3.0)
                    #
                    # In-Plane
                    self.li = 0.90/self.h**(2.0/3.0)
                    #
                #
                #
        #
        # Tee Flexibility
        elif 'tee' in self.SIFs_type.lower():
            #
            # Welding tee per ASME B16.9
            if 'welding-tee' in self.pipe_description.lower():
                #
                # Flexibility Characteristic
                self.h = (3.10*(self.T_/self.r2))
                #
                # Flexibility Factor
                self.k = 1.00
                #
                # Stress Intensification
                #
                # Out-of-Plane
                self.lo = 0.90/self.h**(2.0/3.0)
                #
                # In-Plane
                self.li = ((3.0/4.0)*self.lo + (1.0/4.0))
                #
            #
            # Reinforced fabricated tee
            # with pad or saddle.
            elif 'reinforced' in self.pipe_description.lower():
                #
                # Flexibility Characteristic
                # (8)
                if self.Tr > 1.50*self.T_:
                    self.h = 4.0*self.T_ / self.r2
                #
                else:
                    self.h = ((self.T_ + 0.50*self.Tr)**2.50 / 
                              (self.T_**1.5/self.r2))
                #
                # Flexibility Factor
                self.k = 1.00
                #
                # Stress Intensification
                #
                # Out-of-Plane
                self.lo = 0.90/self.h**(2.0/3.0)
                #
                # In-Plane
                self.li = ((3.0/4.0)*self.lo + (1.0/4.0))
                #
            #
            # Unreinforced fabricated tee
            elif 'unreinforced' in self.pipe_description.lower():
                #
                # Flexibility Characteristic
                self.h = (self.T_/self.r2)
                #
                # Flexibility Factor
                self.k = 1.00
                #
                # Stress Intensification
                #
                # Out-of-Plane
                self.lo = 0.90/self.h**(2.0/3.0)
                #
                # In-Plane
                self.li = ((3.0/4.0)*self.lo + (1.0/4.0))
                #
            #
            # Extruded welding tee
            elif 'extruded' in self.pipe_description.lower():
                #
                # Flexibility Characteristic
                self.h = (1.0 + self.rx/self.r2)*(self.T_/self.r2)
                #
                # Flexibility Factor
                self.k = 1.00
                #
                # Stress Intensification
                #
                # Out-of-Plane
                self.lo = 0.90/self.h**(2.0/3.0)
                #
                # In-Plane
                self.li = ((3.0/4.0)*self.lo + (1.0/4.0))
                #
            #
            # Welding-in contour insert
            elif 'welded-contour' in self.pipe_description.lower():
                #
                # Flexibility Characteristic
                self.h = (3.10*(self.T_/self.r2))
                #
                # Flexibility Factor
                self.k = 1.00
                #
                # Stress Intensification
                #
                # Out-of-Plane
                self.lo = 0.90/self.h**(2.0/3.0)
                #
                # In-Plane
                self.li = ((3.0/4.0)*self.lo + (1.0/4.0))
                #
            #
            # Branch Welded-on fitting
            # (integrally reinforced)
            elif 'welded-branch' in self.pipe_description.lower():
                #
                # Flexibility Characteristic
                self.h = (3.30*(self.T_/self.r2))
                #
                # Flexibility Factor
                self.k = 1.00
                #
                # Stress Intensification
                #
                # Out-of-Plane
                self.lo = 0.90/self.h**(2.0/3.0)
                #
                # In-Plane
                self.li = 0.90/self.h**(2.0/3.0)
                #
            #
            #
            else:
                print ('N/A')
                #
        #
        # Unknow
        else: 
            print(self.SIFs_type, "No Implemented yet")
        #
        #
        # 1 end flanged
        if self.flanges == 1:
            self.C1 = self.h**(1.0/6.0)
        #
        # 2 ends flanged
        elif self.flanges == 2:
            self.C1 = self.h**(1.0/6.0)
        #
        # No flanges
        else:
            self.C1 = 1.0
        #
        #
        #
        self.k = self.k * self.C1
        if self.k < 1.0: self.k = 1.0
        #
        self.lo = self.lo * self.C1
        if self.lo < 1.0: self.lo = 1.0
        #
        self.li = self.li * self.C1
        if self.li < 1.0: self.li = 1.0
        #
        return self.h, self.k, self.lo, self.li, self.C1
        #
    #
    #
#
