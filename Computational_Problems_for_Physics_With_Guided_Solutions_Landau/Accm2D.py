""" From "A SURVEY OF COMPUTATIONAL PHYSICS", Python eBook Version
   by RH Landau, MJ Paez, and CC Bordeianu
   Copyright Princeton University Press, Princeton, 2011; Book  Copyright R Landau, 
   Oregon State Unv, MJ Paez, Univ Antioquia, C Bordeianu, Univ Bucharest, 2011.
   Support by National Science Foundation , Oregon State Univ, Microsoft Corp"""  

# Accmd.Py: Python accelerated motion in 2D

from visual.graph import *              

class Um1D:
        
    def __init__(self, x0, dt, vx0, ttot):            # class constructor
        self.x00 = 0                                 # initial x position
        self.delt = dt                                   # time increment
        self.vx = vx0                                        # x velocity
        self.time = ttot                                     # total time
        self.steps = int(ttot/self.delt)             # total number steps
        
    def x(self, tt):                              # x position at time tt
        return self.x00 + tt*self.vx
    '''to be used in graphics'''
    
    def scenario(self, mxx, myy, mytitle, myxtitle, myytitle, xma, xmi, yma, ymi): 
        graph = gdisplay(x = mxx, y = myy, width = 500, height = 200, 
               title=mytitle, xtitle=myxtitle, ytitle=myytitle, xmax=xma,
               xmin=xmi, ymax=yma, ymin=ymi, foreground = color.black,
               background = color.white)
        
    def archive(self):                   # produce file, plot 1D x motion
        unimotion1D = gcurve(color = color.blue)
        tt = 0.0
        f = open('unimot1D.dat', 'w')  # Disk file produced for 1D motion
        for i in range (self.steps):
            xx = self.x(tt)
            unimotion1D.plot(pos = (tt, xx) )           # Plots x vs time
            f.write(" %f   %f\n"%(tt, xx) )             # x vs t for file
            tt   += self.delt                             # increase time
        f.closed                                        # close disk file
'''Uniform motion in 2D''' 

class Um2D(Um1D):                                 # Um2D subclass of Um1D
        
    def __init__(self, x0, dt, vx0, ttot, y0, vy0):    # Constructor Um2D
        Um1D.__init__(self, x0, dt, vx0, ttot)        # to construct Um1D
        self.y00 = y0                            # initializes y position
        self.vy = vy0                            # initializes y velocity
        
    def y(self, tt):                              # produces y at time tt
        return self.y00 + tt*self.vy    
        
    def archive(self):                         # overrides archive for 1D
        unimot2d = gcurve(color = color.magenta)
        tt = 0.0
        f = open('Um2D.dat', 'w')                   # Opens new Um2D file
        for i in range (self.steps):
            xx = self.x(tt)
            yy = self.y(tt)
            unimot2d.plot(pos = (xx, yy) )        # plots y vs x position
            f.write(" %f   %f\n"%(xx, yy) )       # writes x y in archive
            tt   += self.delt 
        f.closed                                  # closes open Um2D file
        
'''Accelerated motion in 2D'''        
class Accm2D(Um2D):                                    # Daugther of U21D
    def __init__(self, x0, dt, vx0, ttot, y0, vy0, accx, accy): 
        Um2D.__init__(self, x0, dt, vx0, ttot, y0, vy0)# Um2D constructor
        self.ax = accx                               # adds acceleretions
        self.ay = accy                                    # to this class
        
    def xy(self, tt, i):
        self.xxac = self.x(tt) + self.ax*tt**2
        self.yyac = self.y(tt) + self.ay*tt**2
        if(i == 1):                                # if acceleration in x
          return self.xxac
        else:
          return self.yyac                         # if acceletation in y
          
    def archive(self):
        acmotion = gcurve(color = color.red)
        tt = 0.0
        f = open('Accm2D.dat', 'w')
        for i in range (self.steps):
            self.xxac = self.xy(tt, 1)
            self.yyac = self.xy(tt, 2)
            f.write(" %f   %f\n"%(self.xxac, self.yyac) )  # to disk file
            acmotion.plot(pos = (self.xxac, self.yyac)) # plot acc. motion
            tt=tt+ self.delt
        f.closed

# comment unmd um2d or myAcc to change plot        
unmd = Um1D(0.0, 0.1, 2.0, 4.0)                       # x0, dt, vx0, ttot
unmd.scenario(0, 0, 'Uniform motion in  1D ',     # for 1D uniform motion
                 'time', 'x', 4.0, 0, 10.0, 0)  # For tmax tmin xmax xmin
unmd.archive()                                               # archive 1D
um2d = Um2D(0.0, 0.1, 2.0, 4.0, 0.0, 5.0)    # x0, dt, vx0, ttot, y0, vy0
um2d.scenario(0, 200, 'Uniform motion in  2D ',   # for 2D uniform motion
                 'x', 'y', 10.0, 0, 25.0, 0)        # xmx xmin  ymax ymin
um2d.archive()                               # archive in two dim. motion
myAcc = Accm2D(0.0, 0.1, 14.0, 4.0, 0.0, 14.0, 0.0,  - 9.8)
myAcc.scenario(0, 400, 'Accelerated motion ', 'x', 'y', 55, 0, 5,-100.)
myAcc.archive()                           # archive in accelerated motion
