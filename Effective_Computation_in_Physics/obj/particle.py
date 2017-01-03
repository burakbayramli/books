from __future__ import print_function
from scipy import constants as const

class Particle(object):
    """A particle is a constituent unit of the unverse."""
    roar = "I am a particle!"
    n = 0

    def hear_me(self):
        self.myroar = Particle.roar + "\n" +\
        " My mass is: " + str(self.m) + "\n" +\
        " My charge is: " + str(self.c) + "\n" +\
        " My x position is: " + str(self.r['x']) +"\n"+\
        " My y position is: " + str(self.r['y']) +"\n"+\
        " My z position is: " + str(self.r['z'])
        print(self.myroar)

    def __init__(self, c=1, m=0, r={'x':0,'y':0,'z':0}):
        self.c = c
        self.m = m
        self.r = r
        self.spin_ = 1
        self.constituents = []
        self.myroar = ""
        Particle.n+=1

    def spin(self, s):
        self.spin_ = s

    def constituents(self, c):
        self.constituents_ = c

    def charge(self, c):
        self.charge_ = c

    def delta_x_min(self):
        hbar = const.hbar
        delx_min = hbar/float(2*self.p_["x"])
        return delx_min
