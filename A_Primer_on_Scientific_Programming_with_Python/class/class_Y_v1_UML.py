from scitools.Lumpy import Lumpy
lumpy = Lumpy() 
lumpy.make_reference()

class Y:
    def __init__(self, v0):
        self.v0 = v0
        self.g = 9.81

    def value(self, t):
        return self.v0*t - 0.5*self.g*t**2

y = Y(3)
lumpy.class_diagram()



