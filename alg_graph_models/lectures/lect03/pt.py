class Point(object):
    'Simple class to define points'
    def __init__(self,xval,yval):
        self.x = float(xval)
        self.y = float(yval)

    def __add__(self,other):
        return Point(self.x+other.x,self.y+other.y)

    def __str__(self):
        return '(%f,%f)' % (self.x,self.y)

    def norm(self):
        from math import sqrt
        return sqrt(self.x**2 + self.y**2)
