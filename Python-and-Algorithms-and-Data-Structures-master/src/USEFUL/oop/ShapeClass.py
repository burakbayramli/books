#!/usr/bin/python3
# mari von steinkirch @2013
# steinkirch at gmail


import math

class Point(object):
    def __init__(self, x=0, y=0): # self: object reference to the object itself
        self.x = x  # data attribute
        self.y = y
	
    def distance_from_origin(self):
        return math.hypot(self.x, self.y)
	
    def __eq__(self, other):
        return self.x == other.x and self.y == other.y
		
    def __repr__(self):
        return "point ({0.x!r}, {0.y!r})".format(self)
		
    def __str__(self): # cannot be passed to eval
        return "({0.x!r}, {0.y!r})".format(self)	
	
	
class Circle(Point):
    
    def __init__(self, radius, x=0, y=0):
        super().__init__(x,y) # creates and initializes self.x and self.y
        self.radius = radius
	
    def edge_distance_from_origin(self):
        return abs(self.distance_from_origin() - self.radius)
	
    def area(self):
        return math.pi*(self.radius**2)
		
    def circumference(self):
        return 2*math.pi*self.radius
	
    def __eq__(self, other): # let us avoid infinite recursion
        return self.radius == other.radius and super().__eq__(other)	
	
    def __repr__(self):
        return "circle ({0.radius!r}, {0.x!r})".format(self)
		
    def __str__(self):
        return repr(self)
        
        
 

if __name__ == '__main__':
    a = Point(3,4)
    print(a.distance_from_origin())
    c = Circle(3,2,1)
    print(c)
    print(c.circumference())
    print(c. edge_distance_from_origin())



