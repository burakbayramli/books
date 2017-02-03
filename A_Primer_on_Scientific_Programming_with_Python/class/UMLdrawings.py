try:
    from scitools.Lumpy import Lumpy
except ImportError:
    print 'No Lumpy installed'
    sys.exit(1)
import sys
from classes import Y, Derivative, VelocityProfile, Account, AccountP, \
     Person, Circle
from Complex import Complex
from Vec2D import Vec2D
from Polynomial import Polynomial as P

# Run lumpy.object_diagram() from the main program, not a function,
# to avoid seeing the lumpy variable explicitly in the diagrams

lumpy = Lumpy() 
lumpy.make_reference()   # not necessary for class diagrams
y = Y(v0=4)
lumpy.object_diagram()
lumpy.class_diagram()
del y # remove it from the next diagrams

lumpy = Lumpy() 
v = VelocityProfile(beta=0.06, mu0=0.02, n=0.3, R=1)
lumpy.object_diagram()
lumpy.class_diagram()
del v

lumpy = Lumpy() 
v = Account('John Doe', '93351545761', 1000)
lumpy.object_diagram()
lumpy.class_diagram()
del v

lumpy = Lumpy() 
v = AccountP('John Doe', '93351545761', 1000)
lumpy.object_diagram()
lumpy.class_diagram()
del v

lumpy = Lumpy() 
v = Person('John Doe', email='jdoe@some.where.net')
v.add_mobile_phone('18805614211')
lumpy.object_diagram()
lumpy.class_diagram()
del v

lumpy = Lumpy() 
v = Circle(-1.5, 2, 3.5)
lumpy.object_diagram()
lumpy.class_diagram()
del v


lumpy = Lumpy() 
def g(x):
    return 1 + (1-x)**4
d = Derivative(g)
lumpy.object_diagram()
lumpy.class_diagram()
del g, d

lumpy = Lumpy() 
lumpy.make_reference()
p = P([0,0,1,-1,-5,0,0,-0.5])
lumpy.object_diagram()
lumpy.class_diagram()
del p

lumpy = Lumpy() 
lumpy.make_reference()
v = Vec2D(-1,2)
lumpy.object_diagram()
lumpy.class_diagram()
del v

lumpy = Lumpy() 
lumpy.make_reference()
c = Complex(-3,4)
lumpy.object_diagram()
lumpy.class_diagram()
del c





