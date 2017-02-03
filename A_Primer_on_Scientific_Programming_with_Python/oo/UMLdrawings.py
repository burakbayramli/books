"""
Draw the Diff2 class hierarchy.
"""
try:
    from scitools.Lumpy import Lumpy
except ImportError:
    print 'No Lumpy installed'
    sys.exit(1)
import sys

# Run lumpy.object_diagram() from the main program, not a function,
# to avoid seeing the lumpy variable explicitly in the diagrams.

lumpy = Lumpy()
import Line_Parabola
p = Line_Parabola.Parabola(1, -2, 2)
l = Line_Parabola.Line(1,0) # needed this to draw Line-Parabola correctly
lumpy.class_diagram()
del p, Line_Parabola

def f(x):
    return x + 1.0/x**2

lumpy = Lumpy() 

import Diff2
diff = []
for classname in dir(Diff2):
    if not classname.startswith('_'):
        diff.append(eval('Diff2.%s(f)' % classname))
lumpy.class_diagram()
del diff, Diff2, f, classname

import shapes

lumpy = Lumpy() 
r = shapes.Rectangle((0,0), 2, 3)
lumpy.class_diagram()
del r

lumpy = Lumpy() 
w = shapes.Wheel((0,0), 1)
lumpy.class_diagram()
del w






