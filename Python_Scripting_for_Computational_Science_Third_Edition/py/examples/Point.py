#!/usr/bin/env python
import os
from numpy import *

class Point(object):
    """
    1D, 2D or 3D Point object implemented as a NumPy array
    with properties.
    """
    def __new__(self, point):
        a = array(point)

        # define read-only attributes x, y, and z:
        if len(point) >= 1:
            ndarray.x = property(fget=lambda self: self[0])
            # or a.__class__.x = property(fget=lambda self: self[0])
        if len(point) >= 2:
            ndarray.y = property(fget=lambda self: self[1])
        if len(point) == 3:
            ndarray.z = property(fget=lambda self: self[2])
        return a

# equivalent version using a function:
def Point1(point):
    a = array(point)

    # define read-only attributes x, y, and z:
    if len(point) >= 1:
        ndarray.x = property(fget=lambda self: self[0])
        # or a.__class__.x = property(fget=lambda self: self[0])
    if len(point) >= 2:
        ndarray.y = property(fget=lambda self: self[1])
    if len(point) == 3:
        ndarray.z = property(fget=lambda self: self[2])
    return a
    

def _test(P):
    p1 = P((0,1))
    p2 = P((1,2))
    print p1, p2
    p3 = p1 + p2
    print 'sum=', p3
    print 'type of p3:', type(p3)
    
    print p3.x, p3.y
    try:
        print p3.z # should raise an exception
    except AttributeError, msg:
        print msg
    print '\ninspect p3:'
    import scitools.misc
    scitools.misc.dump(p3)
    

if __name__ == '__main__':
    print 'ndarray, class Point with __new__'
    _test(Point)
    print 'function Point1'
    _test(Point1)
    
