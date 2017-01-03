#!/usr/bin/env python
# test hw extension module in C++ (class version)

import sys
from hw import HelloWorld, HelloWorld2

hw = HelloWorld()
r1 = float(sys.argv[1]);  r2 = float(sys.argv[2])
hw.set(r1, r2)
s = hw.get()
print "Hello, World! sin(%g + %g)=%g" % (r1, r2, s)
hw.print_()

hw2 = HelloWorld2()
hw2.set(r1, r2)
s = hw.gets()
print "Hello, World2! sin(%g + %g)=%g" % (r1, r2, s)

