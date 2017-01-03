from __future__ import division
from __future__ import print_function
import sys
from pylab import *
from numpy import *
# End Imports


x = nan
1.0 + x
1.0 * x
0.0 * x
mean(x)

x = 1.0
eps = finfo(float).eps
x = x+eps/2
x == 1
x-1
x = 1 + 2*eps
x == 1
x-1

x=10
x+2*eps
x-10
(x-10) == 0
(1e120 - 1e103) == 1e120
1e103 / 1e120

