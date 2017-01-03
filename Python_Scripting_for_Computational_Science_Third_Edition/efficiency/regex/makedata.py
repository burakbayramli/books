#!/usr/bin/env python
from numpy import random
try:     n = int(sys.argv[1])
except:  n = 100
a = random.normal(0,1,n**3)
a.shape = (n,n,n)
f = open('tmp1.dat', 'w')
for i in range(n):
    for j in range(n):
        for k in range(n):
            f.write('[%d,%d,%d]=%g\n' % (i,j,k,a[i,j,k]))
f.close()
# also make a file with output in NumPy format:
f = open('tmp2.dat', 'w'); f.write(repr(a)); f.close()
    
