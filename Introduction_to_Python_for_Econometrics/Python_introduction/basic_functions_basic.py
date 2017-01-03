from __future__ import division
from __future__ import print_function
import sys
from pylab import *
from numpy import *
# End Imports


x = linspace(0, 10, 11)
x

x = arange(11)
x = arange(11.0)
x = arange(4, 10, 1.25)

x = arange(5)
y = arange(3)
X,Y = meshgrid(x,y)
X
Y

r_[0:10:1] # arange equiv
r_[0:10:.5] # arange equiv
r_[0:10:5j] # linspace equiv, includes end point

r_[0:2, 7:11, 1:4]

c_[0:5:2]
c_[1:5:4j]

x = reshape(arange(25.0),(5,5))
x
x[ix_([2,3],[0,1,2])] # Rows 2 & 3, cols 0, 1 and 2
x[2:4,:3] # Same, standard slice
x[ix_([0,3],[0,1,4])] # No slice equiv

mgrid[0:3,0:2:.5]
mgrid[0:3:3j,0:2:5j]

ogrid[0:3,0:2:.5]
ogrid[0:3:3j,0:2:5j]

x = randn(3)
around(x)
around(x, 2)

x = randn(3)
floor(x)

x = randn(3)
ceil(x)

x = randn(3,4)
x
sum(x) # all elements
sum(x, 0) # Down rows, 4 elements
sum(x, 1) # Across columns, 3 elements
cumsum(x,0) # Down rows

x= randn(3,4)
x
diff(x) # Same as diff(x,1)
diff(x, axis=0)
diff(x, 2, axis=0) # Double difference, column-by-column

x = repeat(randn(3),(2))
unique(x)
y,ind = unique(x, True)
ind
x.flat[ind]

x = arange(10.0)
y = arange(5.0,15.0)
in1d(x,y)

x = arange(10.0)
y = arange(5.0,15.0)
intersect1d(x,y)

x = arange(10.0)
y = arange(5.0,15.0)
union1d(x,y)

x = arange(10.0)
y = arange(5.0,15.0)
setdiff1d(x,y)

x = arange(10.0)
y = arange(5.0,15.0)
setxor1d(x,y)

x = randn(4,2)
x
sort(x)
sort(x, 0)
sort(x, axis=None)

x = randn(3)
x
sort(x)
x
x.sort() # In-place, changes x
x

x = randn(3,4)
x
amax(x)
x.max()
x.max(0)
x.max(1)

x = randn(4)
x
y = randn(4)
maximum(x,y)

x = randn(4)
x[1] = nan
x
sum(x)
nansum(x)
nansum(x) / sum(x[logical_not(isnan(x))])
nansum(x) / sum(1-isnan(x)) # nanmean

x = arange(25.0)
y = x.reshape((5,5))
y
z = reshape(x,(5,5))
z

