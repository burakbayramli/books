"""Game of life in Python with numpy tricks for compuational efficiency.

Note: the program does not show any output, but simply makes a series
of png images, one for each step. For example, if you have the
ImageMagick program installed, type 'animate -delay 100 life*png' to
see a movie with delay of 100ms between framew.
"""

import numpy
import pylab
import time

shape = (252,252)
old = numpy.random.randint(0,2,shape)
new = numpy.zeros(shape)

num_steps = 100
frames = numpy.zeros([num_steps, shape[0], shape[1]])

print "Life at step: ",
for i in range(num_steps):
    print i,
    frames[i] = old
    num = (old[0:-2,0:-2] + old[0:-2,1:-1] + old[0:-2,2:] +
           old[1:-1,0:-2]                  + old[1:-1,2:] +
           old[2:,0:-2]   + old[2:,1:-1]   + old[2:,2:])
    birth = (num==3) & (old[1:-1,1:-1]==0)
    survive = ((num==2) | (num==3)) & (old[1:-1,1:-1]==1)
    new[1:-1,1:-1][birth|survive]  = 1
    new[1:-1,1:-1][~(birth|survive)] = 0
    old = new
print

print "Making images: ",
for i, f in enumerate(frames):
    print i,
    pylab.matshow(f)
    pylab.savefig('life%06d.png' % i)
    
print
