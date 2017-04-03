from PIL import Image
import numpy
import glob

import camera

im1 = numpy.array(Image.open('out_merton/images/001.jpg'))
im2 = numpy.array(Image.open('out_merton/images/002.jpg'))

points2D = [numpy.loadtxt(f).T for f in glob.glob('out_merton/2D/*.corners')]
points3D = numpy.loadtxt('out_merton/3D/p3d').T

corr = numpy.genfromtxt('out_merton/2D/nview-corners', dtype='int', missing='*')

P = [camera.Camera(numpy.loadtxt(f)) for f in glob.glob('out_merton/2D/*.P')]
