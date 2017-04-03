import homography
import numpy

fp = numpy.array([ [0, 0, 1], [1, 0, 1], [0, 1, 1], [1, 1, 1] ]).T
tp = numpy.array([ [0, 0, 1], [1, 0, 1], [0, 1, 1], [1, 1, 1] ]).T
print homography.H_from_points(fp, tp)

fp = numpy.array([ [0, 0, 1], [1, 0, 1], [0, 1, 1] ]).T
tp = numpy.array([ [0, 0, 1], [1, 0, 1], [0, 1, 1] ]).T
print homography.Haffine_from_points(fp, tp)
