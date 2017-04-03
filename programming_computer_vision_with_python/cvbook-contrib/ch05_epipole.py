from pylab import *
import sfm

execfile('load_vggdata.py')

# Points in first 2 views.
ndx = (corr[:, 0] >= 0) & (corr[:, 1] >= 0)

x1 = points2D[0][:, corr[ndx, 0]]
x1 = numpy.vstack( (x1, numpy.ones(x1.shape[1])) )
x2 = points2D[1][:, corr[ndx, 1]]
x2 = numpy.vstack( (x2, numpy.ones(x2.shape[1])) )

F = sfm.compute_fundamental(x1, x2)
e = sfm.compute_right_epipole(F)

# How many points / corresponding epipoles to draw. Should be < x1.shape[1].
N = 5

figure()
imshow(im1)
for i in range(N):
  sfm.plot_epipolar_line(im1, F, x2[:, i], e, False)
axis('off')

figure()
imshow(im2)
for i in range(N):
  plot(x2[0, i], x2[1, i], 'o')
axis('off')

show()
