import numpy
from scipy.ndimage import filters

def plane_sweep_ncc(im_l, im_r, start, steps, wid):
  '''Find disparity image using normalized cross-correlation.'''

  m, n = im_l.shape  # Must match im_r.shape.

  mean_l = numpy.zeros(im_l.shape)
  mean_r = numpy.zeros(im_l.shape)
  s = numpy.zeros(im_l.shape)
  s_l = numpy.zeros(im_l.shape)
  s_r = numpy.zeros(im_l.shape)

  dmaps = numpy.zeros((m, n, steps))

  filters.uniform_filter(im_l, wid, mean_l)
  filters.uniform_filter(im_r, wid, mean_r)

  norm_l = im_l - mean_l
  norm_r = im_r - mean_r

  for disp in range(steps):
    filters.uniform_filter(numpy.roll(norm_l, -disp - start) * norm_r, wid, s)
    filters.uniform_filter(numpy.roll(norm_l, -disp - start) *
                           numpy.roll(norm_l, -disp - start), wid, s_l)
    filters.uniform_filter(norm_r * norm_r, wid, s_r)

    dmaps[:, :, disp] = s / numpy.sqrt(s_l * s_r)

  return numpy.argmax(dmaps, axis=2)


def plane_sweep_ssd(im_l, im_r, start, steps, wid):
  '''Find disparity image using sum of squared differences.'''

  m, n = im_l.shape  # Must match im_r.shape.

  s = numpy.zeros(im_l.shape)

  dmaps = numpy.zeros((m, n, steps))

  for disp in range(steps):
    filters.uniform_filter((numpy.roll(im_l, -disp - start) - im_r) ** 2,
                           wid, s)
    dmaps[:, :, disp] = s

  return numpy.argmin(dmaps, axis=2)


def plane_sweep_gauss(im_l, im_r, start, steps, wid):
  '''Find disparity image using normalized cross-correlation with Gaussian
  weighted neighborhoods.'''

  m, n = im_l.shape  # Must match im_r.shape.

  mean_l = numpy.zeros(im_l.shape)
  mean_r = numpy.zeros(im_l.shape)
  s = numpy.zeros(im_l.shape)
  s_l = numpy.zeros(im_l.shape)
  s_r = numpy.zeros(im_l.shape)

  dmaps = numpy.zeros((m, n, steps))

  filters.gaussian_filter(im_l, wid, 0, mean_l)
  filters.gaussian_filter(im_r, wid, 0, mean_r)

  norm_l = im_l - mean_l
  norm_r = im_r - mean_r

  for disp in range(steps):
    filters.gaussian_filter(numpy.roll(norm_l, -disp - start) *
                            norm_r, wid, 0, s)
    filters.gaussian_filter(numpy.roll(norm_l, -disp - start) *
                            numpy.roll(norm_l, -disp - start), wid, 0, s_l)
    filters.gaussian_filter(norm_r * norm_r, wid, 0, s_r)

    dmaps[:, :, disp] = s / numpy.sqrt(s_l * s_r)

  return numpy.argmax(dmaps, axis=2)


def plane_sweep_gauss_ssd(im_l, im_r, start, steps, wid):
  '''Find disparity image using sum of squared differences with Gaussian
  weighted neighborhoods.'''

  m, n = im_l.shape  # Must match im_r.shape.

  s = numpy.zeros(im_l.shape)

  dmaps = numpy.zeros((m, n, steps))

  for disp in range(steps):
    filters.gaussian_filter((numpy.roll(im_l, -disp - start) - im_r) ** 2,
                            wid, 0, s)
    dmaps[:, :, disp] = s

  return numpy.argmin(dmaps, axis=2)

