from scipy import ndimage
import homography
import numpy


def image_in_image(im1, im2, tp):
  '''Put im1 in im2 with an affine transform such that corners are at tp.
  
  tp is homogeneous and counterclockwise from the top left.'''
  m, n = im1.shape[:2]
  fp = numpy.array([[0, m, m, 0], [0, 0, n, n], [1, 1, 1, 1]])

  H = homograph.Haffine_from_points(tp, fp)
  im1_t = ndimage.affine_transform(
      im1, H[:2, :2], (H[0, 2], H[1, 2]), im2.shape[:2])
  alpha = im1_t > 0

  return (1 - alpha) * im2 + alpha * im1_t


def panorama(H, fromim, toim, padding=2400, delta=2400, alpha=1):
  import imtools

  is_color = len(fromim.shape) == 3
  if is_color:
    toim_zeros = numpy.zeros((toim.shape[0], padding, 3))
  else:
    toim_zeros = numpy.zeros((toim.shape[0], padding))

  if H[0, 2] < 0:  # fromim is on the right
    toim_t = numpy.hstack((toim, toim_zeros))
  else:
    H_delta = numpy.array([[1, 0, -delta], [0, 1, 0], [0, 0, 1]])
    H = numpy.dot(H, H_delta)
    toim_t = numpy.hstack((toim_zeros, toim))

  fromim_t = imtools.Htransform(fromim, H, (toim_t.shape[0], toim_t.shape[1]))

  if is_color:
    # Three separate checks instead of a * b * c > 0 because of uint8 overflow.
    alpha = ((fromim_t[:, :, 0] > 0) *
             (fromim_t[:, :, 1] > 0) *
             (fromim_t[:, :, 2] > 0)) * alpha
    for col in range(3):
      toim_t[:, :, col] = fromim_t[:, :, col] * alpha + \
                          toim_t[:, :, col]   * (1 - alpha)
  else:
    alpha = (fromim_t > 0) * alpha
    toim_t = fromim_t * alpha + toim_t * (1 - alpha)

  return toim_t
