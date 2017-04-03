import numpy


def normalize(points):
  for row in points:
    row /= points[-1]
  return points


def make_homog(points):
  return numpy.vstack((points, numpy.ones((1, points.shape[1]))))


def H_from_points(fp, tp):
  '''Find H such that H * fp = tp.
  
  H has eight degrees of freedom, so this needs at least 4 points in fp and tp.
  '''
  if fp.shape != tp.shape:
    raise RuntimeError('number of points do not match')

  # condition:
  # -from
  m = numpy.mean(fp[:2], axis=1)
  maxstd = numpy.max(numpy.std(fp[:2], axis=1)) + 1e-9
  C1 = numpy.diag([1/maxstd, 1/maxstd, 1])
  C1[0, 2] = -m[0] / maxstd
  C1[1, 2] = -m[1] / maxstd
  fp = numpy.dot(C1, fp)

  # -to
  m = numpy.mean(tp[:2], axis=1)
  maxstd = numpy.max(numpy.std(tp[:2], axis=1)) + 1e-9
  C2 = numpy.diag([1/maxstd, 1/maxstd, 1])
  C2[0, 2] = -m[0] / maxstd
  C2[1, 2] = -m[1] / maxstd
  tp = numpy.dot(C2, tp)

  correspondences_count = fp.shape[1]
  A = numpy.zeros((2 * correspondences_count, 9))
  for i in range(correspondences_count):
    A[2 * i    ] = [-fp[0][i], -fp[1][i], -1, 0, 0, 0,
                    tp[0][i]  * fp[0][i], tp[0][i] * fp[1][i], tp[0][i]]
    A[2 * i + 1] = [0, 0, 0, -fp[0][i], -fp[1][i], -1,
                    tp[1][i]  * fp[0][i], tp[1][i] * fp[1][i], tp[1][i]]

  U, S, V = numpy.linalg.svd(A)
  H = V[8].reshape((3, 3))

  # decondition
  H = numpy.dot(numpy.linalg.inv(C2), numpy.dot(H, C1))
  return H / H[2, 2]


def Haffine_from_points(fp, tp):
  '''Find affine H such that H * fp = tp.
  
  H has six degrees of freedom, so this needs at least 3 points in fp and tp.
  '''
  if fp.shape != tp.shape:
    raise RuntimeError('number of points do not match')

  # condition:
  # -from
  m = numpy.mean(fp[:2], axis=1)
  maxstd = numpy.max(numpy.std(fp[:2], axis=1)) + 1e-9
  C1 = numpy.diag([1/maxstd, 1/maxstd, 1])
  C1[0, 2] = -m[0] / maxstd
  C1[1, 2] = -m[1] / maxstd
  fp_cond = numpy.dot(C1, fp)

  # -to
  m = numpy.mean(tp[:2], axis=1)
  maxstd = numpy.max(numpy.std(tp[:2], axis=1)) + 1e-9
  C2 = numpy.diag([1/maxstd, 1/maxstd, 1])
  C2[0, 2] = -m[0] / maxstd
  C2[1, 2] = -m[1] / maxstd
  tp_cond = numpy.dot(C2, tp)

  A = numpy.concatenate((fp_cond[:2], tp_cond[:2]), axis=0)
  U, S, V = numpy.linalg.svd(A.T)

  tmp = V[:2].T
  B = tmp[:2]
  C = tmp[2:4]

  tmp2 = numpy.concatenate((numpy.dot(C, numpy.linalg.pinv(B)),
                            numpy.zeros((2, 1))),
                           axis=1)
  H = numpy.vstack((tmp2, [0, 0, 1]))

  # decondition
  H = numpy.dot(numpy.linalg.inv(C2), numpy.dot(H, C1))
  return H / H[2, 2]


class RansacModel(object):
  def fit(self, data):
    data = data.T  # for H_from_points()
    fp = data[:3]
    tp = data[3:]
    return H_from_points(fp, tp)

  def get_error(self, data, H):
    data = data.T
    fp = data[:3]
    tp = data[3:]

    fp_transformed = numpy.dot(H, fp)
    normalize(fp_transformed)

    return numpy.sqrt(numpy.sum((tp - fp_transformed) ** 2, axis=0))


def H_from_ransac(fp, tp, model, maxiter=1000, match_threshold=10):
  import ransac
  data = numpy.vstack((fp, tp))
  H, ransac_data = ransac.ransac(data.T, model, 4, maxiter, match_threshold, 10,
                                 return_all=True)
  return H, ransac_data['inliers']


class AffineRansacModel(object):
  def fit(self, data):
    data = data.T  # for Haffine_from_points
    fp = data[:3]
    tp = data[3:]
    return Haffine_from_points(fp, tp)

  def get_error(self, data, H):
    data = data.T
    fp = data[:3]
    tp = data[3:]

    fp_transformed = numpy.dot(H, fp)
    #normalize(fp_transformed)

    return numpy.sqrt(numpy.sum((tp - fp_transformed) ** 2, axis=0))


def Haffine_from_ransac(fp, tp, model, maxiter=1000, match_threshold=10):
  import ransac
  data = numpy.vstack((fp, tp))
  H, ransac_data = ransac.ransac(data.T, model, 3, maxiter, match_threshold, 7,
                                 return_all=True)
  return H, ransac_data['inliers']
