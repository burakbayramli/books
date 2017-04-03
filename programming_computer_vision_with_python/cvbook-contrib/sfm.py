import numpy

def compute_fundamental(x1, x2):
  '''Computes the fundamental matrix from corresponding points x1, x2 using
  the 8 point algorithm.'''
  n = x1.shape[1]
  if x2.shape[1] != n:
    raise ValueError('Number of points do not match.')

  # Normalization is done in compute_fundamental_normalized().
  A = numpy.zeros((n, 9))
  for i in range(n):
    A[i] = [x1[0, i] * x2[0, i],  x1[0, i] * x2[1, i],  x1[0, i] * x2[2, i],
            x1[1, i] * x2[0, i],  x1[1, i] * x2[1, i],  x1[1, i] * x2[2, i],
            x1[2, i] * x2[0, i],  x1[2, i] * x2[1, i],  x1[2, i] * x2[2, i],
           ]

  # Solve A*f = 0 using least squares.
  U, S, V = numpy.linalg.svd(A)
  F = V[-1].reshape(3, 3)

  # Constrain F to rank 2 by zeroing out last singular value.
  U, S, V = numpy.linalg.svd(F)
  S[2] = 0
  F = numpy.dot(U, numpy.dot(numpy.diag(S), V))
  return F / F[2, 2]


def compute_fundamental_normalized(x1, x2):
  '''Computes the fundamental matrix from corresponding points x1, x2 using
  the normalized 8 point algorithm.'''
  n = x1.shape[1]
  if x2.shape[1] != n:
    raise ValueError('Number of points do not match.')

  # normalize.
  x1 = x1 / x1[2]
  mean_1 = numpy.mean(x1[:2], axis=1)
  S1 = numpy.sqrt(2) / numpy.std(x1[:2])
  T1 = numpy.array([[S1, 0, -S1 * mean_1[0]],
                    [0, S1, -S1 * mean_1[1]],
                    [0, 0, 1]])
  x1 = numpy.dot(T1, x1)

  x2 = x2 / x2[2]
  mean_2 = numpy.mean(x2[:2], axis=1)
  S2 = numpy.sqrt(2) / numpy.std(x2[:2])
  T2 = numpy.array([[S2, 0, -S2 * mean_2[0]],
                    [0, S2, -S2 * mean_2[1]],
                    [0, 0, 1]])
  x2 = numpy.dot(T2, x2)

  F = compute_fundamental(x1, x2)

  # denormalize.
  F = numpy.dot(T1.T, numpy.dot(F, T2))
  return F / F[2, 2]


def compute_right_epipole(F):
  '''Returns e with F * e = 0 (call with F.T for left epipole).'''
  U, S, V = numpy.linalg.svd(F)
  e = V[-1]  # S is diag([l1, l2, 0]). e's scale is arbitrary.
  return e / e[2]


def plot_epipolar_line(im, F, x, epipole=None, show_epipole=True):
  '''Plot the epipole and epipolar line F*x = 0.'''
  import pylab

  m, n = im.shape[:2]
  line = numpy.dot(F, x)

  t = numpy.linspace(0, n, 100)
  lt = numpy.array([(line[2] + line[0] * tt) / (-line[1]) for tt in t])

  ndx = (lt >= 0) & (lt < m)
  pylab.plot(t[ndx], lt[ndx], linewidth=2)

  if show_epipole:
    if epipole is None:
      epipole = compute_right_epipole(F)
    pylab.plot(epipole[0] / epipole[2], epipole[1] / epipole[2], 'r*')


def triangulate_point(x1, x2, P1, P2):
  '''Given two image coordinates x1, x2 of the same point X under different
  projections P1, P2, recovers X.'''
  M = numpy.zeros((6, 6))
  M[:3, :4] = P1
  M[:3, 4] = -x1

  M[3:, :4] = P2
  M[3:, 5] = -x2  # Intentionally 5, not 4.

  U, S, V = numpy.linalg.svd(M)
  X = V[-1, :4]
  return X / X[3]


def triangulate(x1, x2, P1, P2):
  '''Given n pairs of points, returns their 3d coordinates.'''
  n = x1.shape[1]
  if x2.shape[1] != n:
    raise ValueError('Number of points do not match.')

  X = [triangulate_point(x1[:, i], x2[:, i], P1, P2) for i in range(n)]
  return numpy.array(X).T


def compute_P(x, X):
  '''Computes camera matrix from corresponding (homogeneous)
  2D and 3D points.'''
  n = x.shape[1]
  if X.shape[1] != n:
    raise ValueError('Number of points do not match.')

  M = numpy.zeros((3 * n, 12 + n))
  for i in range(n):
    M[3 * i          , 0:4] = X[:, i]
    M[3 * i + 1      , 4:8] = X[:, i]
    M[3 * i + 2      , 8:12] = X[:, i]
    M[3 * i:3 * i + 3, i + 12] = -x[:, i]

  U, S, V = numpy.linalg.svd(M)
  return V[-1, :12].reshape((3, 4))


def skew(a):
  '''Skew matrix A such that a x v = A*v for any v.'''
  return numpy.array([[0, -a[2], a[1]],
                      [a[2], 0, -a[0]],
                      [-a[1], a[0], 0]])


def compute_P_from_fundamental(F):
  '''Computes second camera matrix, assuming P1 = [I 0].
  Only up to a homography, since no calibration is given.'''
  e = compute_right_epipole(F.T)  # left epipole
  Te = skew(e)
  return numpy.vstack((numpy.dot(Te, F.T).T, e)).T


def compute_P_from_essential(E):
  # make sure E is rank 2
  U, S, V = numpy.linalg.svd(E)
  if numpy.linalg.det(numpy.dot(U, V)) < 0:
    V = -V
  E = numpy.dot(U, numpy.dot(numpy.diag([1, 1, 0]), V))

  # create matrices ("Hartley p 258" XXX)
  Z = skew([0, 0, -1])  # FIXME: Unused?
  W = numpy.array([[0, -1, 0], [1, 0, 0], [0, 0, 1]])

  P2 = [numpy.vstack((numpy.dot(U, numpy.dot(W, V)).T,  U[:,2])).T,
        numpy.vstack((numpy.dot(U, numpy.dot(W, V)).T, -U[:,2])).T,
        numpy.vstack((numpy.dot(U, numpy.dot(W.T, V)).T,  U[:,2])).T,
        numpy.vstack((numpy.dot(U, numpy.dot(W.T, V)).T, -U[:,2])).T]
  return P2


class RansacModel(object):
  def fit(self, data):
    data = data.T
    x1 = data[:3, :8]
    x2 = data[3:, :8]
    return compute_fundamental_normalized(x1, x2)

  def get_error(self, data, F):
    data = data.T
    x1 = data[:3]
    x2 = data[3:]

    # Sampson distance as error.
    Fx1 = numpy.dot(F, x1)
    Fx2 = numpy.dot(F, x2)
    denom = Fx1[0]**2 + Fx1[1]**2 + Fx2[0]**2 + Fx2[1]**2
    err = (numpy.diag(numpy.dot(x1.T, numpy.dot(F, x2))))**2 / denom
    return err


def F_from_ransac(x1, x2, model, maxiter=5000, match_threshold=1e-6):
  import ransac
  data = numpy.vstack((x1, x2))
  F, ransac_data = ransac.ransac(data.T, model, 8, maxiter, match_threshold, 20,
                                 return_all=True)
  return F, ransac_data['inliers']
