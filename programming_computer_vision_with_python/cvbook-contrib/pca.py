import numpy

def pca(X):
  """Principal Component Analysis.
  Parameter X: matrix, with training images stored as flattened arrays in rows.
  Return: projection matrix (important dimensions first), variance, and mean."""

  num_data, dim = X.shape

  mean_X = X.mean(axis=0)
  X = X - mean_X

  if dim > num_data:
    # Usual case: Fewer images than pixels per image. Use compact trick.
    # (see e.g.
    # http://www.doc.ic.ac.uk/~dfg/ProbabilisticInference/IDAPILecture15.pdf)
    Cov = numpy.dot(X, X.T)  # (Note: not normalized)
    e, EV = numpy.linalg.eigh(Cov)
    tmp = numpy.dot(X.T, EV).T  # compact trick
    V = tmp[::-1]  # Put most important rows first.
    S = numpy.sqrt(e)[::-1]
    for i in range(V.shape[1]):
      V[:, i] /= S
  else:
    # Dense samples. Use SVD.
    U, S, V = numpy.linalg.svd(X)
    V = V[:num_data]
  return V, S, mean_X
