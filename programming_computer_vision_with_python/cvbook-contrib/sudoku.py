import numpy
from scipy.ndimage import measurements

def find_sudoku_edges(im, axis=0):
  """Finds the cell edges for an aligned sudoku image."""
  trim = 1 * (im < 128)
  s = trim.sum(axis=axis)

  s_labels, s_nbr = measurements.label(s > 0.5*max(s))
  m = measurements.center_of_mass(s, s_labels, range(1, s_nbr + 1))
  x = [int(x[0]) for x in m]

  # If minor lines weren't detected, synthesize them from the major ones.
  if len(x) == 4:
    dx = numpy.diff(x)
    x = [x[0], x[0] + dx[0]/3, x[0] + 2 * dx[0]/3,
         x[1], x[1] + dx[1]/3, x[1] + 2 * dx[1]/3,
         x[2], x[2] + dx[2]/3, x[2] + 2 * dx[2]/3,
         x[3]]

  if len(x) != 10:
    raise RuntimeError('Edges not detected')
  return x
