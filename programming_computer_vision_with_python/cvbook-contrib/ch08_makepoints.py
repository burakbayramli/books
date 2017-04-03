from numpy import array, hstack, ones, pi, sin, cos
from numpy.random import randn
import cPickle as pickle

"""Creates points_normal.pkl and pionts_ring.pkl, populated with synthetic
data."""

n = 200

# Create two normal distributions.
class_1 = 0.6 * randn(n, 2)
class_2 = 1.2 * randn(n, 2) + array([5, 1])
labels = hstack((ones(n), -ones(n)))

with open('points_normal.pkl', 'w') as f:
  pickle.dump(class_1, f)
  pickle.dump(class_2, f)
  pickle.dump(labels, f)


# Create one normal distribution with a second ring-shaped distribution around
# it.
class_1 = 0.6 * randn(n, 2)
r = 0.8 * randn(n, 1) + 5
angle = 2*pi * randn(n, 1)
class_2 = hstack((r * cos(angle), r * sin(angle)))
labels = hstack((ones(n), -ones(n)))

with open('points_ring.pkl', 'w') as f:
  pickle.dump(class_1, f)
  pickle.dump(class_2, f)
  pickle.dump(labels, f)
