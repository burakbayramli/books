#!/usr/bin/env python
# -*- coding: utf-8 -*-
from __future__ import division
import numpy as N
import ekf
import rbpf

def wrap_radians(angle):
   "A function to convert an angle in radians to the range -pi...pi ."
   return ((angle + N.pi) % (2*N.pi)) - N.pi

class OccupancyGrid(object):
  """A simple occupancy grid implementation, designed to make it simple to track
  numerous circle intersections."""
  def __init__(self, ll, ur, shape=(120,120)):
    self.grid = N.zeros(shape, dtype=N.uint32)
    self.x, self.y = N.meshgrid(N.linspace(ll[0], ur[0], shape[0]),
                                N.linspace(ll[1], ur[1], shape[1]))
    self.dr = N.sqrt((self.x[0,1] - self.x[0,0])**2 +
                     (self.y[1,0] - self.y[0,0])**2)
    self.count = 0

  def addCircle(self, cx, cy, radius):
    rsq = (self.x-cx)**2 + (self.y-cy)**2
    minradsq, maxradsq = (radius-self.dr)**2, (radius+self.dr)**2
    self.grid[((minradsq < rsq) & (rsq < maxradsq))] += 1
    self.count += 1

  def distinctPeak(self):
    return self.count > 100

  def getMaxLocation(self):
    ii = self.grid.argmax()
    return self.x.flat[ii], self.y.flat[ii]

  def getMaxVariance(self):
    return 3 * self.dr * N.eye(2)

class RangeOnlyFeature(ekf.StaticFeature):
  """This class implements a range-only feature tracker.  New features are
  initialized using an occupancy grid, and then converted to an Extended Kalman
  filter for tracking."""

  # Measurement covariance matrix.
  Qt = N.array([[0.2]])

  def __init__(self, fid, X, S):
    """Return a new RangeOnlyFeature instance

    fid -> feature ID
    X   -> initial state
    S   -> initial covariance"""
    super(RangeOnlyFeature, self).__init__(fid, X, S)
    self.grid = None
    self.lastRanging = None

  @classmethod
  def from_measurement(cls, fid, x, z):
    """A single range measurement is not sufficient to initialize the EKF, so
    the state is set to NaN until initialized with the occupancy grid."""
    ftr = cls(fid, N.array([N.NaN, N.NaN]), N.diag([N.NaN, N.NaN]))
    return ftr

  def update(self, x, z):
    if any(N.isnan(self.X)):
      # Create occupancy grid if one doesn't exist
      if self.grid is None:
        self.grid = OccupancyGrid(x-1.5*z, x+1.5*z)
      # Add circle of current measurement
      self.grid.addCircle(x[0], x[1], z)
      # Check for a significant peak, and transition to EKF if possible.
      if self.grid.distinctPeak():
        self.X = self.grid.getMaxLocation()
        self.S = self.grid.getMaxVariance()
      return 1.0

    else:
      return super(RangeOnlyFeature, self).update(x, z)

  def h(self, veh):
    """Returns h, the measurement prediction.

    veh -> The current vehicle state: [X, Y, Theta]"""
    r = N.sqrt(N.sum((veh[:2] - self.X)**2))
    return N.array([r]) # bearing

  def hp(self, veh):
    """Returns h', the Jacobian of the measurement prediction.

    veh -> The current vehicle state: [X, Y, Theta]"""
    dx, dy = self.X - veh[:2]
    rsq = dx**2 + dy**2
    r = N.sqrt(rsq)
    return N.array([[ dx/r,   dy/r]])

class FastSLAM(rbpf.FastSLAM):
  def __init__(self, state_gen, num_particles=50):
    """Returns a new Range-Only FastSLAM instance.

    state_gen     -> Generator function for (u, [(ci0, zi0), (ci1, zi1), ...])
    num_particles -> Number of particles to generate."""
    dt = (N.double, 3)
    self.FeatureClass = RangeOnlyFeature
    super(FastSLAM, self).__init__(dt, state_gen, num_particles)

  def fwd_ctl_model(self, (v,w), (x,y,theta), dt):
    """Returns a sample from the probability distribution for the new robot
    position after moving forward from the state x,y,theta with velocity v, and
    with angular velocity w, for dt seconds."""
    randv = v * N.random.normal(1.0, 0.2)
    randw = w * N.random.normal(1.0, 0.2)
    newx = x + randv * dt * N.cos(theta + dt * randw / 2.0)
    newy = y + randv * dt * N.sin(theta + dt * randw / 2.0)
    newtheta = wrap_radians(theta + randw * dt)
    return N.array([newx, newy, newtheta])
  
