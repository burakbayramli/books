#!/usr/bin/env python
# -*- coding: utf-8 -*-
from __future__ import division as __DIVISION
import numpy as N

def matmult(*x):
  """Shortcut function for matrix multiplication.
  matmult(A,B,C) returns A*B*C."""
  return reduce(N.dot, x)

class StaticFeature(object):
  """This EKF does not allow the use of control inputs; only measurements are
  considered.  It is appropriate for mapping static beacons, but would be
  inappropriate for localizing dynamic objects like a mobile robot.

  It is based on the feature update step for FastSLAM 1.0, as described in:
    S. Thrun et al., "Probabilistic Robotics", 1st Ed., p.450, Table 13.1"""
  def __init__(self, fid, X, S):
    """Returns a new StaticFeature instance.

    fid -> feature ID
    X   -> initial state
    S   -> initial covariance"""
    self.fid = fid
    self.X = N.array(X)
    self.S = N.array(S)

  def __str__(self):
    """Return a string representation of the feature."""
    return "%s(%s, %s, %s)" % (self.__class__.__name__,
                               str(self.fid), str(self.X), str(self.S))

  @classmethod
  def from_measurement(cls, fid, x, z):
    """Return a new instance of this class from a feature ID,
    measurement, and external state.  This method relies on several other
    methods which must be provided by a deriving class.

    fid -> feature ID
    x   -> External state
    z   -> Measurement"""
    X = cls.hi(x, z)
    ftr = cls(fid, X, N.eye(X.shape[0]))
    Hi = N.linalg.inv(ftr.hp(x))
    ftr.S = matmult(Hi, cls.Qt, Hi.T)
    return ftr

  def update(self, x, z):
    """Performs the Kalman update step, and returns a likelihood weight.
    
    x -> External state
    z -> Measurement"""
    v = self.innovation(x, z)
    H = self.hp(x) # "hp = h prime"
    Q = matmult(H, self.S, H.T) + self.Qt
    Qi = N.linalg.inv(Q)
    K = matmult(self.S, H.T, Qi)
    self.X = self.X + matmult(K, v)
    self.S = matmult((N.eye(self.X.shape[0]) - matmult(K, H)), self.S)
    w = 1.0/N.sqrt(N.linalg.det(2 * N.pi * Q)) * \
          N.exp(matmult(-0.5 * v.T, Qi, v))
    return w

  def innovation(self, x, z):
    """Returns the innovation of a measurement.

    x -> External state
    z -> Measurement"""
    return z - self.h(x)

  # Each of these should match the notation in Thrun et al.
  def h(self, x):
    raise NotImplementedError("h not overridden!")

  def hp(self, x):  # h'
    raise NotImplementedError("hp not overridden!")

  @classmethod
  def hi(cls, x, z): # h^{-1}
    raise NotImplementedError("h not overridden!")

  @property
  def Qt(self): # Q_t or R_t - book is inconsistent.
    raise NotImplementedError("Qt not overridden!")
