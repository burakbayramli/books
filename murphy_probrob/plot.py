#!/usr/bin/env python
# -*- coding: utf-8 -*-
import matplotlib as M; M.use('TkAgg')
import matplotlib.patches
import matplotlib.figure
import numpy as N
import pylab as P
import math

def _wrap_radians(angle):
   return ((angle + N.pi) % (2*N.pi)) - N.pi

class CovarianceEllipse(M.patches.Ellipse):
  def __init__(self, mean, covariance, color='k', scale=1.0):
    self.scale = scale
    self.color = color
    angle, w, h = self._calc_angle_size(covariance)
    M.patches.Ellipse.__init__(self, mean, w, h, angle,
                               ec=color, fill=False)

  def _calc_angle_size(self, covariance):
    u, v = N.linalg.eig(covariance)
    u = self.scale * N.sqrt(u)
    return math.degrees(N.arctan2(v[1, 0], v[0, 0])), u[0], u[1]

  def update(self, mean, covariance):
    self.center = mean
    self.angle, self.width, self.height = self._calc_angle_size(covariance)

class Field(M.figure.Figure):
  def __init__(self, xlim, ylim):
    M.figure.Figure.__init__(self)
    P.axes([0.05,0.05,.9,.9])
    P.axis('scaled')
    P.xlim(xlim)
    P.ylim(ylim)
    P.grid()
    P.ion()

  def refresh(self):
    P.draw()

class Beacons:
  def __init__(self, field, color, scale=1.0):
    self.field = field
    self.color = color
    self.scale = scale
    self.beacons = None
    self.cov_ell = []

  def draw(self, positions, covariances=None):
    if self.beacons is None:
      self.beacons, = P.plot(positions[:,0], positions[:,1],
                             marker='o', linestyle='None', color=self.color,
                             figure=self.field)
    else:
      self.beacons.set_data(positions[:,0], positions[:,1])

    if covariances is None:
      return

    for k, ctr in enumerate(positions):
      if N.isnan(covariances[k]).any():
        continue
      if k < len(self.cov_ell):
        self.cov_ell[k].update(ctr, covariances[k])
      else:
        self.cov_ell.append( CovarianceEllipse(ctr, covariances[k],
                                               self.color, self.scale) )
        P.gca().add_patch(self.cov_ell[-1])

class Robot:
  def __init__(self, field, scale=3, color='k'):
    self.scale = scale
    self.color = color
    self.cov_ellipse = None
    self.robot = None
    self.history = None

  def _clear(self):
    if self.robot:
      self.robot.remove()
      del self.robot
    if self.cov_ellipse:
      self.cov_ellipse.remove()
      del self.cov_ellipse

  def draw(self, pose, covariance=None):
    self._clear()
    if covariance is not None:
      self.cov_ellipse = CovarianceEllipse(map(float, pose[:2]),
                                           covariance[:2,:2],
                                           self.color, self.scale)
      P.gca().add_patch(self.cov_ellipse)
    ang = math.degrees(_wrap_radians(pose[2] + N.pi))
    self.robot = M.patches.Wedge((pose[0], pose[1]), 0.2, ang - 20, ang + 20, 
                                 fc=self.color)
    P.gca().add_artist(self.robot)

class Particles:
  def __init__(self, field, color='k'):
    self.field = field
    self.particles, = P.plot([],[], lw=0, marker='.',
                             ms=3, color=color, figure=self.field)

  def draw(self, poses):
    x,y = poses[:2,:].copy()
    self.particles.set_data(x,y)
