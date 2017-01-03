#!/usr/bin/env python
# -*- coding: utf-8 -*-
from __future__ import division
from numpy import *
import ekf 
import rbpf 

def wrap_radians(angle):
   return ((angle + pi) % (2*pi)) - pi

class RangeBearingEKF(ekf.StaticFeature):
  Qt = array([[0.4, 0   ],
              [0,   0.15]]) # 0.3m, 0.15rad ~ 10deg

  def __init__(self, *args):
    super(RangeBearingEKF, self).__init__(*args)

  def h(self, veh):
    """Measurement prediction.
     veh = Vehicle state: [X, Y, Theta]"""
    r = sqrt(sum((veh[:2] - self.X)**2))
    bearing = arctan2(self.X[1]-veh[1], self.X[0]-veh[0]) - veh[2]
    bearing = wrap_radians(bearing)
    return array([r, bearing]) # bearing

  @classmethod
  def hi(cls, (x,y,theta), (r, bearing)):
    """State prediction based on measurement."""
    return  array([x + r * cos(theta + bearing),
                   y + r * sin(theta + bearing)])

  def hp(self, veh):
    dx, dy = self.X - veh[:2]
    rsq = dx**2 + dy**2
    r = sqrt(rsq)
    return array([[ dx/r,   dy/r],
                  [-dy/rsq, dx/rsq]])

  def innovation(self, x, z):
    v = super(RangeBearingEKF, self).innovation(x, z)
    v[1] = wrap_radians(v[1])
    return v

class FastSLAM(rbpf.FastSLAM):
  def __init__(self, state_gen, num_particles=50):
    dt = (double, 3)
    self.FeatureClass = RangeBearingEKF
    super(FastSLAM, self).__init__(dt, state_gen, num_particles)

  def fwd_ctl_model(self, (v,w), (x,y,theta), dt):
    # Multiplicative gaussian noise like Marec suggested, s.t. v=0 has no noise.
    randv = v * random.normal(1.0, 0.4)
    randw = w * random.normal(1.0, 0.4)
    newx = x + randv * dt * cos(theta + dt * randw / 2.0)
    newy = y + randv * dt * sin(theta + dt * randw / 2.0)
    newtheta = wrap_radians(theta + randw * dt)
    return array([newx, newy, newtheta])
  
