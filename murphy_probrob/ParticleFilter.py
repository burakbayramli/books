#!/usr/bin/env python
# -*- coding: utf-8 -*-
from numpy import *
import pylab
from pf import *

"""
Test simple particle filter.
"""


INVSQRT2PI = 1/sqrt(2.0*pi)
def normal_pdf(x, mu=0.0, sigma=1.0):
  return INVSQRT2PI * exp(-((x-mu)**2 / (2 * sigma**2))) / sigma

class PFTest(ParticleFilter):
  def __init__(self, *args):
    super(PFTest, self).__init__([('x', float64), ('w', float64)], *args)

  def fwd_ctl_model(self, ctl_input, state, dt):
    return random.normal(state + ctl_input, 0.2)

  def measurement_prob(self, measurement, state):
    return normal_pdf(state-measurement, 0, 0.5)

if __name__ == "__main__":
  def state_generator():
    line = None
    x = 0
    while True:
      u = array([0.1])
      x += random.normal(u, 0.2)
      z = [[0, x]]
      yield 0.1, u, z
      if line is None:
        line = pylab.axhline(x, color='b')
      else:
        line.set_ydata(x)

  dots = None
  pylab.ion()
  for X in PFTest(state_generator(), 20).run():
    if dots is None:
      dots, = pylab.plot(X[:]['x'], 'r.')
      pylab.ylim(-2,10)
      pylab.draw()
    else:
      dots.set_ydata(X[:]['x'])
      pylab.draw()
