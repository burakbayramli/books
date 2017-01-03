#!/usr/bin/env python
# -*- coding: utf-8 -*-
from __future__ import division as __DIVISION
import numpy as N

class ParticleFilter(object):
  """Implements a generic particle filter, which can be easily extended to track
  a probability distribution, such as the location of a robot.

  For further details, see the algorithm description in:
  S. Thrun et al., "Probabilistic Robotics", 1st Ed., p.98, Table 4.3
  """
  def __init__(self, particle_type, state_gen, num_particles=100):
    """Returns a new ParticleFilter instance.

    particle_type -> particle datatype, eg: [('x', double, 3), ('w', double)]
    state_gen     -> Generator function for (u, [(ci0, zi0), (ci1, zi1), ...])
    num_particles -> Number of particles to generate"""
    self.particle_type = particle_type            # Datatype for particles
    self.state_generator = state_gen              # Source for dt, u, z
    self.M = num_particles                        # Number of particles
    self.Y = N.zeros(self.M, self.particle_type)  # Create initial particles
    self.Y['w'] = 1.0 / self.M                    # Initially equally probable

  def setall(self, *args):
    """Set the state of all particles to the same value.  Helpful for
    initializing the filter so that it will line up with ground truth."""
    self.Y['x'] = args

  def run(self):
    """Generator function to step Particle Filter.  Iterate over the return value
    of this function -- each iteration will step the filter once, and return the
    set of particles.  Returns a generator instance."""
    for dt, ctl_input, measurements in self.state_generator:
      # Update each particle
      for particle in self.Y:
        self.update_particle(particle, ctl_input, measurements, dt)
      # Resample
      self.resample()
      yield self.Y

  def update_particle(self, particle, ctl_input, measurements, dt):
    """Updates a single particle by sampling from the new distribution, and
    generates a new particle weight based on the likelihood of the measurements
    received.

    particle     -> A single particle to be updated
    ctl_input    -> The control inputs
    measurements -> A list of correspondence + measurement tuples, like:
                    [(c0,z0), (c1,z1), ...]
    dt           -> The length of this time step"""
    # Sample new Y from the new expected distribution
    particle['x'] = self.fwd_ctl_model(ctl_input, particle['x'], dt)
    # Weight based on likelyhood of all measurements
    for ci, zi in measurements:
      particle['w'] *= self.measurement_prob(zi, particle['x'])

  def resample(self):
    """Generates a new sets of particles based on the current particle
    distribution.  Heavily weighted particles are more likely to be resampled
    than particles with low weights."""
    # Sort descending, so that more likely particles are first (for performance)
    self.Y = N.sort(self.Y[:], order='w')[::-1]
    # Calculate cumulative probability
    cumW = N.cumsum(self.Y['w'])
    cumW /= cumW[-1]
    # Pick random indices into Y based on weighting
    ii = cumW.searchsorted(N.random.random(self.M))
    self.Y = self.Y[ii]
    # Each particle is now equally probable
    for particle in self.Y:
      particle['w'] = 1.0

  def fwd_ctl_model(self, ctl_input, state, dt):
    """Returns a sample from the expected new state distribution, given the
    current state and control inputs.  This function must be overridden in
    a deriving class."""
    raise NotImplementedError("fwd_ctl_model not overridden!")

  def measurement_prob(self, measurement, state):
    """Returns the probability of a certain measurement, given the state.  This
    function must be overridden in a derived class."""
    raise NotImplementedError("measurement_prob overridden!")
