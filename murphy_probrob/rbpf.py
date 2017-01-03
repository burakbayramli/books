#!/usr/bin/env python
# -*- coding: utf-8 -*-
from __future__ import division as __DIVISION
import numpy as N
import copy

import pf

class Naive(pf.ParticleFilter):
  """Implements a naive Rao-Blackwellized particle filter, which can be easily
  extended to track probability distributions, such as the location of a robot
  and features.  Note that the FastSLAM implementation below will perform
  significantly better, and this is mainly here for comparison purposes.

  For further details, see the algorithm description in:
    S. Thrun et al., "Probabilistic Robotics", 1st Ed., p.450, Table 13.1"""
  def __init__(self, state_type, state_gen, num_particles=20):
    """Returns a new Naive RBPF instance.
    state_type    -> datatype for the particle 'state' portion, eg: (double, 5)
    state_gen     -> Generator function for (u, [(ci0, zi0), (ci1, zi1), ...])
    num_particles -> Number of particles

    NOTE: This constructor differs from the ParticleFilter constructor.  The
          datatype provided in this case should only be for the particle
          state."""
    particle_type = N.dtype([('x', state_type),
                             ('w', N.double),
                             ('features', object),
                             ('x_hist', object)]) # Datatype for each particle
    super(Naive, self).__init__(particle_type, state_gen, num_particles)
    for row in self.Y:
      # Initialize all empty feature dict's
      row['features'] = {}
      row['x_hist'] = N.array([], dtype=state_type)
  
  def update_particle(self, particle, ctl_input, measurements, dt):
    """Updates a single particle by first sampling from the new distribution,
    then updating each feature estimate for which measurements were received.
    The likelihoods of each measurement are used to generate a new particle
    weight.

    particle     -> A single particle to be updated
    ctl_input    -> The control inputs
    measurements -> A list of correspondence + measurement tuples, like:
                    [(c0,z0), (c1,z1), ...]
    dt           -> The length of this time step"""
    # Sample new X from the new expected distribution
    particle['x'][:] = self.fwd_ctl_model(ctl_input, particle['x'], dt)
    # Store the new state in the particle history for later plotting.
    particle['x_hist'] = N.vstack((particle['x_hist'], particle['x']))

    # Weight based on likelyhood of all measurement
    for ci, zi in enumerate(measurements):
      if N.isnan(zi).any():
        continue
      if ci not in particle['features']: # If this is a new feature
        ftr = self.FeatureClass.from_measurement(ci, particle['x'], zi)
        particle['features'][ci] = ftr
      else:                              # Already being tracked
        feature = particle['features'][ci]
        # Update each feature, and reweight the particle.
        particle['features'][ci], w = self.update_feature(feature,
                                                          particle['x'], zi)
        particle['w'] *= w
        
  def update_feature(self, feature, state, zi):
    """Updates a single feature.  Returns the feature, and the likelihood
    weight for this measurement."""
    # Abstracting this is silly, but allows for overloading in FastSlam below.
    return feature, feature.update(state, zi)

  def resample(self):
    """Generates a new sets of particles based on the current particle
    distribution.  Heavily weighted particles are more likely to be resampled
    than particles with low weights."""
    super(Naive, self).resample()
    # Create new (independent) copies of feature dictionaries and features.
    for particle in self.Y:
      particle['features'] = copy.deepcopy(particle['features'])

class FastSLAM(Naive):
  """Implements a Rao-Blackwellized particle filter using some speed
  improvements suggested by the FastSLAM 1.0 algorithm.  This filter can be
  extended to track probability distributions, such as the location of a robot
  and features.

  Note: this implementation currently uses a hash table for feature lookup
  instead of a balanced binary tree.

  For further details, see the algorithm description in:
    S. Thrun et al., "Probabilistic Robotics", 1st Ed., p.450, Table 13.1
  and the description of the efficient implementation on p.460."""

  def update_feature(self, feature, state, zi):
    """Duplicates and then updates a single feature.  Returns the new feature,
    and the likelihood weight for this measurement.  Duplication here allows
    the resampling process to perform shallow copies instead of deep copies."""
    ftr = copy.deepcopy(feature)
    return ftr, ftr.update(state, zi)

  def resample(self):
    """Generates a new sets of particles based on the current particle
    distribution.  Heavily weighted particles are more likely to be resampled
    than particles with low weights.  Feature trackers (e.g. EKF's) will not be
    duplicated, as FastSLAM performs that duplication during the update step."""
    pf.ParticleFilter.resample(self)
    for particle in self.Y:
      # Create new independent copies of feature dictionaries.
      particle['features'] = particle['features'].copy()
