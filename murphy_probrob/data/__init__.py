#!/usr/bin/env python
# -*- coding: utf-8 -*-
import os, sys
import numpy as __N
import pylab as __P
import matplotlib

def __load_dataset1(start, end, valid_beacons=None):
  """Load and Format Data"""
  data_directory = os.path.dirname(__file__)
  raw = __N.array(matplotlib.mlab.load(os.path.join(data_directory, 'ds1-data.dat')))
  cols = __N.dtype([('t', 'f8'), ('dt', 'f8'), ('v', 'f8'), ('w', 'f8'),
                    ('z', 'f8', (2,32)), ('true', 'f8', 3), ('odom', 'f8', 3)])

  if end:
    data = __N.ndarray(end - start, dtype=cols)
  else:
    data = __N.ndarray(raw.shape[0] - start, dtype=cols)

  data['t'][:] = raw[start:end,0]
  data['odom'][:] = raw[start:end,1:4]
  data['odom'][:,2] += __N.pi/2.
  data['true'][:] = raw[start:end,4:7]
  data['true'][:,2] += __N.pi/2.
  data['z'][:] = raw[start:end,7:].reshape(-1, 2, 32)
  data['z'][:,1,:] -= __N.pi/2.
  if valid_beacons is not None:
    #zeroed = [i for i in range(32) if i not in [3, 10, 20, 25, 29, 30]]
    #zeroed = [i for i in range(32) if i not in [5, 22, 29, 30]]
    NANed = [i for i in range(32) if i not in valid_beacons]
    data['z'][:,:,NANed] = __N.NaN # Use this line to NAN-out unused beacons
  
  data['dt'][:-1] = __N.diff(data['t'], axis=0)
  diffodo = __N.diff(data['odom'], axis=0)
  data['v'][:-1] = __N.sqrt(__N.sum(diffodo[:,:2]**2, axis=1)) / data['dt'][:-1]
  data['w'][:-1] = diffodo[:,2] / data['dt'][:-1]
  data['v'][-1] = data['w'][-1] = 0
  data['dt'][-1] = 0.1
  ## Some of the groundtruth has NaN's (WHY!?!?) -- remove it.
  data = data[~__N.any(__N.isnan(data['true']), axis=1)]
  beacons = matplotlib.mlab.load(os.path.join(data_directory, 'ds1-beacon.dat'))

  return data, beacons

__DS1_START   = 230
__DS1_END     = 3000
#__DS1_BEACONS = [5, 22, 29, 30]
__DS1_BEACONS = __N.uint16(__N.arange(0,31,2.5).round())

ds1, ds1_beacons = __load_dataset1(__DS1_START, __DS1_END, __DS1_BEACONS)

