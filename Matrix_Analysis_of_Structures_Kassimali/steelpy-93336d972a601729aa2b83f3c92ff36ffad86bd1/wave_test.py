#!/usr/bin/env python
# coding: utf-8
#
import matplotlib.pyplot as plt

from steelpy import Metocean
from steelpy import Units


meto = Metocean()
units = Units()
#
# Regular wave
#
waveReg = meto.regular_wave()
#
#
# Fourier
#
fourier = waveReg.Fourier()
#fourier.mean_current.Euler = 0.31 * units.m/units.sec
# [H, T, d]
fourier['100yrs'] = {'Hw':15.0 * units.m, 'Tw':12.0 * units.sec, 'd':100*units.m}
Lf = fourier['100yrs'].L
print(f'Wave length = {Lf: 1.4e} m')
surface = fourier['100yrs'].surface(surface_points=18)
#surface.plot()
#print(surface)
#
kinematic = fourier['100yrs'].kinematics(depth_points=10)
#kinematic.plot()
#print(kinematic)
#
BS, OTM = meto.pile_response(D=1*units.m, L=100*units.m,
                             kinematic=kinematic)
# get maximum
BSgrp = BS.groupby(['length'])['BS'].sum()
BSgrp.plot(kind="line",
           xlabel='Wave length [m]', ylabel='BS [N]')
plt.show()
#
#
##
#waveReg['100yrs'] = {'Hw':15.0 * units.m, 'Tw':12.0 * units.sec, 'd':100*units.m}
##waveReg['100yrs'] = [15.0 * units.m, 12.0 * units.sec, 100*units.m]
#surface = waveReg['100yrs'].surface(surface_points=18)
#surface.printout()
##surface.plot()
##
#kinematic = waveReg['100yrs'].kinematics(depth_points=10)
#print(kinematic._data['u'].max())
#kinematic.printout()
##kinematic.plot()
##kinematic.plot_vectorfield()
##
#meto.pile_response(D=1*units.m, kinematic=kinematic)
##
# -------------------------------------------------------------------
#
# Stokes
#
stokes5 = waveReg.Stokes()
#stokes5.mean_current.Euler = 0.31 * units.m/units.sec
#stokes5.infinite_water_depth
# [H, T, d]
stokes5['100yrs'] = {'Hw':15.0 * units.m, 'Tw':12.0 * units.sec, 'd':100*units.m}
Ls = stokes5['100yrs'].L
#print(f'Wave length = {Ls: 1.4e} m')
#surface = stokes5['100yrs'].surface(surface_points=18)
#surface.plot(phase=True)
#print(surface)
#
kinematic = stokes5['100yrs'].kinematics(depth_points=10)
#print(kinematic)
#kinematic.plot()
#kinematic.plot_vectorfield()
#pressure = kinematic.pressure
phi = kinematic.phi
#
BS, OTM = meto.pile_response(D=1*units.m, L=100*units.m, 
                             kinematic=kinematic)
# get maximum
BSgrp = BS.groupby(['length'])['BS'].sum()
BSgrp.plot(kind="line",
           xlabel='Wave length [m]', ylabel='BS [N]')
plt.show()
#
#
print('-->')