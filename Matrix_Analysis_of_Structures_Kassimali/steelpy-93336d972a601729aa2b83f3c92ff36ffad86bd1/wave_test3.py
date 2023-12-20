#!/usr/bin/env python
# coding: utf-8
#
#import matplotlib.pyplot as plt

from steelpy import f2uModel
from steelpy import Metocean
from steelpy import Trave2D, Trave3D
from steelpy import Units
#
#
units = Units()
#
# ----------------------------------------------------
#
f2umodel = f2uModel(component="wave_test3")
#
mesh = f2umodel.mesh()
#
# ----------------------------------------------------
# Material input
# ----------------------------------------------------
#
material = mesh.materials()
matlinear = material.linear()
matlinear[1] = [345.0 * units.MPa]
print(material)
#
#
# ----------------------------------------------------
# Section Input
# ----------------------------------------------------
#
section = mesh.sections()
section[2] = ['Tubular', 500 * units.mm, 25 * units.mm]
print(section)
#
# ----------------------------------------------------
# Mesh Model
# ----------------------------------------------------
#
#
# Node input
nodes = mesh.nodes()
nodes[10] = [0 * units.m, -100* units.m, 0 * units.m]
nodes[20] = [0 * units.m, -75 * units.m, 0 * units.m]
nodes[30] = [0 * units.m, -50 * units.m, 0 * units.m]
nodes[40] = [0 * units.m, -25 * units.m, 0 * units.m]
nodes[50] = [0 * units.m,  0 * units.m, 0 * units.m]
nodes[60] = [0 * units.m,  25* units.m, 0 * units.m]
nodes[70] = [0 * units.m, 50* units.m, 0 * units.m]
print(nodes)
#
#
# boundary Input
boundary = mesh.boundaries()
#
supports = boundary.nodes()
supports[10] = 'fixed'
supports[20] = 'pinned'
supports[30] = 'pinned'
supports[40] = 'pinned'
#supports[50] = 'pinned'
supports[60] = 'pinned'
supports[70] = 'fixed'
print(boundary)
#
#
# Element input
#
#
elements = mesh.elements()
#
beams = elements.beams()
# beam[number] = [material, section, node1, node2, roll_angle]
beams[12] = [10, 20, 1, 2, 0]
beams[23] = [20, 30, 1, 2, 0]
beams[34] = [30, 40, 1, 2, 0]
beams[45] = [40, 50, 1, 2, 0]
beams[56] = [50, 60, 1, 2, 0]
beams[67] = [60, 70, 1, 2, 0]
#
print(elements)
#
#
# ----------------------------------------------------
# Metocean 
# ----------------------------------------------------
#
meto = Metocean()
#
# ----------------------------------------------------
# hydrodynamic parameters input
# ----------------------------------------------------
#
#
#cdcm = meto.CdCm()
# ['coefficients', Cd, Cm]
#cdcm['cdcm_default'] = ['coefficients', 0.70, 2.0]
#cdcm['cdcm_default'].elements = [12, 23]
#
#
#
mg = meto.MG()
#
mg['MG_1'] = meto.rho_w * 1.35
mg['MG_1'].profile = [[ 10 * units.m, 60 * units.mm],
                      [ 00 * units.m, 60 * units.mm],
                      [-10 * units.m, 30 * units.mm],
                      [-50 * units.m, 10 * units.mm],
                      [-100 * units.m, 0 * units.mm]]
#
#
#
current = meto.current()
#
# [velocity_top, velocity_bottom, profile (linear/exponential)]
current['current_1'] = 0*1.54 * units.m/units.sec # [1.5 * units.m/units.sec, 0.10 * units.m/units.sec]
# profile [elevation, factor]
current['current_1'].profile = [[  5 * units.m, 1.0],
                                [-10 * units.m, 1.0],
                                [-30 * units.m, 0.70],
                                [-60 * units.m, 0.50],
                                [-80 * units.m, 0.20]]
#
# ----------------------------------------------------
# Regular wave [Stokes/Fourier/Cnoidal]
# ----------------------------------------------------
#
wave = meto.wave()
#
# -------------------------------------------------------------------
#
# Stokes
regwave = wave.regular()
#
#wave = regwave.Stokes()
#wave = regwave.Fourier()
#wave.mean_current.Euler = 0.31 * units.m/units.sec
#wave.infinite_water_depth
# [H, T, d]
#wave['100yrs'] = {'Hw':15.0 * units.m, 'Tw':12.0 * units.sec, 'd':100*units.m}
# 
# [Hw, Tw, d, wave_type(Fourier/Stokes)]
regwave['100yrs'] = [15*units.m, 12.0*units.sec, 100*units.m, 'Stokes']
Ls = regwave['100yrs'].L
print(f'Wave length = {Ls: 1.4e} m')
#surface = regwave['100yrs'].surface(surface_points=18)
#surface.plot(phase=True)
#print(surface)
#
#kinematic = regwave['100yrs'].kinematics()
#print(kinematic)
#kinematic.plot()
#kinematic.plot_vectorfield()
#pressure = kinematic.pressure
#phi = kinematic.phi
#
#BS, OTM = meto.pile_response(D=1*units.m, L=100*units.m, 
#                             kinematic=kinematic)
# get maximum
#BSgrp = BS.groupby(['length'])['BS'].sum()
#BSgrp.plot(kind="line",
#           xlabel='Wave length [m]', ylabel='BS [N]')
#plt.show()
#
#meto.get_load(mesh=mesh, kinematic=kinematic,
#              condition=2)
#
metload = meto.load()
# [title, marine_growth, Buoyancy(False/True), conductor_shielding]
metload['sea_1'] = ['storm_0deg', 'MG_1']
# wave =[wave_name, Direction(deg), Kinematics, crest_elevation, title]
metload['sea_1'].wave = ['100yrs', 0.0, 0.95]
# current [current_name,  Direction(deg), Blockage, Stretching, title]
metload['sea_1'].current = ['current_1', 0.0, 0.85, True, 'current_test']
# wind [wind_name, Direction(deg), title]
metload['sea_1'].wind = ['wind_1', 0.0, 'wind_test']
#
#
#
#
# ----------------------------------------------------
# Basic Load
#
# loading
load = mesh.load()
#
# ----------------------------------------------------
# Basic Load
basic = load.basic()
#
#
basic[10] = 'wave load'
basic[10].wave(wave_load=metload['sea_1'],
               design_load='max_BS')
#
# ----------------------------------------------------
# Meshing input
# ----------------------------------------------------
#
mesh.build()
#
#
# ----------------------------------------------------
# Structural Analysis
# ----------------------------------------------------
#
frame = Trave2D()
frame.mesh = mesh
frame.static()
results = frame.solve()
print(results)
#
print('-->')