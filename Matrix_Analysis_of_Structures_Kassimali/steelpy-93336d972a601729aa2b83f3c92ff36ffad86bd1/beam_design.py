from steelpy import Beam #, SimpleBeam
from steelpy import Units
#
import matplotlib.pyplot as plt

# set units
units = Units()
#
#
#
# beam class
beam = Beam(name='beam_test')
beam.L = 4*units.m
#
# set section
#beam.section = ['Rectangle', 350 * units.mm, 200 * units.mm]
#beam.section = ['Circular', 500 * units.mm]
#beam.section = ['Tubular', 500 * units.mm, 25 * units.mm]
beam.section = ['ub', 254.10*units.mm, 8.6*units.mm, 254.6*units.mm, 14.20*units.mm, 12.7*units.mm]
#beam.section = ['box', 500 * units.mm, 6 * units.mm, 250 * units.mm, 12 * units.mm]
#beam.section = ['channel', 250 * units.mm, 6 * units.mm, 150 * units.mm, 12 * units.mm]
#beam.section = ['tee', 150 * units.mm, 6 * units.mm, 150 * units.mm, 6 * units.mm]
#beam.section = ['Angle', 150 * units.mm, 6 * units.mm, 150 * units.mm, 6 * units.mm]
#
#print(beam.section)
#
#
beam.material = ['elastic', 275.0 * units.MPa]
print(beam.material)
#
#
# set supports : pinned/fixed/free/guided/spring [k=F/x]
#
beam.support = ["pinned", "pinned"]
#
#beam.support[1] = "pinned"
#beam.support[1] = ["spring", 200 * units.kN/units.m]
#
#beam.support[2] = "pinned"
#beam.support[2] = ["spring", 200 * units.kN/units.m]
#
#
beam.selfweight = [0, -1* units.gravity, 0] #* units.gravity
#
# set loading
#     point = [L1, Fx, Fy, Fz]
beam.P = [2.0*units.m, 0*units.N, 100*units.kN, 0*units.N,
          7.50*units.kN*units.m, 0*units.kN*units.m,  0*units.kN*units.m, 
          'example_1']

#beam.P = [3.0*units.m, 0*units.N, 0*units.N, 0*units.N,
#          0*units.kN*units.m, 200*units.kN*units.m, 300*units.kN*units.m,
#          'point_2']
#
#beam.P = {'L1':2.5*units.m, 'fy':100*units.N, 'name': 'point_3'}
#beam.load.point = [2.5*units.m, 1000*units.N, 2000*units.N]
#
#beam.load.point = [2*units.m, 18*units.kN, 18*units.kN]
#beam.q = [10*units.N/units.m,  -500*units.N/units.m, 0*units.N/units.m,
#          10*units.N/units.m, -1000*units.N/units.m, 0*units.N/units.m,
#          1*units.m, 1*units.m, 'udl_1']
#
#     line = [qy1,qz1, qy2,qz2, L1,L2]
#beam.load.line = [50*units.kN/units.m, 50*units.kN/units.m,
#                     100*units.kN/units.m, 100*units.kN/units.m,
#                     1*units.m, 1*units.m]
#beam.q = {'qy1':50*units.kN/units.m, 'qy2':50*units.kN/units.m,
#          #'qz1':100*units.kN/units.m, 'qz2':100*units.kN/units.m,
#          'L1':1*units.m, 'L2':1*units.m, 'name': 'udl_2'}
#
#
beam.q = {'qy1':0.97*units.kN/units.m, 'qy2':0.97*units.kN/units.m,
          'name': 'selfweight'}
#
#beam.load[1].line = {'qy1':9*units.kN/units.m, 'qz1':-9*units.kN/units.m}
#
print(beam.load)
#
#
#beam.load_combination = ['udl_1', 0.80]
#beam.load_combination = ['point_1', 0.85]
#
#beam.load_combination = {'point_1': 0.50, 'point_2': 0.75, 'point_3': 1.0}
#
#
# beam results
#
reactions = beam.reactions()
print(reactions)
#
#
forces = beam.response()
print(forces)
#
#
force_grp = (forces.groupby(['load_name', 'load_type',
                             'load_system','element_name' ,'node_end'])
             [['Fx', 'Fy', 'Fz', 'Mx', 'My', 'Mz', 'B', 'Tw']].sum())

force_grp = force_grp.reset_index(names=['load_name', 'load_type',
                                         'load_system', 'element_name' ,'node_end'])
#
#force_grp.plot.line(x='node_end', y='Fx')
#force_grp.plot.line(x='node_end', y='Fy')
#force_grp.plot.line(x='node_end', y='Fz')
#force_grp.plot.line(x='node_end', y='Mx')
#force_grp.plot.line(x='node_end', y='My')
#force_grp.plot.line(x='node_end', y='Mz')
#force_grp.plot.line(x='node_end', y='B')
#force_grp.plot.line(x='node_end', y='Tw')
#plt.show()
#
stress = beam.stress()
print(stress)
#
#
#stress.plot.line(x='node_end', y='tau_y')
#stress.plot.line(x='node_end', y='sigma_x')
#stress.plot.line(x='node_end', y='sigma_z')
#plt.show()
#
#data = beam.shear()
#data = beam.bending_moment()
#beam.bending_moment.plot("in_plane")
#beam.shear.plot("in_plane")
#beam.deflection.plot("in_plane")
#beam.slope.plot("in_plane")
#
#beam.bending_moment.plot("out_plane")
#beam.shear.plot("out_plane")
#beam.deflection.plot("out_plane")
#beam.slope.plot("out_plane")
#
# Design
#design = beam.design
#design.API()
#design.print_results()
#print(design)
#
#
#
#
print("End")
