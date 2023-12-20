#
import math
#
from steelpy import Formulas
from steelpy import Units
from steelpy import Materials
from steelpy import Sections
#
formulas = Formulas()
units = Units()
#
# -----------------------------------
# Define sections
section = Sections()
#
Dp = 219.10 * units.mm
tp = 14.3 * units.mm
section = Sections()
section["TUB"] = ['Tubular', Dp, tp]
#
# -----------------------------------
# define material
SMYS = 448 * units.MPa
SMTS = 517 * units.MPa
E = 207000.0 * units.MPa
alpha = (1.170E-05 + 1/273.15 ) / units.K
#
material = Materials()
material["X65"] = ['elastic', SMYS, SMTS]
#
# Input Element Identification
#BeamID = 'Bm2129'
#
# Select Section Type 
# Sections currently supported:
# I, RECTANGULAR BAR, SOLID CIRCLE, TUBULAR, TEE, CHANNEL & BOX'
#SectionType = "rectangular bar"
#SectionType = "i"
#
#
#
#
#roak_case = 12
W = 40.5 * units.kN
phase = 180 * units.deg
theta = (180-15.893) * units.deg
print(f'theta : {math.degrees(theta.value)}')
phi = 0 * units.deg
#ring1.case(roak_case, apld, theta, phi, phase)
#
# load[name] = [case, load, theta, phi (case 18), phase]
### Input Data
#ring1 = formulas.ring(72)
#ring1.material = material["X65"]
#ring1.geometry = section["TUB"]
##ring1.load[1] = {'case':1, 'W':W, 'phase':90*units.deg}
#ring1.load[1] = [1, W] # , 90*units.deg
###ring1.load[1].view()
###
#forces = ring1.radial_forces
##forces[1].printout()
##forces[1].plot()
###
#stress = ring1.radial_stress
#stress.printout()
###
#
#
#section["TUB2"] = ['Tubular', 10*units.inch, 1.5*units.inch]
ring2 = formulas.ring(72)
ring2.material = material["X65"]
ring2.geometry = section["TUB"]
# 
#ring2.load[4] = {'case':1, 'W':W} # 'phase':90*units.deg
ring2.load[4] = [2, 10*units.kN, 45 * units.deg, 0* units.deg]
#ring2.load[4] = [18, W, 10 * units.deg, 20* units.deg, 90* units.deg]
#ring2.load[4] = {'case':18, 'load':W, 
#                 'theta':10 * units.deg, 
#                 'phi':20* units.deg, 
#                 'phase':90* units.deg}
#
#ring2.load[4] = [17, 1000 * units.kg / units.m**3, 45* units.deg, 90* units.deg]
#
#ring2.load[4] = [15, W]
#
#ring2.load[2] = [2, W, 90 * units.deg, 90 * units.deg]
#ring2.load[2].view()
#forces = ring2.radial_forces
#forces[2].printout()
#phi = 65 * units.deg
#W = 10000 * units.lbf
#ring2.load[4] = [4, W, phase*0, 45 * units.deg]
#ring2.load[4] = [7, W, phase*0, 5 * units.deg]
#ring2.load[4] = [19, W, phase*0, theta*0]
#ring2.load[5] = [20, W, phase*0]
#ring2.load[88] = [12, W*-0.50, phase, theta]
#ring2.load[12] = [12, W*0.50, phase, theta]
#
#
#forces2 = ring2.radial_forces
#forces2[8].plot_radial()
##forces2[12].plot_radial()
#forces2[8].printout()
##forces2[12].printout()
#
## Printing Results
force = ring2.force_combined()
#force.printout()
Mmax = force.Mmax * units.N * units.m
print(f'Mmax = {Mmax.convert("lbf*inch").value : 1.3e} lbf-inch')
#
Nmax = force.Nmax * units.N
print(f'Nmax = {Nmax.convert("lbf").value : 1.3e} lbf')
#
Vmax = force.Vmax * units.N
print(f'Vmax = {Vmax.convert("lbf").value : 1.3e} lbf')
force.plot(x='x', y='M')
#force.plot_radial()
force.plot_force(column="M")
#
stress = ring2.radial_stress()
stress.printout()
##ring1.print_results()
#
print('-->')
#