#
import math
from steelpy import CodeCheck
from steelpy import Units
from steelpy import Materials
from steelpy import Sections
#
units = Units()
code = CodeCheck()
#
#
#
Pp = 100 * units.bar
Ps = 113 * units.bar
#
PipeTemp = 0*(50 + 273.15) * units.K
#
# Gas Lift Piping Section
D_gl = 30 * units.mm
t_gl = 3/4 * units.inch
section = Sections()
section["TUB"] = ['Tubular', D_gl, t_gl]
#
#
SMYS = 170.0 * units.MPa
SMTS = 485.0 * units.MPa
material = Materials()
material["X65"] = ['elastic', SMYS, SMTS]
#
#
## Pressure on Flange
Df = 12 * units.cm
## Load on flange
rad = D_gl.value / Df.value
Af = math.pi * Df**2 / 4.0 * (1 - rad**2)
Ff = Pp * Af
#
f_tilt = 3 * units.degrees
Fx = Ff * math.cos(f_tilt.convert('radian').value)
Fy = Ff * math.sin(f_tilt.convert('radian').value)
Bm = Fy * 55 * units.mm
#
print(f"Flange Area   = {Af.convert('centimetre^2').value: 1.2f} cm^2")
print(f"Force pressure  = {Ff.convert('newton').value: 1.2f} N")
print(f"Pipe Axial Force  = {Fx.convert('newton').value: 1.2f} N")
print(f"Pipe Shear Force  = {Fy.convert('newton').value: 1.2f} N")
print(f"Pipe Bending Moment  = {Bm.convert('newton*metre').value: 1.2f} N-m")
#
##
## =====================
##
pipegl = code.pipe
pipegl.design_code("PD8010 Part 1")
pipegl.design_data(design_condition = "operating", 
                  design_method = 'stress')
# ----------------------
# Pipe details
#
pipegl.pipe_data(pipe_type = "Above Ground", 
                pipe_name="PPM1_gaslift",
                pipe_restraint=True,
                pipe_history = True)
#
pipegl.section = section["TUB"]
pipegl.pipe_section(tcorr = 0.0*units.mm, 
                    tol = 0.08, fo = 0.025)
#
# ----------------------
# 4.- Material Data (N/mm2)
#
pipegl.material = material["X65"]
#pipegl.material(SMYS = 170.0 * units.MPa, 
#                SMTS = 485.0 * units.MPa,
#                E = 207000.0 * units.MPa,
#                alpha = (1.170E-05 + 1/273.15 ) / units.K)
#                #alpha = 1.170E-05 / units.celsius)
#Fu = 530.0

#pipegl.material_derate(material_type="CMn",
#                      derate_code="DNV",
#                      Tmax=60.0)
# ----------------------
# 6.- Design Pressure (N/mm2)
#
pipegl.design_pressure(Pi = Pp, Po = Ps)
#
# ----------------------
# 7.- Design Temperature in degrees Celsius (C)
#
pipegl.design_temperature(T1 = 0 * units.K, 
                          T2 = PipeTemp)
#
# ----------------------
# 8.- Forces from Analysis
#
#pipegl.enviromental_load(Px = Fx,  # [-ve Compression] (N)
#                         Vip = Fy, # (N)
#                         BMip = Bm)  # (N/mm)
#
pipegl.functional_load(Fa = Fx,  # [-ve Compression] (N)
                       Fs = Fy, # (N)
                       Mb = Bm, # (N*m)
                       T = 0 * units.N * units.m)  
#
#
pipegl.set_substance_category("E")
#
# ----------------------
# Perform Calcs & Print Resuls
#
pipegl.get_results()
#Pipe2.print_results()
#
#
#
#
#
#
#
print('end')