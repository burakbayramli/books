# Finite Element Modeling with Abaqus and Python for Thermal and
# Stress Analysis
# (C)  2017, Petr Krysl
#

"""
Make some useful material models for use in the classroom.
"""

from abaqus import *
from abaqusConstants import *
import material

# Create a material.

TheModel = getInput('Enter the model name:')


TheMaterial = 'Steel (mm)'
Description = 'Mechanical and thermal properties of steel, units: SI (mm)'
mdb.models[TheModel].Material(name=TheMaterial, description=Description)
mdb.models[TheModel].materials[TheMaterial].Elastic(table=((209000, 0.3), ))
mdb.models[TheModel].materials[TheMaterial].Density(table=((7.850e-9, ), ))
mdb.models[TheModel].materials[TheMaterial].Expansion(table=((13.0e-6, ), ))
mdb.models[TheModel].materials[TheMaterial].Conductivity(table=((45.0e-3, ), ))

TheMaterial = 'Steel (m)'
Description = 'Mechanical and thermal properties of steel, units: SI (m)'
mdb.models[TheModel].Material(name=TheMaterial, description=Description)
mdb.models[TheModel].materials[TheMaterial].Elastic(table=((209000e6, 0.3), ))
mdb.models[TheModel].materials[TheMaterial].Density(table=((7.850e3, ), ))
mdb.models[TheModel].materials[TheMaterial].Expansion(table=((13.0e-6, ), ))
mdb.models[TheModel].materials[TheMaterial].Conductivity(table=((45.0, ), ))
mdb.models[TheModel].materials[TheMaterial].SpecificHeat(table=((1200., ), ))

TheMaterial = 'Aluminum 6061-T6 (mm)'
Description = 'Mechanical and thermal properties of aluminum, units: SI (mm)'
mdb.models[TheModel].Material(name=TheMaterial, description=Description)
mdb.models[TheModel].materials[TheMaterial].Elastic(table=((70000, 0.35), ))
mdb.models[TheModel].materials[TheMaterial].Density(table=((2.700e-9, ), ))
mdb.models[TheModel].materials[TheMaterial].Expansion(table=((23.4e-6, ), ))
mdb.models[TheModel].materials[TheMaterial].Conductivity(table=((180.0e-3, ), ))

TheMaterial = 'Aluminum 6061-T6 (m)'
Description = 'Mechanical and thermal properties of aluminum, units: SI (m)'
mdb.models[TheModel].Material(name=TheMaterial, description=Description)
mdb.models[TheModel].materials[TheMaterial].Elastic(table=((70000e6, 0.35), ))
mdb.models[TheModel].materials[TheMaterial].Density(table=((2.700e3, ), ))
mdb.models[TheModel].materials[TheMaterial].Expansion(table=((23.4e-6, ), ))
mdb.models[TheModel].materials[TheMaterial].Conductivity(table=((180.0, ), ))
mdb.models[TheModel].materials[TheMaterial].SpecificHeat(table=((900., ), ))

TheMaterial = 'Polyurethane (m)'
Description = 'Thermal properties of dense polyurethane foam, units: SI (meter)'
mdb.models[TheModel].Material(name=TheMaterial, description=Description)
mdb.models[TheModel].materials[TheMaterial].Conductivity(table=((0.07, ), ))

TheMaterial = 'Concrete (m)'
Description = 'Thermal properties of concrete, units: SI (meter)'
mdb.models[TheModel].Material(name=TheMaterial, description=Description)
mdb.models[TheModel].materials[TheMaterial].Conductivity(table=((1.8, ), ))
mdb.models[TheModel].materials[TheMaterial].SpecificHeat(table=((880.0, ), ))
mdb.models[TheModel].materials[TheMaterial].Density(table=((2350.0, ), ))

TheMaterial = 'Orthotropic uniaxial FRC (m)'
Description = 'Orthotropic uniaxial FRC from Zenkour (2007), units: SI (mm)'
mdb.models[TheModel].Material(name=TheMaterial, description=Description)
mdb.models[TheModel].materials[TheMaterial].Elastic(\
          table=((172350.0, 6894.0, 6894.0, 0.25, 0.25, 0.25, 3447.0, 3447.0, 1378.0), ), \
                type=ENGINEERING_CONSTANTS)