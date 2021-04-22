# Finite Element Modeling with Abaqus and Python for Thermal and
# Stress Analysis
# (C)  2017, Petr Krysl
#

"""
Make some useful Python macros for use in the classroom.
"""

from abaqus import *
from abaqusConstants import *
import __main__

def MakeUsefulMaterials():
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
                   
    TheMaterial = 'AS4D/9310 carbon/epoxy SI (mm)'
    Description = 'Lamina AS4D/9310 carbon/epoxy from Barbero 2013 book, units: SI (mm)'
    mdb.models[TheModel].Material(name=TheMaterial, description=Description)
    mdb.models[TheModel].materials[TheMaterial].Elastic(type=LAMINA, table=((
        133860.0, 7706.0, 0.301, 4306.0, 4306.0, 2760.0), ))
                    
def MakeCylinderPart():
    import section
    import regionToolset
    import displayGroupMdbToolset as dgm
    import part
    import material
    import assembly
    import optimization
    import step
    import interaction
    import load
    import mesh
    import job
    import sketch
    import visualization
    import xyPlot
    import displayGroupOdbToolset as dgo
    import connectorBehavior
    from abaqus import getInputs
    fields = (('Model:', 'Model-1'), ('Radius:','10'), ('Length:', '20'))
    Model, Radius, Length = getInputs(fields=fields, label='Specify cylinder parameters:',
        dialogTitle='Create Cylinder Part', )
    Radius = float(Radius)
    Length = float(Length)
    session.viewports['Viewport: 1'].partDisplay.setValues(sectionAssignments=OFF, 
        engineeringFeatures=OFF)
    session.viewports['Viewport: 1'].partDisplay.geometryOptions.setValues(
        referenceRepresentation=ON)
    s = mdb.models[Model].ConstrainedSketch(name='__profile__', 
        sheetSize=2*max(Radius, Length))
    g, v, d, c = s.geometry, s.vertices, s.dimensions, s.constraints
    s.setPrimaryObject(option=STANDALONE)
    s.CircleByCenterPerimeter(center=(0.0, 0.0), point1=(0.0, Radius))
    p = mdb.models[Model].Part(name='Cylinder', dimensionality=THREE_D, 
        type=DEFORMABLE_BODY)
    p = mdb.models[Model].parts['Cylinder']
    p.BaseSolidExtrude(sketch=s, depth=Length)
    s.unsetPrimaryObject()
    p = mdb.models[Model].parts['Cylinder']
    session.viewports['Viewport: 1'].setValues(displayedObject=p)
    del mdb.models[Model].sketches['__profile__']


