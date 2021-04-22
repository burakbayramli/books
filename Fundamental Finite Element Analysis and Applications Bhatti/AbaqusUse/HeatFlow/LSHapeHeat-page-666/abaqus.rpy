#
# ABAQUS/CAE Version 6.3-1 replay file
# Internal Version: 2002_09_03-10.38.43 36270
# Run by mabhatti on Tue Jul 22 14:55:04 2003
#

# from driverUtils import executeOnCaeGraphicsStartup
# executeOnCaeGraphicsStartup()
#: Executing "onCaeGraphicsStartup()" in the site directory ...
#: Warning: This hardware does not support the OpenGL driver with double buffering.
from abaqus import *
from abaqusConstants import *
session.Viewport(name='Viewport: 1', origin=(1.32468557357788, 
    1.32261407375336), width=194.993713378906, height=130.409744262695)
session.viewports['Viewport: 1'].makeCurrent()
from driverUtils import executeOnCaeStartup
executeOnCaeStartup()
openMdb(
    '/tmp_mnt/nfs/server00/local/vol02/user5/mabhatti/WileyVol1/abaqus/Chap1/thermal/LShape.cae')
#: The model database "/tmp_mnt/nfs/server00/local/vol02/user5/mabhatti/WileyVol1/abaqus/Chap1/thermal/LShape.cae" has been opened.
session.viewports['Viewport: 1'].setValues(displayedObject=None)
#--- Recover file: 'LShape.rec' ---
from part import *
mdb.models['Model-1'].Sketch(name='__profile__', sheetSize=0.1)
mdb.models['Model-1'].sketches['__profile__'].sketchOptions.setValues(
    constructionGeometry=ON, decimalPlaces=3, dimensionTextHeight=0.002, 
    grid=ON, gridFrequency=2, gridSpacing=0.002, sheetSize=0.1)
mdb.models['Model-1'].sketches['__profile__'].Line(point1=(0.0, 0.0), point2=(
    0.06, 0.0))
mdb.models['Model-1'].sketches['__profile__'].Line(point1=(0.06, 0.0), point2=(
    0.06, 0.015))
mdb.models['Model-1'].sketches['__profile__'].Line(point1=(0.06, 0.015), 
    point2=(0.03, 0.015))
mdb.models['Model-1'].sketches['__profile__'].Line(point1=(0.03, 0.015), 
    point2=(0.03, 0.03))
mdb.models['Model-1'].sketches['__profile__'].Line(point1=(0.03, 0.03), 
    point2=(0.0, 0.03))
mdb.models['Model-1'].sketches['__profile__'].Line(point1=(0.0, 0.03), point2=(
    0.0, 0.0))
mdb.models['Model-1'].Part(dimensionality=TWO_D_PLANAR, name='Part-1', 
    type=DEFORMABLE_BODY)
mdb.models['Model-1'].parts['Part-1'].BaseShell(
    sketch=mdb.models['Model-1'].sketches['__profile__'])
del mdb.models['Model-1'].sketches['__profile__']
mdb.models['Model-1'].parts['Part-1'].PartitionEdgeByPoint(
    edge=mdb.models['Model-1'].parts['Part-1'].edges[0], 
    point=mdb.models['Model-1'].parts['Part-1'].InterestingPoint(
    mdb.models['Model-1'].parts['Part-1'].edges[0], MIDDLE))
mdb.models['Model-1'].Sketch(gridSpacing=0.002, name='__profile__', 
    sheetSize=0.1, 
    transform=mdb.models['Model-1'].parts['Part-1'].MakeSketchTransform(
    sketchPlane=mdb.models['Model-1'].parts['Part-1'].faces[0], 
    sketchPlaneSide=SIDE1, sketchOrientation=RIGHT, origin=(0.025, 0.0125, 
    0.0)))
mdb.models['Model-1'].sketches['__profile__'].sketchOptions.setValues(
    constructionGeometry=ON, decimalPlaces=3, dimensionTextHeight=0.002, 
    grid=ON, gridFrequency=2, gridSpacing=0.002, sheetSize=0.1)
mdb.models['Model-1'].parts['Part-1'].projectReferencesOntoSketch(
    filter=COPLANAR_EDGES, 
    sketch=mdb.models['Model-1'].sketches['__profile__'])
del mdb.models['Model-1'].sketches['__profile__']
mdb.models['Model-1'].Sketch(gridSpacing=0.002, name='__profile__', 
    sheetSize=0.1, 
    transform=mdb.models['Model-1'].parts['Part-1'].MakeSketchTransform(
    sketchPlane=mdb.models['Model-1'].parts['Part-1'].faces[0], 
    sketchPlaneSide=SIDE1, sketchOrientation=RIGHT, origin=(0.025, 0.0125, 
    0.0)))
mdb.models['Model-1'].sketches['__profile__'].sketchOptions.setValues(
    constructionGeometry=ON, decimalPlaces=3, dimensionTextHeight=0.002, 
    grid=ON, gridFrequency=2, gridSpacing=0.002, sheetSize=0.1)
mdb.models['Model-1'].parts['Part-1'].projectReferencesOntoSketch(
    filter=COPLANAR_EDGES, 
    sketch=mdb.models['Model-1'].sketches['__profile__'])
del mdb.models['Model-1'].sketches['__profile__']
del mdb.models['Model-1'].parts['Part-1'].features['Partition edge-1']
mdb.models['Model-1'].parts['Part-1'].backup()
mdb.models['Model-1'].Sketch(gridSpacing=0.002, name='__profile__', 
    sheetSize=0.1, 
    transform=mdb.models['Model-1'].parts['Part-1'].MakeSketchTransform(
    sketchPlane=mdb.models['Model-1'].parts['Part-1'].faces[0], 
    sketchPlaneSide=SIDE1, sketchOrientation=RIGHT, origin=(0.025, 0.0125, 
    0.0)))
mdb.models['Model-1'].sketches['__profile__'].sketchOptions.setValues(
    constructionGeometry=ON, decimalPlaces=3, dimensionTextHeight=0.002, 
    grid=ON, gridFrequency=2, gridSpacing=0.002, sheetSize=0.1)
mdb.models['Model-1'].parts['Part-1'].projectReferencesOntoSketch(
    filter=COPLANAR_EDGES, 
    sketch=mdb.models['Model-1'].sketches['__profile__'])
del mdb.models['Model-1'].sketches['__profile__']
mdb.models['Model-1'].Sketch(gridSpacing=0.002, name='__profile__', 
    sheetSize=0.1, 
    transform=mdb.models['Model-1'].parts['Part-1'].MakeSketchTransform(
    sketchPlane=mdb.models['Model-1'].parts['Part-1'].faces[0], 
    sketchPlaneSide=SIDE1, sketchOrientation=RIGHT, origin=(0.025, 0.0125, 
    0.0)))
mdb.models['Model-1'].sketches['__profile__'].sketchOptions.setValues(
    constructionGeometry=ON, decimalPlaces=3, dimensionTextHeight=0.002, 
    grid=ON, gridFrequency=2, gridSpacing=0.002, sheetSize=0.1)
mdb.models['Model-1'].parts['Part-1'].projectReferencesOntoSketch(
    filter=COPLANAR_EDGES, 
    sketch=mdb.models['Model-1'].sketches['__profile__'])
del mdb.models['Model-1'].sketches['__profile__']
mdb.models['Model-1'].Sketch(gridSpacing=0.002, name='__profile__', 
    sheetSize=0.1, 
    transform=mdb.models['Model-1'].parts['Part-1'].MakeSketchTransform(
    sketchPlane=mdb.models['Model-1'].parts['Part-1'].faces[0], 
    sketchPlaneSide=SIDE1, sketchOrientation=RIGHT, origin=(0.025, 0.0125, 
    0.0)))
mdb.models['Model-1'].sketches['__profile__'].sketchOptions.setValues(
    constructionGeometry=ON, decimalPlaces=3, dimensionTextHeight=0.002, 
    grid=ON, gridFrequency=2, gridSpacing=0.002, sheetSize=0.1)
mdb.models['Model-1'].parts['Part-1'].projectReferencesOntoSketch(
    filter=COPLANAR_EDGES, 
    sketch=mdb.models['Model-1'].sketches['__profile__'])
del mdb.models['Model-1'].sketches['__profile__']
mdb.models['Model-1'].parts['Part-1'].PartitionFaceByShortestPath(faces=(
    mdb.models['Model-1'].parts['Part-1'].faces[0], ), 
    point1=mdb.models['Model-1'].parts['Part-1'].InterestingPoint(
    mdb.models['Model-1'].parts['Part-1'].edges[0], MIDDLE), 
    point2=mdb.models['Model-1'].parts['Part-1'].vertices[3])
del mdb.models['Model-1'].parts['Part-1'].features['Partition face-1']
mdb.models['Model-1'].parts['Part-1'].backup()
mdb.models['Model-1'].parts['Part-1'].PartitionFaceByShortestPath(faces=(
    mdb.models['Model-1'].parts['Part-1'].faces[0], ), 
    point1=mdb.models['Model-1'].parts['Part-1'].InterestingPoint(
    mdb.models['Model-1'].parts['Part-1'].edges[0], MIDDLE), 
    point2=mdb.models['Model-1'].parts['Part-1'].vertices[3])
from material import *
from section import *
mdb.models['Model-1'].Material('Material-1')
mdb.models['Model-1'].materials['Material-1'].Conductivity(table=((45.0, ), ))
mdb.models['Model-1'].Material('Material-2')
mdb.models['Model-1'].materials['Material-2'].Conductivity(table=((25.0, ), ))
mdb.models['Model-1'].HomogeneousSolidSection(material='Material-1', 
    name='Section-1', thickness=1.0)
mdb.models['Model-1'].HomogeneousSolidSection(material='Material-2', 
    name='Section-2', thickness=1.0)
mdb.models['Model-1'].parts['Part-1'].assignSection(region=Region(
    faces=mdb.models['Model-1'].parts['Part-1'].faces[0:1]), 
    sectionName='Section-1')
#: The section "Section-1" has been assigned to the selected regions.
mdb.models['Model-1'].parts['Part-1'].assignSection(region=Region(
    faces=mdb.models['Model-1'].parts['Part-1'].faces[0:1]), 
    sectionName='Section-1')
#: The section "Section-1" has been assigned to the selected regions.
mdb.models['Model-1'].parts['Part-1'].assignSection(region=Region(
    faces=mdb.models['Model-1'].parts['Part-1'].faces[1:2]), 
    sectionName='Section-2')
#: The section "Section-2" has been assigned to the selected regions.
from assembly import *
mdb.models['Model-1'].rootAssembly.DatumCsysByDefault(CARTESIAN)
mdb.models['Model-1'].rootAssembly.Instance(name='Part-1-1', 
    part=mdb.models['Model-1'].parts['Part-1'])
from step import *
mdb.models['Model-1'].HeatTransferStep(amplitude=RAMP, name='Step-1', 
    previous='Initial', response=STEADY_STATE)
mdb.models['Model-1'].FieldOutputRequest(createStepName='Step-1', 
    name='F-Output-2', variables=('NT', 'HFL'))
from interaction import *
mdb.models['Model-1'].FilmCondition(createStepName='Step-1', 
    definition=EMBEDDED_COEFF, filmCoeff=55.0, filmCoeffAmplitude='', 
    name='Int-1', sinkAmplitude='', sinkTemperature=20.0, surface=Region(
    side1Edges=mdb.models['Model-1'].rootAssembly.instances['Part-1-1'].edges[1:2]+\
    mdb.models['Model-1'].rootAssembly.instances['Part-1-1'].edges[2:3]+\
    mdb.models['Model-1'].rootAssembly.instances['Part-1-1'].edges[7:8]))
#: The interaction "Int-1" has been created.
from load import *
mdb.models['Model-1'].SurfaceHeatFlux(createStepName='Step-1', 
    magnitude=8000.0, name='Load-1', region=Region(
    side1Edges=mdb.models['Model-1'].rootAssembly.instances['Part-1-1'].edges[3:4]))
mdb.models['Model-1'].BodyHeatFlux(createStepName='Step-1', 
    magnitude=5000000.0, name='Load-2', region=Region(
    faces=mdb.models['Model-1'].rootAssembly.instances['Part-1-1'].faces[0:1]))
mdb.models['Model-1'].TemperatureBC(amplitude=UNSET, createStepName='Step-1', 
    distribution=UNIFORM, fixed=OFF, magnitude=110.0, name='BC-1', 
    region=Region(
    edges=mdb.models['Model-1'].rootAssembly.instances['Part-1-1'].edges[4:5]+\
    mdb.models['Model-1'].rootAssembly.instances['Part-1-1'].edges[5:6]))
from mesh import *
mdb.models['Model-1'].rootAssembly.seedPartInstance(regions=(
    mdb.models['Model-1'].rootAssembly.instances['Part-1-1'], ), size=0.002)
#: Global seeds have been assigned.
mdb.models['Model-1'].rootAssembly.deleteSeeds(regions=(
    mdb.models['Model-1'].rootAssembly.instances['Part-1-1'], ))
#: Global seeds have been deleted.
mdb.models['Model-1'].rootAssembly.seedEdgeByBias(edges=(((
    mdb.models['Model-1'].rootAssembly.instances['Part-1-1'].edges[1], 
    mdb.models['Model-1'].rootAssembly.instances['Part-1-1'].edges[0]), END1), 
    ((mdb.models['Model-1'].rootAssembly.instances['Part-1-1'].edges[7], ), 
    END2)), number=20, ratio=5.0)
mdb.models['Model-1'].rootAssembly.deleteSeeds(regions=(
    mdb.models['Model-1'].rootAssembly.instances['Part-1-1'].edges[0], 
    mdb.models['Model-1'].rootAssembly.instances['Part-1-1'].edges[1], 
    mdb.models['Model-1'].rootAssembly.instances['Part-1-1'].edges[7]))
mdb.models['Model-1'].rootAssembly.seedEdgeByBias(edges=(((
    mdb.models['Model-1'].rootAssembly.instances['Part-1-1'].edges[1], 
    mdb.models['Model-1'].rootAssembly.instances['Part-1-1'].edges[0]), END1), 
    ((mdb.models['Model-1'].rootAssembly.instances['Part-1-1'].edges[7], ), 
    END2)), number=20, ratio=1.0)
mdb.models['Model-1'].rootAssembly.seedEdgeByBias(edges=(((
    mdb.models['Model-1'].rootAssembly.instances['Part-1-1'].edges[1], 
    mdb.models['Model-1'].rootAssembly.instances['Part-1-1'].edges[0]), END1), 
    ((mdb.models['Model-1'].rootAssembly.instances['Part-1-1'].edges[7], ), 
    END2)), number=20, ratio=2.0)
mdb.models['Model-1'].rootAssembly.seedPartInstance(regions=(
    mdb.models['Model-1'].rootAssembly.instances['Part-1-1'], ), size=0.002)
#: Global seeds have been assigned.
mdb.models['Model-1'].rootAssembly.setMeshControls(algorithm=ADVANCING_FRONT, 
    elemShape=QUAD_DOMINATED, regions=(
    mdb.models['Model-1'].rootAssembly.instances['Part-1-1'].faces[0], 
    mdb.models['Model-1'].rootAssembly.instances['Part-1-1'].faces[1]))
mdb.models['Model-1'].rootAssembly.setElementType(elemTypes=(ElemType(
    elemCode=DC2D4, elemLibrary=STANDARD), ElemType(elemCode=DC2D3, 
    elemLibrary=STANDARD)), regions=(
    mdb.models['Model-1'].rootAssembly.instances['Part-1-1'].faces[0:2], ))
mdb.models['Model-1'].rootAssembly.generateMesh(regions=(
    mdb.models['Model-1'].rootAssembly.instances['Part-1-1'], ))
#--- End of Recover file ------
mdb.save()
#: The model database has been saved to "/tmp_mnt/nfs/server00/local/vol02/user5/mabhatti/WileyVol1/abaqus/Chap1/thermal/LShape.cae".
import mesh
import regionToolset
import displayGroupMdbToolset as dgm
session.viewports['Viewport: 1'].assemblyDisplay.setValues(mesh=ON)
session.viewports['Viewport: 1'].assemblyDisplay.meshOptions.setValues(
    meshTechnique=ON)
a = mdb.models['Model-1'].rootAssembly
session.viewports['Viewport: 1'].setValues(displayedObject=a)
session.viewports['Viewport: 1'].assemblyDisplay.setValues(loads=ON, bcs=ON, 
    interactions=ON)
mdb.save()
#: The model database has been saved to "/tmp_mnt/nfs/server00/local/vol02/user5/mabhatti/WileyVol1/abaqus/Chap1/thermal/LShape.cae".
session.viewports['Viewport: 1'].assemblyDisplay.bcOptions.setValues(
    displacement=OFF, velocity=OFF, acceleration=OFF, symmetry=OFF, 
    connectorDisplacement=OFF, connectorVelocity=OFF, 
    connectorAcceleration=OFF, porePressure=OFF, electricPotential=OFF, 
    concentration=OFF, acousticPressure=OFF, submodel=OFF)
mdb.save()
#: The model database has been saved to "/tmp_mnt/nfs/server00/local/vol02/user5/mabhatti/WileyVol1/abaqus/Chap1/thermal/LShape.cae".
import interaction
session.viewports['Viewport: 1'].assemblyDisplay.meshOptions.setValues(
    meshTechnique=OFF)
import job
session.viewports['Viewport: 1'].assemblyDisplay.meshOptions.setValues(
    meshTechnique=OFF)
mdb.Job(name='Job-1', model='Model-1', type=ANALYSIS, 
    explicitPrecision=SINGLE, nodalOutputPrecision=SINGLE, description='', 
    userSubroutine='', numCpus=1, preMemory=256.0, standardMemory=256.0, 
    standardMemoryPolicy=MODERATE, scratch='', echoPrint=OFF, modelPrint=OFF, 
    contactPrint=OFF, historyPrint=OFF)
session.printToFile(fileName='thermalModel', format=TIFF, canvasObjects=(
    session.viewports['Viewport: 1'], ))
import visualization
import xyPlot
import displayGroupOdbToolset as dgo
session.viewports['Viewport: 1'].setValues(displayedObject=None)
a = mdb.models['Model-1'].rootAssembly
session.viewports['Viewport: 1'].setValues(displayedObject=a)
mdb.jobs['Job-1'].submit()
#: The job input file "Job-1.inp" has been submitted for analysis.
#: Job Job-1: Solver Input File Processor completed successfully.
#: Job Job-1: ABAQUS/Standard completed successfully.
#: Job Job-1 completed successfully. 
session.viewports['Viewport: 1'].setValues(displayedObject=None)
o0 = session.openOdb(
    '/tmp_mnt/nfs/server00/local/vol02/user5/mabhatti/WileyVol1/abaqus/Chap1/thermal/Job-1.odb')
#: Model: /tmp_mnt/nfs/server00/local/vol02/user5/mabhatti/WileyVol1/abaqus/Chap1/thermal/Job-1.odb
#: Number of Assemblies:         1
#: Number of Assembly instances: 0
#: Number of Part instances:     1
#: Number of Meshes:             1
#: Number of Element Sets:       1
#: Number of Node Sets:          1
#: Number of Steps:              1
session.viewports['Viewport: 1'].setValues(displayedObject=o0)
session.viewports['Viewport: 1'].odbDisplay.setPlotMode(CONTOUR)
session.viewports['Viewport: 1'].odbDisplay.setPlotMode(SYMBOL)
session.viewports['Viewport: 1'].odbDisplay.setPlotMode(CONTOUR)
session.viewports['Viewport: 1'].odbDisplay.setPrimaryVariable(
    variableLabel='NT11', outputPosition=NODAL)
a = mdb.models['Model-1'].rootAssembly
session.viewports['Viewport: 1'].setValues(displayedObject=a)
mdb.Job(name='LShapeHeat', model='Model-1', type=ANALYSIS, 
    explicitPrecision=SINGLE, nodalOutputPrecision=SINGLE, description='', 
    userSubroutine='', numCpus=1, preMemory=256.0, standardMemory=256.0, 
    standardMemoryPolicy=MODERATE, scratch='', echoPrint=OFF, modelPrint=OFF, 
    contactPrint=OFF, historyPrint=OFF)
mdb.jobs['LShapeHeat'].submit()
#: The job input file "LShapeHeat.inp" has been submitted for analysis.
odb = session.odbs['/tmp_mnt/nfs/server00/local/vol02/user5/mabhatti/WileyVol1/abaqus/Chap1/thermal/Job-1.odb']
session.viewports['Viewport: 1'].setValues(displayedObject=odb)
#: Job LShapeHeat: Solver Input File Processor completed successfully.
o0 = session.openOdb(
    '/tmp_mnt/nfs/server00/local/vol02/user5/mabhatti/WileyVol1/abaqus/Chap1/thermal/LShapeHeat.odb')
#: Model: /tmp_mnt/nfs/server00/local/vol02/user5/mabhatti/WileyVol1/abaqus/Chap1/thermal/LShapeHeat.odb
#: Number of Assemblies:         1
#: Number of Assembly instances: 0
#: Number of Part instances:     1
#: Number of Meshes:             1
#: Number of Element Sets:       1
#: Number of Node Sets:          1
#: Number of Steps:              1
session.viewports['Viewport: 1'].setValues(displayedObject=o0)
#: Job LShapeHeat: ABAQUS/Standard completed successfully.
#: Job LShapeHeat completed successfully. 
a = mdb.models['Model-1'].rootAssembly
session.viewports['Viewport: 1'].setValues(displayedObject=a)
odb = session.odbs['/tmp_mnt/nfs/server00/local/vol02/user5/mabhatti/WileyVol1/abaqus/Chap1/thermal/LShapeHeat.odb']
session.viewports['Viewport: 1'].setValues(displayedObject=odb)
o0 = session.openOdb(
    '/tmp_mnt/nfs/server00/local/vol02/user5/mabhatti/WileyVol1/abaqus/Chap1/thermal/LShapeHeat.odb')
session.viewports['Viewport: 1'].setValues(displayedObject=o0)
session.viewports['Viewport: 1'].odbDisplay.setPlotMode(CONTOUR)
session.viewports['Viewport: 1'].odbDisplay.setPrimaryVariable(
    variableLabel='NT11', outputPosition=NODAL)
session.viewports['Viewport: 1'].odbDisplay.setPlotMode(SYMBOL)
#* VisError: Symbol plotting can not be performed using scalar results. Please 
#* select another variable using the "Results->Field Output" dialog or select 
#* another plot mode.
session.viewports['Viewport: 1'].odbDisplay.setPlotMode(CONTOUR)
session.viewports['Viewport: 1'].odbDisplay.setPlotMode(DEFORMED_SHAPE)
session.viewports['Viewport: 1'].odbDisplay.setPlotMode(CONTOUR)
session.printToFile(fileName='thermalNodalSoln', format=TIFF, canvasObjects=(
    session.viewports['Viewport: 1'], ))
session.viewports['Viewport: 1'].odbDisplay.setPrimaryVariable(
    variableLabel='HFL', outputPosition=INTEGRATION_POINT, refinement=(
    INVARIANT, 'Magnitude'))
session.viewports['Viewport: 1'].odbDisplay.setPlotMode(SYMBOL)
session.printToFile(fileName='thermalHeatflux', format=TIFF, canvasObjects=(
    session.viewports['Viewport: 1'], ))
a = mdb.models['Model-1'].rootAssembly
session.viewports['Viewport: 1'].setValues(displayedObject=a)
odb = session.odbs['/tmp_mnt/nfs/server00/local/vol02/user5/mabhatti/WileyVol1/abaqus/Chap1/thermal/LShapeHeat.odb']
session.viewports['Viewport: 1'].setValues(displayedObject=odb)
session.viewports['Viewport: 1'].odbDisplay.setPrimaryVariable(
    variableLabel='NT11', outputPosition=NODAL)
#* VisError: Symbol plotting can not be performed using scalar results. Please 
#* select another variable using the "Results->Field Output" dialog or select 
#* another plot mode.
session.viewports['Viewport: 1'].odbDisplay.setPlotMode(CONTOUR)
mdb.save()
#: The model database has been saved to "/tmp_mnt/nfs/server00/local/vol02/user5/mabhatti/WileyVol1/abaqus/Chap1/thermal/LShape.cae".
