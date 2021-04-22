# %load code.beamExample.py

"""
beamExample.py

Reproduce the cantilever beam example from the
Appendix of the Getting Started with
Abaqus: Interactive Edition Manual.
From: http://ifcuriousthenlearn.com/blog/2015/04/02/Abaqus-FEA-Scripting-with-python/
Retrieved: 02/12/2017
"""

from abaqus import *
from abaqusConstants import *
backwardCompatibility.setValues(includeDeprecated=True,
                                reportDeprecated=False)

# Create a model.

myModel = mdb.Model(name='Beam')

# Create a new viewport in which to display the model
# and the results of the analysis.

myViewport = session.Viewport(name='Cantilever Beam Example',
    origin=(20, 20), width=150, height=120)
    
#-----------------------------------------------------

import part

# Create a sketch for the base feature.

mySketch = myModel.ConstrainedSketch(name='beamProfile',
    sheetSize=250.)

# Create the rectangle.

mySketch.rectangle(point1=(-100,10), point2=(100,-10))

# Create a three-dimensional, deformable part.

myBeam = myModel.Part(name='Beam', dimensionality=THREE_D,
    type=DEFORMABLE_BODY)

# Create the part's base feature by extruding the sketch 
# through a distance of 25.0.

myBeam.BaseSolidExtrude(sketch=mySketch, depth=25.0)

#-----------------------------------------------------

import material

# Create a material.

mySteel = myModel.Material(name='Steel')

# Create the elastic properties: youngsModulus is 209.E3
# and poissonsRatio is 0.3

elasticProperties = (209.E3, 0.3)
mySteel.Elastic(table=(elasticProperties, ) )

#-------------------------------------------------------

import section

# Create the solid section.

mySection = myModel.HomogeneousSolidSection(name='beamSection',
    material='Steel', thickness=1.0)

# Assign the section to the region. The region refers 
# to the single cell in this model.

region = (myBeam.cells,)
myBeam.SectionAssignment(region=region,
    sectionName='beamSection')

#-------------------------------------------------------

import assembly

# Create a part instance.

myAssembly = myModel.rootAssembly
myInstance = myAssembly.Instance(name='beamInstance',
    part=myBeam, dependent=OFF)

#-------------------------------------------------------

import step

# Create a step. The time period of the static step is 1.0, 
# and the initial incrementation is 0.1; the step is created
# after the initial step. 

myModel.StaticStep(name='beamLoad', previous='Initial',
    timePeriod=1.0, initialInc=0.1,
    description='Load the top of the beam.')

#-------------------------------------------------------

import load

# Find the end face using coordinates.

endFaceCenter = (-100,0,12.5)
endFace = myInstance.faces.findAt((endFaceCenter,) )

# Create a boundary condition that encastres one end
# of the beam.

endRegion = (endFace,)
myModel.EncastreBC(name='Fixed',createStepName='beamLoad',
    region=endRegion)

# Find the top face using coordinates.

topFaceCenter = (0,10,12.5)
topFace = myInstance.faces.findAt((topFaceCenter,) )

# Create a pressure load on the top face of the beam.

topSurface = ((topFace, SIDE1), )
myModel.Pressure(name='Pressure', createStepName='beamLoad',
    region=topSurface, magnitude=0.5)

#-------------------------------------------------------

import mesh

# Assign an element type to the part instance.

region = (myInstance.cells,)
elemType = mesh.ElemType(elemCode=C3D8I, elemLibrary=STANDARD)
myAssembly.setElementType(regions=region, elemTypes=(elemType,))

# Seed the part instance.

myAssembly.seedPartInstance(regions=(myInstance,), size=10.0)

# Mesh the part instance.

myAssembly.generateMesh(regions=(myInstance,))

# Display the meshed beam.

myViewport.assemblyDisplay.setValues(mesh=ON)
myViewport.assemblyDisplay.meshOptions.setValues(meshTechnique=ON)
myViewport.setValues(displayedObject=myAssembly)

#-------------------------------------------------------

import job

# Create an analysis job for the model and submit it.

jobName = 'beam_tutorial'
myJob = mdb.Job(name=jobName, model='Beam',
    description='Cantilever beam tutorial')

# Wait for the job to complete.

myJob.submit()
myJob.waitForCompletion()

#-------------------------------------------------------

import visualization

# Open the output database and display a
# default contour plot.

myOdb = visualization.openOdb(path=jobName + '.odb')
myViewport.setValues(displayedObject=myOdb)
myViewport.odbDisplay.display.setValues(plotState=CONTOURS_ON_DEF)

myViewport.odbDisplay.commonOptions.setValues(renderStyle=FILLED)