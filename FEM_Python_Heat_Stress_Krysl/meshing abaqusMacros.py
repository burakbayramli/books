# -*- coding: mbcs -*-
# Do not delete the following import lines
from abaqus import *
from abaqusConstants import *
import __main__

def MeshWithSize():
	import section
	import regionToolset
	import displayGroupMdbToolset as dgm
	import part
	import material
	import assembly
	import step
	import interaction
	import load
	import mesh
	import optimization
	import job
	import sketch
	import visualization
	import xyPlot
	import displayGroupOdbToolset as dgo
	import connectorBehavior
	p = mdb.models['Model-1'].parts['Part-1']
	session.viewports['Viewport: 1'].setValues(displayedObject=p)
	session.viewports['Viewport: 1'].partDisplay.setValues(mesh=ON)
	session.viewports['Viewport: 1'].partDisplay.meshOptions.setValues(
		meshTechnique=ON)
	session.viewports['Viewport: 1'].partDisplay.geometryOptions.setValues(
		referenceRepresentation=OFF)
	p = mdb.models['Model-1'].parts['Part-1']
	ms = float(getInput('Element size'))
	p.seedPart(size=ms, deviationFactor=0.1, minSizeFactor=0.1)
	p = mdb.models['Model-1'].parts['Part-1']
	p.generateMesh()


def RefineByBisection():
	import section
	import regionToolset
	import displayGroupMdbToolset as dgm
	import part
	import material
	import assembly
	import step
	import interaction
	import load
	import mesh
	import optimization
	import job
	import sketch
	import visualization
	import xyPlot
	import displayGroupOdbToolset as dgo
	import connectorBehavior
	p = mdb.models['Model-1'].parts['Part-1']
	session.viewports['Viewport: 1'].setValues(displayedObject=p)
	session.viewports['Viewport: 1'].partDisplay.setValues(mesh=ON)
	session.viewports['Viewport: 1'].partDisplay.meshOptions.setValues(
		meshTechnique=ON)
	session.viewports['Viewport: 1'].partDisplay.geometryOptions.setValues(
		referenceRepresentation=OFF)
	p = mdb.models['Model-1'].parts['Part-1']
	ms = p.getPartSeeds(attribute=mesh.SIZE)
	p.seedPart(size=ms/2., deviationFactor=0.1, minSizeFactor=0.1)
	p = mdb.models['Model-1'].parts['Part-1']
	p.generateMesh()


