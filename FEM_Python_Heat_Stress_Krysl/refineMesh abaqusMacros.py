# -*- coding: mbcs -*-
# Do not delete the following import lines
from abaqus import *
from abaqusConstants import *
import __main__

def refineModel():
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
    # Change these variables according to your need
    model = 'Model-1'
    part = 'Part-1'
    refinement_factor = 0.5
    # No need to change anything below
    p = mdb.models[model].parts[part]
    elementsize = p.getPartSeeds(attribute=mesh.SIZE) * refinement_factor
    f = p.faces
    pickedRegions = f.getSequenceFromMask(mask=('[#1 ]', ), )
    p.deleteMesh(regions=pickedRegions)
    p = mdb.models[model].parts[part]
    e = p.edges
    pickedEdges = e.getSequenceFromMask(mask=('[#1 ]', ), )
    p.seedEdgeBySize(edges=pickedEdges, size=elementsize/2.0, deviationFactor=0.1, constraint=FINER)
    p = mdb.models[model].parts[part]
    p.seedPart(size=elementsize, deviationFactor=0.1, minSizeFactor=0.1)
    p = mdb.models[model].parts[part]
    p.generateMesh()


