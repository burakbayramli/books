#
# ABAQUS/Viewer Version 6.3-1 replay file
# Internal Version: 2002_09_03-10.38.43 36270
# Run by mabhatti on Fri Jul 25 16:16:43 2003
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
import visualization
import xyPlot
import displayGroupOdbToolset as dgo
o0 = session.openOdb('NotchedBeam.odb')
#: Model: /tmp_mnt/nfs/server00/local/vol02/user5/mabhatti/WileyVol1/abaqus/Chap1/stress/NotchedBeam.odb
#: Number of Assemblies:         1
#: Number of Assembly instances: 0
#: Number of Part instances:     1
#: Number of Meshes:             1
#: Number of Element Sets:       2
#: Number of Node Sets:          1
#: Number of Steps:              1
session.viewports['Viewport: 1'].setValues(displayedObject=o0)
session.viewports['Viewport: 1'].odbDisplay.setPlotMode(CONTOUR)
session.viewports['Viewport: 1'].odbDisplay.setPrimaryVariable(
    variableLabel='S', outputPosition=INTEGRATION_POINT, refinement=(
    INVARIANT, 'Mises'))
session.viewports['Viewport: 1'].odbDisplay.setDeformedVariable(
    variableLabel='U')
