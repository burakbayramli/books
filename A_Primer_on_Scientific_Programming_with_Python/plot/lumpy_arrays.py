from scitools.Lumpy import Lumpy

lumpy = Lumpy() 
import numpy as np
B = np.zeros((100,20))
c = B[-1,-1]
del np  # clean up unnecessary names for Lumpy diagram
lumpy.object_diagram()

