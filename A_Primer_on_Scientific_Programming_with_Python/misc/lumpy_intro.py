from scitools.Lumpy import Lumpy
lumpy = Lumpy() 
lumpy.make_reference()
l0 = [1, 4, 3]
l1 = l0
l2 = l1[:-1]
l1[0] = 100
lumpy.object_diagram()

lumpy = Lumpy() 
lumpy.make_reference()  # not necessary
n1 = 21.5
n2 = 21
l3 = [l1, l2, [n1, n2]]
s1 = 'some string'
lumpy.object_diagram()

lumpy = Lumpy() 
lumpy.make_reference()  # not necessary
import numpy as np
a = np.zeros(4)
lumpy.object_diagram()



