# string_format.py
# -------------------------------------------------------------------------
# Illustrate formatting strings using the .format() method.
# ------------------------------------------------------------------------- 
import numpy as np

print("The value of pi is approximately " + str(np.pi))
print("The value of {} is approximately {:.5f}".format('pi', np.pi))

s = "{1:d} plus {0:d} is {2:d}"
print(s.format(2,4,2+4))

print("Every {2} has its {3}.".format('dog','day','rose','thorn'))
print("The third element of the list is {0[2]:g}.".format(np.arange(10)))
