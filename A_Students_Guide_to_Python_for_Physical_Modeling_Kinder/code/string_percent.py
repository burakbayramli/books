# string_percent.py
# -------------------------------------------------------------------------
# Illustrate string formatting using the % method.
# ------------------------------------------------------------------------- 
import numpy as np

print("The value of pi is approximately " + str(np.pi))
print("The value of %s is approximately %.5f" % ('pi', np.pi))

s = "%d plus %d is %d"
print(s % (2, 4, 2 + 4))
