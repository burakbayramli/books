# nesting.py
# -------------------------------------------------------------------------
# Use nested for loops to fill a two-dimensional array of values.
# ------------------------------------------------------------------------- 
import numpy as np

# Set dimensions of array.
rows = 3
columns = 4

# Create empty array then fill with values.
A = np.zeros((rows, columns))
for i in range(rows):
    for j in range(columns):
        A[i, j] = i**2 + j**3
