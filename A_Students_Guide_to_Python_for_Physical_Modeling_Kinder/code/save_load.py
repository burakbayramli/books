# save_load.py
# -------------------------------------------------------------------------
# Save array data using NumPy's available methods, then load saved data.
# ------------------------------------------------------------------------- 
import numpy as np

# Generate array data.
x = np.linspace(0, 1, 1001)
y = 3*np.sin(x)**3 - np.sin(x)

# Save data to text, .npy, and .npz files.
np.save('x_values', x)
np.save('y_values', y)
np.savetxt('x_values.dat', x)
np.savetxt('y_values.dat', y)
np.savez('xy_values', x_vals=x, y_vals=y) 

# Load saved data.
x2 = np.load('x_values.npy')
y2 = np.loadtxt('y_values.dat')
w = np.load('xy_values.npz')

# Check equality of original and saved data.
print((x2 == x).all())
print((y2 == y).all())
print((w['x_vals'] == x2).all())
print((w['y_vals'] == y2).all())
