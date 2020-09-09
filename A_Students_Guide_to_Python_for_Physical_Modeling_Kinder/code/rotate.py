# rotate.py
# -------------------------------------------------------------------------
# Define function to rotate a vector in two dimensions.
# ------------------------------------------------------------------------- 
import numpy as np

def rotate_vector(vector, angle):
	"""
	Rotate two-dimensional vector through given angle.
		vector = (x,y)
		angle = rotation angle in radians (counterclockwise)
	Returns the image of vector under rotation as a NumPy array.
	"""
	rotation_matrix = np.array([[ np.cos(angle), -np.sin(angle) ],
								[ np.sin(angle),  np.cos(angle) ]])
	return np.dot(rotation_matrix, vector)
