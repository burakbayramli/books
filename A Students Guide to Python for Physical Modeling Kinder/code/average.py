# average.py
# -------------------------------------------------------------------------
# Compute and return the cummulative average of an array.
# ------------------------------------------------------------------------- 
import numpy as np

def running_average(x):
	"""
	Return cummulative average of an array.
	"""
	y = np.zeros(len(x))					# empty array to store result
	current_sum = 0.0						# running sum of elements of x
	for i in range(len(x)):
		current_sum += x[i]					# increment sum
		y[i] = current_sum / (i + 1.0)		# update running average
	return y
