# measurements.py
"""
Functions to calculate distance between points using different metrics.
"""
import numpy as np

def crow(pointA, pointB):
	"""
	Distance between points A and B "as the crow flies."
		pointA = (x1, y1)
		pointB = (x2, y2)
	returns sqrt( (x2-x1)**2 + (y2-y1)**2 )
	"""
	interval = np.sqrt( (pointA[0] - pointB[0])**2 + \
						(pointA[1] - pointB[1])**2 )
	return interval


def taxicab(pointA, pointB):
	"""
	Distance between points A and B "as the cab drives."
		pointA = (x1, y1)
		pointB = (x2, y2)
	returns |x2-x1| + |y2-y1|
	"""
	interval =	abs(pointB[0] - pointA[0]) + \
				abs(pointB[1] - pointA[1])
	return interval


def distance(pointA, pointB=(0,0), metric='taxi'):
	"""
	Return distance between points A and B. If metric is 'taxi', use taxicab
	metric. Otherwise, use Euclidean distance.
		pointA = (x1, y1)
		pointB = (x2, y2)
	"""
	if metric == 'taxi':
		return taxicab(pointA, pointB)
	else:
		return crow(pointA, pointB)
