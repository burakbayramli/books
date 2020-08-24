# PyBoundMSet.py
# Plot an M-Set
# Using boundary scanning

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from random import *
from numpy import *
from time import *
import sys

# If psyco isn't installed, delete the next two lines!
import psyco
psyco.full()

axrng = 2.0
width = 400
height = 400
hstep = 2.0*axrng/width
vstep = 2.0*axrng/height

def init():
	glClearColor(0.0, 0.0, 0.0, 0.0)
	gluOrtho2D(-axrng,axrng,-axrng,axrng)

def drawboundmandel():
	global escape
	glClear(GL_COLOR_BUFFER_BIT)

	# for a timer!
	tim = time()

	# Chooses the number of random pixels to check
	# Increase this number for a more dense plot.
	for i in range(0,100000):
			
			# limits the M-Set ranges
			# to speed up execution
			# and choose random pixels in
			# this more limited range
			x = -2*axrng*random()+.5
			y = 1.25*axrng*random()-1.25
			
			# draws a triangle around the M-Set
			# So we have fewer points to choose from
			if y < .625*x + 1.25 and y > -.625*x - 1.25:
			
				bound = 0
				
				# check pixels at North, South,
				# East, and West locations to see
				# if these points escape to infinity
				# If so, add 1 to bound variable
				leng = escapetime(x+hstep,y)
				if leng < 2:
					bound += 1
				leng = escapetime(x-hstep,y)
				if leng < 2:
					bound += 1
				leng = escapetime(x,y-vstep)
				if leng < 2:
					bound += 1
				leng = escapetime(x,y+vstep)
				if leng < 2:
					bound += 1
				glBegin(GL_POINTS)
				
				# If any, but not ALL neighboring 
				# pixels escape, then the current pixel
				# is on the border of the M-Set, so plot it!
				#
				# Change second bound to bound < 5 for 
				# an interesting effect!
				if bound > 0 and bound < 4:
					glColor3ub(85*bound,50*bound,90*bound)
					glVertex2f(x,y)
				glEnd()
				glFlush()

	# Print the elapsed time in the console window
	print time() - tim
		
def escapetime(x,y):
	n = 0
	z = a = complex(x,y)

	# Low escape time for a quick plot
	while n < 25:
		
		# M-Set equation
		z = z**2 + a
		zz = abs(z)

		# escape distance
		if zz > 2:
			n = 5001
		n += 1
	return zz
			
def main():
	glutInit(sys.argv)
	glutInitDisplayMode(GLUT_RGB | GLUT_SINGLE)
	glutInitWindowPosition(50, 50)
	glutInitWindowSize(width, height)
	glutCreateWindow("Julia Set")
	glutDisplayFunc(drawboundmandel)
	init()
	glutMainLoop()
	
main()
