# PyChaosGame.py

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from random import *
from numpy import *
import sys

#  Initial values of width and height
width = 250
height = 250
				
def init():
	# White background
	glClearColor(1.0, 1.0, 1.0, 0.0)
	
	#  Set the projection matrix... our "view"
	glMatrixMode(GL_PROJECTION)
	glLoadIdentity()
	
	#  Set the plot window range
	gluOrtho2D(-8.0, 10.0, -8.0, 10.0)
	
	#  Set the matrix for the object we are drawing
	glMatrixMode(GL_MODELVIEW)
	glLoadIdentity()

def plotFunc():
	glClear(GL_COLOR_BUFFER_BIT)
	
	# Initial values for parameters
	# This attractor is very sensitive
	# To the values for x, y, a, and b
	x = -0.1
	y = 0
	
	glBegin(GL_POINTS)
	
	# Plot a significant number of points
	for n in arange(0,50000):
		xx = 1 - y + abs(x)
		y = x
		x = xx
		
		# Don't plot anything until we've hit the attractor
		if n > 100:
				# How does this color statement work?
				glColor3f(sqrt(x*x+y*y)/15, 0.0, 0.0)
				glVertex2f(x,y)
	glEnd()
	glFlush()
		
def keyboard(key, x, y):
	#  Allows us to quit by pressing 'Esc' or 'q'
	if key == chr(27):
		sys.exit()
	if key == "q":
		sys.exit()

def main():
	#global width
	#global height
	
	glutInit(sys.argv)
	glutInitDisplayMode(GLUT_RGB|GLUT_SINGLE)
	glutInitWindowPosition(100,100)
	glutInitWindowSize(width,height)
	glutCreateWindow("The GingerBread Man")
	glutDisplayFunc(plotFunc)
	glutKeyboardFunc(keyboard)
	
	init()	
	glutMainLoop()

main()    