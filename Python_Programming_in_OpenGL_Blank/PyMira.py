# PyChaosGame.py

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from random import *
from numpy import *
import sys

#  Initial values of width and height
width = 300
height = 300
				
def init():
	# White background
	glClearColor(1.0, 1.0, 1.0, 0.0)
	
	#  Set the projection matrix... our "view"
	glMatrixMode(GL_PROJECTION)
	glLoadIdentity()
	
	#  Set the plot window range
	gluOrtho2D(-15.0, 15.0, -15.0, 15.0)
	
	#  Set the matrix for the object we are drawing
	glMatrixMode(GL_MODELVIEW)
	glLoadIdentity()

def plotFunc():
	glClear(GL_COLOR_BUFFER_BIT)
	
	# Initial values for parameters
	# This attractor is very sensitive
	# To the values for x, y, a, and b
	x = 15
	y = 0
	a = 0.107
	b = .9998
	c = 2 - 2*a
	w = a*x + c*x*x/(1+x*x)
	
	glBegin(GL_POINTS)
	
	# Plot a significant number of points
	for n in arange(0,100000):
		z = x
		x = b*y + w
		u = x*x
		w = a*x + c*u/(1 + u)
		y = w - z
		
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
	glutCreateWindow("The Mira Attractor")
	glutDisplayFunc(plotFunc)
	glutKeyboardFunc(keyboard)
	
	init()	
	glutMainLoop()

main()    