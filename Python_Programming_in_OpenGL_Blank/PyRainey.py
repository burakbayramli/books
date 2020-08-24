# Lorenz.py

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from random import *
from numpy import *
import sys

# Globals for window width and height
global width
global height

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
	#  This will coincide with the for... arange loops
	gluOrtho2D(-5.0, 5.0, -5.0, 5.0)
	
	#  Set the matrix for the object we are drawing
	glMatrixMode(GL_MODELVIEW)
	glLoadIdentity()

def plotroessler():
	glClear(GL_COLOR_BUFFER_BIT)
	
	# Initial values
	x = 1.0
	y = 1.0
	z = 1.0
	dt = 0.001
	
	glColor3f(1.0,0.0,0.0)
	glBegin(GL_POINTS)
	
	for n in arange(-90.0, 90.0, 0.0001):
		x = x + (-2*x + z*y)*dt
		y = y + (-2*y + (z - 5)*x)*dt
		z = z  + (1 - x*y)*dt

		glColor3f(sin(z),cos(x),sin(y))
		glVertex2f(y,z-5)
	
	glEnd()
	glFlush()
		
def keyboard(key, x, y):
	#  Allows us to quit by pressing 'Esc' or 'q'
	if key == chr(27):
		sys.exit()
	if key == "q":
		sys.exit()

def main():
	global width
	global height
	
	glutInit(sys.argv)
	glutInitDisplayMode(GLUT_RGB|GLUT_SINGLE)
	glutInitWindowPosition(100,100)
	glutInitWindowSize(width,height)
	glutCreateWindow("Rikitake")
	glutDisplayFunc(plotroessler)
	glutKeyboardFunc(keyboard)
	
	init()	
	glutMainLoop()

main()    
