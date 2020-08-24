# PyMay.py

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from numpy import *
import sys

#  Set the width and height of the window
global width
global height

#  Initial values
width = 250
height = 250
				
def init():
	glClearColor(1.0, 1.0, 1.0, 1.0)
	gluOrtho2D(-8.0,7.0,0.0,1.0)
	
def plotPolar():
	glClear(GL_COLOR_BUFFER_BIT)
	
	glPointSize(1.0)
	glBegin(GL_POINTS)

	x = 0.5
	r = 2.5
	
	for a in arange(-8.0, 7.0,.0001):	
		r = r + .00001
		
		x = x*r*(1-x)
		glColor3f(0.0, 0.0, 0.0)
		glVertex2f(a,x)
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
	glutInitWindowPosition(200,200)
	glutInitWindowSize(width,height)
	glutCreateWindow("Chaos")
	glutDisplayFunc(plotPolar)
	glutKeyboardFunc(keyboard)
	
	init()	
	
	glutMainLoop()

main()    