# PyMayParabola.py

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
	gluOrtho2D(0.7333275,.7333375,0.7333275,.7333375)
	
def plotPolar():
	glClear(GL_COLOR_BUFFER_BIT)
	
	glPointSize(2.0)
	glBegin(GL_POINTS)

	r = 3.75
	
	for x in arange(-1.0, 1.0,.0000001):	
		y = x*r*(1-x)
		
		glColor3f(0.0, 0.0, 0.0)
		glVertex2f(x,x)
		glVertex2f(x,y)
		
	glEnd()
	
	x = .3
	y = 0.0
	glBegin(GL_LINE_STRIP)
	for n in arange(1,5000000):
		glColor3f(sin(x),cos(x*n),sin(n*y))
		glVertex2f(x,y)
		y = x*r*(1-x)
		glVertex2f(x,y)
		x = y
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