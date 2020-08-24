# PyFunc.py
# Plotting functions

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from numpy import *

def init():

	glClearColor(1.0, 1.0, 1.0, 1.0)
	gluOrtho2D(-2.0, 2.0, -2.0, 2.0)

def plotFunc():
	glClear(GL_COLOR_BUFFER_BIT)
	glColor3f(0.0, 0.0, 0.0)
	
	#glPointSize(3.0)

	glColor3f(0.0,0.0,0.0)
	glBegin(GL_POINTS)
	for t in arange(-200.0,200.0,.005):
		x = sin(.99*t) - .7*cos(3.01*t)
		y = cos(1.01*t) + .1*sin(15.03*t)

		glVertex2f(x, y)
	glEnd()
	glFlush()

def main():
	glutInit([])
	glutInitDisplayMode(GLUT_SINGLE|GLUT_RGB)
	glutInitWindowPosition(50,50)
	glutInitWindowSize(250,250)
	glutCreateWindow("Function Plotter")
	glutDisplayFunc(plotFunc)
	
	init()
	glutMainLoop()

main()
