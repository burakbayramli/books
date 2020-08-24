# PyFunc.py
# Plotting functions

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from numpy import *

def init():
	glClearColor(1.0, 1.0, 1.0, 1.0)
	gluOrtho2D(-5.0, 5.0, -5.0, 5.0)

def plotFunc():
	glClear(GL_COLOR_BUFFER_BIT)
	glColor3f(0.0, 0.0, 0.0)
	
	glBegin(GL_LINES)
	glVertex2f(-5,0)
	glVertex2f(5,0)
	glVertex2f(0,5)
	glVertex2f(0,-5)
	glEnd()
	
	#glPointSize(3.0)

	glColor3f(0.0,0.0,0.0)
	glBegin(GL_POINTS)
	for x in arange(-5, 5, .01):
		y = x*x
		a = x + 1
		glVertex2f(x, y)
		glVertex2f(x, a)
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
