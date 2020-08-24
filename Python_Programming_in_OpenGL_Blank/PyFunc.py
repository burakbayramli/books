# PyFunc.py
# Plotting functions

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from numpy import *
import sys

def init():
	glClearColor(1.0, 1.0, 1.0, 1.0)
	gluOrtho2D(-10.0, 10.0, -10.0, 10.0)

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

	glColor3f(1.0,0.0,0.0)
	glBegin(GL_POINTS)
	for x in arange(-5.0,5.0,0.001):
		
		y = 2*x**3 - 9*x**2 - 24*x -12

		glVertex2f(x, y)
	glEnd()
	glFlush()

def main():
	glutInit(sys.argv)
	glutInitDisplayMode(GLUT_SINGLE|GLUT_RGB)
	glutInitWindowPosition(50,50)
	glutInitWindowSize(400,400)
	glutCreateWindow("Function Plotter")
	glutDisplayFunc(plotFunc)
	
	init()
	glutMainLoop()

main()
