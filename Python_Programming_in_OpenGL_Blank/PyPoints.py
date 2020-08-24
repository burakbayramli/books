#  PyPoints.py
#  Setting a coordinate system with central origin

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
import sys

def init():
	glClearColor(0.0, 0.0, 0.0, 1.0)
	gluOrtho2D(-2.0, 2.0, -2.0, 2.0)

def plotPoints():
	glClear(GL_COLOR_BUFFER_BIT)
	glColor3f(1.0, 0.0, 0.0)
	
	glBegin(GL_POINTS)
	glVertex2f(0.0, 0.0)
	glEnd()
	
	glFlush()

def main():
	glutInit(sys.argv)
	glutInitDisplayMode(GLUT_SINGLE|GLUT_RGB)
	glutInitWindowPosition(50,50)
	glutInitWindowSize(600,600)
	glutCreateWindow("Plot Points")
	glutDisplayFunc(plotPoints)
	
	init()
	glutMainLoop()

main()
