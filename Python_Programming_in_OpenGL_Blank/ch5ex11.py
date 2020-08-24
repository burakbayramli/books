#  PyPoints.py
#  Setting a coordinate system with central origin

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
import sys

def init():
	glClearColor(1.0, 1.0, 1.0, 1.0)
	gluOrtho2D(-1.0, 1.0, -1.0, 1.0)

def plotPoints():
		glClear(GL_COLOR_BUFFER_BIT)
	
		# First draw x and y axes
		# Using black as a color
		glColor3f(0.0, 0.0, 0.0)
		glBegin(GL_LINES)
		glVertex2f(-1.0, 0.0)
		glVertex2f(1.0,0.0)
		glVertex2f(0.0, 1.0)
		glVertex2f(0.0, -1.0)
		glVertex2f(-1.0, -1.0)
		glVertex2f(1.0, 1.0)
		glEnd()
	
		# Store an ordered pair in variables
		x = 0.35
		y = 0.65
	
		# Plot points in bright red
		glColor3f(1.0, 0.0, 0.0)

		# Increase the point size
		glPointSize(3.0)
		glBegin(GL_POINTS)
	
		# Plot the point
		glVertex2f(x, y)

		# Plot the mirror image or reflection of the point
		# in the x axis.  Note the sign change!
		glVertex2f(y, x)
	
		glEnd()
		glFlush()


def main():
	glutInit(sys.argv)
	glutInitDisplayMode(GLUT_SINGLE|GLUT_RGB)
	glutInitWindowPosition(50,50)
	glutInitWindowSize(250,250)
	glutCreateWindow("Plot Points")
	glutDisplayFunc(plotPoints)
	
	init()
	glutMainLoop()

main()
