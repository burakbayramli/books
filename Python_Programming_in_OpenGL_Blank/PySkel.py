# PySkel.py

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from numpy import *
import sys

#  Set the width and height of the window
global width
global height

#  Set the axis range globally
global nRange

#  Initial values
width = 500
height = 500
nRange = 1.0
				
def init():
	glClearColor(1.0, 1.0, 1.0, 1.0)

def plotFunc():
	glClear(GL_COLOR_BUFFER_BIT)
	glBegin(GL_POINTS)

	# Plotting routines
	
	glEnd()
	glFlush()
	
def Reshape(  w,  h):
	
	# To insure we don't have a zero height
	if h==0:
		h = 1
	
	#  Fill the entire graphics window!
	glViewport(0, 0, w, h)
	
	#  Set the projection matrix... our "view"
	glMatrixMode(GL_PROJECTION)
	glLoadIdentity()
	
	#  Set the aspect ratio of the plot so that it
	#  Always looks "OK" and never distorted.
	if w <= h:
		gluOrtho2D(-nRange, nRange, -nRange*h/w, nRange*h/w)
	else:
		gluOrtho2D(-nRange*w/h, nRange*w/h, -nRange, nRange)
	
	#  Set the matrix for the object we are drawing
	glMatrixMode(GL_MODELVIEW)
	glLoadIdentity()
	
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
	glutCreateWindow("PySkel")
	glutReshapeFunc(Reshape)
	glutDisplayFunc(plotFunc)
	glutKeyboardFunc(keyboard)
	
	init()	
	glutMainLoop()

main()    