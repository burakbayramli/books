# PyMathArt.py

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from numpy import *
import sys

#  Set the width and height of the window
global width
global height

global stepsize

#  Set the axis range globally
global nRange

#  Initial values
width = 500
height = width
nRange = 10.0

stepsize = 2*nRange/width
				
def init():
	glClearColor(1.0, 1.0, 1.0, 1.0)
	
def plotPolar():
	glClear(GL_COLOR_BUFFER_BIT)
	glBegin(GL_POINTS)
	
	for x in arange(-nRange, nRange, stepsize):
		for y in arange(-nRange, nRange, stepsize):
			#r = cos(x)**2 + sin(y*y)**2+tan(x*y)**2
			r = cos(x) + sin(y)
			glColor3f(tan(r*r), sin(x*r)*cos(r*y), sin(y*x)*tan(r))
			#if x <> 0:
			#	r = (cos(x)**2 + sin(y/3) - log(abs(x)))
			#glColor3f(x*y*sin(r), sin(x*r*y)*cos(r*y), sin(y*x)*cos(r))
			glVertex2f(x,y)
	glEnd()
	#glFlush()
	
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
	glutInitWindowPosition(200,200)
	glutInitWindowSize(width,height)
	glutCreateWindow("Math Art Patterns")
	glutReshapeFunc(Reshape)
	glutDisplayFunc(plotPolar)
	glutKeyboardFunc(keyboard)
	
	init()	
	
	glutMainLoop()

main()    