# PyPolar.py
# Plotting Polar Equations

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
width = 200
height = 200
nRange = 2.0
				
def init():
	glClearColor(.35, .79, .60, 1.0)
	
def plotPolar():
	glClear(GL_COLOR_BUFFER_BIT)
	
	# Plot axis lines for reference
	glColor3f(0.0, 0.0, 0.0)
	glBegin(GL_LINES)
	glVertex2f(-nRange,0)
	glVertex2f(nRange,0)
	glVertex2f(0,nRange)
	glVertex2f(0,-nRange)
	glEnd()
	
	# Plot polar equation for a Limacon
	glPointSize(1.0)
	a = 1
	b = 4.0
	c = 1.725
	
	glBegin(GL_POINTS)
	for t in arange(-2.0, 2.0, .001):
		
		x = a*(2*cos(t) - cos(2*t))
		y = a*(2*sin(t) - sin(2*t))

		glVertex2f(x,y)
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
	glutCreateWindow("Witch of Agnesi")
	glutReshapeFunc(Reshape)
	glutDisplayFunc(plotPolar)
	glutKeyboardFunc(keyboard)
	
	init()	
	
	glutMainLoop()

main()    