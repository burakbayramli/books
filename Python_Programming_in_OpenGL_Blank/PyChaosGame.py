# PyChaosGame.py

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from random import *
import sys

#  Set the global width, height, and axis ranges of the window
global width
global height
global axrng

# Initial values
width = 500
height = 500
axrng = 2.0
				
def init():
	# White background
	glClearColor(1.0, 1.0, 1.0, 0.0)
	
	# Black Plot
	glColor3f(0.0, 0.0, 0.0)

def plotfunc():
	# Store the triangle vertices in an array
	verts = [[0.0,2.0],[-2.0,-2.0],[2.0,-2.0]]
	
	# Choose an initial point... any point
	x = -1.5
	y = 0.75
	
	glClear(GL_COLOR_BUFFER_BIT)
	glBegin(GL_POINTS)
	
	for n in range(0,100000):
		v = randint(0,2)
		x = (x + verts[v][0])/2
		y = (y + verts[v][1])/2
		
		if n > 30:
			glVertex2f(x,y)
	glEnd()
	glFlush()
	
def reshape(  w,  h):
	
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
		gluOrtho2D(-axrng, axrng, -axrng*h/w, axrng*h/w)
	else:
		gluOrtho2D(-axrng*w/h, axrng*w/h, -axrng, axrng)
	
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
	glutCreateWindow("The Chaos Game")
	glutReshapeFunc(reshape)
	glutDisplayFunc(plotfunc)
	glutKeyboardFunc(keyboard)
	
	init()	
	glutMainLoop()

main()
