# Lorenz1.py

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from random import *
from numpy import *
import sys

# Globals for window width and height
global width
global height

#  Initial values of width and height
width = 250
height = 250
				
def init():
	
	# White background
	glClearColor(1.0, 1.0, 1.0, 0.0)
	
	#  Set the projection matrix... our "view"
	glMatrixMode(GL_PROJECTION)
	glLoadIdentity()
	
	#  Set the plot window range
	#  This will coincide with the for... arange loops
	gluOrtho2D(-30.0, 30.0, -30.0, 30.0)
	
	#  Set the matrix for the object we are drawing
	glMatrixMode(GL_MODELVIEW)
	glLoadIdentity()

def plotLorenz():
	glClear(GL_COLOR_BUFFER_BIT)
	
	# Let's plot x versus y
	x = .5
	y = .5
	z = .5
	dt = .0005
	glColor3f(1.0,0.0,0.0)
	glBegin(GL_POINTS)
	for n in arange(-30,30,.0005):
		x = x + (-10*x + 10*y)*dt
		y = y + (28*x - y - x*z)*dt
		#z = z + (-.5*z + x*x)*dt
		z = z + (-2.66667*z + x*y)*dt
		glColor3f(sqrt((x-2)**2+(y+3)**2)/10,0.20,0.60)
		glVertex2f(y,z-30)
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
	glutInitWindowPosition(100,100)
	glutInitWindowSize(width,height)
	glutCreateWindow("Lorenz")
	glutDisplayFunc(plotLorenz)
	glutKeyboardFunc(keyboard)
	
	init()	
	glutMainLoop()

main()    