# PyChaosGame.py

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from numpy import *
from random import *
import sys

# Globals for window width and height
global width
global height

#  Initial values of width and height
width = 300
height = 300
				
def init():
	
	# White background
	glClearColor(1.0, 1.0, 1.0, 0.0)
	
	# Fern Green Plot
	glColor3f(0.3, .6, 0.2)
	
	#  Set the projection matrix... our "view"
	glMatrixMode(GL_PROJECTION)
	glLoadIdentity()
	
	#  Set the plot window range
	gluOrtho2D(-3.0, 3.0, 0.0, 10.5)
	
	#  Set the matrix for the object we are drawing
	glMatrixMode(GL_MODELVIEW)
	glLoadIdentity()

def plotfunc():
	
	# Choose an initial point... any point
	x = -1.5
	y = 0.75
	
	glClear(GL_COLOR_BUFFER_BIT)
	glBegin(GL_POINTS)
	
	for n in range(0,100000):
	
		v = random()
		
		if v >= 0 and v <= .8000:
			a = 0
			b = 1.6
			c = -2.5*pi/180
			d = -2.5*pi/180
			e = .85
			f = .85
			#glColor3f(1.0, 0.0, 0.0)
			
		elif v > .8000 and v <= .8050:
			a = 0
			b = 0
			c = 0*pi/180
			d = 0*pi/180
			e = 0
			f = .16
			#glColor3f(0.0, 1.0, 0.0)
		
		elif v > .8050 and v <= .9025:
			a = 0
			b = 1.6
			c = 49*pi/180
			d = 49*pi/180
			e = .3
			f = .34
			#glColor3f(0.0, 0.0, 1.0)
		
		elif v > .9025 and v <= 1.0:
			a = 0
			b = .44
			c = 120*pi/180
			d = -50*pi/180
			e = .3
			f = .37
			#glColor3f(1.0, 0.0, 1.0)
			
		xx = x
		yy = y
		x = e * xx * cos(c) - f * yy * sin(d) + a
		y = e * xx * sin(c) + f * yy * cos(d) + b
		
		if n > 10:
			glVertex2f(x,y)
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
	glutCreateWindow("The Chaos Game... Fern!")
	glutDisplayFunc(plotfunc)
	glutKeyboardFunc(keyboard)
	
	init()	
	glutMainLoop()

main()    