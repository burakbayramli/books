# PyChaosGame.py

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from random import *
from numpy import *
import sys

import psyco
psyco.full()

# Globals for window width and height
#global width
#global height

#  Initial values of width and height
width = 400
height = 400
				
def init():
	# White background
	glClearColor(1.0, 1.0, 1.0, 0.0)
	
	#  Set the projection matrix... our "view"
	glMatrixMode(GL_PROJECTION)
	glLoadIdentity()
	
	#  Set the plot window range
	gluOrtho2D(2.5, 4.5, 2.5, 4.5)
	
	#  Set the matrix for the object we are drawing
	glMatrixMode(GL_MODELVIEW)
	glLoadIdentity()

def plotFunc():
	glClear(GL_COLOR_BUFFER_BIT)
	glBegin(GL_POINTS)
	x = .52
	for a in arange(2.5001,4.5,.005):
		for b in arange(2.5001,4.5,.005):
			c = 0
			sum = 0
			x = .52
			
			while c <500:
				c += 1
				if c%2 == 0:
					r = a
				else:
					r = b
				x = r*x*(1-x)
				sum = sum+log(abs(r-2*r*x))/log(2)
			lyap = sum/c
			if lyap<0:
				
				glColor3f(x,r,c*a)
				glVertex2f(a,b)
			else:
				glColor3f(.75*c,.1*b,.3*r)
				glVertex2f(a,b)
	glEnd()
	glFlush()
		
def keyboard(key, x, y):
	#  Allows us to quit by pressing 'Esc' or 'q'
	if key == chr(27):
		sys.exit()
	if key == "q":
		sys.exit()

def main():
	#global width
	#global height
	
	glutInit(sys.argv)
	glutInitDisplayMode(GLUT_RGB|GLUT_SINGLE)
	glutInitWindowPosition(100,100)
	glutInitWindowSize(width,height)
	glutCreateWindow("Lyap")
	glutDisplayFunc(plotFunc)
	glutKeyboardFunc(keyboard)
	
	init()	
	glutMainLoop()

main()    