# PyIkedaAttractor.py

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from random import *
from numpy import *
import sys

#  Initial values of width and height
width = 600
height = 600
				
def init():
	# White background
	glClearColor(1.0, 1.0, 1.0, 0.0)
	
	# Ugly Purple Plot
	glColor3f(0.45, 0.3, 0.5)
	
	#  Set the projection matrix... our "view"
	glMatrixMode(GL_PROJECTION)
	glLoadIdentity()
	
	#  Set the plot window range
	gluOrtho2D(-0.75, 2.0, -2.25, 1.25)
	
	#  Set the matrix for the object we are drawing
	glMatrixMode(GL_MODELVIEW)
	glLoadIdentity()

def plotikeda():
	
	# Choose an initial point... any point
	x = 0.5
	y = 0.5
	
	glClear(GL_COLOR_BUFFER_BIT)
	glBegin(GL_POINTS)
	
	for n in range(0,100000):
		temp = 0.4 - 7.7/(1+x*x + y*y)
		xx = 1 + 0.9*(x*cos(temp) - y*sin(temp))
		y = 0.9*(x*sin(temp) + y*cos(temp))
		x = xx
		
		#glColor3f(cos(x), sin(y), tan(x))
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
	glutInit(sys.argv)
	glutInitDisplayMode(GLUT_RGB|GLUT_SINGLE)
	glutInitWindowPosition(100,100)
	glutInitWindowSize(width,height)
	glutCreateWindow("The Ikeda Attractor")
	glutDisplayFunc(plotikeda)
	glutKeyboardFunc(keyboard)
	
	init()	
	glutMainLoop()

main()    

# End Program
