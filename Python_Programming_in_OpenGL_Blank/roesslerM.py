# PyRoessler.py

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from random import *
#from numpy import *
from math import *
import sys

# Globals for window width and height
global width
global height
global x
global y
global rr
global gg
global bb
rr =1.
gg =0.
bb =1.
x = 0.0
y = 0.1
z = 0.2
dt = 0.05


#  Initial values of width and height
width = 250
height = 250
				
def init():
	
	# White background
	glClear(GL_COLOR_BUFFER_BIT)
 #	glClearColor(1.0, 1.0, 1.0, 0.0)
	
	#  Set the projection matrix... our "view"
	glMatrixMode(GL_PROJECTION)
	glLoadIdentity()
	
	#  Set the plot window range
	#  This will coincide with the for... arange loops
	gluOrtho2D(-15.0, 15.0, -15.0, 15.0)
	
	#  Set the matrix for the object we are drawing
	glMatrixMode(GL_MODELVIEW)
	glLoadIdentity()

def viewRoessler():
#	glClear(GL_COLOR_BUFFER_BIT)
	glColor3f(1.0,1.0,1.0)
	glBegin(GL_POINTS)
	glColor3f(rr,gg,bb)
	glVertex2f(x,y)
	glEnd()
        glutSwapBuffers()

def calcRoessler():
                global x
                global y 
                global z
                global rr
                global gg
                global bb
		x = x - (y + z)*dt
		y = y + (x + .2*y)*dt
		z = z + (.2 + x*z - 5.7*z)*dt
		rr= sqrt(x*x+y*y)/15
                gg= sqrt(x*x+z*z)/15
                bb=sqrt(y*y + z*z)/15
                glutPostRedisplay() 
		
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
	glutInitDisplayMode(GLUT_RGB|GLUT_DOUBLE)
	glutInitWindowPosition(100,100)
	glutInitWindowSize(width,height)
	glutCreateWindow("Roessler")
	glutDisplayFunc(viewRoessler)
	glutIdleFunc(calcRoessler)
	glutKeyboardFunc(keyboard)
	
	init()	
	glutMainLoop()

main()    
