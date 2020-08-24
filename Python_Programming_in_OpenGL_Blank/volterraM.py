# PyPredPrey.py

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
#from random import *
#from numpy import *
import sys

global x,y,n
#  habitat and population parameters
a = .7; b = .5; c = .3 ; e = .2
# time increment
dt = .001  #was .001
dn = .001
# initial populations
x = .5
y = .5
n = 0

#initial values of width and height
width = 250
height = 250
				
def init():
	glClearColor(1.0,1.0,1.0,0.0)
	glClear(GL_COLOR_BUFFER_BIT)
	#  Set the projection matrix... our "view"
	glMatrixMode(GL_PROJECTION)
	glLoadIdentity()
	#  Set the plot window range
	gluOrtho2D(0,20,0,6)
	#  Set the matrix for the object we are drawing
	glMatrixMode(GL_MODELVIEW)
	glLoadIdentity()

def calcVolterra():
    global x, y, n
    # predator-prey equations
    x = x + (a*x - b*x*y)*dt
    y = y + (-c*y + e*x*y)*dt
    n += dn
    glutPostRedisplay()

def viewVolterra():
    #glClear(GL_COLOR_BUFFER_BIT)
    glBegin(GL_POINTS)
    # parametric plot
    glColor3f(0.0,0.0,0.00)
    glVertex2f(x,y)
    # prey
    glColor3f(1.0, 0.0, 0.0)
    glVertex2f(n,x)
    # predator
    glColor3f(0.0, 0.0, 1.0)
    glVertex2f(n,y)
    glEnd()
    glutSwapBuffers()

def keyboard(key, x, y):
	#  Allows us to quit by pressing 'Esc' or 'q'
	if key == chr(27):
		sys.exit()
	if key == "q":
		sys.exit()
def main():
	glutInit(sys.argv)
	glutInitDisplayMode(GLUT_RGB|GLUT_DOUBLE)
	glutInitWindowPosition(100,100)
	glutInitWindowSize(width,height)
	glutCreateWindow("Predator Prey Simulation")
	glutDisplayFunc(viewVolterra)
	glutIdleFunc(calcVolterra)
	glutKeyboardFunc(keyboard)
	init()	
	glutMainLoop()
main()    
