# PySkel.py

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from numpy import *
import sys

import psyco
psyco.full()

#  Set the width and height of the window
global width
global height


#  Initial values
width = 300
height = 300
		
#initial values for position, velocity components, and time increment
global vx, vy, vz, x, y, z, r2, r3, ax, ay, az, dt
x = 1.0
y = 0.0
z = 0.0
vx = 0.0
vy = 0.5
vz = 0.0
m1 = 0.7
m2 = 1 - m1
r2 = x*x + y*y + z*z
r3 = r2*sqrt(r2)
ax = -x/r3
ay = -y/r3
az = -z/r3

#This value keeps a smooth orbit on my workstation
#Smaller values slow down the orbit, higher values speed things up
dt = 0.0001
		
def init():
	glClearColor(0.0, 0.0, 0.0, 1.0)

def plotFunc():
	glClear(GL_COLOR_BUFFER_BIT)
	glPushMatrix()
	glTranslate(m1*x,m1*y,z)
	glColor3ub(245, 150, 30)
	glutSolidSphere(0.02, 10, 10)
	glPopMatrix()
	glPushMatrix()
	glTranslate(-m2*x,-m2*y,z)
	glColor3ub(245, 230, 100)
	glutSolidSphere(0.035, 10, 10)
	glPopMatrix()
	glutSwapBuffers()
	
def Reshape(  w,  h):
	
	# To insure we don't have a zero height
	if h==0:
		h = 1
	
	#  Fill the entire graphics window!
	glViewport(0, 0, w, h)
	
	#  Set the projection matrix... our "view"
	glMatrixMode(GL_PROJECTION)
	glLoadIdentity()
	
	gluPerspective(45.0, 1.0, 1.0, 10.0)
	
	gluLookAt(-1.0, 0.0, 1.5, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0)
	#  Set the aspect ratio of the plot so that it
	#  Always looks "OK" and never distorted.
	#if w <= h:
	#	gluOrtho2D(-nRange, nRange, -nRange*h/w, nRange*h/w)
	#else:
	#	gluOrtho2D(-nRange*w/h, nRange*w/h, -nRange, nRange)
	
	#  Set the matrix for the object we are drawing
	glMatrixMode(GL_MODELVIEW)
	glLoadIdentity()
	
def keyboard(key, x, y):
	#  Allows us to quit by pressing 'Esc' or 'q'
	if key == chr(27):
		sys.exit()
	if key == "q":
		sys.exit()

def orbits():
	global vx, vy, vz, x, y, z, r2, r3, ax, ay, az
	
	vx += 0.5*ax*dt
	vy += 0.5*ay*dt
	vz += 0.5*az*dt
	x += vx*dt
	y += vy*dt
	z += vz*dt
	r2 = x*x + y*y + z*z
	r3 = r2*sqrt(r2)
	ax = -x/r3
	ay = -y/r3
	az = -z/r3
	vx += 0.5*ax*dt
	vy += 0.5*ay*dt
	vz += 0.5*az*dt
	#send x,y,z to the display
	glutPostRedisplay()
	
def main():
	global width
	global height
	
	glutInit(sys.argv)
	glutInitDisplayMode(GLUT_RGB|GLUT_DOUBLE)
	glutInitWindowPosition(100,100)
	glutInitWindowSize(width,height)
	glutCreateWindow("PySkel")
	glutReshapeFunc(Reshape)
	glutDisplayFunc(plotFunc)
	glutKeyboardFunc(keyboard)
	glutIdleFunc(orbits)
	
	init()	
	glutMainLoop()

main()    