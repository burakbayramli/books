# NBody

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from numpy import *
from random import *
from visual import *
import sys

import psyco
psyco.full()

#  Set the width and height of the window
global width
global height

#  Initial values for window width and height
width = 800
height = 800
		
# global variables for position, velocity  and
# acceleration components, time increment, and Gravity
global n, m, v, a, r, rad, G, dt

# Time increment
dt = 0.0001

# Gravitational Constant
G = 1.0

# Initial number of stars
n = 75

# Initialize arrays
m = zeros(n+1, float)
vx = zeros(n+1, float)
vy = zeros(n+1, float)
vz = zeros(n+1, float)
ax = zeros(n+1, float)
ay = zeros(n+1, float)
az = zeros(n+1, float)
rx = zeros(n+1, float)
ry = zeros(n+1, float)
rz = zeros(n+1, float)
rad = zeros(n+1, float)
colr = zeros(n+1, float)
colg = zeros(n+1, float)
colb = zeros(n+1, float)


def init():
	global m, r, a, v, rad, colr, colg, colb
	glClearColor(0.0, 0.0, 0.0, 1.0)
	glShadeModel(GL_SMOOTH)
	glEnable(GL_DEPTH_TEST)
	lightdiffuse = [1.0, 1.0, 1.0, 1.0]
	lightposition = [1.0, 1.0, 1.0, 0.0]
	lightambient = [0.0, 0.0, 0.0, 1.0]
	lightspecular = [1.0, 1.0, 1.0, 1.0]
	
	glLightfv(GL_LIGHT1, GL_DIFFUSE, lightdiffuse)
	glLightfv(GL_LIGHT1, GL_POSITION, lightposition)
	glLightfv(GL_LIGHT1, GL_AMBIENT, lightambient)
	glMaterialfv(GL_FRONT, GL_DIFFUSE, lightdiffuse)
	glMaterialfv(GL_FRONT, GL_SPECULAR, lightspecular)
	glEnable(GL_LIGHT1)
	glEnable(GL_LIGHTING)
	glEnable(GL_COLOR_MATERIAL)
	
	for i in range(1, n+1):
		
		m[i] = 500.*random() + 100.
		rad[i] = 0.0002*m[i]
		colr[i] = abs(sin(m[i]))
		colg[i] = abs(cos(m[i]))
		colb[i] = sqrt(abs(sin(m[i])*cos(m[i])))
		
		
	for i in range(1, n+1):
		rx[i] = cos(2*random()-1.25)*cos(5*random()-1.25)
		ry[i] = sin(2*random()-1.25)*cos(5*random()-1.25)
		rz[i] = sin(2*random()-1.25)
		
		vx[i] = 0.0
		vy[i] = 0.0
		vz[i] = 0.0
		ax[i] = 0.0
		ay[i] = 0.0
		az[i] = 0.0
		
def orbits():
	global r, v, a	
	for i in range(1,n+1):
		vx[i] += 0.5*ax[i]*dt
		vy[i] += 0.5*ay[i]*dt
		vz[i] += 0.5*az[i]*dt

		rx[i] += vx[i]*dt
		ry[i] += vy[i]*dt
		rz[i] += vz[i]*dt
		
		ax[i] = 0.0
		ay[i] = 0.0
		az[i] = 0.0
		
		for j in range(1,n+1):
			if j != i:
				r2 = (rx[i]-rx[j])*(rx[i]-rx[j]) 
				r2 += (ry[i]-ry[j])*(ry[i]-ry[j]) 
				r2 += (rz[i]-rz[j])*(rz[i]-rz[j])
				r3 = r2*sqrt(r2)+0.01
				
				ax[i] += -G*(rx[i]-rx[j])*m[j]/r3
				ay[i] += -G*(ry[i]-ry[j])*m[j]/r3
				az[i] += -G*(rz[i]-rz[j])*m[j]/r3

		vx[i] += 0.5*ax[i]*dt
		vy[i] += 0.5*ay[i]*dt
		vz[i] += 0.5*az[i]*dt
	
	glutPostRedisplay()

def reshape(  w,  h):
	
	# To insure we don't have a zero height
	if h==0:
		h = 1
	
	#  Fill the entire graphics window!
	glViewport(0, 0, w, h)
	
	#  Set the projection matrix... our "view"
	glMatrixMode(GL_PROJECTION)
	glLoadIdentity()
	
	gluPerspective(45.0, 1.0, 1.0, 1000.0)
	
	glMatrixMode(GL_MODELVIEW)
	glLoadIdentity()
	gluLookAt(0.0, 0.0, 5.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0)
	
def keyboard(key, x, y):
	#  Allows us to quit by pressing 'Esc' or 'q'
	if key == chr(27):
		sys.exit()
	if key == "q":
		sys.exit()

def plotfunc():
	global m
	glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT)
	for i in range(1,n+1):
		glPushMatrix()
		glTranslatef(rx[i],ry[i],rz[i])
		
		glColor3f(colr[i],colg[i],colb[i])
		glutSolidSphere(rad[i],25,25)
		#glutSolidTeapot(rad[i])
		glPopMatrix()
	glutSwapBuffers()

def main():
	global width
	global height
	
	glutInit(sys.argv)
	glutInitDisplayMode(GLUT_RGB|GLUT_DOUBLE)
	glutInitWindowPosition(100,100)
	glutInitWindowSize(width,height)
	glutCreateWindow("3 Body Problem")
	glutReshapeFunc(reshape)
	glutDisplayFunc(plotfunc)
	glutKeyboardFunc(keyboard)
	glutIdleFunc(orbits)
	
	init()	
	glutMainLoop()

main()    