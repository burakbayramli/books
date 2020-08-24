# NBody Code
# for multiple stars
# based on Piet Hut and Jun Makino's
# MSA Text

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from numpy import *
from random import *
import sys

#import psyco
#psyco.full()

#  Set the width and height of the window
global width
global height

#  Initial values for window width and height
width = 1000
height = 1000
		
# global variables for position, velocity  and
# acceleration components, time increment, and Gravity
global n, m, v, a, r, rad, G, dt

# Time increment
dt = 0.001

# Gravitational Constant
G = 1.0

# Initial number of stars
n = 50

# Initialize arrays
m = zeros(3*n+1,float)
v = zeros(3*n+1,float)
a = zeros(3*n+1,float)
r = zeros(3*n+1,float)
rad = zeros(3*n+1,float)
col = zeros(3*n+1,float)

def init():
	global m, r, a, v
	glClearColor(0.0, 0.0, 0.0, 1.0)
	
	# Add lighting and shading effects
	glShadeModel(GL_SMOOTH)
	glEnable(GL_DEPTH_TEST)
	lightdiffuse = [0.85, 0.85, 0.85, 0.0]
	lightposition = [10.0, 10.0, 100.0, 0.0]
	lightambient = [0.25, 0.25, 0.25, 1.0]
	
	glLightfv(GL_LIGHT1, GL_DIFFUSE, lightdiffuse)
	glLightfv(GL_LIGHT1, GL_POSITION, lightposition)
	glLightfv(GL_LIGHT1, GL_AMBIENT, lightambient)
	glEnable(GL_LIGHT1)
	glEnable(GL_LIGHTING)
	glMaterialfv(GL_FRONT, GL_DIFFUSE, lightdiffuse)
	glEnable(GL_COLOR_MATERIAL)
	
	# Create a random set of n stars
	for i in range(1, 3*n+1,3):
		
		m[i] = 5000.*random() + 1000.
		rad[i] = 0.0001*m[i]
		col[i] = abs(sin(m[i]))
		col[i+1] = abs(cos(m[i]))
		col[i+2] = sqrt(abs(sin(m[i])*cos(m[i])))
		
	# Assign random positions to each star	
	for i in range(1, 3*n+1,3):
		r[i] = 10*cos(10*random())*cos(10*random())
		r[i+1] = 10*sin(10*random())*cos(10*random())
		r[i+2] = 10*sin(10*random())
		
		# Set initial velocities of each star
		v[i] = 0.0
		v[i+1] = 0.0
		v[i+2] = 0.0
		a[i] = 0.0
		a[i+1] = 0.0
		a[i+2] = 0.0
		
def orbits():
	global r, v, a	
	# array calculations make things easier!
	for i in range(1,3*n+1,3):
		v[i] += 0.5*a[i]*dt
		v[i+1] += 0.5*a[i+1]*dt
		v[i+2] += 0.5*a[i+2]*dt

		r[i] += v[i]*dt
		r[i+1] += v[i+1]*dt
		r[i+2] += v[i+2]*dt
		
		a[i] = 0.0
		a[i+1] = 0.0
		a[i+2] = 0.0
		
		for j in range(1,3*n+1,3):
			if j != i:
				
				# Arrays are more efficient and require less code!
				r2 = (r[i]-r[j])*(r[i]-r[j]) 
				r2 += (r[i+1]-r[j+1])*(r[i+1]-r[j+1])
				r2 += (r[i+2]-r[j+2])*(r[i+2]-r[j+2])
				r3 = r2*sqrt(r2) + 0.001
				
				a[i] += -G*(r[i]-r[j])*m[j]/r3
				a[i+1] += -G*(r[i+1]-r[j+1])*m[j]/r3
				a[i+2] += -G*(r[i+2]-r[j+2])*m[j]/r3

		
		v[i] += 0.5*a[i]*dt
		v[i+1] += 0.5*a[i+1]*dt
		v[i+2] += 0.5*a[i+2]*dt
	
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
	gluLookAt(0.0, 0.0, 50.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0)
	
def keyboard(key, x, y):
	#  Allows us to quit by pressing 'Esc' or 'q'
	if key == chr(27):
		sys.exit()
	if key == "q":
		sys.exit()

def plotfunc():
	global m
	glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT)
	
	# Again, hooray for arrays!
	for i in range(1,3*n+1,3):
		glPushMatrix()
		glTranslatef(r[i],r[i+1],r[i+2])
		
		glColor3f(col[i],col[i+1],col[i+2])
		glutSolidSphere(rad[i],20,20)
		glPopMatrix()
	glutSwapBuffers()

def main():
	global width
	global height
	
	glutInit(sys.argv)
	glutInitDisplayMode(GLUT_RGB|GLUT_DOUBLE)
	glutInitWindowPosition(100,100)
	glutInitWindowSize(width,height)
	glutCreateWindow("NBody Problem")
	glutReshapeFunc(reshape)
	glutDisplayFunc(plotfunc)
	glutKeyboardFunc(keyboard)
	glutIdleFunc(orbits)
	
	init()	
	glutMainLoop()

main()    