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

import psyco
psyco.full()

#  Set the width and height of the window

global width
global height

#  Initial values for window width and height

width = 500
height = 500
		
# global variables for position, velocity  and
# acceleration components, time increment, and Gravity

global n, m, v, a, r, rad, G, dt

# gluLookAt variables

global x, y, z, lx, ly, lz

# Time increment

dt = 0.0001

# Gravitational Constant

G = 1.0

# Initial number of stars

n = 75

# Initialize arrays for mass, velocity, acceleration
# position, radius, and color

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

def initgl():
	global m, r, a, v, rad, colr, colg, colb
	glClearColor(0.0, 0.0, 0.0, 1.0)

	# Enable depth testing for true 3D effects 
	
	glEnable(GL_DEPTH_TEST)
	
	# Add lighting and shading effects

	glShadeModel(GL_SMOOTH)
	lightdiffuse = [1.0, 1.0, 1.0, 1.0]
	lightposition = [1.0, 1.0, 1.0, 0.0]
	lightambient = [0.0, 0.0, 0.0, 1.0]
	lightspecular = [1.0, 1.0, 1.0, 1.0]
	
	# Turn on the light

	glLightfv(GL_LIGHT1, GL_DIFFUSE, lightdiffuse)
	glLightfv(GL_LIGHT1, GL_POSITION, lightposition)
	glLightfv(GL_LIGHT1, GL_AMBIENT, lightambient)
	glMaterialfv(GL_FRONT, GL_DIFFUSE, lightdiffuse)
	glMaterialfv(GL_FRONT, GL_SPECULAR, lightspecular)
	glEnable(GL_LIGHT1)
	glEnable(GL_LIGHTING)
	glEnable(GL_COLOR_MATERIAL)

def init():
	# Create a random set of n stars

	for i in range(1, n+1):
		
		m[i] = 500.*random() + 100.
		rad[i] = 0.0002*m[i]
		colr[i] = abs(sin(m[i]))
		colg[i] = abs(cos(m[i]))
		colb[i] = sqrt(abs(sin(m[i])*cos(m[i])))

	# Assign random positions to each star
	
	for i in range(1, n+1):
		rx[i] = cos(2*random()-1.25)*cos(5*random()-1.25)
		ry[i] = sin(2*random()-1.25)*cos(5*random()-1.25)
		rz[i] = sin(2*random()-1.25)

		# Set initial velocities and accelerations
		# of each star

		vx[i] = 0.0
		vy[i] = 0.0
		vz[i] = 0.0
		ax[i] = 0.0
		ay[i] = 0.0
		az[i] = 0.0

def tilt(azim):
	# Tilts the camera/spaceship up and down
	global ly
	ly = sin(azim)
	lz = -cos(azim)
	glLoadIdentity()
	gluLookAt(x, y, z, x + lx, y + ly, z + lz, 0.0, 1.0, 0.0)
	
def latmove(mov):
	# Moves the camera/spaceship forward and backward
	# along the line of sight
	global x, y, z
	x += mov*lx*n/10
	y += mov*ly*n/10
	z += mov*lz*n/10
	glLoadIdentity()
	gluLookAt(x, y, z, x + lx, y + ly, z + lz, 0.0, 1.0, 0.0)
	
def swivel(theta):
	# Swivels the camera/spaceship left and right
	global lx, lz
	lx = sin(theta)
	lz = -cos(theta)
	glLoadIdentity()
	gluLookAt(x, y, z, x + lx, y + ly, z + lz, 0.0, 1.0, 0.0)

def orbits():
	global rx, ry, rz, vx, vy, vz, ax, ay, az
	
	# array calculations make things easier!

	for i in range(1,n+1):

		# First half of leapfrog algorithm		

		vx[i] += 0.5*ax[i]*dt
		vy[i] += 0.5*ay[i]*dt
		vz[i] += 0.5*az[i]*dt

		rx[i] += vx[i]*dt
		ry[i] += vy[i]*dt
		rz[i] += vz[i]*dt
		
		ax[i] = 0.0
		ay[i] = 0.0
		az[i] = 0.0

		
		# Loop through ALL stars

		for j in range(1,n+1):
			
			# Do NOT act on self to avoid infinity!
			# Only calculate acceleration components
			# if we are working with OTHER stars

			if j != i:

				# Arrays are more efficient!
				# r2 calculation could be on 1 line
				# but it wouldn't fit the page margins

				r2 = (rx[i]-rx[j])*(rx[i]-rx[j]) 
				r2 += (ry[i]-ry[j])*(ry[i]-ry[j]) 
				r2 += (rz[i]-rz[j])*(rz[i]-rz[j])
				r3 = r2*sqrt(r2)
				
				ax[i] += -G*(rx[i]-rx[j])*m[j]/r3
				ay[i] += -G*(ry[i]-ry[j])*m[j]/r3
				az[i] += -G*(rz[i]-rz[j])*m[j]/r3

		# Second half of leapfrog algorithm

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
	
	# Slight change of position of gluLookAt
	# Avoids a jump in scene on first movement
	gluLookAt(x, y, z, x + lx, y + ly, z + lz, 0.0, 1.0, 0.0)
	
def keyboard(key, x, y):
	#  Allows us to quit by pressing 'Esc' or 'q'

	if key == chr(27):
		sys.exit()
	if key == "q":
		sys.exit()
		
	# Reset view to original position
	if key == "z":
		zap()

# The specialkey function looks for arrow keys
def specialkey(key, x, y):
	global ang, updown, move
	
	# mode checks for SHIFT key
	mode = glutGetModifiers()
	
	# Left arrow key
	if key == GLUT_KEY_LEFT:
		ang = -0.01
	
	# Right arrow key
	if key == GLUT_KEY_RIGHT:
		ang = 0.01
	
	# Up arrow key
	if key == GLUT_KEY_UP:
		# If shift key is used, tilt upward
		if mode == GLUT_ACTIVE_SHIFT:
			updown = -0.01
		# Otherwise move forward
		else:
			move = 0.05
	
	# Down arrow key
	if key == GLUT_KEY_DOWN:
		# If shift key is used, tilt downward
		if mode == GLUT_ACTIVE_SHIFT:
			updown = 0.01
		# Otherwise move backward
		else:
			move = -0.05
			
def plotfunc():
	global m, move, angle, ang, azim, updown
	
	# moving around in 3D
	# Based on specialkeys
	
	# move forward and backward
	# along the line of sight
	if move != 0:
		latmove(move)
		move = 0
	
	# Swivel left and right
	if ang != 0.0:
		angle += ang*n/10
		swivel(angle)
		ang = 0.0
		
	# Tilt up and down	
	if updown != 0.0:
		azim += updown
		tilt(azim)
		updown = 0.0
	
	glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT)
	
	# Again, hooray for arrays!

	for i in range(1,n+1):
		glPushMatrix()
		glTranslatef(rx[i],ry[i],rz[i])
		
		glColor3f(colr[i],colg[i],colb[i])
		glutSolidSphere(rad[i],20,20)
		glPopMatrix()

	glutSwapBuffers()

def zap():
	global angle, ang, updown, azim, move
	global x, y, z, lx, ly, lz
	# Navigation variables
	
	angle = 0.0
	ang = 0.0
	updown = 0.0
	azim = 0.0
	move = 0.0
	
	# Initial values for eyeball position
	
	x = 0.0
	y = 0.0
	z = 10.0
	lx = 0.0
	ly = 0.0
	lz = -10.0
	# makes certain we set the view "straight ahead"
	swivel(0)

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
	glutSpecialFunc(specialkey)
	glutIdleFunc(orbits)
	
	initgl()
	init()	
	zap()
	glutMainLoop()

main()    
