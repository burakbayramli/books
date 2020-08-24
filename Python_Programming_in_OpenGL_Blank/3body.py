# 3body.py
# a 3 star system based on Piet Hut
# and Jun Makino's MSA 2 body text
# with 3 body modifications by Stan Blank
# for use in Python OpenGL/GLUT

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from numpy import *
import sys

#  Set the width and height of the window

global width
global height

#  Initial values for window width and height

width = 400
height = 400
		
# global variables for position, velocity  and
# acceleration components, time increment, and Gravity

global vx1, vy1, vz1, x1, y1, z1, ax1, ay1, az1
global vx2, vy2, vz2, x2, y2, z2, ax2, ay2, az2
global vx3, vy3, vz3, x3, y3, z3, ax3, ay3, az3, dt, G

# Initial values for position components in x,y,z space
# for each of the 3 star masses.  Note that the z-axis
# position is zero, so there is no z-component.  This is
# a 2D simulation at this point.

x1 = 1.0
y1 = 1.0
z1 = 0.0
x2 = -1.0
y2 = -1.0
z2 = 0.0
x3 = 0.50
y3 = -1.0
z3 = 0.0

# Initial values for velocity components in x,y,z space

vx1 = 0.0
vy1 = 0.0
vz1 = 0.0
vx2 = 0.0
vy2 = 0.0
vz2 = 0.0
vx3 = 0.0
vy3 = 0.0
vz3 = 0.0

# Initial acceleration components

ax1 = 0.0
ay1 = 0.0
az1 = 0.0
ax2 = 0.0
ay2 = 0.0
az2 = 0.0
ax3 = 0.0
ay3 = 0.0
az3 = 0.0

# Initial star masses

m1 = 0.7
m2 = 0.4
m3 = 0.5

# Gravitational Constant

G = 1.0

# radius of stars used in the plotFunc function

rad1 = 0.2*m1
rad2 = 0.2*m2
rad3 = 0.2*m3

# Calculate r**3 denominators for 3 Body Gravitation
# More complex because the motion of EACH star depends
# on where the other two stars are located!

r12 = (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2) + (z1-z2)*(z1-z2)
r312 = r12*sqrt(r12)
r13 = (x1-x3)*(x1-x3) + (y1-y3)*(y1-y3) + (z1-z3)*(z1-z3)
r313 = r13*sqrt(r13)
r23 = (x2-x3)*(x2-x3) + (y2-y3)*(y2-y3) + (z2-z3)*(z2-z3)
r323 = r23*sqrt(r23)

# Calculate the initial accelerations
# MUCH more complex than 2 Body dynamics
# Because each star must use the combined forces
# due to gravity of the other 2 stars.
# This is why there are TWO ax1, etc statements.  

ax1 += -G*(x1-x2)*m2/r312
ax1 += -G*(x1-x3)*m3/r313
ay1 += -G*(y1-y2)*m2/r312
ay1 += -G*(y1-y3)*m3/r313
az1 += -G*(z1-z2)*m2/r312
az1 += -G*(z1-z3)*m3/r313
ax2 += -G*(x2-x1)*m1/r312
ax2 += -G*(x2-x3)*m3/r323
ay2 += -G*(y2-y1)*m1/r312
ay2 += -G*(y2-y3)*m3/r323
az2 += -G*(z2-z1)*m1/r312
az2 += -G*(z2-z3)*m3/r323
ax3 += -G*(x3-x2)*m2/r323
ax3 += -G*(x3-x1)*m1/r313
ay3 += -G*(y3-y2)*m2/r323
ay3 += -G*(y3-y1)*m1/r313
az3 += -G*(z3-z2)*m2/r323
az3 += -G*(z3-z1)*m1/r313

# This value keeps a smooth orbit on my workstation
# Smaller values slow down the orbit, higher values speed things # up

#dt = 0.001
dt = 0.01

def init():
	glClearColor(0.0, 0.0, 0.0, 1.0)
	glEnable(GL_DEPTH_TEST)
	
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
	gluLookAt(0.0, 0.0, 8.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0)
	
	glMatrixMode(GL_MODELVIEW)
	glLoadIdentity()
	
def keyboard(key, x, y):
	#  Allows us to quit by pressing 'Esc' or 'q'

	if key == chr(27):
		sys.exit()
	if key == "q":
		sys.exit()

def orbits():
	global vx1, vy1, vz1, x1, y1, z1, r2, r3, ax1, ay1, az1
	global vx2, vy2, vz2, x2, y2, z2, ax2, ay2, az2
	global vx3, vy3, vz3, x3, y3, z3, ax3, ay3, az3
	
	# More complex due to 3 Body instead of simply 2 Body 
	# interactions.  This is the first half of the velocity
	# Calculations.  Known as Leap Frog!

	vx1 += 0.5*ax1*dt
	vy1 += 0.5*ay1*dt
	vz1 += 0.5*az1*dt
	vx2 += 0.5*ax2*dt
	vy2 += 0.5*ay2*dt
	vz2 += 0.5*az2*dt
	vx3 += 0.5*ax3*dt
	vy3 += 0.5*ay3*dt
	vz3 += 0.5*az3*dt
	
	# Calculate new positions

	x1 += vx1*dt
	y1 += vy1*dt
	z1 += vz1*dt
	x2 += vx2*dt
	y2 += vy2*dt
	z2 += vz2*dt
	x3 += vx3*dt
	y3 += vy3*dt
	z3 += vz3*dt
	
	# Reset acceleration components to zero.
	# This is important!

	ax1 = 0.0
	ay1 = 0.0
	az1 = 0.0
	ax2 = 0.0
	ay2 = 0.0
	az2 = 0.0
	ax3 = 0.0
	ay3 = 0.0
	az3 = 0.0
	
	# Recalculate r**3 denominators

	r12 = (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2) + (z1-z2)*(z1-z2)
	r312 = r12*sqrt(r12)
	r13 = (x1-x3)*(x1-x3) + (y1-y3)*(y1-y3) + (z1-z3)*(z1-z3)
	r313 = r13*sqrt(r13)
	r23 = (x2-x3)*(x2-x3) + (y2-y3)*(y2-y3) + (z2-z3)*(z2-z3)
	r323 = r23*sqrt(r23)
	
	# Calculate acceleration components from each body.
	# We add or accumulate the acceleration components provided
	# by each of the other two stars to arrive at ONE resultant
	# Acceleration.  We avoid self-gravity!

	ax1 += -G*(x1-x2)*m2/r312
	ax1 += -G*(x1-x3)*m3/r313
	ay1 += -G*(y1-y2)*m2/r312
	ay1 += -G*(y1-y3)*m3/r313
	az1 += -G*(z1-z2)*m2/r312
	az1 += -G*(z1-z3)*m3/r313
	ax2 += -G*(x2-x1)*m1/r312
	ax2 += -G*(x2-x3)*m3/r323
	ay2 += -G*(y2-y1)*m1/r312
	ay2 += -G*(y2-y3)*m3/r323
	az2 += -G*(z2-z1)*m1/r312
	az2 += -G*(z2-z3)*m3/r323
	ax3 += -G*(x3-x2)*m2/r323
	ax3 += -G*(x3-x1)*m1/r313
	ay3 += -G*(y3-y2)*m2/r323
	ay3 += -G*(y3-y1)*m1/r313
	az3 += -G*(z3-z2)*m2/r323
	az3 += -G*(z3-z1)*m1/r313
	
	# Calculate the second half of the velocity components

	vx1 += 0.5*ax1*dt
	vy1 += 0.5*ay1*dt
	vz1 += 0.5*az1*dt
	vx2 += 0.5*ax2*dt
	vy2 += 0.5*ay2*dt
	vz2 += 0.5*az2*dt
	vx3 += 0.5*ax3*dt
	vy3 += 0.5*ay3*dt
	vz3 += 0.5*az3*dt
	
	#send x,y,z to the display

	glutPostRedisplay()

def plotfunc():
	glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT)
	
	# Plot the position of m1
	
	glPushMatrix()
	glTranslatef(x1,y1,z1)
	glColor3ub(245, 150, 30)
	glutSolidSphere(rad1, 10, 10)
	glPopMatrix()

	# Plot the position of m2	

	glPushMatrix()
	glTranslatef(x2,y2,z2)
	glColor3ub(245, 230, 100)
	glutSolidSphere(rad2, 10, 10)
	glPopMatrix()

	# Plot the position of m3

	glPushMatrix()
	glTranslatef(x3,y3,z3)
	glColor3ub(100, 230, 200)
	glutSolidSphere(rad3, 10, 10)
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
