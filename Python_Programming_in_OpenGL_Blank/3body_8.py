# PySkel.py

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from numpy import *
import sys

#  Set the width and height of the window
global width
global height

#  Initial values
width = 250
height = 250
		
#initial values for position, velocity components, and time increment
global vx1, vy1, vz1, x1, y1, z1, r2, r3, ax1, ay1, az1, dt
global vx2, vy2, vz2, x2, y2, z2, ax2, ay2, az2
global vx3, vy3, vz3, x3, y3, z3, ax3, ay3, az3, G

x1 = -0.97000436
y1 = 0.24308753
z1 = 0.0
x2 = 0.97000436
y2 = -0.24308753
z2 = 0.0
x3 = 0.0
y3 = 0.0
z3 = 0.0

vx1 = -0.93240737/2
vy1 = -0.86473146/2
vz1 = 0.0
vx2 = -0.93240737/2
vy2 = -0.86473146/2
vz2 = 0.0
vx3 = 0.93240737
vy3 = 0.86473146
vz3 = 0.0

ax1 = 0.0
ay1 = 0.0
az1 = 0.0
ax2 = 0.0
ay2 = 0.0
az2 = 0.0
ax3 = 0.0
ay3 = 0.0
az3 = 0.0

m1 = 1.01
m2 = 1.0
m3 = 1.0

G = 1.0

r12 = (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2) + (z1-z2)*(z1-z2)
r312 = r12*sqrt(r12)
r13 = (x1-x3)*(x1-x3) + (y1-y3)*(y1-y3) + (z1-z3)*(z1-z3)
r313 = r13*sqrt(r13)
r23 = (x2-x3)*(x2-x3) + (y2-y3)*(y2-y3) + (z2-z3)*(z2-z3)
r323 = r23*sqrt(r23)

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

#This value keeps a smooth orbit on my workstation
#Smaller values slow down the orbit, higher values speed things up
dt = 0.0005

def init():
	glClearColor(0.0, 0.0, 0.0, 1.0)
	glEnable(GL_DEPTH_TEST)
	
def plotFunc():
	glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT)
	glPushMatrix()
	glTranslatef(x1,y1,z1)
	glColor3ub(245, 150, 30)
	glutSolidSphere(0.1, 10, 10)
	glPopMatrix()
	glPushMatrix()
	glTranslatef(x2,y2,z2)
	glColor3ub(245, 230, 100)
	glutSolidSphere(0.1, 10, 10)
	glPopMatrix()
	glPushMatrix()
	glTranslatef(x3,y3,z3)
	glColor3ub(100, 230, 200)
	glutSolidSphere(0.1, 10, 10)
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
	
	gluPerspective(45.0, 1.0, 1.0, 1000.0)
	
	gluLookAt(0.0, 0.0, 5.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0)
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
	global vx1, vy1, vz1, x1, y1, z1, r2, r3, ax1, ay1, az1
	global vx2, vy2, vz2, x2, y2, z2, ax2, ay2, az2
	global vx3, vy3, vz3, x3, y3, z3, ax3, ay3, az3
	
	vx1 += 0.5*ax1*dt
	vy1 += 0.5*ay1*dt
	vz1 += 0.5*az1*dt
	vx2 += 0.5*ax2*dt
	vy2 += 0.5*ay2*dt
	vz2 += 0.5*az2*dt
	vx3 += 0.5*ax3*dt
	vy3 += 0.5*ay3*dt
	vz3 += 0.5*az3*dt
	
	x1 += vx1*dt
	y1 += vy1*dt
	z1 += vz1*dt
	x2 += vx2*dt
	y2 += vy2*dt
	z2 += vz2*dt
	x3 += vx3*dt
	y3 += vy3*dt
	z3 += vz3*dt
	
	#print x1, y1, z1, x2, y2, z2, x3, y3, z3
	
	ax1 = 0.0
	ay1 = 0.0
	az1 = 0.0
	ax2 = 0.0
	ay2 = 0.0
	az2 = 0.0
	ax3 = 0.0
	ay3 = 0.0
	az3 = 0.0
	
	r12 = (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2) + (z1-z2)*(z1-z2)
	r312 = r12*sqrt(r12)
	r13 = (x1-x3)*(x1-x3) + (y1-y3)*(y1-y3) + (z1-z3)*(z1-z3)
	r313 = r13*sqrt(r13)
	r23 = (x2-x3)*(x2-x3) + (y2-y3)*(y2-y3) + (z2-z3)*(z2-z3)
	r323 = r23*sqrt(r23)
	
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
	
def main():
	global width
	global height
	
	glutInit(sys.argv)
	glutInitDisplayMode(GLUT_RGB|GLUT_DOUBLE)
	glutInitWindowPosition(100,100)
	glutInitWindowSize(width,height)
	glutCreateWindow("3 Body Problem")
	glutReshapeFunc(Reshape)
	glutDisplayFunc(plotFunc)
	glutKeyboardFunc(keyboard)
	glutIdleFunc(orbits)
	
	init()	
	glutMainLoop()

main()    
