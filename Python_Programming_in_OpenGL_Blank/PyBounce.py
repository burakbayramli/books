# PyBounce.py

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from Numeric import *
import sys

#  Set the width and height of the window
global width, height, axrng, anim, x, y ,dx, dy
global xborder, yborder, radius

#initial values
x = -0.67
y = 0.34
dx = dy = 1
radius = 0.5
width = height = 500
axrng = 10.0
anim = 1
xborder = yborder = axrng

def init():
	glClearColor(0.0, 0.0, 0.0, 1.0)
	glColor3ub(255, 0, 0)

def idle():
	if anim == 1:
		glutPostRedisplay()

def plotfunc():
	global x, y, dx, dy
	glClear(GL_COLOR_BUFFER_BIT)

	x = x + 0.005*dx
	y = y + 0.005*dy
	
	glPushMatrix()
	glTranslate(x,y,0)
	glutSolidSphere(radius, 50, 50)
	#glScalef(0.5, 0.5, 0.5)
	#glutSolidTeapot(radius)
	glPopMatrix()
	
	if x >= xborder-radius or x <= -xborder+radius:
		dx = -1*dx
	if y >= yborder-radius or y <= -yborder+radius:
		dy = -1*dy

	glutSwapBuffers()
	
def reshape(  w,  h):
	global xborder, yborder, x, y
	# To insure we don't have a zero height
	if h==0:
		h = 1
	
	#  Fill the entire graphics window!
	glViewport(0, 0, w, h)
	
	#  Set the projection matrix... our "view"
	glMatrixMode(GL_PROJECTION)
	glLoadIdentity()
	
	#  Set the aspect ratio of the plot so that it
	#  Always looks "OK" and never distorted.
	if w <= h:
		gluOrtho2D(-axrng, axrng, -axrng*h/w, axrng*h/w)
		yborder = axrng*h/w
		xborder = axrng
	else:
		gluOrtho2D(-axrng*w/h, axrng*w/h, -axrng, axrng)
		xborder = axrng*w/h
		yborder = axrng
	
	if x <= -xborder:
		x = -xborder + (2*radius)
	if x >= xborder:
		x = xborder - (2*radius)
	if y <= -yborder:
		y = -yborder + (2*radius)
	if y >= yborder:
		y = yborder - (2*radius)
	
	#  Set the matrix for the object we are drawing
	glMatrixMode(GL_MODELVIEW)
	glLoadIdentity()
	
def keyboard(key, x, y):
	#  Allows us to quit by pressing 'Esc' or 'q'
	#  We can animate by "a" and stop by "s"
	global anim
	if key == chr(27):
		sys.exit()
	if key == "a":
		anim = 1
	if key == "s":
		anim = 0
	if key == "q":
		sys.exit()

def main():
	global width, height
	
	glutInit(sys.argv)
	glutInitDisplayMode(GLUT_RGB|GLUT_DOUBLE)
	glutInitWindowPosition(100,100)
	glutInitWindowSize(width,height)
	glutCreateWindow("PyBounce")
	glutReshapeFunc(reshape)
	glutDisplayFunc(plotfunc)
	glutKeyboardFunc(keyboard)
	glutIdleFunc(idle)
	
	init()	
	glutMainLoop()

main()    
