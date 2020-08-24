# PyBounce.py

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from Numeric import *
import sys

#  Set the width and height of the window
#  delete dx and dy
global width, height, axrng, anim, x, y

# add hvel and vvel
global xborder, yborder, radius, hvel, vvel

#initial values
x = -0.67
y = 0.34

# delete dx = dy = 1
# add
hvel = vvel = 0.000

radius = 0.2
width = height = 500
axrng = 10.0
anim = 0
xborder = yborder = axrng

def init():
	glClearColor(0.0, 0.0, 0.0, 1.0)
	glColor3ub(255, 0, 0)

def idle():
	if anim == 1:
		glutPostRedisplay()

def plotfunc():
	# remove dx and dy, add hvel, vvel
	global x, y, hvel, vvel
	glClear(GL_COLOR_BUFFER_BIT)

	# change this to implement hvel and vvel
	x = x + hvel
	y = y + vvel
	
	glPushMatrix()
	glTranslate(x,y,0)
	glutSolidSphere(radius, 50, 50)
	
	#glScalef(0.5, 0.5, 0.5)
	#glutSolidTeapot(radius)
	glPopMatrix()
	
	# change this to implement hvel and vvel
	if x >= xborder-radius or x <= -xborder+radius:
		hvel = -1*hvel
	if y >= yborder-radius or y <= -yborder+radius:
		vvel = -1*vvel

	glutSwapBuffers()
	
def reshape(  w,  h):
	global xborder, yborder
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
	else:
		gluOrtho2D(-axrng*w/h, axrng*w/h, -axrng, axrng)
		xborder = axrng*w/h
	
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
		
# add this function
def specialkey(key , x, y):
	global hvel, vvel
	if key == GLUT_KEY_LEFT:
		hvel -= 0.001
	if key == GLUT_KEY_RIGHT:
		hvel += 0.001
	if key == GLUT_KEY_UP:
		vvel += 0.001
	if key == GLUT_KEY_DOWN:
		vvel -= 0.001

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
	
	# add
	glutSpecialFunc(specialkey)
	glutIdleFunc(idle)
	
	init()	
	glutMainLoop()

main()    