# PyGravity.py

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
import sys

# uncomment these lines later
# to see if there is any difference
# in the speed of the ball
# import psyco
# psyco.full()

#  globals for animation, ball position
#  and direction of motion
global anim, x, y ,hvel, vvel, radius
global xborder, yborder

# initial position of the ball
x = -0.67
y = 0.34
dtime = 0.005
radius = 0.1
hvel = 0.75
vvel = 3.0

# Window dimensions
width = height = 600
xborder = yborder = axrng = 1.0

# No animation to start
anim = 1

def init():
	glClearColor(0.0, 0.0, 0.0, 1.0)
	glColor3ub(255, 0, 0)
	
	# Dimensions of the screen
	# Make axrng larger and see what happens!
	gluOrtho2D(-axrng, axrng, -axrng, axrng)

def idle():
	# We animate only if anim == 1, otherwise
	# the ball doesn't move
	if anim == 1:
		glutPostRedisplay()

def plotfunc():
	global x, y, hvel, vvel
	glClear(GL_COLOR_BUFFER_BIT)

	# changes x and y
	x += hvel*dtime
	vvel = vvel - 9.8*dtime
	y += vvel*dtime
	
	# Keep the motion mathematics
	# Safe from harm
	glPushMatrix()
	
	if y <= -axrng+radius:
		y = -axrng+radius
	
	# Move the ball location based on x and y
	glTranslate(x,y,0)
	glutSolidSphere(radius, 50, 50)
	glPopMatrix()
	
	# Collision detection!
	# What happens here and why does this work?
	if x >= xborder - radius or x <= -xborder + radius:
		hvel = -1*hvel
	if y >= yborder - radius or y <= -yborder + radius:
		vvel = -1*vvel

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
		# Notice we are making anim = 1
		# What does this mean?  Look at the idle function
		anim = 1
	if key == "s":
		# STOP the animation!
		anim = 0
	if key == "q":
		sys.exit()

def main():
	glutInit(sys.argv)
	glutInitDisplayMode(GLUT_RGB|GLUT_DOUBLE)
	glutInitWindowPosition(100,100)
	glutInitWindowSize(width,height)
	glutCreateWindow("PyBounce")
	glutDisplayFunc(plotfunc)
	glutKeyboardFunc(keyboard)
	glutReshapeFunc(reshape)
	glutIdleFunc(idle)
	
	init()	
	glutMainLoop()

main()    
