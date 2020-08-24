# PyBounce.py

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from numpy import *
import sys

# uncomment these lines later
# to see if there is any difference
# in the speed of the ball
# import psyco
# psyco.full()

#  globals for animation, ball position
#  and direction of motion
global anim, x, y ,hvel, vvel, mousx, mousy

# initial position of the ball
x = -0.67
y = 0.34
dtime = 0.0002
hvel = 0.0
vvel = 0.0
mousx = 0.0
mousy = 0.0

# Window dimensions
width = height = 600
axrng = 100.0

# No animation to start
anim = 0

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
	hvel -= mousx*dtime
	vvel -= mousy*dtime
	y += vvel*dtime
	x += hvel*dtime
	
	# Keep the motion mathematics
	# Safe from harm
	glPushMatrix()
	
	# Move the ball location based on x and y
	glTranslate(x,y,0)
	glutSolidSphere(1.0, 50, 50)
	glPopMatrix()
	
	# Collision detection!
	# What happens here and why does this work?
	if x >= axrng - 0.1 or x <= -axrng + 0.1:
		hvel = -1*hvel
	if y >= axrng - 0.1 or y <= -axrng + 0.1:
		vvel = -1*vvel

	glutSwapBuffers()
	
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
		# STOP the ball!
		anim = 0
	if key == "q":
		sys.exit()

def mouse(button, state, a, b):
	global mousx, mousy
	if button == GLUT_LEFT and state == GLUT_DOWN:
		wd = (a - (width/2))/3
		ht = ((height/2) - b)/3
		if anim == 1:
				mousx = 500*(x-wd)/(sqrt((x-wd)**2 + (y-ht)**2)*abs(x-wd))
				mousy = 500*(y-ht)/(sqrt((x-wd)**2 + (y-ht)**2)*abs(y-ht))
				glutPostRedisplay()
	if button == GLUT_LEFT and state == GLUT_UP:
		mousx = 0.0
		mousy = 0.0
		glutPostRedisplay()

def main():
	glutInit(sys.argv)
	glutInitDisplayMode(GLUT_RGB|GLUT_DOUBLE)
	glutInitWindowPosition(100,100)
	glutInitWindowSize(width,height)
	glutCreateWindow("PyBounce")
	glutDisplayFunc(plotfunc)
	glutKeyboardFunc(keyboard)
	glutMouseFunc(mouse)
	glutIdleFunc(idle)
	
	init()	
	glutMainLoop()

main()    