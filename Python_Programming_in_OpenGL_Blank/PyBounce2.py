# PyBounce.py

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
global anim, x, y ,hvel, vvel

# initial position of the ball
x = -0.67
y = 0.34
dtime = 0.0002
hvel = 0.75
vvel = 3.0

# Window dimensions
width = height = 600
axrng = 1.0

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
	x += hvel*dtime
	vvel = vvel - 9.8*dtime
	y += vvel*dtime
	
	# Keep the motion mathematics
	# Safe from harm
	glPushMatrix()
	
	# Move the ball location based on x and y
	glTranslate(x,y,0)
	glutSolidSphere(0.1, 50, 50)
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

def main():
	glutInit(sys.argv)
	glutInitDisplayMode(GLUT_RGB|GLUT_DOUBLE)
	glutInitWindowPosition(100,100)
	glutInitWindowSize(width,height)
	glutCreateWindow("PyBounce")
	glutDisplayFunc(plotfunc)
	glutKeyboardFunc(keyboard)
	glutIdleFunc(idle)
	
	init()	
	glutMainLoop()

main()    