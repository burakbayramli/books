# Py3D1.py
# Orthographic vs. Perspective
from compiler import *
from OpenGL.GL import *
from OpenGL.GLUT import *
from OpenGL.GLU import *
import sys

global width
global height
global ortho		

#initial window and mouse settings
width = 600
height = 600
ortho = 1

def init():
	glClearColor(0.0,0.0,0.0,1.0)
	glEnable(GL_DEPTH_TEST)
	glViewport(0,0,width,height)
	glMatrixMode(GL_PROJECTION)
	glLoadIdentity()
	if ortho == 1:
		print "Orthographic Projection"
		# creates a viewing cube for drawing
		# there is no foreshortening at a distance
		glOrtho(-2.0,2.0,-2.0,2.0,-2.0,2.0)
	else:
		print "Perspective Projection"
		# creates a viewing frustum
		# with a viewing angle of 40 degrees
		# and a near-far boundary of 1 to 40
		# and an aspect ratio of 1
		gluPerspective(30.,1.,1.,100.)
		
		# place our "eye" at (0,0,5) while
		# we look at the origin (0,0,0)
		# and the y-axis is up (0,1,0)
		gluLookAt(0,0,5,
			0,0,0,
			0,1,0)
	
	glMatrixMode(GL_MODELVIEW)	
	glLoadIdentity()
	glutPostRedisplay()

#The usual display routine
def display():
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
	glColor3f(1.0,0.0,0.0)
	glutWireCube(1.50)
	
	# Put wire spheres at each cube vertex
	# glPushMatrix() is a valuable command
	# It allows us to move the focus of the drawing
	# to other places without affecting the original origin
	glPushMatrix()
	glTranslatef(0.75,0.75,0.75)
	glColor3ub(255,255,64)
	glutWireSphere(.10,10,10)
	glPopMatrix()
	
	glPushMatrix()
	glTranslatef(-0.75,0.75,0.75)
	glColor3ub(64,196,255)
	glutWireSphere(.10,10,10)
	glPopMatrix()
	
	glPushMatrix()
	glTranslatef(0.75,-0.75,0.75)
	glColor3ub(255,64,255)
	glutWireSphere(.10,10,10)
	glPopMatrix()
	
	glPushMatrix()
	glTranslatef(0.75,0.75,-0.75)
	glColor3ub(196,255,128)
	glutWireSphere(.10,10,10)
	glPopMatrix()
	
	glPushMatrix()
	glTranslatef(-0.75,-0.75,0.75)
	glColor3ub(128,255,64)
	glutWireSphere(.10,10,10)
	glPopMatrix()
	
	glPushMatrix()
	glTranslatef(0.75,-0.75,-0.75)
	glColor3ub(255,32,196)
	glutWireSphere(.10,10,10)
	glPopMatrix()
	
	glPushMatrix()
	glTranslatef(-0.75,0.75,-0.75)
	glColor3ub(255,128,255)
	glutWireSphere(.10,10,10)
	glPopMatrix()
	
	glPushMatrix()
	glTranslatef(-0.75,-0.75,-0.75)
	glColor3ub(64,255,255)
	glutWireSphere(.10,10,10)
	glPopMatrix()
	
	glutSwapBuffers()
	
#keyboard stuff
def keyboard(key, x, y):
	global ortho
	if key == chr(27) or key == 'q':
		sys.exit(0)
	if key == "o":
		ortho = 1
	if key == "p":
		ortho = 0
	init()

#Traditional main subroutine	
def main() :
	global width
	global height
	glutInit(sys.argv)
	glutInitDisplayMode(GLUT_RGB | GLUT_DEPTH | GLUT_DOUBLE)
	glutInitWindowPosition(50, 50)
	glutInitWindowSize(width, height)
	
	glutCreateWindow("Ortho vs. Perspective")
	glutKeyboardFunc(keyboard)
	glutDisplayFunc(display)
	init()
	
	glutMainLoop()
	
#Calls main()
main()

