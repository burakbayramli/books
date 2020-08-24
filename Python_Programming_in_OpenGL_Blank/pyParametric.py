import string
from OpenGL.GL import *
from OpenGL.GLUT import *
from OpenGL.GLU import *
from numpy import *
from random import *
import sys

global ptcloud
global s2
global p2
global axrng

axrng = 5.0

p2 = 0
s2 = 0

global aff
aff = (1.0,0.0,0.0,0.0,
       0.0,1.0,0.0,0.0,
       0.0,0.0,1.0,0.0,
       0.0,0.0,0.0,1.0)

global wd
wd = 400
global ht
ht = 400
global MouseX
MouseX = wd/2.0
global MouseY
MouseY = ht/2.0

def display():
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
	glMatrixMode(GL_MODELVIEW)
	glPushMatrix()
	glLoadIdentity()
	glMultMatrixf(aff)
	
	glColor3f(1.0, 0.0, 1.0)
	#glutWireSphere(1.,50,50)
	glCallList(ptcloud)
	
	glPopMatrix()
	glFlush()
	glutSwapBuffers()
	
def keyboard(key, x, y):
	global s2
	global p2
	if key == "s":
		s2 = 1
	if key == "z":
		s2 = 0
	if key == "p":
		p2 = 1
	if key == "o":
		p2 = 0
	if key == chr(27) or key == 'q':
		sys.exit(0)
	lists()
	glutPostRedisplay()

def reshape(width, height):
	global wd
	global ht
	glClearColor(0.0, 0.0, 0.0, 0.0)
	if height == 0:
		height = 1
	wd = width
	ht = height
	glViewport(0,0,wd,ht)
	glMatrixMode(GL_PROJECTION)
	glLoadIdentity()
	
	if wd<=ht:
		glOrtho(-axrng,axrng,-axrng*ht/wd,axrng*ht/wd,-axrng,axrng)
	else:
		glOrtho(-axrng*wd/ht,axrng*wd/ht,-axrng,axrng,-axrng,axrng)
		
	glMatrixMode(GL_MODELVIEW)
	glLoadIdentity()
	
#def motion():
#	return 0

def chaptrack():
	global MouseX
	global MouseY
	global wd
	global ht
	dx = (MouseX-wd/2.0)/128 
	dy = (MouseY-ht/2.0)/128 
	glMatrixMode(GL_MODELVIEW)
	glPushMatrix()
	glLoadIdentity()
	glRotatef(dx,0,1.0,0.0) 
	glRotatef(dy,1.0,0.0,0.0)
	global aff
	glMultMatrixf(aff) 
	aff = glGetFloatv(GL_MODELVIEW_MATRIX)
	glPopMatrix()

def lists():
	global ptcloud
	ptcloud = glGenLists(1)
	glNewList(ptcloud, GL_COMPILE)
	glPointSize(2.0)
	glBegin(GL_POINTS)
	for n in range(25000):
		u = random()*6.28-3.14
		v = random()*6.28-3.14
				
		x = cos(u)*cos(v)
		y = sin(u)*cos(v)
		z = sin(v)
		glColor3f(0.30,0.0,1.0)
		glVertex3f(x,y,z)
		
		if s2 == 1:
			x = cos(u)*cos(v)
			y = sin(u)*cos(v)
			z = sin(v)
			glColor3f(0.80,0.40,0.60)
			glVertex3f(2*x,2*y,2*z)
			glColor3f(0.90,0.10,0.25)
			glVertex3f(4*x,4*y,4*z)
		
		if p2 == 1:
			x = cos(u)*sin(2*v)
			y = sin(u)*sin(2*v)
			z = sin(v)
			glColor3f(0.50,0.70,0.30)
			glVertex3f(x,y,3.5*z)
			glVertex3f(x,3.5*z,y)
			glVertex3f(3.5*z,x,y)
			
	glEnd()
	glEndList()

def idle():
	chaptrack()
	glutPostRedisplay()
	
def mousemotion(x,y):
	global MouseX
	MouseX = x
	global MouseY 
	MouseY = y
	
def main() :
	global wd
	global ht
	glutInitDisplayMode(GLUT_RGB | GLUT_DEPTH | GLUT_DOUBLE)
	glutInitWindowPosition(50, 50)
	glutInitWindowSize(wd, ht)
	glutInit([])
	glutCreateWindow("illiOct")
	glutKeyboardFunc(keyboard)
	glutReshapeFunc(reshape)
	glutDisplayFunc(display)
	#glutMotionFunc(motion)
	glutIdleFunc(idle)
	glutPassiveMotionFunc(mousemotion)
	glEnable(GL_DEPTH_TEST)	
	lists()

	glutMainLoop()
	
main()
