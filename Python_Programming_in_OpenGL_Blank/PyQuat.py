# pyQuat.py

from OpenGL.GL import *
from OpenGL.GLUT import *
from OpenGL.GLU import *
from math import *
from random import *
import sys

#define some globals
global vv
global aff
global wd
global ht
global MouseX
global MouseY

global count

# variable to store the display list
global ptcloud

#define the vertex points
vv = []

#define the affine identity matrix
aff = (1.0,0.0,0.0,0.0,
		0.0,1.0,0.0,0.0,
		0.0,0.0,1.0,0.0,
		0.0,0.0,0.0,1.0)
		
#initial window and mouse settings
wd = 400
ht = 400
MouseX = wd/2
MouseY = ht/2

# calculate the quaternion fractal
def calcit():
	global vv
	global count
	count = 1
	s = 1
	pi = 3.141592653588
	a = 0.1
	b = -0.75
	theta = 20.

	theta = 2*pi*theta/360.

	x = a
	y = b*cos(theta)
	z = b*sin(theta)

    # change the values of k and l to change the shape
	k = 0.2
	l = 0.7
	phi = 20.

	phi = 2*pi*phi/360.

	a = k
	b = l*cos(phi)
	c = l*sin(phi)

	while count < 20000:
		x = x + a
		y = y + b
		z = z + c
	
		r = sqrt(x*x + y*y + z*z)
		x = x/r
		y = y/r
		z = z/r
		r = sqrt(r)
		m = sqrt(y*y + z*z)
		e = sqrt(.5*(1+x))
		g = sqrt(.5*(1-x))
		
		if m == 0:
			x = r
			y = 0
			z = 0
		else:
			x = r*e
			y = r*y*g/m
			z = r*z*g/m
		
		count = count + 1
		if random() > 0.5:
			s = -s
		x = s*x
		y = s*y
		z = s*z
		if count > 30:
			vv = vv + [(x,y,z)]
			vv = vv + [(-x,-y,-z)]

        dolist()

def dolist():
    global ptcloud
    
    # start storing the display list in ptcloud
    ptcloud = glGenLists(1)
    
    # compile the ptcloud points
    glNewList(ptcloud, GL_COMPILE)
    glPointSize(3.0)
    glBegin(GL_POINTS)
    for n in range(1,2*count-60,3):
        glColor3f(sin(n),cos(n),sin(n)*cos(n))
        glVertex3fv(vv[n])
    glEnd()        
    glEndList()
    
def display():
	global vv
	global count

	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
	glMatrixMode(GL_MODELVIEW)
	glPushMatrix()
	glLoadIdentity()
	glMultMatrixf(aff)
	
	glCallList(ptcloud)
		
	glPopMatrix()
	glFlush()
	glutSwapBuffers()

def keyboard(key, x, y):
	if key == chr(27) or key == 'q':
		sys.exit(0)
	glutPostRedisplay()

#if we change the screen dimensions
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
		glOrtho(-2.0,2.0,-2.0*ht/wd,2.0*ht/wd,-2.0,2.0)
	else:
		glOrtho(-2.0*wd/ht,2.0*wd/ht,-2.0,2.0,-2.0,2.0)
		
	glMatrixMode(GL_MODELVIEW)
	glLoadIdentity()
	
#does nothing at this point
#def motion():
#	return 0

def chaptrack():
	global MouseX
	global MouseY
	global wd
	global ht
	global aff
	dx = (MouseX-wd/2)/128.0
	dy = (MouseY-ht/2)/128.0 
	glMatrixMode(GL_TEXTURE)
	glPushMatrix()
	glLoadIdentity()
	glRotatef(dx,0,1.0,0.0) 
	glRotatef(dy,1.0,0.0,0.0)
	glMultMatrixf(aff) 
	aff = glGetFloatv(GL_TEXTURE_MATRIX)
	glPopMatrix()

def idle():
	chaptrack()
	glutPostRedisplay()

def mousemotion(x,y):
	global MouseX
	global MouseY
	MouseX = x 
	MouseY = y

def init():
    glEnable(GL_DEPTH_TEST)
    glShadeModel(GL_SMOOTH)

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
	#glutMouseFunc(mouse)
	glutIdleFunc(idle)
	glutPassiveMotionFunc(mousemotion)
	
	init()

	# calculate the fractal 
	calcit()

	glutMainLoop()
	
main()