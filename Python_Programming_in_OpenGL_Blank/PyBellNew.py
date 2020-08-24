#pyOc1.py... George K. Francis
#Translated to Python by Stan Blank
#November 3, 2004

#import important GL stuff
from OpenGL.GL import *
from OpenGL.GLUT import *
from OpenGL.GLU import *
import sys

#define some globals
global belltop
global bellbottom
global aff
global wd
global ht
global MouseX
global MouseY
global brake

# Light values and coordinates
global  ambientLight
global  diffuseLight
global  specular
global  specref
global  lightPos

ambientLight =  (0.3, 0.3, 0.3, 1.0)
diffuseLight = ( 0.7, 0.7, 0.7, 0.7)
specular = (1.0, 1.0, 1.0, 1.0)
specref = (1.0, 1.0, 1.0, 1.0)
lightPos = (20.0, -20.0, 20.0, 1.0)

#define the vertex points of the bell top
belltop = [(0.0,1.0,0.0),
		(-.75,-.25,0.0),
		(-.475,0.0,.475),
		(0.0,-.25,.75),
		(.475,0.0,.475),
		(.75, -.25, 0.0),
		(.475,0.0,-.475),
		(0.0,-.25,-.75),
		(-.475, 0.0, -.475),
		(-.75, -.25, 0.0)]
		
#define the bell bottom vertex points
bellbottom = [(0.0, -.50, 0.0),
			(-.75,-.25,0.0),
			(-.475,0.0,.475),
			(0.0,-.25,.75),
			(.475,0.0,.475),
			(.75, -.25, 0.0),
			(.475,0.0,-.475),
			(0.0,-.25,-.75),
			(-.475, 0.0, -.475),
			(-.75, -.25, 0.0)]

#define the affine identity matrix
aff = (1.0,0.0,0.0,0.0,
		0.0,1.0,0.0,0.0,
		0.0,0.0,1.0,0.0,
		0.0,0.0,0.0,1.0)
		
#initial window and mouse settings
wd = 300
ht = 300
MouseX = wd/2.0
MouseY = ht/2.0

# rotation speed
brake = 1024.0

#The usual display routine
def display():
	global lightPos
	
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

	glPushMatrix()
	glLoadIdentity()
	
	glMultMatrixf(aff)
	
	glLightfv(GL_LIGHT0, GL_POSITION, lightPos)
	#draw the top of the bell
	glBegin(GL_TRIANGLE_FAN)
	
	for v in range(10):
		if v/2. <> int(v/2.):
			glColor3f(0.0,1.0,0.0)
		else:
			glColor3f(1.0,0.0,0.0)
		
		glVertex3fv(belltop[v])
	glEnd()
	
	#draw the bottom of the bell
	glBegin(GL_TRIANGLE_FAN)
	for v in range(10):
		if v/2. <> int(v/2.):
			glColor3f(0.0,1.0,0.0)
		else:
			glColor3f(1.0,0.0,0.0)
			
		glVertex3fv(bellbottom[v])
	glEnd()

	#glColor3f(0.0,0.0,0.0)
	#glutWireTeapot(1.0)
	
	glPopMatrix()
	
	glutSwapBuffers()
	

#keyboard stuff
def keyboard(key, x, y):
	if key == chr(27) or key == 'q':
		sys.exit(0)
	glutPostRedisplay()

#if we change the screen dimensions
def reshape(width, height):
	global wd
	global ht
	global lightPos
	
	glClearColor(0.0, 0.0, 0.0, 0.0)
	if height == 0:
		height = 1
	wd = width
	ht = height
	glViewport(0,0,wd,ht)
	glMatrixMode(GL_PROJECTION)
	glLoadIdentity()
	
	gluPerspective(40.,1.,1.,40.)
	
	gluLookAt(0,0,4,0,0,0,0,1,0)
		
	glMatrixMode(GL_MODELVIEW)	
	glLoadIdentity()
	
#does nothing at this point
def motion():
	return 0

#chaptrack, python style.
#Note that we must declare the globals again
def chaptrack():
	global MouseX
	global MouseY
	global wd
	global ht
	global aff
	
	dx = (MouseX-wd/2)/brake
	dy = (MouseY-ht/2)/brake
	glMatrixMode(GL_MODELVIEW)
	glPushMatrix()
	glLoadIdentity()
	
	glRotatef(dx,0,1.0,0.0) 
	glRotatef(dy,1.0,0.0,0.0)
	
	glMultMatrixf(aff) 
	
	#this line is different from the C
	#version.  Python handles it a bit
	#differently... was a pain to figure out!
	aff = glGetFloatv(GL_MODELVIEW_MATRIX)
	glPopMatrix()
	
	
#traditional idle
def idle():
	chaptrack()
	glutPostRedisplay()

#ditto traditional mousemotion
#Note globals
def mousemotion(x,y):
	global MouseX
	global MouseY
	MouseX = x 
	MouseY = y

def setup():
	glShadeModel(GL_SMOOTH)
	glEnable(GL_DEPTH_TEST)

	glEnable(GL_LIGHTING)

	glLightfv(GL_LIGHT0, GL_AMBIENT, ambientLight)
	glLightfv(GL_LIGHT0, GL_DIFFUSE, diffuseLight)
	glLightfv(GL_LIGHT0, GL_SPECULAR, specular)
	glEnable(GL_LIGHT0)
	glEnable(GL_COLOR_MATERIAL)
	glColorMaterial(GL_FRONT, GL_AMBIENT_AND_DIFFUSE)
	glMaterialfv(GL_FRONT, GL_SPECULAR, specref)
	glMateriali(GL_FRONT, GL_SHININESS, 128)

#Traditional main subroutine	
def main() :
	global wd
	global ht
	
	glutInitDisplayMode(GLUT_RGB | GLUT_DEPTH | GLUT_DOUBLE)
	glutInitWindowPosition(50, 50)
	glutInitWindowSize(wd, ht)
	glutInit(sys.argv)
	glutCreateWindow("German Bell... Merry Christmass from DrB")
	glutKeyboardFunc(keyboard)
	glutReshapeFunc(reshape)
	glutDisplayFunc(display)
	glutMotionFunc(motion)

	glutIdleFunc(idle)
	glutPassiveMotionFunc(mousemotion)
		
	setup()
	glutMainLoop()
	
main()