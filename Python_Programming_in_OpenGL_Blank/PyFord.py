#pyOc1.py... George K. Francis
#Translated to Python by Stan Blank
#November 3, 2004

#import important GL stuff
from OpenGL.GL import *
from OpenGL.GLUT import *
from OpenGL.GLU import *
from numpy import *
from random import *
import sys

#define some globals
global aff
global wd
global ht
global MouseX
global MouseY
global brake
global x, y, i

# Light values and coordinates
global  ambientLight
global  diffuseLight
global  specular
global  specref
global  lightPos

ambientLight =  (0.5, 0.5, 0.5, 1.0)
diffuseLight = ( 0.7, 0.7, 0.7, 0.7)
specular = (1.0, 1.0, 1.0, 1.0)
specref = (1.0, 1.0, 1.0, 1.0)
lightPos = (20.0, 20.0, 200.0, 1.0)

#define the affine identity matrix
aff = (1.0,0.0,0.0,0.0,
		0.0,1.0,0.0,0.0,
		0.0,0.0,1.0,0.0,
		0.0,0.0,0.0,1.0)
		
#initial window and mouse settings
wd = 600
ht = 600
MouseX = wd/2.0
MouseY = ht/2.0

n = 50
x = zeros(n, float)
y = zeros(n, float)
h = zeros(n, float)
k = zeros(n, float)
r = zeros(n, float)
red = zeros(n, float)
green = zeros(n, float)
blue = zeros(n, float)
alpha = zeros(n, float)


# rotation speed
brake = 64.0

#The usual display routine
def display():
    global lightPos
    
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

    glLightfv(GL_LIGHT0, GL_POSITION, lightPos)
    #draw the top of the bell
    for i in range(0,n):
        glColor4f(red[i],green[i],blue[i],alpha[i])
        glPushMatrix()
        glLoadIdentity()

        glMultMatrixf(aff)
        glTranslatef(x[i],y[i],0.0)
        glutWireSphere(r[i], 50,50)
        glPushMatrix()
        glLoadIdentity()
        glMultMatrixf(aff)
        glTranslatef(x[i],-y[i],0.0)
        glutWireSphere(r[i], 50,50)
        glPopMatrix()
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
	
	gluPerspective(60.,1.,1.,400.)
	
	gluLookAt(0,0,10,0,0,0,0,1,0)
		
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
    global i, x, y, h, k, r, red, green, blue, alpha
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
    for i in range(0,n):
        k = randint(-i,i)
        h = randint(-i,i)
        try:
            x[i] =  1.0*(h/k)
            y[i] = 0.5*k*k
            r[i] = 0.5*k*k
            red[i] = abs(0.20*h/k)
            green[i] = abs(1.0/(h*h))
            blue[i] = abs(5.0/(k*k))
            alpha[i] = 1.0
        except:
            pass
        #print x[i], y[i], r[i]
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