# pyRandQuat.py
# Dr. Blank's version
# of the Quaternion Julia Set
# with a Mandelbrot option

from OpenGL.GL import *
from OpenGL.GLUT import *
from OpenGL.GLU import *
from math import *
from random import *
import sys

import psyco
psyco.full()

#define some globals
global vv
global aff
global wd
global ht
global MouseX
global MouseY

# for the complex arithmetic calculations
global cr
global ci
global cj
global ck
global wk
global count
global mand
global iter
global maxpoints
global quatpoints

# initial values for complex parameters
# change these for a different set
cr = -0.20
ci = 0.80
cj = 0.0
ck = 0.0
wk = 0.0

# start out in the Julia Set... mand = 0
# mand = 1 is the Mandelbrot set
mand = 0
iter = 10

# one million random points to test
# be patient!
maxpoints = 1000000
quatpoints = 0

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
    global quatpoints
    vv = []
    count = 0
    n = 0
    quatpoints = 0

    while count < maxpoints:
        count = count + 1
        x = 4*random() - 2
        y = 4*random() - 2
        z = 4*random() - 2

        leng = calcleng(x, y, z)
        
        # the point is constrained, plot it!
        if leng < 4:
            quatpoints = quatpoints + 1
            vv = vv + [(x,y,z)]

    dolist()

def calcleng(x, y, z):
    n = 0
    w = wk
    if mand == 1:
        kr = x
        ki = y
        kj = z
        kk = 0
    else:
        kr = cr
        ki = ci
        kj = cj
        kk = ck
    
    while n < iter:
        n = n + 1 

        # quaternion multiplication
        temp = x+x
        x = x*x - y*y - z*z - w*w + kr
        y = temp*y + ki
        z = temp*z + kj
        w = temp*w + kk     
        
        # a form of the distance formula
        dist = x*x + y*y + z*z + w*w
        
        # if the point escapes to infinity, don't store it!
        if dist > 4:
            break   
    
    return dist            
    
def dolist():
    global ptcloud
    
    # start storing the display list in ptcloud
    ptcloud = glGenLists(1)
    
    # compile the ptcloud points
    glNewList(ptcloud, GL_COMPILE)
    glPointSize(2.0)
    glBegin(GL_POINTS)
    for n in range(quatpoints):
        glColor3f(sin(n),cos(n),4*sin(n)*cos(n))
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
    global mand
    
    # toggle between the Julia and Mandelbrot sets
    if key == 'm':
        mand = 1
        calcit()
    if key == 'j':
        mand = 0
        calcit()
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
	glutCreateWindow("Quaternion Fractals!")
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