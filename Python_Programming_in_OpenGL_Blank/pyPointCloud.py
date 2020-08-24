# pyPointCloud.py
# uses a display list

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
wd = 800
global ht
ht = 600
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
	
	glCallList(ptcloud)
	
	glPopMatrix()
	glFlush()
	glutSwapBuffers()
	
def keyboard(key, x, y):
	global s2
	global p2
    
    # toggle the 2s orbital
	if key == "s":
		s2 = 1
    
    # hide the 2s orbital
	if key == "z":
		s2 == 0
        
    # toggle the 2p orbitals
	if key == "p":
		p2 = 1
    
    # hide the 2p orbitals
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
    
    # start storing the display list in ptcloud
    ptcloud = glGenLists(1)
    
    # compile the ptcloud points
    glNewList(ptcloud, GL_COMPILE)
    
    glPointSize(2.0)
    glBegin(GL_POINTS)
    for n in range(50000):
        x = random()*2-1.0
        y = random()*2-1.0
        z = random()*2-1.0
        orb = sqrt(x*x + y*y + z*z)
        glColor3f(0.30,0.0,1.0)
        if orb > 0.25 and orb <=0.75:
            glVertex3f(x,y,z)
        if s2 == 1:
            x = random()*5.0-2.5
            y = random()*5.0-2.5
            z = random()*5.0-2.5
            orb = sqrt(x*x + y*y + z*z)
            glColor3f(0.70, 0.3, 0.30)
            if orb > 1.5 and orb <= 2.5:
                glVertex3f(x,y,z)
        if p2 == 1:
            x = random()*8-4.0
            y = random()*8-4.0
            z = random()*8-4.0
            orb = sqrt(x*x + y*y + z*z)
            glColor3f(0.50, 0.70, 0.30)
            if orb > 1.5 and orb <= 2.0:
                glVertex3f(x-2.0,0.5*y,0.5*z)
                glVertex3f(x+2.0,0.5*y,0.5*z)
                glVertex3f(0.5*x,y+2.0,0.5*z)
                glVertex3f(0.5*x,y-2.0,0.5*z)
                glVertex3f(0.5*x,0.5*y,z+2.0)
                glVertex3f(0.5*x,0.5*y,z-2.0)
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
	glutCreateWindow("Orbitals")
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
