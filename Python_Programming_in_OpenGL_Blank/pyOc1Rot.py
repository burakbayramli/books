#PyOc1.py
#From George K. Francis
#and Glenn Chappell for chaptrack rotations
#Ported to Python by Stan Blank

from OpenGL.GL import *
from OpenGL.GLUT import *
from OpenGL.GLU import *
import sys

global vv
vv = [(1.0,0.0,0.0),
      (0.0,1.0,0.0),
      (0.0,0.0,1.0),
      (-1.0,0.0,0.0),
      (0.0,-1.0,0.0),
     (0.0,0.0,-1.0)]
	
#vv = vv + [(0.0, 0.0, -1.0)]
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
MouseX = wd/2
global MouseY
MouseY = ht/2

#rotation globals
global rotclock
global rotanticlock

rotclock = 0
rotanticlock = 0

def display():
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
	glMatrixMode(GL_MODELVIEW)
	glPushMatrix()
	glLoadIdentity()
	glMultMatrixf(aff)
	
	glBegin(GL_TRIANGLE_FAN)
        glColor3f(0.0,255.0,0.0)
        glVertex3fv(vv[1])
        glColor3f(0.,0.,255.)  
        glVertex3fv(vv[0])
        glColor3f(0.,255.,255.)
        glVertex3fv(vv[5])
        glColor3f(255.,255.,0.)
        glVertex3fv(vv[3])
        glColor3f(255.,0.,0.)  
        glVertex3fv(vv[2])
        glColor3f(0.,0.,255.)  
        glVertex3fv(vv[0])	
	glEnd()
	
	glBegin(GL_TRIANGLE_FAN)
	glColor3f(255.0,0.0,255.0)
        glVertex3fv(vv[4])
        glColor3f(0.0,0.0,255.0)  
        glVertex3fv(vv[0])
        glColor3f(0.0,255.0,255.0)
        glVertex3fv(vv[5])
        glColor3f(255.,255.,0.)
        glVertex3fv(vv[3])
        glColor3f(255.,0.,0.)  
        glVertex3fv(vv[2])
        glColor3f(0.,0.,255.)  
        glVertex3fv(vv[0])	
	glEnd()
	
	#glutWireSphere(1.,50,50)
	
	glPopMatrix()
	glFlush()
	glutSwapBuffers()
	
def keyboard(key, x, y):
	if key == chr(27) or key == 'q':
		sys.exit(0)
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
		glOrtho(-2.0,2.0,-2.0*ht/wd,2.0*ht/wd,-2.0,2.0)
	else:
		glOrtho(-2.0*wd/ht,2.0*wd/ht,-2.0,2.0,-2.0,2.0)
		
	glMatrixMode(GL_MODELVIEW)
	glLoadIdentity()
	
#def motion():
#	return 0

def chaptrack():
    global MouseX
    global MouseY
    global wd
    global ht

    #mouse button rotations
    global rotclock
    global rotanticlock
    
    dx = (MouseX-wd/2)/256.0 
    dy = (MouseY-ht/2)/256.0 
    glMatrixMode(GL_TEXTURE)
    glPushMatrix()
    glLoadIdentity()
    
    #rotation using mouse button
    glRotatef(rotclock, 0.0, 0.0, 1.0)
    glRotatef(rotanticlock, 0.0, 0.0, 1.0)
    glRotatef(dx,0,1.0,0.0) 
    glRotatef(dy,1.0,0.0,0.0)
    global aff
    glMultMatrixf(aff) 
    aff = glGetFloatv(GL_TEXTURE_MATRIX)
    glPopMatrix()

def idle():
	chaptrack()
	glutPostRedisplay()
	
def mousemotion(x,y):
	global MouseX
	MouseX = x
	global MouseY 
	MouseY = y
    
def mouse(button ,state, x, y):
    global rotclock
    global rotanticlock
    if button == GLUT_LEFT_BUTTON and state == GLUT_DOWN:
        rotanticlock = 1.0
        rotclock = 0.0
    if glutGetModifiers() == GLUT_ACTIVE_SHIFT and button == GLUT_LEFT_BUTTON and state == GLUT_DOWN:
        rotanticlock = 5.0
        rotclock = 0.0
    if button == GLUT_LEFT_BUTTON and state == GLUT_UP:
        rotanticlock = 0.0
        
    if button == GLUT_RIGHT_BUTTON and state == GLUT_DOWN:
        rotanticlock = 0.0
        rotclock = -1.0
    if glutGetModifiers() == GLUT_ACTIVE_SHIFT and button == GLUT_RIGHT_BUTTON and state == GLUT_DOWN:
        rotanticlock = 0.0
        rotclock = -5.0
    if button == GLUT_RIGHT_BUTTON and state == GLUT_UP:
        rotclock = 0.0
	
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
	glutMouseFunc(mouse)

	glutIdleFunc(idle)
	glutPassiveMotionFunc(mousemotion)
	glEnable(GL_DEPTH_TEST)
	glShadeModel(GL_SMOOTH)
	glMatrixMode(GL_PROJECTION)
	glLoadIdentity()
	glOrtho(-2.0,2.0,-2.0,2.0,-2.0,2.0)
	glMatrixMode(GL_MODELVIEW)
	glLoadIdentity()
	

	glutMainLoop()
	
main()
