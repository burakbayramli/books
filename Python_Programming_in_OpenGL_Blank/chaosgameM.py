# PyChaosGame.py

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from random import *
import sys

#  Set the global width, height, and axis ranges of the window
global width
global height
global axrng
global verts 
global xx
global yy
global vv
# Initial values
verts = [[0.0,2.0],[-2.0,-2.0],[2.0,-2.0]]
xx = 0.25
yy = 1.75

width = 500
height = 500
axrng = 2.0
				
def init():
	glClearColor(0.0, 0.0, 0.0, 0.0)
	glColor3f(0.0, 0.0, 0.0)
	glClear(GL_COLOR_BUFFER_BIT)

def idle():
    global xx
    global yy  
    global vv
    vv=randint(0,2)
    glColor3f(vv==0,vv==1,vv==2)	
    xx = (xx + verts[vv][0])/2
    yy = (yy + verts[vv][1])/2
    glutPostRedisplay()
    

def plotfunc():
        glPointSize(3)
#	glClear(GL_COLOR_BUFFER_BIT)
#uncomment the glClear and see why it's not there
        glBegin(GL_POINTS)
        glVertex2f(xx,yy)
        glEnd()
        glutSwapBuffers()
	
def reshape(w,  h):
	# To insure we don't have a zero height
	if h==0:
		h = 1
	
	#  Fill the entire graphics window!
	glViewport(0, 0, w, h)
	
	#  Set the projection matrix... our "view"
	glMatrixMode(GL_PROJECTION)
	glLoadIdentity()
	
	#  Set the aspect ratio of the plot so that it
	#  Always looks "OK" and never distorted.
	if w <= h:
		gluOrtho2D(-axrng, axrng, -axrng*h/w, axrng*h/w)
	else:
		gluOrtho2D(-axrng*w/h, axrng*w/h, -axrng, axrng)
	
	#  Set the matrix for the object we are drawing
	glMatrixMode(GL_MODELVIEW)
	glLoadIdentity()
	
def keyboard(key, x, y):
	#  Allows us to quit by pressing 'Esc' or 'q'
	if key == chr(27):
		sys.exit()
	if key == "q":
		sys.exit()

def main():
	global width
	global height
	
	glutInit(sys.argv)
	glutInitDisplayMode(GLUT_RGB|GLUT_DOUBLE)
	glutInitWindowPosition(100,100)
	glutInitWindowSize(width,height)
	glutCreateWindow("The Chaos Game")
	glutReshapeFunc(reshape)
        glutIdleFunc(idle)
	glutDisplayFunc(plotfunc)
	glutKeyboardFunc(keyboard)
	
	init()      #nothing is needed unless you black on white
	glutMainLoop()

main()
