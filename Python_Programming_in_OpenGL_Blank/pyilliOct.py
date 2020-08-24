# pyilliOct.py
# From George K. Francis
# and Glenn Chappell for chaptrack rotations

from OpenGL.GL import *
from OpenGL.GLUT import *
from OpenGL.GLU import *
import sys

# this array stores the 6 octahedron vertices
global vv
vv = [(1.0, 0.0, 0.0),
      (0.0, 1.0, 0.0),
      (0.0, 0.0, 1.0),
      (-1.0, 0.0, 0.0),
      (0.0, -1.0, 0.0),
      (0.0, 0.0, -1.0)]
	
# this aff array is the affine matrix which is used
# for translation and transformation of coordinates
# during rotation
global aff
aff = (1.0,0.0,0.0,0.0,
       0.0,1.0,0.0,0.0,
       0.0,0.0,1.0,0.0,
       0.0,0.0,0.0,1.0)

global wd
global ht
wd = 400
ht = 400

# initial mouse position is center of the window
global mouseX
global mouseY
mouseX = wd/2
mouseY = ht/2

def display():
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
	glMatrixMode(GL_MODELVIEW)
	glPushMatrix()
	glLoadIdentity()
	glMultMatrixf(aff)
	
	# creates the top half of the octahedron
	# if the indents cause problems, fix them!
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
	
	# creates the bottom half of the octahedron
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
	
	# uncomment this later
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
	
# Here is the mysterious rotation function in homage
# to Glenn Chappell
def chaptrack():
	global mouseX
	global mouseY
	global wd
	global ht
	dx = (mouseX-wd/2)/256.0 
	dy = (mouseY-ht/2)/256.0 
	glMatrixMode(GL_TEXTURE)
	glPushMatrix()
	glLoadIdentity()
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
    global mouseX
    global mouseY
    mouseX = x 
    mouseY = y
    
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
	
	glutIdleFunc(idle)
	glutPassiveMotionFunc(mousemotion)
	
	init()
    
	glutMainLoop()
	
main()
