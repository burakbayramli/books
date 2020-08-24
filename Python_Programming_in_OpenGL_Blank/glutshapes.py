# glutshapes.py
# Shapes and rotations with special keys

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
import sys

#import psyco
#psyco.full()

#  Set the width and height of the window
global width
global height

#  Global variables for rotation angles
global xrot
global yrot

xrot = 0.0
yrot = 0.0

width = 300
height = 300

# Light values and coordinates
global  ambientLight
global  diffuseLight
global  specular
global  specref

ambientLight =  (0.35, 0.35, 0.35, 1.0)
diffuseLight = ( 0.75, 0.75, 0.75, 0.7)
specular = (1.0, 1.0, 1.0, 1.0)
specref = (1.0, 1.0, 1.0, 1.0)

global glutshape
global solid
solid = "w"
glutshape = 1
	
def renderscene():
	global xrot
	global yrot
	
	glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT)
	glPushMatrix()
	glRotatef(xrot, 1.0, 0.0, 0.0)
	glRotatef(yrot, 0.0, 1.0, 0.0)
	
	if solid == "w":
		if glutshape == 1:
			glutWireSphere(1.0, 25, 25)
		elif glutshape == 2:
			glutWireCube(1.0)
		elif glutshape == 3:
			glutWireCone(0.3, 1.1, 20, 20)
		elif glutshape == 4:
			glutWireTorus(0.3, 1.0, 10, 25)
		elif glutshape == 5:
			glutWireDodecahedron()
		elif glutshape == 6:
			glutWireOctahedron()
		elif glutshape == 7:
			glutWireTetrahedron()
		elif glutshape == 8:
			glutWireIcosahedron()
		elif glutshape == 9:
			glutWireTeapot(1.0)
	elif solid == "s":
		if glutshape == 1:
			glutSolidSphere(1.0, 25, 25)
		elif glutshape == 2:
			glutSolidCube(1.0)
		elif glutshape == 3:
			glutSolidCone(0.3, 1.1, 20, 20)
		elif glutshape == 4:
			glutSolidTorus(0.3, 1.0, 10, 25)
		elif glutshape == 5:
			glutSolidDodecahedron()
		elif glutshape == 6:
			glutSolidOctahedron()
		elif glutshape == 7:
			glutSolidTetrahedron()
		elif glutshape == 8:
			glutSolidIcosahedron()
		elif glutshape == 9:
			glutSolidTeapot(1.0)
	
	glPopMatrix()
	glutSwapBuffers()
			
def init():
    global width
    global height

    glClearColor(0.0, 0.0, 0.0, 1.0)
    
    # Enable depth testing
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
    
    glColor3ub(230,100,100)

def specialkeys(  key,  x,  y):
	global xrot
	global yrot
	
	if key == GLUT_KEY_UP:
		xrot -= 2.0
	if key == GLUT_KEY_DOWN:
		xrot += 2.0
	if key == GLUT_KEY_LEFT:
		yrot -= 2.0
	if key == GLUT_KEY_RIGHT:
		yrot += 2.0
	
	glutPostRedisplay()
	
def reshape(  w,  h):
	lightPos = (-50.0, 50.0, 100.0, 1.0)
	nRange = 2.0
	
	if h==0:
		h = 1
	
	glViewport(0, 0, w, h)
	
	glMatrixMode(GL_PROJECTION)
	glLoadIdentity()
	
	if w <= h:
		glOrtho(-nRange, nRange, -nRange*h/w, nRange*h/w, -nRange, nRange)
	else:
		glOrtho(-nRange*w/h, nRange*w/h, -nRange, nRange, -nRange, nRange)

	glMatrixMode(GL_MODELVIEW)
	glLoadIdentity()
	
	glLightfv(GL_LIGHT0, GL_POSITION, lightPos)
	
def keyboard(key, x, y):
	global glutshape, solid
	if key == chr(27) or key == "q":
		sys.exit()
	try:
		if int(key) < 10:
			glutshape = int(key)
	except:
		pass
	
	if key == "w" or key == "s":
		solid = key
	
	glutPostRedisplay()

def main():
    
    global width
    global height
    
    #  Setup for double-buffered display and depth testing
    glutInitDisplayMode(GLUT_RGB|GLUT_DOUBLE|GLUT_DEPTH)
    glutInitWindowPosition(100,100)
    glutInitWindowSize(width,height)
    glutInit(sys.argv)
    glutCreateWindow("GLUT Shapes... Rotations")
  
    init()

    glutReshapeFunc(reshape)

    glutDisplayFunc(renderscene)
    glutKeyboardFunc(keyboard)
    glutSpecialFunc(specialkeys)
    glutMainLoop()

main()    
