# glShapes.py
# Shapes and rotations with special keys

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
import sys

#  Set the width and height of the window
global width
global height

#  Global variables for rotation angles
global xrot
global yrot

xrot = 0.0
yrot = 0.0

width = 800
height = 800

# Light values and coordinates
global  ambientLight
global  diffuseLight
global  specular
global  specref

ambientLight =  (0.3, 0.3, 0.3, 1.0)
diffuseLight = ( 0.7, 0.7, 0.7, 0.7)
specular = (1.0, 1.0, 1.0, 1.0)
specref = (1.0, 1.0, 1.0, 1.0)

global glutshape
glutshape = 1

def processmenu( value):
	global glutshape
	glutshape = value
	glutPostRedisplay()
	
def renderscene():
	global glutshape
	global xrot
	global yrot
	
	glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT)
	glPushMatrix()
	glRotatef(xrot, 1.0, 0.0, 0.0)
	glRotatef(yrot, 0.0, 1.0, 0.0)
	
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
	
	elif glutshape == 11:
		glutSolidSphere(1.0, 25, 25)
	elif glutshape == 12:
		glutSolidCube(1.0)
	elif glutshape == 13:
		glutSolidCone(0.3, 1.1, 20, 20)
	elif glutshape == 14:
		glutSolidTorus(0.3, 1.0, 10, 25)
	elif glutshape == 15:
		glutSolidDodecahedron()
	elif glutshape == 16:
		glutSolidOctahedron()
	elif glutshape == 17:
		glutSolidTetrahedron()
	elif glutshape == 18:
		glutSolidIcosahedron()
	elif glutshape == 19:
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
    
    glColor3ub(230,100,155)

    wiremenu = glutCreateMenu(processmenu)
    glutAddMenuEntry("Sphere", 1)
    glutAddMenuEntry("Cube", 2)
    glutAddMenuEntry("Cone", 3)
    glutAddMenuEntry("Torus", 4)
    glutAddMenuEntry("Dodecahedron", 5)
    glutAddMenuEntry("Octahedron", 6)
    glutAddMenuEntry("Tetrahedron", 7)
    glutAddMenuEntry("Icosahedron", 8)
    glutAddMenuEntry("Teapot", 9)

    solidmenu = glutCreateMenu(processmenu)
    glutAddMenuEntry("Sphere", 11)
    glutAddMenuEntry("Cube", 12)
    glutAddMenuEntry("Cone", 13)
    glutAddMenuEntry("Torus", 14)
    glutAddMenuEntry("Dodecahedron", 15)
    glutAddMenuEntry("Octahedron", 16)
    glutAddMenuEntry("Tetrahedron", 17)
    glutAddMenuEntry("Icosahedron", 18)
    glutAddMenuEntry("Teapot", 19)

    mainmenu = glutCreateMenu(processmenu)
    glutAddSubMenu("Wire", wiremenu)
    glutAddSubMenu("Solid", solidmenu)
    glutAttachMenu(GLUT_RIGHT_BUTTON)
    
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
	
	# allows for reshaping the window without distorting
	# the glut shape

	if w <= h:
		glOrtho(-nRange, nRange, -nRange*h/w, nRange*h/w, -nRange, nRange)
	else:
		glOrtho(-nRange*w/h, nRange*w/h, -nRange, nRange, -nRange, nRange)

	glMatrixMode(GL_MODELVIEW)
	glLoadIdentity()
	
	glLightfv(GL_LIGHT0, GL_POSITION, lightPos)
	
def keyboard(key, x, y):
	if key == chr(27):
		sys.exit()

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
