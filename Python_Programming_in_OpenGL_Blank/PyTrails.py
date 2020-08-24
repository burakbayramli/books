#PyLorenz.py
#This program illustrates the Lorenz attractor
#Feb 24, 2005

#import important GL stuff
from OpenGL.GL import *
from OpenGL.GLUT import *
from OpenGL.GLU import *
import sys

#define some globals
global aff
global wd
global ht
global MouseX
global MouseY

#Lorenz globals
global dt
global a
global b
global c
global numspheres
global vertx
global colors
global trails
global trailflag
global flag

#For lighting
global whiteLight
global sourceLight
global lightPos
global attract

dt = .004
numspheres = 3000
trailflag = -1
flag = 1
attract = 1
vertx = []
colors = []
trails = []

whiteLight = (0.5, 0.5, 0.5, 1.0)
sourceLight = (0.3, 0.3, 0.3, 1.0)
lightPos = ( 20.0, 20.0, 20.0, 1.0 )

#define the affine identity matrix
#for mouse motion
aff = (1.0,0.0,0.0,0.0,
		0.0,1.0,0.0,0.0,
		0.0,0.0,1.0,0.0,
		0.0,0.0,0.0,1.0)
		
#initial window and mouse settings
wd = 800
ht = 800
MouseX = wd/2
MouseY = ht/2

#The usual display routine
def display():
	global trails
	global trailflag
	global flag
	
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
	
	glMatrixMode(GL_MODELVIEW)
	
	#glPushMatrix()
	glLoadIdentity()
	
	#glTranslatef(0,0,-60)
	# Uncomment the next line for mouse motion
	#glMultMatrixf(aff)
	
	#draw the Lorenz attractor
	for n in range(0,numspheres/3):
		glColor3ub(colors[n][0],colors[n][1],colors[n][2])
		if attract == 1:
			a = (10*vertx[n][1] - 10*vertx[n][0])*dt + vertx[n][0]
			b = (28*vertx[n][0] - vertx[n][1] - vertx[n][0]*vertx[n][2])*dt + vertx[n][1]
			c = (-2.66667*vertx[n][2] + vertx[n][0]*vertx[n][1])*dt + vertx[n][2]
		else:
			a =  vertx[n][0] - (vertx[n][1] + vertx[n][2])*dt
			b =  vertx[n][1] + (vertx[n][0] + 0.2*vertx[n][1])*dt 
			c =  vertx[n][2] + (vertx[n][0]*vertx[n][2] - 6.7*vertx[n][2])*dt 
		
		vertx[n] = [a,b,c]
		#glRotatef(15, 0,0,1)
		glPushMatrix()
		glTranslatef(a,b,c-60)
		glutSolidSphere(1.0,10,10)
		glPopMatrix()
		
	trails = trails + [[a,b,c]]
	if trailflag == -1:
		for n in range(2,flag):
			trails[n] = trails[n-1]
	if trailflag == 1:
		for n in range(flag-1,2,-1):
			trails[n] = trails[n-1]
		
		glBegin(GL_POINTS)
		for n in range(0,flag):
			glColor3ub(100,100,100)
			glVertex3f(trails[n][0],trails[n][1], trails[n][2] - 60)
		glEnd()
		
	#glPopMatrix()
	
	flag +=1
	if flag > 599:
		flag = 599
	
	glutSwapBuffers()

#keyboard stuff
def keyboard(key, x, y):
	global trailflag
	global attract
	if key == chr(27) or key == 'q':
		sys.exit(0)
	if key == "t":
		trailflag = -1*trailflag
	if key == "r":
		attract = -1*attract
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
	
	gluPerspective(45.,1.,1.,180.)
	
	gluLookAt(0,0,40,
              0,0,0,
              0,1,0)
	
	#if wd<=ht:
	#	glOrtho(-2.0,2.0,-2.0*ht/wd,2.0*ht/wd,-2.0,2.0)
	#else:
	#	glOrtho(-2.0*wd/ht,2.0*wd/ht,-2.0,2.0,-2.0,2.0)
	
	glMatrixMode(GL_MODELVIEW)	
	glLoadIdentity()
	glLightfv(GL_LIGHT0, GL_POSITION, lightPos)
	
#does nothing at this point
def motion(x, y):
	return 0

#chaptrack, python style.
#Note that we must declare the globals again
def chaptrack():
	global MouseX
	global MouseY
	global wd
	global ht
	global aff
	
	dx = (MouseX-wd/2)/96
	dy = (MouseY-ht/2)/96 
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
	#setup the spheres
	global vertx
	global colors
	
	# z position of first sphere
	zpos = 23.990
	
	#Places the spheres NEARLY on top of each other
	for n in range(0,numspheres):
		if (n+1)%3==0:
			zpos+=.0001
			vertx = vertx + [[8.0,8.0,zpos]]
	
	#Give the spheres a range of colors
	for n in range(0,numspheres,3):
		if n <= numspheres/10.:
			red = 255
			green = 0
			blue = 0
		elif n > numspheres/10. and n <= numspheres/5.:
			red = 255
			green = 128
			blue = 0
		elif n > numspheres/5. and n <= numspheres/3.3333:
			red = 255
			green = 255
			blue = 0
		elif n > numspheres/3.3333 and n <= numspheres/2.5:
			red = 128
			green = 255
			blue = 0
		elif n > numspheres/2.5 and n <= numspheres/2.:
			red = 0
			green = 255
			blue =0
		elif n > numspheres/2. and n <= numspheres/1.66667:
			red = 0
			green = 255
			blue = 128
		elif n > numspheres/1.66667 and n <= numspheres/1.428571:
			red = 0
			green = 128
			blue = 255
		elif n > numspheres/1.428571 and n <= numspheres/1.25:
			red = 0
			green = 0
			blue = 255
		elif n > numspheres/1.25 and n <= numspheres/1.11111:
			red = 128
			green = 0
			blue = 255
		else:
			red = 255
			green = 0
			blue = 255
			
		colors = colors + [(red,green,blue)]
		
	#setup for lighting... doesn't work well :-(
	glShadeModel(GL_SMOOTH)
	glEnable(GL_DEPTH_TEST)
	glEnable(GL_LIGHTING)
	glLightModelfv(GL_LIGHT_MODEL_AMBIENT, whiteLight)
	glLightfv(GL_LIGHT0, GL_DIFFUSE, sourceLight)
	
	glEnable(GL_LIGHT0)
	glEnable(GL_COLOR_MATERIAL)
	glColorMaterial(GL_FRONT, GL_AMBIENT_AND_DIFFUSE)

#Traditional main subroutine	
def main(argv) :
	global wd
	global ht
	glutInitDisplayMode(GLUT_RGB | GLUT_DEPTH | GLUT_DOUBLE)
	glutInitWindowPosition(50, 50)
	glutInitWindowSize(wd, ht)
	glutInit(argv)
	glutCreateWindow("Lorenz Attractor")
	glutKeyboardFunc(keyboard)
	glutReshapeFunc(reshape)
	glutDisplayFunc(display)
	glutMotionFunc(motion)
	##glutMouseFunc(mouse)
	glutIdleFunc(idle)
	glutPassiveMotionFunc(mousemotion)
	
	glMatrixMode(GL_MODELVIEW)
	glLoadIdentity()
	
	#Could add additional function calls  
	##init()
	##lists()
	setup()
	glutMainLoop()
	
#Necessary if we want to this program to run
main(sys.argv)
