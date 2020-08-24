# PyMandelBrot.py
# Plot a Mandelbrot set
# And include a mouse zoom
# Julia Set option enabled

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from math import *
from time import *
import sys

# If psyco isn't installed, delete the next two lines!
import psyco
psyco.full()

# Set initial window width and height
# Declare global variables
global width
global height
global hCenter
global vCenter
global nRange
global hStep
global vStep
global yInit
global xInit

# The next lines allow for the
# Julia Set options
global mandel
global tmpRng
global julHCenter
global julVCenter
global julFlag
global julX
global julY

width = 300
height = 300

# mandel = 1 for M-Set
# mandel = 1 for Julia Set
mandel = 1

def zap():
	# Reset everything
	global hCenter
	global vCenter
	global nRange
	global mandel
	global julFlag
	hCenter = 0.0
	vCenter = 0.0
	nRange = 1.5
	mandel = 1
	julFlag = 0
	init()

def init():
	# Identify the globals
	global hCenter
	global vCenter
	global nRange
	global hStep
	global vStep
	global yInit
	global xInit
	global yFinal
	global xFinal
	global mandel
	global julHCenter
	global julVCenter
	
	# Set the screen plotting coordinates and the step
	glClearColor(0.0, 0.0, 0.0, 0.0)
	hStep = 2*nRange/(width+1)
	vStep = 2*nRange/(height+1)
	if mandel == 1:
		yInit = vCenter + nRange
		xInit = hCenter - nRange
		yFinal = vCenter - nRange
		xFinal = hCenter + nRange
	else:
		yInit = julVCenter + nRange
		xInit = julHCenter - nRange
		yFinal = julVCenter - nRange
		xFinal = julHCenter + nRange
		
	#  Fill the entire graphics window!
	glViewport(0, 0, width, height)
	
	#  Set the projection matrix... our "view"
	glMatrixMode(GL_PROJECTION)
	glLoadIdentity()
	
	# Set the window plot coordinates
	#gluOrtho2D(xInit,xFinal,yFinal,yInit)
	gluOrtho2D(xInit,xFinal,yFinal,yInit)
	
	#  Set the matrix for the object we are drawing
	glMatrixMode(GL_MODELVIEW)
	glLoadIdentity()
	glutPostRedisplay()
	
def keyboard(key, x, y):
	global mandel
	global tmpRng
	global nRange
	global hCenter
	global vCenter
	global julFlag
	
	if mandel == 1:
		tmpRng = nRange
	
	#  Allows us to quit by pressing 'Esc' or 'q'
	if key == chr(27):
		sys.exit()
	if key == "z":
		zap()
	if key == "j":
		mandel = 0
		nRange = 2.0
	if key == "m":
		mandel = 1
		nRange = tmpRng
		julFlag = 0
		init()
	if key == "q":
		sys.exit()
	
def drawMandel():
    glClear(GL_COLOR_BUFFER_BIT)
    y = yInit
    
    # toggle Julia Set or M-Set
    # mandel == 0 is the Julia Set
    if mandel == 0:
        a = complex(julX, julY)
    glBegin(GL_POINTS)
    tim = time()
    while y > yFinal:
        y-= vStep
        x = xInit
        while x < xFinal:
            x+= hStep
            
            n = 0
            
            # Choose M-Set or Julia Set
            if mandel == 1:
                z = a = complex(x,y)
            else:
                z = complex(x,y)

            while n < 200:
                n+=1 
                try:
                    z = z*z + a
                    #z = sinh(z*z) + a
                    #z = tanh(z*z*z) + a
                    #z = sin(z)**2/(abs(z)-1)
                except:
                    pass
                zz = abs(z)
                if zz > 2:
                    # Weird colors around the M-Set
                    glColor3f((3*z.real),(3*z.real),4*(zz))
                    glVertex2f(x,y)
                    n = 5001
            if zz < 2:
                # Coloration in the M-Set
                #glColor3f(z.real*tan(9)+1,3*sin(20*zz)*cos(abs(z.imag)),2*cos(zz)*sin(40*zz)/sin(z.real))	
                glColor3f(3*(3*zz),(3*z.real),2*(zz))
                #glColor3f(.9,.2,.5)
                glVertex2f(x,y)
    glEnd()
    glFlush()
    print time()-tim

def mouse(button, state, x, y):
	global hCenter
	global vCenter
	global nRange
	global julHCenter
	global julVCenter
	global julFlag
	global julX
	global julY
	
	# Detect the left/right mouse buttons and the click
	# Followed by resetting the origin
	# Left mouse button zooms in, right button zooms out
	if button == GLUT_LEFT_BUTTON and state == GLUT_DOWN:
		if mandel == 1:
			hCenter = xInit + (xFinal - xInit)*x/width
			vCenter = yInit + (yFinal - yInit)*y/height
			nRange = nRange/2
			init()
		else:
			julHCenter = xInit + (xFinal - xInit)*x/width
			julVCenter = yInit + (yFinal - yInit)*y/height
			if julFlag == 0:
				print "Julia", julHCenter, julVCenter
				julX = julHCenter
				julY = julVCenter
				julHCenter = 0.0
				julVCenter = 0.0
				init()
			else:
				julHCenter = xInit + (xFinal - xInit)*x/width
				julVCenter = yInit + (yFinal - yInit)*y/height
				nRange = nRange/2
				init()
			julFlag = 1
			
	if button == GLUT_RIGHT_BUTTON and state == GLUT_DOWN:
		if mandel == 1:
			hCenter = xInit + (xFinal - xInit)*x/width
			vCenter = yInit + (yFinal - yInit)*y/height
			nRange = 2*nRange
			init()
		else:
			julHCenter = xInit + (xFinal - xInit)*x/width
			julVCenter = yInit + (yFinal - yInit)*y/height
			if julFlag == 0:
				print "Julia", julHCenter, julVCenter
				julX = julHCenter
				julY = julVCenter
				julHCenter = 0.0
				julVCenter = 0.0
				init()
			else:
				julHCenter = xInit + (xFinal - xInit)*x/width
				julVCenter = yInit + (yFinal - yInit)*y/height
				nRange = 2*nRange
				init()
			julFlag = 1
def main():
	glutInit(sys.argv)
	glutInitDisplayMode(GLUT_RGB | GLUT_SINGLE)
	glutInitWindowPosition(50, 50)
	glutInitWindowSize(width, height)
	glutCreateWindow("Mandelbrot Set")
	glutDisplayFunc(drawMandel)
	glutMouseFunc(mouse)
	glutKeyboardFunc(keyboard)
	zap()
	glutMainLoop()
	
main()	