# PyJulia.py
# Plot a Julia set

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from numpy import *
import sys

# Initalize screen dimensions and the screen origin
width = 500
height = 500
hcenter = 0.0
vcenter = 0.0
axrng = 1.5
hstep = 2*axrng/width
vstep = 2*axrng/height

def init():
	# White background
	glClearColor(1.0, 1.0, 1.0, 1.0) 

	# The next statement is all one line!!!
	gluOrtho2D(hcenter-axrng,hcenter+axrng,vcenter-	axrng,vcenter+axrng)

def drawjulia():
    glClear(GL_COLOR_BUFFER_BIT)
    glBegin(GL_POINTS)
    
    # Julia set complex number
    z = complex(-0.74543, 0.11301)
    
    y = vcenter + axrng
    while y > vcenter - axrng:
        y-= vstep
        x = hcenter - axrng
        while x < hcenter + axrng:
            x+= hstep

            n = 0
            a = complex(x,y)

            # n < 100 is the number of iterations
            # Increase this value to show finer detail
            # Decrease the value if nothing shows on the screen
            while n < 200:
                n+=1 
                a = a**z+a**9 + sin(z*z*a)*a + z
                zz = abs(a)

                # zz > 2 is the critical escape value
                # Some functions require larger escape values
                # This zz > 2 conditional provides coloration for 				# points outside the Julia set
                #if abs(a.real) > 50 or abs(a.imag) > 50 or zz > 1:
                if zz > 9:		
                    glColor3f(sin(2*zz),cos(zz),sin(4*zz))
                    glVertex2f(x,y)
                    n = 5001
        
            # This zz < 2 conditional provides coloration for
            # points inside the Julia set.
            if abs(a.real) < 10 or abs(a.imag) < 9:
                glColor3f(0.0,0.0,0.0)	
                glVertex2f(x,y)
            else:
                glColor3f(1.0,1.0,1.0)	
                glVertex2f(x,y)
    glEnd()
    glFlush()
        
def main():
	glutInitDisplayMode(GLUT_RGB | GLUT_SINGLE)
	glutInitWindowPosition(50, 50)
	glutInitWindowSize(width, height)
	glutInit(sys.argv)
	glutCreateWindow("Julia Set")
	glutDisplayFunc(drawjulia)
	init()
	glutMainLoop()
	
main()

# End Program	
