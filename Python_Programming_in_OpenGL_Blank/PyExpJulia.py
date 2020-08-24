# PyJulia.py
# Plot a Julia set

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from numpy import *
import sys

# If psyco isn't installed, delete the next two lines!
#import psyco
#psyco.full()

# Initalize screen dimensions and the screen origin
width = 200
height = 200
hcenter = 0.0
vcenter = 0.0
axrng = 2.0
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
	z = complex(-0.75, 0.05)
	
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
				a = a**3 + z
				zz = abs(a)
				if zz > 2:
					glColor3f(cos(zz),sin(zz)*cos(zz),sin(zz))
					glVertex2f(x,y)
					n = 5001
			if zz < 200:
				glColor3f(tan(zz),sin(zz),cos(n))	
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
