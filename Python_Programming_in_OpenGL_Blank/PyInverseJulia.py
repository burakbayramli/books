# PyInverseJulia.py
# Plot a Julia set
# Using inverse iteration

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from random import *
from numpy import *
import sys

# If psyco isn't installed, delete the next two lines!
#import psyco
#psyco.full()

nRange = 2.0
width = 1000
height = 1000

def init():
	glClearColor(0.0, 0.0, 0.0, 0.0)
	gluOrtho2D(-nRange,nRange,-nRange,nRange)

def drawMandel():
	glClear(GL_COLOR_BUFFER_BIT)
	
	# complex seed point
	a = complex(-.780737,-.105882)
	for i in range(0,10000):
			glBegin(GL_POINTS)
			x = 2*nRange*random()-2
			y = 2*nRange*random()-2
			
			n = 0
			z = complex(x,y)
			
			# since there are two square roots
			# we randomly choose between them
			while n < 10:
				n+=1 
				if random() < .5:
					z = sqrt(z-a)
				else:
					z = -sqrt(z-a)
				zz = abs(z)
				
			glColor3f(3/sin(3*zz),cos(3*z.real),2*sin(zz))
			glVertex2f(z.real,z.imag)
			glEnd()
			glFlush()
			
def main():
	glutInitDisplayMode(GLUT_RGB | GLUT_SINGLE)
	glutInitWindowPosition(50, 50)
	glutInitWindowSize(width, height)
	glutInit(sys.argv)
	glutCreateWindow("Julia Set")
	glutDisplayFunc(drawMandel)
	init()
	glutMainLoop()
	
main()	
