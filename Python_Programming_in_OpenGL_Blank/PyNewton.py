from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from math import *
from numpy import *
import sys

#import psyco
#psyco.full()

def init():
	glClearColor(0.0, 0.0, 0.0, 0.0)  #Black background
	glColor3f(1.0, 0.0, 0.5)
	glOrtho(-2.0,2.0,-2.0,2.0,-1.5,1.5)

def drawNewton():
	glClear(GL_COLOR_BUFFER_BIT)
	glBegin(GL_POINTS)
	
	y = 2.0
	while y > -2.0:
		y-=.00375
		x = -2.0
		while x < 2.0:
			x+=.00375
			
			a = x
			b = y
			n = 0
			old = 0
			z = 0
			
			endit = 0
			while n < 200 and endit == 0:
				n+=1
				old = z
				den = (3 * a * a * a * a + 6 * a * a * b * b + 3 * b * b * b * b)
				xx = (a * a + 2 * a * a * a * a * a - b * b + 4 * a * a * a * b * b + 2 * a * b * b * b * b) / den
				yy = (-2 * a * b + 2 * a * a * a * a * b + 4 * a * a * b * b * b + 2 * b * b * b * b * b) / den
				z = xx + yy
				a = xx    # xx-1 is neat! also xx + sin(xx)
				b = yy
				
				if abs(z - old) < .000001:
					endit = 1
				
			if yy >= 0 and xx < 1:
				c1 = 6
				c2 = 12
				c3 = 18
			elif yy < 0 and xx < 1:
				c1 = 18
				c2 = 6
				c3 = 12
			
			if xx > 0:
				c1 = 12
				c2 = 18
				c3 = 6
			
			glColor3ub(n*c1,n*c2,n*c3)	
			glVertex3f(x,y,0)
	glEnd()
	glFlush()
			
def main():
	glutInitDisplayMode(GLUT_RGB | GLUT_SINGLE)
	glutInitWindowPosition(50, 50)
	glutInitWindowSize(800, 800)
	glutInit(sys.argv)
	glutCreateWindow("Newton's Madness")
	glutDisplayFunc(drawNewton)
	init()
	glutMainLoop()
	
main()	
