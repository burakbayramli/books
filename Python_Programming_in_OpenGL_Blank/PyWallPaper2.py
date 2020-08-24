# PyWallPaper.py

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from numpy import *
import sys

#  Set the width and height of the window
global width
global height

#  Set the axis range globally
global nRange

#  Initial values
width = 500
height = 500
nRange = 150
				
def init():
	glClearColor(1.0, 1.0, 1.0, 1.0)
	
def plotPolar():
	glClear(GL_COLOR_BUFFER_BIT)
	
	glPointSize(1.0)
	glBegin(GL_POINTS)
	
	a = pi+.07
	b = .1
	c = 80
	x = 0
	y = 0
	
	for i in arange(1,100000,.1):
		glColor3f(sin(x),cos(y)*sin(x),sin(y))
		glVertex2f(x,y)
		xx = y - sin(x)   #   sign(x)*sqrt(abs(b*x-c))
		yy = a - x
		x = xx
		y = yy
	glEnd()
	
	
	#for x in arange(-nRange, nRange,.1):
	#	for y in arange(-nRange, nRange,.1):
	#		i = x
	#		j = y
	#		i = j - sign(i)*sqrt(abs(b*x-c))
	#		j = a-x 
			#r = (x**2 + y**2)
			#if int(r)%2 == 0:
	#		glColor3f(cos(i), cos(j), sin(i*j))
	#		glVertex2f(i,j)
	#glEnd()
	glFlush()
	
def Reshape(  w,  h):
	
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
		gluOrtho2D(-nRange, nRange, -nRange*h/w, nRange*h/w)
	else:
		gluOrtho2D(-nRange*w/h, nRange*w/h, -nRange, nRange)
	
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
	glutInitDisplayMode(GLUT_RGB|GLUT_SINGLE)
	glutInitWindowPosition(200,200)
	glutInitWindowSize(width,height)
	glutCreateWindow("Wallpaper for the Mind")
	glutReshapeFunc(Reshape)
	glutDisplayFunc(plotPolar)
	glutKeyboardFunc(keyboard)
	
	init()	
	
	glutMainLoop()

main()    