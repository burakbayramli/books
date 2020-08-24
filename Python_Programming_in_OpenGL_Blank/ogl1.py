#  First Python OpenGL Program
#  ogl1.py

from OpenGL.GLUT import *
from OpenGL.GLU import *
from OpenGL.GL import *
import sys

def draw():
	glClear(GL_COLOR_BUFFER_BIT)
	glutWireTeapot(0.5)
	glFlush()
	
glutInit(sys.argv)
glutInitDisplayMode(GLUT_SINGLE|GLUT_RGB)
glutInitWindowPosition(100,100)
glutInitWindowSize(250, 250)
glutCreateWindow("My First OGL Program")
glutDisplayFunc(draw)
glutMainLoop()