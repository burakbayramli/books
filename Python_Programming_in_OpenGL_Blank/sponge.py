# The Chaos Game

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from random import *
import sys

def init():
	glClearColor(0.0, 0.0, 0.0, 0.0)  #Black background
	glColor3f(1.0, 0.0, 0.5)
	#Uncomment the glColor line below and comment
	#the glColor above for random colors
	#Where would you put the line below IF you
	#Want each dot to have a random color?
	
	#glColor3f(random(),random(),random())
	gluPerspective(45,1.0,1.0,40.0)

def chaos():
    glClear(GL_COLOR_BUFFER_BIT)

    # You can start at ANY random point
    x = random()
    y = random()
    z = random()
    
    for n in range(0,50000):
        n+=1
        vertx = randint(1,4)
        if vertx == 1:
            x = x/2.0
            y = (y+2.0)/2.0
            z = (z+2.0)/2.0
        if vertx == 2:
            x = (x-2.0)/2.0
            y = (y-2.0)/2.0
            z = (z+2.0)/2.0
        if vertx == 3:
            x = (x+2.0)/2.0
            y = (y-2.0)/2.0
            z = (z-2.0)/2.0
        if vertx == 4:
            x = (x+2.0)/2.0
            y = (y-2.0)/2.0
            z = (z+2.0)/2.0
        
        if n > 30: 
            glBegin(GL_POINTS)
            glVertex3f(x,y,z-8)
            glEnd()
            glFlush()

def main():
	glutInitDisplayMode(GLUT_RGB | GLUT_SINGLE)
	glutInitWindowPosition(50, 50)
	glutInitWindowSize(300, 300)
	glutInit(sys.argv)
	glutCreateWindow("Sierpinski")
	glutDisplayFunc(chaos)
	init()
	glutMainLoop()
	
main()	