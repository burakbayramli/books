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
        
            n = 0
        
            z =  complex(x,y)

            endit = 0
            while n < 200 and endit == 0:
                n+=1
                old = z

                try:
                    #z = z - (sin(z*z) - z*z + 1)/(2*z*cos(z*z) - 2*z)
                    z = z-(z**3 - 1)/(3*z**2)
                    #z = z - (z**4+.84*z**2-.16)/(sin(4*z**3)+cos(1.68*z))
                    #z = z-(z**3.75-1)/(3.75*z**2.75)
                    #z = (z**3+1)/(z**2)
                except:
                    pass
                if abs(z - old) < .001:
                    endit = 1

			#if z.imag >= 0 and z.real < 1:
			#	c1 = 6
			#	c2 = 12
			#	c3 = 18

			#elif z.imag < 0 and z.real < 1:
			#	c1 = 18
			#	c2 = 6
			#	c3 = 12
			
			#if z.real > 0:
			#	c1 = 12
			#	c2 = 18
			#	c3 = 6

            #glColor3ub(n*c1,n*c2,n*c3)	
            if z.imag < 0:
                glColor3ub(n,int(abs(z)),20*n)
            else:
                glColor3ub(10*n,50,50*n)
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