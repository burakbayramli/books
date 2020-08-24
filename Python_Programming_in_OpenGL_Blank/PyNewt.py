# PyNewton.py
# Newton's Method in the complex plane

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from math import *
from numpy import *
from time import *
import sys

# If psyco isn't installed, delete the next two lines!
import psyco
psyco.full()

width = 400
height = 400
hCenter = 0.0
vCenter = 0.0
nRange = 2.0
hStep = 2*nRange/(width+1)
vStep = 2*nRange/(height+1)

def init():
	# Black background
	glClearColor(0.0, 0.0, 0.0, 0.0)  
	gluOrtho2D(hCenter-nRange,hCenter+nRange,vCenter-nRange,vCenter+nRange)

def drawNewton():
    glClear(GL_COLOR_BUFFER_BIT)
    glBegin(GL_POINTS)
    a = time()
    y = vCenter + nRange
    while y > vCenter - nRange:
        y-= vStep
        x = hCenter - nRange
        while x < hCenter + nRange:
            x+=hStep

            n = 0

            z = complex(x,y)
            endit = 0

            # 100 iterations at maximum
            while n < 150 and endit == 0:
                n+=1
                old = z

                try:
                    # Newton's Method Equation
                    #z = z - (z*z*z - 1)/(3*z*z)
                    #z = z - (sin(z)/cos(z) - 1)/(1/((cos(z)*cos(z))))
                    #z = z - (z**4 + .84*z**2 - .16)/(4*z**3 + 1.68*z)
                    #z = z - (z**3 - 1)/(3*z**2)
                    #z = z - (sin(z)-1)/cos(z)
                    #z = z - (z**5 - 1)/(5*z**4)
                    #z = z - (z*z - 2**z - 1)/(2*z - 2**z*log(2))
                    #z = z - (z**3 - 3**z - 1)/(3*z**2 - 3**z*log(3))
                    #z = z - log(z)/(1/z)
                    #z = z - (z**3 + z**2 + z - 2)/(3*z**2 + 2*z + 1)
                    #z = z - (z**4 + .84*z**3 + .16*z - 2)/(4*z**3 + 2.52*z**2 + .16)
                    #z = z - (z**4 + .84*z**2 - .16)/(4*z**3 + 1.68*z)
                    #z = z - (z**3 - 5*z)/(3*z**2 - 5)
                    #z = z - (sin(z*z) - z*z + 1)/(2*z*cos(z*z) - 2*z)
                    #z = z - (z**3.7 - 1)/(3.7*z**2.7)
                    #z = z - (sin(z*z) - z*z*z + cos(z*z*z))/(2*z*cos(z*z) - 3*z*z - 3*z*z*sin(z*z*z))
                    #z = z - (z**z - 3**z - z**3 - 1)/((1+log(z))*z**z - log(3)*3**z - 3*z**2)
                    #z = z - (sin(z*z) - z*z + 1)/(2*z*cos(z*z) - 2*z)
                    z = (z**3+1)/(z**2)
                except:
                    pass

                if abs(z-old) < 0.001:
                    endit = 1
    
            #if z.imag >= 0 and z.real >= 1:
            #	c1 = 18
            #	c2 = 6
            #	c3 = 0
            #if z.imag >= 0 and z.real < 1:
            #	c1 = 6
            #	c2 = 12
            #	c3 = 18
            #if z.imag < 0 and z.real >= 1:
            #	c1 = 12
            #	c2 = 18
            #	c3 = 6
            #if z.imag < 0 and z.real < 1:
            #	c1 = 18
            #	c2 = 6
            #	c3 = 12
            
            if z.imag >= 0 and z.real < 1:
                c1 = 6
                c2 = 12
                c3 = 18
                
            if z.imag < 0 and z.real < 1:
                c1 = 18
                c2 = 6
                c3 = 12
        
            if z.real > 0:
                c1 = 12
                c2 = 18
                c3 = 6
        
            #glColor3f(sin(abs(z)),cos(n),sin(z.real))	
            glColor3ub(n*c1,n*c2,n*c3)	
            glVertex2f(x,y)
    glEnd()
    glFlush()
    print time()-a
    
def main():
	glutInitDisplayMode(GLUT_RGB | GLUT_SINGLE)
	glutInitWindowPosition(0, 0)
	glutInitWindowSize(width, height)
	glutInit(sys.argv)
	glutCreateWindow("Newton's Madness")
	glutDisplayFunc(drawNewton)
	init()
	glutMainLoop()
	
main()
