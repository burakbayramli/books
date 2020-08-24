#!/usr/bin/env python

"""
This program draws a single red rectangle in a blue background.
It is a demonstration of the structure needed to mix
OpenGL (for graphics) and GLUT (for window control),
all within Python via PyOpenGL.

This version marks a little cross at the origin.
"""

#import pydoc #Not used
__author__= "Joseph O'Rourke"
__version__ = "1.0"
__date__= "Sep10"

import sys
#import math #Not used, but often needed
from OpenGL.GL import *
from OpenGL.GLUT import *

##############################################################################
def rect():
    """
    Draw a single rectangle centered on the origin.
    Assumes [-1,+1]^3 coordinate system.  Note, points are in 3D,
    but placed at z=0.0.
    """

    print "==>rect1"
    
    glColor3f (1.0, 0.0, 0.0) #Red
    glBegin(GL_POLYGON)
    glVertex3f(-0.5, -0.5, 0.0)
    glVertex3f(-0.5, 0.5, 0.0)
    glVertex3f(0.5, 0.5, 0.0)
    glVertex3f(0.5, -0.5, 0.0)
    glEnd()

    # Mark origin after, and so on top of, rectangle
    MarkOrigin()


def MarkOrigin( ):
    """
    Marks a cross at the origin just so we can see it.
    """
    glColor3f (1.0, 1.0, 1.0) #White
    glBegin(GL_LINES) # disconnected line segments
    glVertex2f( -0.1,0.0 )
    glVertex2f(  0.1,0.0 )
    glVertex2f( 0.0,-0.1 )
    glVertex2f( 0.0, 0.1 )
    glEnd()
    
##############################################################################
def displayGL():
    """
    What to do when the screen is repainted: draw the rectangle.
    """
    glClear( GL_COLOR_BUFFER_BIT )
    rect()
    glFlush() #Flush the graphics pipeline buffer
    
def reshapeGL(width,height):
    """
    What to do when the window is resized: reset coord system etc.
    Initial creation of window constitutes a resize.
    """
    
    w = width / float(height)
    h = 1.0
    print "==>reshapeGL: w,h=",w,h
    
    glViewport( 0, 0, width, height )
    glMatrixMode(GL_PROJECTION) #Work on projection matrix
    glLoadIdentity()
    glOrtho(-1., 1., -1., 1., -1., 1.) # Origin at center of window
    # Note: first four numbers are xmin, xmax, ymin, ymax.
    glMatrixMode(GL_MODELVIEW)
    glLoadIdentity()

def initializeGL():
    """
    What to do upon first invocation.
    """
    print "==>initializeG:"
    glClearColor(0.0, 0.0, 1.0, 0.0) #blue


##############################################################################
if __name__=='__main__':
    glutInit(sys.argv)
    glutInitDisplayMode(GLUT_RGB | GLUT_SINGLE)

    glutInitWindowPosition(0, 0) #Upperleft corner.
    glutInitWindowSize(500, 500) #500 x 500 pixels
    glutCreateWindow("Rectangle")
    initializeGL()
    
    glutDisplayFunc(displayGL) #Register display callback
    glutReshapeFunc(reshapeGL) #Register reshape callback
    
    glutMainLoop() #Infinite loop
