#!/usr/bin/env python

"""
Draws a smiley, to illustrate the use of Push & Pop Matrix.
Also includes procedures to draw an arc, and fill a disk.
"""

#import pydoc #Not used
__author__= "Joseph O'Rourke"
__version__ = "1.0"
__date__= "Sep10"

import sys
from math import *
from OpenGL.GL import *
from OpenGL.GLUT import *

##############################################################################
def arc(x, y, r, a, b, n):
    """
    Draw an arc of a circle.
    a and b are assumed to be in degrees!
    n is the number of points in a full circle.
    This is modeled after the Postscript arc operator.
    """

    print "==>arc"
    glBegin( GL_LINE_STRIP )
    # Need to go up to n, not n-1:
    for i in range( n+1 ):
        angdeg = 360. * i / n; 
        angrad = 2 * pi * angdeg / 360.;
        if (angdeg >= a) and (angdeg <= b):
            x = r * cos( angrad );
            y = r * sin( angrad );
            glVertex2f( x, y )
    glEnd()

def disk(x, y, r, n):
    """
    Draw a full disk.
    n is the number of points in a full circle.
    [gluDisk() can do this, but requires setting up quadrics.]
    """

    print "==>disk"
    
    glBegin( GL_POLYGON )
    for i in range( n ):
        angdeg = 360. * i / n; 
        angrad = 2 * pi * angdeg / 360.;
        x = r * cos( angrad );
        y = r * sin( angrad );
        glVertex2f( x, y )
    glEnd()

def Smiley( ):
    """
    Draw a smiley!
    """

    # Round yellow face, outlined in black
    glColor3f (1., 1., 0.)
    disk( 0., 0., 0.5, 36 )
    glColor3f ( 0., 0., 0. )
    arc ( 0., 0., 0.5, 0., 360., 36 )

    # Right eye
    glColor3f ( 0., 0., 1. )
    glPushMatrix( )
    glTranslate( 0.2, 0.2, 0. )
    glScale( 0.5, 1., 1. )
    disk( 0., 0., 0.1, 18 )
    glPopMatrix( )

    # Left eye
    glPushMatrix( )
    glTranslate( -0.2, 0.2, 0. )
    glScale( 0.5, 1., 1. )
    disk( 0., 0., 0.1, 18 )
    glPopMatrix( )

    # Mouth
    glColor3f( 1., 0., 0. )
    glLineWidth( 5. )
    arc (0., 0., 0.25, 190., 350., 36 )

def MarkOrigin( ):
    """
    Marks a cross at the origin just so we can see it.
    """
    glColor3f (1., 1., 1.) #White
    glLineWidth( 1. ) # Thin
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
    glLineWidth( 3. ) # Thick
    #glColor3f (0., 1., 0.)
    #arc( 0., 0., 0.5, 0., 90., 36 )
    #glColor3f (1., 0., 0.)
    #disk( 0., 0., 0.25, 36 )
    Smiley()
    
    # Mark origin afterward, and so on top of, whatever was drawn above.
    MarkOrigin()

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
    glClearColor(0.5, 0.5, 1.5, 0.) # Lightblue
    # Turn on antialiasing
    glHint( GL_LINE_SMOOTH_HINT, GL_NICEST )
    glEnable ( GL_LINE_SMOOTH )
    glHint( GL_POLYGON_SMOOTH_HINT, GL_NICEST )
    glEnable ( GL_POLYGON_SMOOTH )


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
