#!/usr/bin/python

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from random import random
import sys


n   = 100
r   = 0.05
g   = 9.8
dt  = 0.001
cor = 0.6

balls = []
tm  = 0.0
th  = 0.0
max =  1.0-r
min = -1.0+r
rt = False


def init():
    global tm
    for i in range(n):
        p = [
            min + random()*(max-min),
            min + random()*(max-min),
            0.9]
        v = [
            -1.5 + random()*3.0,
            -1.5 + random()*3.0,
            -1.0 + random()*2.0]
        balls.append({'pos':p,'vel':v})
    tm = 0.0

    glEnable(GL_LIGHTING)
    glEnable(GL_LIGHT0)
    glEnable(GL_DEPTH_TEST)
    glClearColor(1.0,1.0,1.0,1.0)

    glMatrixMode(GL_PROJECTION)
    glLoadIdentity()
    gluPerspective(60.0,1.0,1.0,50.0)
    glTranslatef(0.0,0.0,-3.5)
    glMatrixMode(GL_MODELVIEW)
    glLoadIdentity()

def update():
    global balls,g,dt,max,min,cor,rt,th
    for b in balls:
        b['vel'][2] += -g*dt
        b['pos'][0] += b['vel'][0]*dt
        b['pos'][1] += b['vel'][1]*dt
        b['pos'][2] += b['vel'][2]*dt

        if (abs(b['pos'][0]) >= max):
            b['vel'][0] *= -cor
            if b['pos'][0] < 0:
                b['pos'][0] = min
            else:
                b['pos'][0] = max

        if (abs(b['pos'][1]) >= max):
            b['vel'][1] *= -cor
            if b['pos'][1] < 0:
                b['pos'][1] = min
            else:
                b['pos'][1] = max

        if (abs(b['pos'][2]) >= max):
            b['vel'][2] *= -cor
            if b['pos'][2] < 0:
                b['pos'][2] = min
            else:
                b['pos'][2] = max

    if rt:
        th += 0.2
        if th>360.0:
            th -= 360.0

    glutPostRedisplay()

def display():
    global balls,th
    glClear( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
    glPushMatrix()
    glRotatef(th,0.0,1.0,0.0)
    glRotatef(90.0,-1.0,0.0,0.0)
    glutWireCube(2.0)
    for b in balls:
        glPushMatrix()
        glTranslatef(b['pos'][0],b['pos'][1],b['pos'][2])
        glutSolidSphere(r,50,50)
        glPopMatrix()
    glPopMatrix()
    glutSwapBuffers()

def mouse(button,state,x,y):
    global rt
    if button == GLUT_LEFT_BUTTON:
        rt = not state
    elif button == GLUT_RIGHT_BUTTON:
        sys.exit(0)

if __name__ == '__main__':
    glutInit(())
    glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH)
    glutInitWindowSize(500,500)
    glutCreateWindow("GLUT Bouncing Ball in Python")
    glutDisplayFunc(display)
    glutIdleFunc(update)
    glutMouseFunc(mouse)
    init()
    glutMainLoop()
