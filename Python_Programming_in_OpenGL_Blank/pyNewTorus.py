# pyNewTorus.py 
# Credit to George K. Francis for the excellent original program

from OpenGL.GL import *
from OpenGL.GLUT import *
from OpenGL.GLU import *
from math import *
import sys

#import psyco
#psyco.full()

# define some new trig functions here for
# later use in the program.  This is NEAT STUFF!
def C(u): return cos(u*0.01745)
def S(u): return sin(u*0.01745)

global wd
global ht
global mouseX
global mouseY
global aff 
global nrange
nrange = 3.0
pi = 3.1415926

aff = [1.0, 0.0, 0.0, 0.0,
       0.0, 1.0, 0.0, 0.0,
       0.0, 0.0, 1.0, 0.0,
       0.0, 0.0, 0.0, 1.0]

def drawvert(th,ta):
   
   # parametric sphere equations
   #n0= C(th)*C(ta)
   #n1= S(th)*C(ta)
   #n2=       S(ta)

   # Crosscap
   #n0= C(th)*S(2*ta)
   #n1= S(th)*S(2*ta)
   #n2= C(ta)*C(ta) - C(th)*C(th)*S(ta)*S(ta)

   # Klein Bottle! 
   #n0 = C(th)*(1 + S(ta)*C(th/2) - (S(2*ta)*S(th/2))/2) 
   #n1 = S(th)*(1 + S(ta)*C(th/2) - (S(2*ta)*S(th/2))/2) 
   #n2 = S(th/2)*S(ta) + (C(th/2)*S(2*ta))/2
   
   # Steiner's Roman Surface
   #n0 = C(ta)*C(ta)*S(2*th)
   #n1 = S(th)*S(2*ta)
   #n2 = C(th)*S(2*ta)
   
   # Another torus
   #n0 = (1 + 0.5*C(ta))*C(th)
   #n1 = (1 + 0.5*C(ta))*S(th)
   #n2 = 0.5*S(ta)
   
   # George's Etruscan Venus
   n0 = 1.2*C(2*th)*C(ta) + S(ta)*C(th)
   n1 = 1.2*S(2*th)*C(ta) - S(ta)*S(th)
   n2 = 2*C(ta)
   
   # figure 8... beautiful!
   #n0 = C(th)*(1 + S(ta)*C(th) - S(2*ta)*S(th)/2)
   #n1 = S(th)*(1 + S(ta)*C(th) - S(2*ta)*S(th)/2)
   #n2 = S(th)*S(ta) + C(th)*S(2*ta)/2
   
   glColor3f(S(th),C(ta),S(th*ta)) 
   
   # torus!
   #glVertex3f(C(th)+.5*n0, S(th)+.5*n1, .5*n2)
   
   # sphere
   glVertex3f(n0, n1, n2)

def drawtor():
   for th in range(0 ,348,15):
      glBegin(GL_TRIANGLE_STRIP)
      for ta in range (0,348, 15):
         drawvert(th,ta); drawvert(th+8,ta) 
      glEnd() 

#assign initial window and mouse settings
wd = 300
ht = 300
mouseX = wd/2
mouseY = ht/2

brake = 128.0 

def display():
   glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
   glMatrixMode(GL_MODELVIEW)
   glLoadIdentity()
   glMultMatrixf(aff)
   drawtor()
   glutSwapBuffers()

#typical keyboard callback 
def keyboard(key, x, y):
   if key == chr(27) or key == 'q':
     sys.exit(0)
   glutPostRedisplay()

#adjust to resizing of the window
def reshape(width, height):
   global wd
   global ht
   glClearColor(0.0, 0.0, 0.0, 0.0)
   if height == 0:
      height = 1
   wd = width
   ht = height
   glViewport(0,0,wd,ht)
   glMatrixMode(GL_PROJECTION)
   glLoadIdentity()
   if wd<=ht:
      glOrtho(-nrange,nrange,-nrange*ht/wd,nrange*ht/wd,-nrange,nrange)
   else:
      glOrtho(-nrange*wd/ht,nrange*wd/ht,-nrange,nrange,-nrange,nrange)
   glMatrixMode(GL_MODELVIEW)
   glLoadIdentity()

#Note that we must declare the globals again
def chaptrack():
   global mouseX
   global mouseY
   global wd
   global ht
   global aff
   dx = (mouseX-wd/2)/brake  
   dy = (mouseY-ht/2)/brake
   glMatrixMode(GL_TEXTURE)
   glPushMatrix()
   glLoadIdentity()
   glRotatef(dx,0,1.0,0.0)
   glRotatef(dy,1.0,0.0,0.0)
   glMultMatrixf(aff)
   aff = glGetFloatv(GL_TEXTURE_MATRIX)
   glPopMatrix()

#traditional idle
def idle():
   chaptrack()
   glutPostRedisplay()

#ditto traditional mousemotion
def mousemotion(x,y):
   global mouseX
   global mouseY
   mouseX = x
   mouseY = y

def init():
    glEnable(GL_DEPTH_TEST)
    glShadeModel(GL_SMOOTH)

#Traditional main subroutine
def main() :
   global wd
   global ht
   glutInitDisplayMode(GLUT_RGB | GLUT_DEPTH | GLUT_DOUBLE)
   glutInitWindowPosition(50, 50)
   glutInitWindowSize(wd, ht)
   glutInit()
   glutCreateWindow("illiTorus")
   glutKeyboardFunc(keyboard)
   glutDisplayFunc(display)
   glutIdleFunc(idle)
   glutReshapeFunc(reshape)
   glutPassiveMotionFunc(mousemotion)
   
   init()
   glutMainLoop()

main()

