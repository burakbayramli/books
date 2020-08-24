#tr0.py 
# gkf 6apr05
# there is still a python type error in here but it works as intended.
from OpenGL.GL import *
from OpenGL.GLUT import *
from OpenGL.GLU import *
from math import *
import sys

# Bryn Keller's version of (cond?expr1:expr2)
def iif(cond, expr1, expr2):
    if cond: return float(expr1) 
    else: float(expr2) 

def DOT(a,b): return float(a[0]*b[0]+a[1]*b[1]+a[2]*b[2])
def C(u): return cos(u*0.01745)
def S(u): return sin(u*0.01745)

global wd
global ht
global MouseX
global MouseY
global lux 
global lu
global pwr
global aff

lux =(0.3, 0.5, 0.8)
lu = [1,0,0]  # this should not be necessary 
pwr = 10.  

aff = [1.0, 0.0, 0.0, 0.0,
       0.0, 1.0, 0.0, 0.0,
       0.0, 0.0, 1.0, 0.0,
       0.0, 0.0, 0.0, 1.0]

def drawvert(th,ta):
   global lu;  global pwr
   
   #sphere
   n0= C(th)*C(ta)
   n1= S(th)*C(ta)
   n2=       S(ta)
     
   nn=(n0,n1,n2)
   lmb = nn[0]*lu[0]+nn[1]*lu[1]+nn[2]*lu[2]
   lmb = min(max(lmb, 0.0), 1.0)  #clamp
   spec=min(1., 1. - pwr + pwr*lmb)
   dog = (th- 10.)/(346.-10.)
   cat = (ta- 10.)/(346.-10.)
   rr = dog*lmb*1.5
   gg = (.25 + abs(cat-.5))*lmb*1.5
   bb = (1-cat)*lmb*1.5
   glColor3f(max(rr,spec), max(gg,spec), max(bb,spec))
   glVertex3f(C(th)+.5*n0, S(th)+.5*n1, .5*n2)
   #glVertex3f(n0, n1, n2)
#end drawvert

def drawtor():
   global lmb
   global vv
   global nn
   for th in range(0 ,354,12):
      glBegin(GL_TRIANGLE_STRIP)
      for ta in range (0,354, 12): 
         drawvert(th,ta); drawvert(th+8,ta) 
      glEnd() 
#end drawtor

def calculite(aff):
    global lu
    for ii in range(3):
        lu[ii]= 0 
        for jj in range(3): 
            lu[ii]+=aff[ii][jj]*lux[jj]
#end calculite

#assign initial window and mouse settings
wd = 800
ht = 800
MouseX = wd/2.0
MouseY = ht/2.0

brake = 128.0 

def display():
   glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
   glMatrixMode(GL_MODELVIEW)
   glLoadIdentity()
   glMultMatrixf(aff)
   drawtor()
   glutSwapBuffers()
   calculite(aff)
#end display()



#typical keyboard callback 
def keyboard(key, x, y):
   if key == chr(27) or key == 'q':
     sys.exit(0)
   glutPostRedisplay()
#end keyboard()

#adjust to resizing of the window
def reshape(width, height):
   global wd
   global ht
   glClearColor(0.0, 0.0, 0.0, 0.0)
   if height == 0:
      height = 1
   wd = width
   ht = height
   #glViewport(0,0,wd,ht)
   glMatrixMode(GL_PROJECTION)
   glLoadIdentity()
   if wd<=ht:
      glOrtho(-2.0,2.0,-2.0*ht/wd,2.0*ht/wd,-2.0,2.0)
   else:
      glOrtho(-2.0*wd/ht,2.0*wd/ht,-2.0,2.0,-2.0,2.0)
   glMatrixMode(GL_MODELVIEW)
   glLoadIdentity()
#end reshape()

#Note that we must declare the globals again
def chaptrack():
   global MouseX
   global MouseY
   global wd
   global ht
   global aff
   dx = (MouseX-wd/2)/brake  
   dy = (MouseY-ht/2)/brake
   glMatrixMode(GL_MODELVIEW)
   glPushMatrix()
   glLoadIdentity()
   glRotatef(dx,0,1.0,0.0)
   glRotatef(dy,1.0,0.0,0.0)
   glMultMatrixf(aff)
   aff = glGetFloatv(GL_MODELVIEW_MATRIX)
   glPopMatrix()
#end chaptrack()

#traditional idle
def idle():
   chaptrack()
   glutPostRedisplay()
#end idle()

#ditto traditional mousemotion
#Note globals
def mousemotion(x,y):
   global MouseX
   global MouseY
   MouseX = x
   MouseY = y
#end mousemotion()

def init():
    glEnable(GL_DEPTH_TEST)
    glShadeModel(GL_SMOOTH)

#Traditional main subroutine
def main(argv) :
   global wd
   global ht
   glutInitDisplayMode(GLUT_RGB | GLUT_DEPTH | GLUT_DOUBLE)
   glutInitWindowPosition(50, 50)
   glutInitWindowSize(wd, ht)
   glutInit(argv)
   glutCreateWindow("illiTorus")
   glutKeyboardFunc(keyboard)
   glutDisplayFunc(display)
   glutIdleFunc(idle)
   glutReshapeFunc(reshape)
   glutPassiveMotionFunc(mousemotion)
   
   init()
   glutMainLoop()

#Necessary if we want to this program to run
main(sys.argv)

