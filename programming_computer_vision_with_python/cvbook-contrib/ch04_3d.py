from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
import pygame, pygame.image
from pygame.locals import *
import numpy
import pickle


width, height = 2592 / 2, 1944 / 2


def set_projection_from_camera(K):
  glMatrixMode(GL_PROJECTION)
  glLoadIdentity()

  fx = float(K[0, 0])
  fy = float(K[1, 1])
  fovy = 2 * numpy.arctan(0.5 * height / fy) * 180 / numpy.pi
  aspect = (width * fy) / (height * fx)

  near, far = 0.1, 100
  gluPerspective(fovy, aspect, near, far)
  glViewport(0, 0, width, height)


def set_modelview_from_camera(Rt):
  glMatrixMode(GL_MODELVIEW)
  glLoadIdentity()

  # Rotate 90 deg around x, so that z is up.
  Rx = numpy.array([[1, 0, 0], [0, 0, -1], [0, 1, 0]])

  # Remove noise from rotation, make sure it's a pure rotation.
  R = Rt[:, :3]
  U, S, V = numpy.linalg.svd(R)
  R = numpy.dot(U, V)
  R[0, :] = -R[0, :]  # Change sign of x axis.

  print S
  t = Rt[:, 3]

  M = numpy.eye(4)
  M[:3, :3] = numpy.dot(R, Rx)
  M[:3, 3] = t

  m = M.T.flatten()
  glLoadMatrixf(m)


def draw_background(imname):
  bg_image = pygame.image.load(imname).convert()
  width, height = bg_image.get_size()
  bg_data = pygame.image.tostring(bg_image, "RGBX", 1)

  glEnable(GL_TEXTURE_2D)
  tex = glGenTextures(1)
  glBindTexture(GL_TEXTURE_2D, tex)
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, width, height, 0,
               GL_RGBA, GL_UNSIGNED_BYTE, bg_data)
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)

  glBegin(GL_QUADS)
  glTexCoord2f(0, 0); glVertex3f(-1, -1, -1)
  glTexCoord2f(1, 0); glVertex3f( 1, -1, -1)
  glTexCoord2f(1, 1); glVertex3f( 1,  1, -1)
  glTexCoord2f(0, 1); glVertex3f(-1,  1, -1)
  glEnd()

  glDeleteTextures(tex)


def load_and_draw_model(filename):
  glEnable(GL_LIGHTING)
  glEnable(GL_LIGHT0)
  glEnable(GL_DEPTH_TEST)
  glClear(GL_DEPTH_BUFFER_BIT)
  glMaterialfv(GL_FRONT, GL_AMBIENT, [0, 0, 0, 0])
  glMaterialfv(GL_FRONT, GL_DIFFUSE, [0.5, 0.75, 1, 0])
  glMaterialfv(GL_FRONT, GL_SHININESS, 0.25 * 128)
  import objloader
  obj = objloader.OBJ(filename, swapyz=True)
  glScale(0.1, 0.1, 0.1)
  glCallList(obj.gl_list)


def setup():
  pygame.init()
  pygame.display.set_mode((width, height), OPENGL | DOUBLEBUF)
  pygame.display.set_caption('Look, an OpenGL window!')


with open('out_ch4_camera.pickle', 'rb') as f:
  K = pickle.load(f)
  Rt = pickle.load(f)

setup()
draw_background('out_ch4pics/h_image.jpg')

# FIXME: The origin ends up in a different place than in ch04_markerpose.py
# somehow.
set_projection_from_camera(K)
set_modelview_from_camera(Rt)

glEnable(GL_LIGHTING)
glEnable(GL_LIGHT0)
glEnable(GL_DEPTH_TEST)
glClear(GL_DEPTH_BUFFER_BIT)
glMaterialfv(GL_FRONT, GL_AMBIENT, [0, 0, 0, 0])
glMaterialfv(GL_FRONT, GL_DIFFUSE, [0.5, 0, 0, 0])
glMaterialfv(GL_FRONT, GL_SHININESS, 0.25 * 128)
for y in range(0, 1):
  for x in range(0, 1):
    glutSolidTeapot(0.02)
    glTranslatef(0.04, 0, 0)
  glTranslatef(-3 * 0.04, 0, 0.04)
#load_and_draw_model('out_toyplane.obj')
pygame.display.flip()

while True:
  event = pygame.event.poll()
  if event.type in (QUIT, KEYDOWN):
    break
  #pygame.display.flip()
