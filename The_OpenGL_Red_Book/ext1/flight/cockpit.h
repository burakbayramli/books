// An OpenGL display of a ship's cockpit.

#ifndef COCKPIT_H_
#define COCKPIT_H_

#ifdef __APPLE_CC__
#include <GLUT/glut.h>
#else
#include <GL/glut.h>
#endif
#include "ship.h"

class Cockpit {
  Ship ship;
  int cockpitId;
public:
  Cockpit(Ship ship): ship(ship) {}
  void create();
  void draw();
};

void Cockpit::create() {
  cockpitId = glGenLists(1);
  glNewList(cockpitId, GL_COMPILE);
  glDisable(GL_LIGHTING);
  glColor3f(0.8, 0.8, 0.7);
  glBegin(GL_TRIANGLE_FAN);
  glVertex3f(0, -1, 0);
  glVertex3f(1, -1, 0);
  for (double x = 1.0; x >= -1.05; x -= 0.05) {
    glVertex3f(x, 20*cos(x / 10.0) - 20.6, 0);
  }
  glVertex3f(-1, -1, 0);
  glEnd();
  glEnable(GL_LIGHTING);

  glEndList();
}

inline void Cockpit::draw() {
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  glLoadIdentity();
  glMatrixMode(GL_PROJECTION);
  glPushMatrix();
  glLoadIdentity();
  glCallList(cockpitId);
  glPopMatrix();
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix();
}

#endif
