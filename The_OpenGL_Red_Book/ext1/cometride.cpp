// https://cs.lmu.edu/~ray/notes/openglexamples/
// g++ cometride.cpp -lX11 -lGL -lGLU -lglut -g -Wall -O2 -o cr.exe
//
// This program does a little solar system simulation from the point of view
// of a comet.  Here there is a yellow sun and a blue  planet; the planet
// spins on its own axis and it rotates around the sun.  The viewer is
// sitting on a comet and is always facing the sun.

#ifdef __APPLE_CC__
#include <GLUT/glut.h>
#else
#include <GL/glut.h>
#endif
#include <cmath>

// In the GLUT library someone put the polar regions on the z-axis - yech!!
// We fixed it so that they are on the y-axis.  We do this by rotating -90
// degrees about the x-axis which brings (0,0,1) to (0,1,0).
void myWireSphere(GLfloat radius, int slices, int stacks) {
  glPushMatrix();
  glRotatef(-90.0, 1.0, 0.0, 0.0);
  glutWireSphere(radius, slices, stacks);
  glPopMatrix();
}

// In our solar system simulation we keep track of the current "year" and
// current "day" in global variables.  Actually we just make the planet go
// around the sun in increments of 5 degrees (the "year") and rotate the
// planet on its own axis in increments of 10 degrees (the "day").
static int year = 0, day = 0;

// As usual, the routine to display the current state of the system is
// bracketed with a clearing of the window and a glFlush call.  Immediately
// within those calls the drawing code itself is bracketed by pushing and
// popping the current transformation.  And also as usual, we are assuming the
// current matrix mode is GL_MODELVIEW.  We finish with a SwapBuffers call
// because we'll animate.
void display() {
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glPushMatrix();

  // Draw sun: a yellow sphere of radius 1 centered at the origin.
  glColor3f(1.0, 1.0, 0.0);
  myWireSphere(1.0, 15, 15);

  // Draw planet: a blue sphere of radius 0.2, 2 units away from sun, with
  // a white "pole" for its axis.
  glRotatef((GLfloat)year, 0.0, 1.0, 0.0);
  glTranslatef (2.0, 0.0, 0.0);
  glRotatef((GLfloat)day, 0.0, 1.0, 0.0);
  glColor3f(0.0, 0.0, 1.0);
  myWireSphere(0.2, 15, 15);
  glColor3f(1, 1, 1);
  glBegin(GL_LINES);
    glVertex3f(0, -0.3, 0);
    glVertex3f(0, 0.3, 0);
  glEnd();

  glPopMatrix();
  glFlush();
  glutSwapBuffers();
}

// Viewing routines.  Basically the camera is sitting on a comet orbiting
// the sun with a "year" 8 times that of the planet.  To animate we have
// the function nextAnimationFrame() registered as the idle function.  It
// increments the value of u (to "move" the camera), ticks off another
// portion of a day and portion of a year, then reorients the camera and
// refreshes the display.
static GLfloat u = 0.0;                 // curve parameter for comet pos
static GLfloat du = 0.1;                // amt to increment u each frame

void timer(int v) {
  u += du;
  day = (day + 1) % 360;
  year = (year + 2) % 360;
  glLoadIdentity();
  gluLookAt(20*cos(u/8.0)+12,5*sin(u/8.0)+1,10*cos(u/8.0)+2, 0,0,0, 0,1,0);
  glutPostRedisplay();
  glutTimerFunc(1000/60, timer, v);
}

// As usual we reset the projection transformation whenever the window is
// reshaped.  This is done (of course) by setting the current matrix mode
// to GL_PROJECTION and then setting the matrix.  It is easiest to use the
// perspective-projection-making matrix from the GL utiltiy library.  Here
// we set a perspective camera with a 60-degree vertical field of view,
// an aspect ratio to perfectly map into the system window, a near clipping
// plane distance of 1.0 and a far clipping distance of 40.0.  The last
// thing done is to reset the current matrix mode to GL_MODELVIEW, as
// that is expected in all the calls to display().
void reshape(GLint w, GLint h) {
  glViewport(0, 0, w, h);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(60.0, (GLfloat)w/(GLfloat)h, 1.0, 40.0);
  glMatrixMode(GL_MODELVIEW);
}

// The usual main() for a GLUT application.
int main(int argc, char** argv) {
  glutInit(&argc, argv);
  glutInitDisplayMode (GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);
  glutInitWindowSize(800, 600);
  glutCreateWindow("On a Comet");
  glutDisplayFunc(display);
  glutReshapeFunc(reshape);
  glutTimerFunc(100, timer, 0);
  glEnable(GL_DEPTH_TEST);
  glutMainLoop();
}


