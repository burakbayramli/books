// https://cs.lmu.edu/~ray/notes/openglexamples/
// g++ moon.cpp -lX11 -lGL -lGLU -lglut -g -Wall -O2 -o r.exe
//
// In this program the camera orbits a lit moon to simulate the phases of the
// moon.

#ifdef __APPLE_CC__
#include <GLUT/glut.h>
#else
#include <GL/glut.h>
#endif
#include <cmath>

// A class for the moon.  A moon is really just an OpenGL display list.  The
// display list is created with create(), and the draw() method calls it.
// The reason we don't create the display list in the constructor is that
// clients may want to declare moon objects before the call to initializing
// OpenGL.
//
// The moon is a sphere of radius 1 centered at the origin, built from 25
// slices and 25 stacks, lit with GL_LIGHT0 as a directional light pointing
// along <1,1,1>.
class Moon {
  int displayListId;
public:
  void create() {
    displayListId = glGenLists(1);
    glNewList(displayListId, GL_COMPILE);
    GLfloat direction[] = {-1.0, -1.0, -1.0, 0.0};
    glLightfv(GL_LIGHT0, GL_POSITION, direction);
    glutSolidSphere(1.0, 25, 25);
    glEndList();
  }
  void draw() {
    glCallList(displayListId);
  }
};

// The one and only moon.
static Moon moon;

// An orbiter is an object that flies on a circle of a certain radius on the
// xz plane.  You supply the radius at construction time.
class Orbiter {
  double radius;
  double u;
public:
  Orbiter(double radius): radius(radius), u(0.0) {}
  void advance(double delta) {u += delta;}
  void getPosition(double& x, double& y, double& z) {
    x = radius * cos(u);
    y = 0;
    z = radius * sin(u);
  }
};

// The one and only orbiter.
static Orbiter orbiter(5.0);

// Clears the window (and the depth buffer) and draws the moon as viewed from
// the current position of the orbiter.
void display() {
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  glLoadIdentity();
  double x, y, z;
  orbiter.getPosition(x, y, z);
  gluLookAt(x, y, z, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0);
  moon.draw();
  glPopMatrix();
  glutSwapBuffers();
}

// Advances the orbiter and requests to draw the next frame.
void timer(int v) {
  orbiter.advance(0.01);
  glutPostRedisplay();
  glutTimerFunc(1000/60, timer, v);
}

// reshape() fixes up the projection matrix so that we always see a sphere
// instead of an ellipsoid.
void reshape(GLint w, GLint h) {
  glViewport(0, 0, w, h);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(40.0, GLfloat(w) / GLfloat(h), 1.0, 10.0);
}

// Enables depth testing, enables lighting for a bright yellowish diffuse
// light, and creates a moon.
void init() {
  glEnable(GL_DEPTH_TEST);
  GLfloat yellow[] = {1.0, 1.0, 0.5, 1.0};
  glLightfv(GL_LIGHT0, GL_DIFFUSE, yellow);
  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
  moon.create();
}

// The usual application code.
int main(int argc, char** argv) {
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);
  glutInitWindowPosition(80, 80);
  glutInitWindowSize(500, 500);
  glutCreateWindow("The Moon");
  glutDisplayFunc(display);
  glutTimerFunc(100, timer, 0);
  glutReshapeFunc(reshape);
  init();
  glutMainLoop();
}
