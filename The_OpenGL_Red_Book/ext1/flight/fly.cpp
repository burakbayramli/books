// This program is a trivial little flight simulator.  You control a ship
// with the keyboard.  Use
//
//   J and L keys to roll,
//   I and K keys to pitch,
//   H and ; keys to yaw,
//   8 key to increase speed and the M key to decrease speed.
//   W key toggles wireframe mode
//   R key generates a new landscape
//
// In this little program you fly around a single fractal landscape. It would
// be best to extend the program so that one could plug in any arbitrary
// scene.

#ifdef __APPLE_CC__
#include <GLUT/glut.h>
#else
#include <GL/glut.h>
#endif
#include "cockpit.h"
#include "landscape.h"
#include <iostream>

// A landscape to fly around, with some parameters that are manipuated by the
// program.
Landscape landscape(200, 143);

// Wireframe view or solid view?
static bool wireframe = false;

void newLandscape() {
  static double rug = ((double)rand()) / RAND_MAX;
  landscape.create(rug);
}

// A ship and some functions to control it: Later, we need to add a ship
// controller class so even the navigation controls are pluggable.
static Ship theShip(Point(60, 40, 220));
static Cockpit cockpit(theShip);

void keyboard(unsigned char key, int, int) {
  const double deltaSpeed = 0.01;
  const double angle = 0.02;
  switch(key) {
    case '8': theShip.setSpeed(theShip.getSpeed() + deltaSpeed); break;
    case 'm': theShip.setSpeed(theShip.getSpeed() - deltaSpeed); break;
    case 'w': wireframe = !wireframe; break;
    case 'r': newLandscape();
    case 'j': theShip.roll(angle); break;
    case 'l': theShip.roll(-angle); break;
    case 'h': theShip.yaw(angle); break;
    case ';': theShip.yaw(-angle); break;
    case 'i': theShip.pitch(-angle); break;
    case 'k': theShip.pitch(angle);  break;
  }
}

// Display and Animation: To draw we just clear the window and draw the scene.
// Because our main window is double buffered we have to swap the buffers to
// make the drawing visible.  Animation is achieved by successively moving
// the ship and drawing.
void display() {
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  wireframe ? landscape.drawWireFrame() : landscape.draw();
  cockpit.draw();
  glFlush();
  glutSwapBuffers();
}

// Move the ship one step, recompute the view, and ask to redisplay.
void timer(int v) {
  theShip.fly();
  Point eye(theShip.getPosition());
  Point at(theShip.getPosition() + theShip.getDirection());
  Vector up(theShip.getVertical());
  glLoadIdentity();
  gluLookAt(eye.x, eye.y, eye.z, at.x, at.y, at.z, up.i, up.j, up.k);
  glutPostRedisplay();
  glutTimerFunc(1000/60, timer, v);
}

// Reshape callback: Make the viewport take up the whole window, recompute the
// camera settings to match the new window shape, and go back to modelview
// matrix mode.
void reshape(int w, int h) {
  glViewport(0, 0, w, h);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(60.0, (GLfloat)w/(GLfloat)h, 0.05, 300.0);
  glMatrixMode(GL_MODELVIEW);
}

// init(): Initialize GLUT and enter the GLUT event loop.
void init() {
  srand(9903);
  glEnable(GL_DEPTH_TEST);
  newLandscape();
  cockpit.create();
  GLfloat black[] = { 0.0, 0.0, 0.0, 1.0 };
  GLfloat dark[] = { 0.2, 0.15, 0.2, 1.0 };
  GLfloat white[] = { 1.0, 1.0, 1.0, 1.0 };
  GLfloat direction[] = { 0.2, 1.0, 0.5, 0.0 };

  glMaterialfv(GL_FRONT, GL_SPECULAR, white);
  glMaterialf(GL_FRONT, GL_SHININESS, 30);

  glLightfv(GL_LIGHT0, GL_AMBIENT, dark);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, white);
  glLightfv(GL_LIGHT0, GL_SPECULAR, white);
  glLightfv(GL_LIGHT0, GL_POSITION, direction);

  glEnable(GL_LIGHTING);                // so the renderer considers light
  glEnable(GL_LIGHT0);                  // turn LIGHT0 on
}

// Writes some trivial help text to the console.
void writeHelpToConsole() {
  std::cout << "j/l = roll left / right\n";
  std::cout << "i/k - pitch down / up\n";
  std::cout << "h/; - yaw left / right\n";
  std::cout << "8/m - speed up / slow down\n";
  std::cout << "w - toggle wireframe mode\n";
  std::cout << "r - generate a new landscape\n";
}

// main(): Initialize GLUT and enter the GLUT event loop.
int main(int argc, char** argv) {
  writeHelpToConsole();
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);
  glutInitWindowPosition(80, 80);
  glutInitWindowSize(780, 500);
  glutCreateWindow("Simple Flight");
  glutReshapeFunc(reshape);
  glutTimerFunc(100, timer, 0);
  glutDisplayFunc(display);
  glutKeyboardFunc(keyboard);
  init();
  glutMainLoop();
}
