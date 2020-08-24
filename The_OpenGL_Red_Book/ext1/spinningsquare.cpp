// https://cs.lmu.edu/~ray/notes/openglexamples/
// g++ spinningsquare.cpp -lX11 -lGL -lGLU -lglut -g -Wall -O2 -o r.exe
//
// This program demonstrates double buffering for flicker-free animation.
// It spins a white square on a black background.  It comes from Chapter 1
// of the OpenGL Programming Guide, but I made some minor changes, and did
// the animation properly, using timers, not the idle function.  Start the
// animation with the left mouse button and stop it with the right.

#ifdef __APPLE_CC__
#include <GLUT/glut.h>
#else
#include <GL/glut.h>
#endif

// Set this to true to animate.
static bool spinning = true;

// This is the number of frames per second to render.
static const int FPS = 60;

// This global variable keeps track of the current orientation of the square.
// It is better to maintain the "state" of the animation globally rather
// than trying to successively accumulate transformations.  Over a period of
// time the approach of accumulating transformation matrices generally
// degrades due to rounding errors.
static GLfloat currentAngleOfRotation = 0.0;

// Handles the window reshape event by first ensuring that the viewport fills
// the entire drawing surface.  Then we use a simple orthographic projection
// with a logical coordinate system ranging from -50..50 in the smaller of
// the width or height, and scale the larger dimension to make the whole
// window isotropic.
void reshape(GLint w, GLint h) {
  glViewport(0, 0, w, h);
  GLfloat aspect = (GLfloat)w / (GLfloat)h;
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  if (w <= h) {
    // width is smaller, go from -50 .. 50 in width
    glOrtho(-50.0, 50.0, -50.0/aspect, 50.0/aspect, -1.0, 1.0);
  } else {
    // height is smaller, go from -50 .. 50 in height
    glOrtho(-50.0*aspect, 50.0*aspect, -50.0, 50.0, -1.0, 1.0);
  }
}

// Handles the display callback as follows: first clears the window, then draws
// a 50 x 50 rectangle centered at the origin and rotated the correct number
// of degrees around the vector <0,0,1>.  This function ends with a
// 'glutSwapBuffers' call because when the display mode is double buffered,
// drawing takes place on the back buffer; we have to call glutSwapBuffers()
// to show what we have drawn.
void display() {
  glClear(GL_COLOR_BUFFER_BIT);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glRotatef(currentAngleOfRotation, 0.0, 0.0, 1.0);
  glRectf(-25.0, -25.0, 25.0, 25.0);
  glFlush();
  glutSwapBuffers();
}

// Handles the timer by incrementing the angle of rotation and requesting the
// window to display again, provided the program is in the spinning state.
// Since the timer function is only called once, it sets the same function to
// be called again.
void timer(int v) {
  if (spinning) {
    currentAngleOfRotation += 1.0;
    if (currentAngleOfRotation > 360.0) {
      currentAngleOfRotation -= 360.0;
    }
    glutPostRedisplay();
  }
  glutTimerFunc(1000/FPS, timer, v);
}

// Handles mouse events as follows: when the left button is pressed, generate
// new animation frames while the application is idle; when the right button
// is pressed, remove any idle-time callbacks, thus stopping the animation.
void mouse(int button, int state, int x, int y) {
  if (button == GLUT_LEFT_BUTTON && state == GLUT_DOWN) {
    spinning = true;
  } else if (button == GLUT_RIGHT_BUTTON && state == GLUT_DOWN) {
    spinning = false;
  }
}

// Initializes GLUT, the display mode, and main window; registers callbacks;
// enters the main event loop.
int main(int argc, char** argv) {
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB);
  glutInitWindowPosition(80, 80);
  glutInitWindowSize(800, 500);
  glutCreateWindow("Spinning Square");
  glutReshapeFunc(reshape);
  glutDisplayFunc(display);
  glutTimerFunc(100, timer, 0);
  glutMouseFunc(mouse);
  glutMainLoop();
}
