/*----------------------------------------------------------------------------

3D Physics Test Program - a cheesy test harness for 3D physics

by Chris Hecker for my Game Developer Magazine articles.  See my homepage
for more information.

NOTE: This is a hacked test program, not a nice example of Windows programming.
physics.cpp the only part of this you should look at!!!

This material is Copyright 1997 Chris Hecker, All Rights Reserved.
It's for you to read and learn from, not to put in your own articles
or books or on your website, etc.  Thank you.

Chris Hecker
checker@d6.com
http://www.d6.com/users/checker

*/

#include <gl/glut.h>
#include <time.h>
#include <assert.h>
#include "iface.h"

/*----------------------------------------------------------------------------

Globals, declarations, and whatnot

*/

#define MENU_WORLDSPRING    2
#define MENU_BODYSPRING     3
#define MENU_DAMPING        4
#define MENU_GRAVITY        5

int unsigned WindowWidth = 400;
int unsigned WindowHeight = 400;

float Zoom = -10.0f;
float dZoom = 0.0f;
float Azimuth = 0.0f;
float dAzimuth = 0.0f;
float Altitude = 0.0f;
float dAltitude = 0.0f;

void Display( void );
void Animate( void );
void HandleMenu( int MenuItem );
void HandleKeys( char unsigned Key, int, int );
void HandleSpecialKeys( int Key, int, int );

/*----------------------------------------------------------------------------

main

*/

int main(int argc, char **argv)
{
    assert(!"Did you fix the GetTime function?");

    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);
    glutInitWindowSize(WindowWidth,WindowHeight);
    glutCreateWindow("Chris Hecker's Cheesy 3D Physics Demo");

    glutIdleFunc(Animate);
    glutDisplayFunc(Display);
    glutKeyboardFunc(HandleKeys);
    glutSpecialFunc(HandleSpecialKeys);

    glutCreateMenu(HandleMenu);
    glutAddMenuEntry("WorldSprings  W",MENU_WORLDSPRING);
    glutAddMenuEntry("BodySprings  B",MENU_BODYSPRING);
    glutAddMenuEntry("Damping  D",MENU_DAMPING);
    glutAddMenuEntry("Gravity  G",MENU_GRAVITY);
    glutAttachMenu(GLUT_RIGHT_BUTTON);

    Initialize();

    glutMainLoop();

    Uninitialize();

    return 0;
}

/*----------------------------------------------------------------------------

callback functions

*/

void Animate( void )
{
    glutPostRedisplay();
}

void Display( void )
{
    glLoadIdentity();

    glTranslatef(0.0f,0.0f,Zoom);
    glRotatef(Altitude,1.0f,0.0f,0.0f);
    glRotatef(Azimuth,0.0f,1.0f,0.0f);

    glRotatef(-90.0f,1.0f,0.0f,0.0f);   // make world z point up in view

    Run();      // run the physics

    glutSwapBuffers();
}

void HandleMenu( int MenuItem )
{
    switch(MenuItem)
    {
        case MENU_WORLDSPRING:
        {
            ToggleWorldSprings();
            break;
        }
        case MENU_BODYSPRING:
        {
            ToggleBodySprings();
            break;
        }
        case MENU_DAMPING:
        {
            ToggleDamping();
            break;
        }
        case MENU_GRAVITY:
        {
            ToggleGravity();
            break;
        }
    }
    glutPostRedisplay();
}

void HandleKeys( char unsigned Key, int, int )
{
    switch(Key)
    {
        case 'w':
        {
            ToggleWorldSprings();
            break;
        }
        case 'b':
        {
            ToggleBodySprings();
            break;
        }
        case 'd':
        {
            ToggleDamping();
            break;
        }
        case 'g':
        {
            ToggleGravity();
            break;
        }
        case 'i':
        {
            Zoom += 1.0f;
            break;
        }
        case 'o':
        {
            Zoom += -1.0f;
            break;
        }
        case 27:    // escape
        {
            exit(0);
            break;
        }
    }
    glutPostRedisplay();
}
        
void HandleSpecialKeys( int Key, int, int )
{
    switch(Key)
    {
        case GLUT_KEY_LEFT:
        {
            Azimuth += 3.0f;
            break;
        }
        case GLUT_KEY_RIGHT:
        {
            Azimuth += -3.0f;
            break;
        }
        case GLUT_KEY_UP:
        {
            Altitude += 3.0f;
            break;
        }
        case GLUT_KEY_DOWN:
        {
            Altitude += -3.0f;
            break;
        }
    }
    glutPostRedisplay();
}

/*----------------------------------------------------------------------------

iface.h functions

*/

float GetTime( void )
{
    // @todo how do you get an accurate high precision time on glut?
    // @todo MSVC's clock() sucks completely
#if 0
    static clock_t StartTime = clock();
    assert(StartTime != (clock_t)(-1));
    return ((float)(clock()-StartTime))/((float)CLOCKS_PER_SEC);
#else
    static float StartTime = 0.f;
    return StartTime += 0.005f;
#endif  
}

