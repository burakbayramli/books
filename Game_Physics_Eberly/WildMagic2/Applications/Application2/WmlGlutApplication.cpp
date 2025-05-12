// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifdef WML_USE_GLUT

#include "WmlApplication.h"
#include "WmlGlutRenderer.h"
#include "WmlOpenGLCamera.h"

#ifdef WIN32

// The DirectShow header dshow.h includes windowsx.h.  The latter header file
// #define-s GetWindowID(hwnd) to be GetDlgCtrlID(hwnd).  I received a report
// that WmlGlutApplication.cpp fails to compile using VC 7.1, the failure
// being that the #define interferes with Application::GetWindowID.  I was
// not able to reproduce the problem, but just in case...
#ifdef GetWindowID
#undef GetWindowID
#endif

#pragma comment(lib,"opengl32.lib")
#pragma comment(lib,"glu32.lib")
#pragma comment(lib,"glut32.lib")
#pragma comment(lib,"WildMagic2.lib")
#pragma comment(lib,"WmlRenderer.lib")
#endif

using namespace Wml;

const int Application::KEY_ESCAPE = 0x1B;
const int Application::KEY_LEFT_ARROW = GLUT_KEY_LEFT;
const int Application::KEY_RIGHT_ARROW = GLUT_KEY_RIGHT;
const int Application::KEY_UP_ARROW = GLUT_KEY_UP;
const int Application::KEY_DOWN_ARROW = GLUT_KEY_DOWN;
const int Application::KEY_HOME = GLUT_KEY_HOME;
const int Application::KEY_END = GLUT_KEY_END;
const int Application::KEY_PAGE_UP = GLUT_KEY_PAGE_UP;
const int Application::KEY_PAGE_DOWN = GLUT_KEY_PAGE_DOWN;
const int Application::KEY_INSERT = GLUT_KEY_INSERT;
const int Application::KEY_DELETE = 0x2E;
const int Application::KEY_F1 = GLUT_KEY_F1;
const int Application::KEY_F2 = GLUT_KEY_F2;
const int Application::KEY_F3 = GLUT_KEY_F3;
const int Application::KEY_F4 = GLUT_KEY_F4;
const int Application::KEY_F5 = GLUT_KEY_F5;
const int Application::KEY_F6 = GLUT_KEY_F6;
const int Application::KEY_F7 = GLUT_KEY_F7;
const int Application::KEY_F8 = GLUT_KEY_F8;
const int Application::KEY_F9 = GLUT_KEY_F9;
const int Application::KEY_F10 = GLUT_KEY_F10;
const int Application::KEY_F11 = GLUT_KEY_F11;
const int Application::KEY_F12 = GLUT_KEY_F12;

const int Application::KEY_SHIFT = GLUT_ACTIVE_SHIFT;
const int Application::KEY_CONTROL = GLUT_ACTIVE_CTRL;
const int Application::KEY_ALT = GLUT_ACTIVE_ALT;
const int Application::KEY_COMMAND = 0;

const int Application::MOUSE_LEFT_BUTTON = GLUT_LEFT_BUTTON;
const int Application::MOUSE_MIDDLE_BUTTON = GLUT_MIDDLE_BUTTON;
const int Application::MOUSE_RIGHT_BUTTON = GLUT_RIGHT_BUTTON;
const int Application::MOUSE_UP = GLUT_UP;
const int Application::MOUSE_DOWN = GLUT_DOWN;

// Reading the state of the modifiers from GLUT cannot be done within
// the motion callback. Hence, we cache it here.

static unsigned int gs_uiGLUTModifiers = 0;

//----------------------------------------------------------------------------
void Application::RequestTermination ()
{
    exit(0);
}
//----------------------------------------------------------------------------
float Application::GetTimeInSeconds ()
{
    float fTime = 0.001f*glutGet((GLenum)GLUT_ELAPSED_TIME);
    return fTime;
}
//----------------------------------------------------------------------------
float Application::StringWidth (const char* acText)
{
    assert( acText && strlen(acText) > 0 );
    return 8.0f*strlen(acText);
}
//----------------------------------------------------------------------------
float Application::CharWidth (const char)
{
    return 8.0f;
}
//----------------------------------------------------------------------------
float Application::FontHeight ()
{
    return 13.0f;
}
//----------------------------------------------------------------------------
static void ReshapeCallback (int iWidth, int iHeight)
{
    Application* pkTheApp = Application::GetApplication();
    if ( pkTheApp )
        pkTheApp->OnReshape(iWidth,iHeight);
}
//----------------------------------------------------------------------------
static void DisplayCallback ()
{
    Application* pkTheApp = Application::GetApplication();
    if ( pkTheApp )
        pkTheApp->OnDisplay();
}
//----------------------------------------------------------------------------
static void IdleCallback ()
{
    Application* pkTheApp = Application::GetApplication();
    if ( pkTheApp )
        pkTheApp->OnIdle();
}
//----------------------------------------------------------------------------
static void KeyDownCallback (unsigned char ucKey, int iX, int iY)
{
    Application* pkTheApp = Application::GetApplication();
    if ( pkTheApp )
        pkTheApp->OnKeyDown(ucKey,iX,iY);
}
//----------------------------------------------------------------------------
static void KeyUpCallback (unsigned char ucKey, int iX, int iY)
{
    Application* pkTheApp = Application::GetApplication();
    if ( pkTheApp )
        pkTheApp->OnKeyUp(ucKey,iX,iY);
}
//----------------------------------------------------------------------------
static void SpecialKeyDownCallback (int iKey, int iX, int iY)
{
    Application* pkTheApp = Application::GetApplication();
    if ( pkTheApp )
        pkTheApp->OnSpecialKeyDown(iKey,iX,iY);
}
//----------------------------------------------------------------------------
static void SpecialKeyUpCallback (int iKey, int iX, int iY)
{
    Application* pkTheApp = Application::GetApplication();
    if ( pkTheApp )
        pkTheApp->OnSpecialKeyUp(iKey,iX,iY);
}
//----------------------------------------------------------------------------
static void MouseClickCallback (int iButton, int iState, int iX, int iY)
{
    Application* pkTheApp = Application::GetApplication();
    if ( pkTheApp )
    {
        int iModifiers = glutGetModifiers();
        gs_uiGLUTModifiers = *(unsigned int*)&iModifiers;
        pkTheApp->OnMouseClick(iButton,iState,iX,iY,gs_uiGLUTModifiers);
    }
}
//----------------------------------------------------------------------------
static void MotionCallback (int iX, int iY)
{
    Application* pkTheApp = Application::GetApplication();
    if ( pkTheApp )
        pkTheApp->OnMotion(iX,iY,gs_uiGLUTModifiers);
}
//----------------------------------------------------------------------------
static void PassiveMotionCallback (int iX, int iY)
{
    Application* pkTheApp = Application::GetApplication();
    if ( pkTheApp )
        pkTheApp->OnPassiveMotion(iX,iY);
}
//----------------------------------------------------------------------------
static void Terminate ()
{
    Application* pkTheApp = Application::GetApplication();
    if ( pkTheApp )
    {
        pkTheApp->OnTerminate();
        glutDestroyWindow(pkTheApp->GetWindowID());
    }
}
//----------------------------------------------------------------------------
int main (int iQuantity, char** apcArgument)
{
    // The unique Application object must be created pre-main by a
    // constuctor call in the application source file.

    Application* pkTheApp = Application::GetApplication();
    assert( pkTheApp );
    if ( !pkTheApp )
        return -1;

    if ( atexit(Terminate) != 0 )
        return -2;

    glutInit(&iQuantity,apcArgument);
    pkTheApp->SetCommand(new Command(iQuantity,apcArgument));

    if ( !pkTheApp->OnPrecreate() )
        return -3;

    glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGBA | GLUT_DEPTH | GLUT_STENCIL);
    glutInitWindowSize(pkTheApp->GetWidth(),pkTheApp->GetHeight());
    pkTheApp->SetWindowID(glutCreateWindow(pkTheApp->GetWindowTitle()));

    pkTheApp->SetRenderer(new GlutRenderer(pkTheApp->GetWindowID(),
        pkTheApp->GetWidth(),pkTheApp->GetHeight()));

    pkTheApp->SetCamera(new OpenGLCamera(pkTheApp->GetWidth(),
        pkTheApp->GetHeight()));

    if ( !pkTheApp->OnInitialize() )
        return -4;

    glutReshapeFunc(ReshapeCallback);
    glutDisplayFunc(DisplayCallback);
    glutIdleFunc(IdleCallback);
    glutKeyboardFunc(KeyDownCallback);
    glutKeyboardUpFunc(KeyUpCallback);
    glutSpecialFunc(SpecialKeyDownCallback);
    glutSpecialUpFunc(SpecialKeyUpCallback);
    glutMouseFunc(MouseClickCallback);
    glutMotionFunc(MotionCallback);
    glutPassiveMotionFunc(PassiveMotionCallback);

    glutMainLoop();
    return 0;
}
//----------------------------------------------------------------------------

#endif
