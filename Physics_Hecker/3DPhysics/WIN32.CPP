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

#define WIN32_LEAN_AND_MEAN
#define WIN32_EXTRA_LEAN
#include <windows.h>
#include <windowsx.h>
#include <mmsystem.h>
#include <commdlg.h>
#include <stdlib.h>
#include <strstrea.h>
#include <fstream.h>
#include <math.h>
#include <assert.h>

#include <gl/gl.h>
#include <gl/glu.h>

#include "win32.h"
#include "iface.h"

#include "math3d.h"

float Zoom = -10.0f;
float dZoom = 0.0f;
float Azimuth = 0.0f;
float dAzimuth = 0.0f;
float Altitude = 0.0f;
float dAltitude = 0.0f;

#pragma warning (disable:4244) // conversion from float to int

/*----------------------------------------------------------------------------

Globals and declarations

*/

int unsigned WindowWidth = 400;
int unsigned WindowHeight = 400;

char szAppName[200] = "Cheesy 3D Physics App";

HINSTANCE hInstApp;
HWND      hwndApp;
HPALETTE  hpalApp;
BOOL fAppActive;

LONG FAR PASCAL AppWndProc(HWND hwnd,UINT msg,WPARAM wParam,LPARAM lParam);
LONG AppCommand (HWND hwnd,UINT msg,WPARAM wParam,LPARAM lParam);
int AppPaint (HWND hwnd, HDC hdc);

void AppExit(void);
BOOL AppIdle(void);

HDC WindowDC;
HGLRC WindowRC;


/*----------------------------------------------------------------------------*\
|   AppAbout( hDlg, uiMessage, wParam, lParam )                                |
|                                                                              |
|   Description:                                                               |
|       This function handles messages belonging to the "About" dialog box.    |
|       The only message that it looks for is WM_COMMAND, indicating the use   |
|       has pressed the "OK" button.  When this happens, it takes down         |
|       the dialog box.                                                        |
|                                                                              |
|   Arguments:                                                                 |
|       hDlg            window handle of about dialog window                   |
|       uiMessage       message number                                         |
|       wParam          message-dependent                                      |
|       lParam          message-dependent                                      |
|                                                                              |
|   Returns:                                                                   |
|       TRUE if message has been processed, else FALSE                         |
|                                                                              |
\*----------------------------------------------------------------------------*/
BOOL FAR PASCAL  AppAbout(HWND hwnd,UINT msg,WPARAM wParam,
            LPARAM /* lParam */ )
{
    switch (msg)
    {
        case WM_COMMAND:
            if (LOWORD(wParam) == IDOK)
            {
                EndDialog(hwnd,TRUE);
            }
            break;

        case WM_INITDIALOG:
            return TRUE;
    }
    return FALSE;
}

/*----------------------------------------------------------------------------*\
|   AppInit( hInst, hPrev)                                                     |
|                                                                              |
|   Description:                                                               |
|       This is called when the application is first loaded into               |
|       memory.  It performs all initialization that doesn't need to be done   |
|       once per instance.                                                     |
|                                                                              |
|   Arguments:                                                                 |
|       hInstance       instance handle of current instance                    |
|       hPrev           instance handle of previous instance                   |
|                                                                              |
|   Returns:                                                                   |
|       TRUE if successful, FALSE if not                                       |
|                                                                              |
\*----------------------------------------------------------------------------*/

BOOL AppInit(HINSTANCE hInst,HINSTANCE hPrev,int sw,LPSTR szCmdLine)
{

    WNDCLASS cls;

    /* Save instance handle for DialogBoxs */
    hInstApp = hInst;

    if (!hPrev)
    {
        /*
         *  Register a class for the main application window
         */
        cls.hCursor        = LoadCursor(NULL,IDC_ARROW);
        cls.hIcon          = LoadIcon(hInst,"AppIcon");
        cls.lpszMenuName   = "AppMenu";
        cls.lpszClassName  = szAppName;
        cls.hbrBackground  = (HBRUSH)(COLOR_WINDOW + 1);
        cls.hInstance      = hInst;
        cls.style          = CS_BYTEALIGNCLIENT |
                                CS_VREDRAW | CS_HREDRAW | CS_DBLCLKS | CS_OWNDC;
        cls.lpfnWndProc    = (WNDPROC)AppWndProc;
        cls.cbWndExtra     = 0;
        cls.cbClsExtra     = 0;

        if (!RegisterClass(&cls))
            return FALSE;
    }

    DWORD Style = WS_OVERLAPPEDWINDOW | WS_CLIPCHILDREN | WS_CLIPSIBLINGS;
    RECT WindowRect = { 0, 0 };
    WindowRect.right = WindowWidth;
    WindowRect.bottom = WindowHeight;

    AdjustWindowRect(&WindowRect,Style,TRUE);
    
    hwndApp = CreateWindow(szAppName,szAppName,Style,
                    CW_USEDEFAULT,0,
                    WindowRect.right-WindowRect.left,
                    WindowRect.bottom-WindowRect.top,
                    0,0,hInst,0);

    // create RC

    WindowDC = GetDC(hwndApp);
    PIXELFORMATDESCRIPTOR FormatDescriptor =
    {
        sizeof (PIXELFORMATDESCRIPTOR), 1,
        PFD_DRAW_TO_WINDOW | PFD_SUPPORT_OPENGL | PFD_DOUBLEBUFFER,
        PFD_TYPE_RGBA, 16,
        0,0, 0,0, 0,0, 0,0,
        0, 0,0,0,0,
        16,
        0, 0,
        PFD_MAIN_PLANE,
        0,
        0,0,0
    };

#if 0
    int const NumberOfFormats = DescribePixelFormat(WindowDC,1,
           sizeof(FormatDescriptor),&FormatDescriptor);
    for(int i=0;i < NumberOfFormats;i++)
      { 
    DescribePixelFormat(WindowDC,i,
           sizeof(FormatDescriptor),&FormatDescriptor);
      }
#endif
    assert(!(GetDeviceCaps(WindowDC,RASTERCAPS) & RC_PALETTE));

    int FormatIndex = ChoosePixelFormat(WindowDC,&FormatDescriptor);
    SetPixelFormat(WindowDC,FormatIndex,&FormatDescriptor);
    WindowRC = wglCreateContext(WindowDC);
    wglMakeCurrent(WindowDC,WindowRC);

    // everything's set up, so call the app
    Initialize();
    
    ShowWindow(hwndApp,sw);

    return TRUE;
}


/*----------------------------------------------------------------------------*\
|   AppExit()                                      |
|                                                                              |
|   Description:                                                               |
|   app is just about to exit, cleanup                     |
|                                                                              |
\*----------------------------------------------------------------------------*/
void AppExit()
{
    Uninitialize();

    wglMakeCurrent(0,0);
    wglDeleteContext(WindowRC);
    ReleaseDC(hwndApp,WindowDC);
}

BOOL AppIdle()
{
    if (fAppActive)
    {
        HDC WindowDC = GetDC(hwndApp);
        AppPaint(hwndApp,WindowDC);
        ReleaseDC(hwndApp,WindowDC);

        return FALSE;
    }
    else
    {
        return TRUE;        // nothing to do.
    }
}

/*----------------------------------------------------------------------------*\
|   WinMain( hInst, hPrev, lpszCmdLine, cmdShow )                              |
|                                                                              |
|   Description:                                                               |
|       The main procedure for the App.  After initializing, it just goes      |
|       into a message-processing loop until it gets a WM_QUIT message         |
|       (meaning the app was closed).                                          |
|                                                                              |
|   Arguments:                                                                 |
|       hInst           instance handle of this instance of the app            |
|       hPrev           instance handle of previous instance, NULL if first    |
|       szCmdLine       ->null-terminated command line                         |
|       cmdShow         specifies how the window is initially displayed        |
|                                                                              |
|   Returns:                                                                   |
|       The exit code as specified in the WM_QUIT message.                     |
|                                                                              |
\*----------------------------------------------------------------------------*/
int PASCAL WinMain(HINSTANCE hInst, HINSTANCE hPrev, LPSTR szCmdLine, int sw)
{
    MSG     msg;

    /* Call initialization procedure */
    if (!AppInit(hInst,hPrev,sw,szCmdLine))
    return FALSE;

    /*
     * Polling messages from event queue
     */
    for (;;)
    {
        if (PeekMessage(&msg, NULL, 0, 0,PM_REMOVE))
        {
            if (msg.message == WM_QUIT)
                break;

            TranslateMessage(&msg);
            DispatchMessage(&msg);
        }
        else
        {
            if (AppIdle())
                WaitMessage();
        }
    }


    AppExit();
    return msg.wParam;
}

/*----------------------------------------------------------------------------*\
|   AppPaint(hwnd, hdc)                                                        |
|                                                                              |
|   Description:                                                               |
|       The paint function.  Right now this does nothing.                      |
|                                                                              |
|   Arguments:                                                                 |
|       hwnd             window painting into                                  |
|       hdc              display context to paint to                           |
|                                                                              |
|   Returns:                                                                   |
|       nothing                                                                |
|                                                                              |
\*----------------------------------------------------------------------------*/
int AppPaint (HWND hwnd, HDC hdc)
{
    glLoadIdentity();

    glTranslatef(0.0f,0.0f,Zoom);
    glRotatef(Altitude,1.0f,0.0f,0.0f);
    glRotatef(Azimuth,0.0f,1.0f,0.0f);

    glRotatef(-90.0f,1.0f,0.0f,0.0f);   // make world z point up in view

    Run();      // run the physics

    Altitude += dAltitude;
    Azimuth += dAzimuth;
    Zoom += dZoom;

    SwapBuffers(WindowDC);

    return TRUE;
}

/*----------------------------------------------------------------------------*\
|   AppWndProc( hwnd, uiMessage, wParam, lParam )                              |
|                                                                              |
|   Description:                                                               |
|       The window proc for the app's main (tiled) window.  This processes all |
|       of the parent window's messages.                                       |
|                                                                              |
\*----------------------------------------------------------------------------*/
LONG FAR PASCAL AppWndProc(HWND hwnd,UINT msg,WPARAM wParam,LPARAM lParam)
{
    PAINTSTRUCT ps;
    HDC hdc;

    switch (msg)
    {
        case WM_CREATE:
        break;

        case WM_ACTIVATEAPP:
            fAppActive = (BOOL)wParam;
            break;

        case WM_KEYDOWN:
        {
            switch(wParam)
            {
                case 'W':
                {
                    SendMessage(hwnd,WM_COMMAND,MENU_WORLDSPRING,0);
                    break;
                }
                case 'B':
                {
                    SendMessage(hwnd,WM_COMMAND,MENU_BODYSPRING,0);
                    break;
                }
                case 'D':
                {
                    SendMessage(hwnd,WM_COMMAND,MENU_DAMPING,0);
                    break;
                }
                case 'G':
                {
                    SendMessage(hwnd,WM_COMMAND,MENU_GRAVITY,0);
                    break;
                }
                case VK_LEFT:
                {
                    dAzimuth = 3.0f;
                    break;
                }
                case VK_RIGHT:
                {
                    dAzimuth = -3.0f;
                    break;
                }
                case VK_UP:
                {
                    dAltitude = 3.0f;
                    break;
                }
                case VK_DOWN:
                {
                    dAltitude = -3.0f;
                    break;
                }
                case 'I':
                {
                    dZoom = 1.0f;
                    break;
                }
                case 'O':
                {
                    dZoom = -1.0f;
                    break;
                }
            }
            break;
        }
        case WM_KEYUP:
        {
            switch(wParam)
            {
                case VK_LEFT:
                case VK_RIGHT:
                {
                    dAzimuth = 0.0f;
                    break;
                }
                case VK_UP:
                case VK_DOWN:
                {
                    dAltitude = 0.0f;
                    break;
                }
                case 'I':
                case 'O':
                {
                    dZoom = 0.0f;
                    break;
                }
            }
            break;
        }

        case WM_LBUTTONDOWN:
        {
            break;
        }

        case WM_MOUSEMOVE:
        {
            break;
        }

        case WM_LBUTTONUP:
        {
            break;
        }

        case WM_ERASEBKGND:
            break;

        case WM_INITMENU:
            break;

        case WM_COMMAND:
            return AppCommand(hwnd,msg,wParam,lParam);

        case WM_DESTROY:
            PostQuitMessage(0);
            break;

        case WM_CLOSE:
            break;

        case WM_PAINT:
            hdc = BeginPaint(hwnd,&ps);
            AppPaint (hwnd,hdc);
            EndPaint(hwnd,&ps);
            return 0L;

        case WM_SIZE:
            break;
    }
    return DefWindowProc(hwnd,msg,wParam,lParam);
}

/*----------------------------------------------------------------------------*\
|   AppCommand(hwnd, msg, wParam, lParam )                     |
|                                                                              |
|   Description:                                                               |
|   handles WM_COMMAND messages for the main window (hwndApp)          |
|       of the parent window's messages.                                       |
|                                                                              |
\*----------------------------------------------------------------------------*/
LONG AppCommand (HWND hwnd,UINT /* msg */,WPARAM wParam,LPARAM /* lParam */)
{
    switch(wParam)
    {
        case MENU_ABOUT:
            DialogBox(hInstApp,"AppAbout",hwnd,AppAbout);
            break;

        case MENU_WORLDSPRING:
            ToggleWorldSprings();
            break;
        case MENU_BODYSPRING:
            ToggleBodySprings();
            break;
        case MENU_DAMPING:
            ToggleDamping();
            break;
        case MENU_GRAVITY:
            ToggleGravity();
            break;

        case MENU_EXIT:
            PostMessage(hwnd,WM_CLOSE,0,0L);
            break;
    }
    return 0L;
}
    
extern "C" void __cdecl _assert( void *pExpression, void *pFile,
    unsigned LineNumber )
{
    char aBuffer[500];
    wsprintf(aBuffer,"Assertion: %s\nFile: %s, Line: %d\n" \
                    "Hit Abort to exit, Retry to debug, Ignore to continue",
                    pExpression,pFile,LineNumber);

    int Hit = MessageBox(hwndApp,aBuffer,"Assert!",MB_ABORTRETRYIGNORE |
                    MB_ICONHAND);

    if(Hit == IDABORT)
    {
        exit(0);
    }
    else
    if(Hit == IDRETRY)
    {
        DebugBreak();
    }
}

/*----------------------------------------------------------------------------

iface.h functions

*/

#include <time.h>
float GetTime( void )
{
#if 0   
    static clock_t StartTime = clock();
    assert(StartTime != (clock_t)(-1));
    return ((float)(clock()-StartTime))/((float)CLOCKS_PER_SEC);
#else
    static DWORD StartMilliseconds;
    if(!StartMilliseconds)
    {
        // yes, the first time through will be a 0 timestep
        StartMilliseconds = timeGetTime();
    }

    DWORD CurrentMilliseconds = timeGetTime();
    return float(CurrentMilliseconds - StartMilliseconds) / 1000.0f;
#endif  
}
