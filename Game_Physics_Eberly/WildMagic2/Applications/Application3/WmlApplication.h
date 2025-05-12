// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLAPPLICATION_H
#define WMLAPPLICATION_H

#include "WmlCommand.h"
#include "WmlGraphics.h"

namespace Wml
{

class Application
{
public:
    // abstract base class
    virtual ~Application ();

    // derived classes implement these as needed
    virtual bool OnPrecreate ();
    virtual bool OnInitialize ();
    virtual void OnTerminate ();
    virtual void OnMove (int iX, int iY);
    virtual void OnReshape (int iWidth, int iHeight);
    virtual void OnDisplay ();
    virtual void OnIdle ();
    virtual void OnKeyDown (unsigned char ucKey, int iX, int iY);
    virtual void OnKeyUp (unsigned char ucKey, int iX, int iY);
    virtual void OnSpecialKeyDown (int iKey, int iX, int iY);
    virtual void OnSpecialKeyUp (int iKey, int iX, int iY);
    virtual void OnMouseClick (int iButton, int iState, int iX, int iY,
        unsigned int uiModifiers);
    virtual void OnMotion (int iX, int iY, unsigned int uiModifiers);
    virtual void OnPassiveMotion (int iX, int iY);

    // turret-based camera motion
    void SetTurretAxes ();  // use camera frame
    void SetTurretAxes (const Vector3f& rkLeft, const Vector3f& rkUp,
        const Vector3f& rkDirection);
    
    // performance measurements
    static float GetTimeInSeconds ();
    void ResetTime ();
    void MeasureTime ();
    void UpdateClicks ();
    void DrawFrameRate (int iX, int iY, const ColorRGB& rkColor);

    // access to the unique application object
    static Application* GetApplication ();

    // access to members set by constructor
    static char* GetWindowTitle ();
    static int GetXPosition ();
    static int GetYPosition ();
    static int GetWidth ();
    static int GetHeight ();
    static const ColorRGB& GetBackgroundColor ();
    static bool UseCLI();

    // font management
    virtual float StringWidth (const char* acText);
    virtual float CharWidth (const char cCharacter);
    virtual float FontHeight ();

    // access for initialization/termination
    static void SetCommand (Command* pkCommand);
    static Command* GetCommand ();
    static void SetWindowID (int iWindowID);
    static int GetWindowID ();
    static void SetRenderer (Renderer* pkRenderer);
    static void SetCamera (Camera* pkCamera);
    void RequestTermination ();

    // keyboard bindings
    static const int KEY_ESCAPE;
    static const int KEY_LEFT_ARROW;
    static const int KEY_RIGHT_ARROW;
    static const int KEY_UP_ARROW;
    static const int KEY_DOWN_ARROW;
    static const int KEY_HOME;
    static const int KEY_END;
    static const int KEY_PAGE_UP;
    static const int KEY_PAGE_DOWN;
    static const int KEY_INSERT;
    static const int KEY_DELETE;
    static const int KEY_F1;
    static const int KEY_F2;
    static const int KEY_F3;
    static const int KEY_F4;
    static const int KEY_F5;
    static const int KEY_F6;
    static const int KEY_F7;
    static const int KEY_F8;
    static const int KEY_F9;
    static const int KEY_F10;
    static const int KEY_F11;
    static const int KEY_F12;

    // keyboard modifier bindings
    static const int KEY_SHIFT;
    static const int KEY_CONTROL;
    static const int KEY_ALT;
    static const int KEY_COMMAND;

    // mouse bindings
    static const int MOUSE_LEFT_BUTTON;
    static const int MOUSE_MIDDLE_BUTTON;
    static const int MOUSE_RIGHT_BUTTON;
    static const int MOUSE_UP;
    static const int MOUSE_DOWN;

    // mouse modifier bindings
    static const int MOD_LBUTTON;
    static const int MOD_MBUTTON;
    static const int MOD_RBUTTON;

protected:
    // construction and destruction
    Application (char* acWindowTitle, int iXPos, int iYPos, int iWidth,
        int iHeight, const ColorRGB& rkBackgroundColor, bool bUseCLI = true);

    // turret-based camera motion
    virtual bool MoveCamera ();
    virtual void MoveForward ();
    virtual void MoveBackward ();
    virtual void MoveUp ();
    virtual void MoveDown ();
    virtual void TurnLeft ();
    virtual void TurnRight ();
    virtual void LookUp ();
    virtual void LookDown ();

    // camera motion
    bool m_bTurretActive;
    Vector3f m_akAxis[3];
    float m_fTrnSpeed;
    float m_fRotSpeed;
    bool m_bUArrowPressed;
    bool m_bDArrowPressed;
    bool m_bLArrowPressed;
    bool m_bRArrowPressed;
    bool m_bPgUpPressed;
    bool m_bPgDnPressed;
    bool m_bHomePressed;
    bool m_bEndPressed;

    // object motion 
    bool MoveObject ();
    int m_iDoRoll, m_iDoYaw, m_iDoPitch;
    SpatialPtr m_spkMotionObject;

    // performance measurements
    float m_fLastTime, m_fAccumulatedTime, m_fFrameRate;
    int m_iClicks, m_iTimer, m_iMaxTimer;

    // There can be only one.
    static Application* ms_pkApplication;
    static Command* ms_pkCommand;
    static char* ms_acWindowTitle;
    static int ms_iWindowID, ms_iXPos, ms_iYPos, ms_iWidth, ms_iHeight;
    static ColorRGB ms_kBackgroundColor;
    static bool ms_bUseCLI;
    static RendererPtr ms_spkRenderer;
    static CameraPtr ms_spkCamera;
};

#include "WmlApplication.inl"

}

#endif
