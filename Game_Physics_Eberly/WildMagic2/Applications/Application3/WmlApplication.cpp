// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlApplication.h"
#include "WmlCommand.h"
using namespace Wml;

Application* Application::ms_pkApplication = NULL;
Command* Application::ms_pkCommand = NULL;
char* Application::ms_acWindowTitle = NULL;
int Application::ms_iWindowID = 0;
int Application::ms_iXPos = 0;
int Application::ms_iYPos = 0;
int Application::ms_iWidth = 0;
int Application::ms_iHeight = 0;
ColorRGB Application::ms_kBackgroundColor;
bool Application::ms_bUseCLI;
RendererPtr Application::ms_spkRenderer;
CameraPtr Application::ms_spkCamera;

//----------------------------------------------------------------------------
Application::Application (char* acWindowTitle, int iXPos, int iYPos,
    int iWidth, int iHeight, const ColorRGB& rkBackgroundColor, bool bUseCLI)
{
    assert( ms_pkApplication == NULL );
    ms_pkApplication = this;

    ms_acWindowTitle = acWindowTitle;
    ms_iXPos = iXPos;
    ms_iYPos = iYPos;
    ms_iWidth = iWidth;
    ms_iHeight = iHeight;
    ms_kBackgroundColor = rkBackgroundColor;    
    ms_bUseCLI = bUseCLI;

    m_iDoRoll = 0;
    m_iDoYaw = 0;
    m_iDoPitch = 0;
    m_spkMotionObject = NULL;
}
//----------------------------------------------------------------------------
Application::~Application ()
{
    assert( ms_pkApplication != NULL );
    ms_pkApplication = NULL;
    delete ms_pkCommand;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// application callbacks
//----------------------------------------------------------------------------
bool Application::OnPrecreate ()
{
    return true;
}
//----------------------------------------------------------------------------
bool Application::OnInitialize ()
{
    ms_spkRenderer->SetBackgroundColor(ms_kBackgroundColor);
    ms_spkRenderer->SetCamera(ms_spkCamera);

    m_bTurretActive = false;
    m_fTrnSpeed = 0.01f;
    m_fRotSpeed = 0.01f;
    m_bUArrowPressed = false;
    m_bDArrowPressed = false;
    m_bLArrowPressed = false;
    m_bRArrowPressed = false;
    m_bPgUpPressed = false;
    m_bPgDnPressed = false;
    m_bHomePressed = false;
    m_bEndPressed = false;

    m_fLastTime = -1.0f;
    m_fAccumulatedTime = 0.0f;
    m_fFrameRate = 0.0f;
    m_iClicks = 0;
    m_iTimer = 30;
    m_iMaxTimer = 30;

    return true;
}
//----------------------------------------------------------------------------
void Application::OnTerminate ()
{
    ms_spkRenderer->SetCamera(NULL);
    ms_spkCamera = NULL;
    ms_spkRenderer = NULL;
    m_spkMotionObject = NULL;
}
//----------------------------------------------------------------------------
void Application::OnMove (int iX, int iY)
{
    if ( ms_spkRenderer )
        ms_spkRenderer->Move(iX,iY);

    ms_iXPos = iX;
    ms_iYPos = iY;
}
//----------------------------------------------------------------------------
void Application::OnReshape (int iWidth, int iHeight)
{
    if ( ms_spkRenderer )
        ms_spkRenderer->Resize(iWidth,iHeight);

    ms_iWidth = iWidth;
    ms_iHeight = iHeight;
}
//----------------------------------------------------------------------------
void Application::OnDisplay ()
{
    OnIdle();
}
//----------------------------------------------------------------------------
void Application::OnIdle ()
{
}
//----------------------------------------------------------------------------
void Application::OnKeyDown (unsigned char ucKey, int, int)
{
    // quit application if the ESCAPE key is pressed
    if ( ucKey == KEY_ESCAPE )
        RequestTermination();
}
//----------------------------------------------------------------------------
void Application::OnKeyUp (unsigned char, int, int)
{
}
//----------------------------------------------------------------------------
void Application::OnSpecialKeyDown (int iKey, int, int)
{
    if ( !m_bTurretActive )
        return;

    if ( iKey == KEY_LEFT_ARROW )
    {
        m_bLArrowPressed = true;
        return;
    }

    if ( iKey == KEY_RIGHT_ARROW )
    {
        m_bRArrowPressed = true;
        return;
    }

    if ( iKey == KEY_UP_ARROW )
    {
        m_bUArrowPressed = true;
        return;
    }

    if ( iKey == KEY_DOWN_ARROW )
    {
        m_bDArrowPressed = true;
        return;
    }

    if ( iKey == KEY_PAGE_UP )
    {
        m_bPgUpPressed = true;
        return;
    }

    if ( iKey == KEY_PAGE_DOWN )
    {
        m_bPgDnPressed = true;
        return;
    }

    if ( iKey == KEY_HOME )
    {
        m_bHomePressed = true;
        return;
    }

    if ( iKey == KEY_END )
    {
        m_bEndPressed = true;
        return;
    }

    if ( iKey == KEY_F1 )
    {
        m_iDoRoll = -1;
        return;
    }
    else if ( iKey == KEY_F2 )
    {
        m_iDoRoll = +1;
        return;
    }

    if ( iKey == KEY_F3 )
    {
        m_iDoYaw = -1;
        return;
    }
    else if ( iKey == KEY_F4 )
    {
        m_iDoYaw = 1;
        return;
    }

    if ( iKey == KEY_F5 )
    {
        m_iDoPitch = -1;
        return;
    }
    else if ( iKey == KEY_F6 )
    {
        m_iDoPitch = 1;
        return;
    }
}
//----------------------------------------------------------------------------
void Application::OnSpecialKeyUp (int iKey, int, int)
{
    if ( !m_bTurretActive )
        return;

    if ( iKey == KEY_LEFT_ARROW )
    {
        m_bLArrowPressed = false;
        return;
    }

    if ( iKey == KEY_RIGHT_ARROW )
    {
        m_bRArrowPressed = false;
        return;
    }

    if ( iKey == KEY_UP_ARROW )
    {
        m_bUArrowPressed = false;
        return;
    }

    if ( iKey == KEY_DOWN_ARROW )
    {
        m_bDArrowPressed = false;
        return;
    }

    if ( iKey == KEY_PAGE_UP )
    {
        m_bPgUpPressed = false;
        return;
    }

    if ( iKey == KEY_PAGE_DOWN )
    {
        m_bPgDnPressed = false;
        return;
    }

    if ( iKey == KEY_HOME )
    {
        m_bHomePressed = false;
        return;
    }

    if ( iKey == KEY_END )
    {
        m_bEndPressed = false;
        return;
    }

    if ( iKey == KEY_F1 || iKey == KEY_F2 )
    {
        m_iDoRoll = 0;
        return;
    }

    if ( iKey == KEY_F3 || iKey == KEY_F4 )
    {
        m_iDoYaw = 0;
        return;
    }

    if ( iKey == KEY_F5 || iKey == KEY_F6 )
    {
        m_iDoPitch = 0;
        return;
    }
}
//----------------------------------------------------------------------------
void Application::OnMouseClick (int, int, int, int, unsigned int)
{
}
//----------------------------------------------------------------------------
void Application::OnMotion (int, int, unsigned int)
{
}
//----------------------------------------------------------------------------
void Application::OnPassiveMotion (int, int)
{
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// turret-based camera motion
//----------------------------------------------------------------------------
void Application::SetTurretAxes ()
{
    m_akAxis[0] = ms_spkCamera->GetLeft();
    m_akAxis[1] = ms_spkCamera->GetUp();
    m_akAxis[2] = ms_spkCamera->GetDirection();
}
//----------------------------------------------------------------------------
void Application::SetTurretAxes (const Vector3f& rkLeft, const Vector3f& rkUp,
    const Vector3f& rkDirection)
{
    m_akAxis[0] = rkLeft;
    m_akAxis[1] = rkUp;
    m_akAxis[2] = rkDirection;
}
//----------------------------------------------------------------------------
void Application::MoveForward ()
{
    Vector3f kLoc = ms_spkCamera->GetLocation();
    kLoc += m_fTrnSpeed*m_akAxis[2];
    ms_spkCamera->SetLocation(kLoc);
    ms_spkCamera->Update();
}
//----------------------------------------------------------------------------
void Application::MoveBackward ()
{
    Vector3f kLoc = ms_spkCamera->GetLocation();
    kLoc -= m_fTrnSpeed*m_akAxis[2];
    ms_spkCamera->SetLocation(kLoc);
    ms_spkCamera->Update();
}
//----------------------------------------------------------------------------
void Application::MoveUp ()
{
    Vector3f kLoc = ms_spkCamera->GetLocation();
    kLoc += m_fTrnSpeed*m_akAxis[1];
    ms_spkCamera->SetLocation(kLoc);
    ms_spkCamera->Update();
}
//----------------------------------------------------------------------------
void Application::MoveDown ()
{
    Vector3f kLoc = ms_spkCamera->GetLocation();
    kLoc -= m_fTrnSpeed*m_akAxis[1];
    ms_spkCamera->SetLocation(kLoc);
    ms_spkCamera->Update();
}
//----------------------------------------------------------------------------
void Application::TurnLeft ()
{
    Matrix3f kIncr(m_akAxis[1],m_fRotSpeed);
    m_akAxis[0] = kIncr*m_akAxis[0];
    m_akAxis[2] = kIncr*m_akAxis[2];

    Vector3f kLeft = kIncr*ms_spkCamera->GetLeft();
    Vector3f kUp = kIncr*ms_spkCamera->GetUp();
    Vector3f kDirection = kIncr*ms_spkCamera->GetDirection();
    ms_spkCamera->SetAxes(kLeft,kUp,kDirection);
    ms_spkCamera->Update();
}
//----------------------------------------------------------------------------
void Application::TurnRight ()
{
    Matrix3f kIncr(m_akAxis[1],-m_fRotSpeed);
    m_akAxis[0] = kIncr*m_akAxis[0];
    m_akAxis[2] = kIncr*m_akAxis[2];

    Vector3f kLeft = kIncr*ms_spkCamera->GetLeft();
    Vector3f kUp = kIncr*ms_spkCamera->GetUp();
    Vector3f kDirection = kIncr*ms_spkCamera->GetDirection();
    ms_spkCamera->SetAxes(kLeft,kUp,kDirection);
    ms_spkCamera->Update();
}
//----------------------------------------------------------------------------
void Application::LookUp ()
{
    Matrix3f kIncr(m_akAxis[0],-m_fRotSpeed);

    Vector3f kLeft = kIncr*ms_spkCamera->GetLeft();
    Vector3f kUp = kIncr*ms_spkCamera->GetUp();
    Vector3f kDirection = kIncr*ms_spkCamera->GetDirection();
    ms_spkCamera->SetAxes(kLeft,kUp,kDirection);
    ms_spkCamera->Update();
}
//----------------------------------------------------------------------------
void Application::LookDown ()
{
    Matrix3f kIncr(m_akAxis[0],m_fRotSpeed);

    Vector3f kLeft = kIncr*ms_spkCamera->GetLeft();
    Vector3f kUp = kIncr*ms_spkCamera->GetUp();
    Vector3f kDirection = kIncr*ms_spkCamera->GetDirection();
    ms_spkCamera->SetAxes(kLeft,kUp,kDirection);
    ms_spkCamera->Update();
}
//----------------------------------------------------------------------------
bool Application::MoveCamera ()
{
    if ( m_bUArrowPressed )
    {
        MoveForward();
        return true;
    }

    if ( m_bDArrowPressed )
    {
        MoveBackward();
        return true;
    }

    if ( m_bHomePressed )
    {
        MoveUp();
        return true;
    }

    if ( m_bEndPressed )
    {
        MoveDown();
        return true;
    }

    if ( m_bLArrowPressed )
    {
        TurnLeft();
        return true;
    }

    if ( m_bRArrowPressed )
    {
        TurnRight();
        return true;
    }

    if ( m_bPgUpPressed )
    {
        LookUp();
        return true;
    }

    if ( m_bPgDnPressed )
    {
        LookDown();
        return true;
    }

    return false;
}
//----------------------------------------------------------------------------
bool Application::MoveObject ()
{
    if ( !m_spkMotionObject )
        return false;

    Matrix3f kRot, kIncr;

    if ( m_iDoRoll )
    {
        kRot = m_spkMotionObject->Rotate();
        kIncr.FromAxisAngle(Vector3f::UNIT_X,m_iDoRoll*m_fRotSpeed);
        m_spkMotionObject->Rotate() = kIncr*kRot;
        return true;
    }

    if ( m_iDoYaw )
    {
        kRot = m_spkMotionObject->Rotate();
        kIncr.FromAxisAngle(Vector3f::UNIT_Y,m_iDoYaw*m_fRotSpeed);
        m_spkMotionObject->Rotate() = kIncr*kRot;
        return true;
    }

    if ( m_iDoPitch )
    {
        kRot = m_spkMotionObject->Rotate();
        kIncr.FromAxisAngle(Vector3f::UNIT_Z,m_iDoPitch*m_fRotSpeed);
        m_spkMotionObject->Rotate() = kIncr*kRot;
        return true;
    }

    return false;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// performance measurements
//----------------------------------------------------------------------------
void Application::ResetTime ()
{
    m_fLastTime = -1.0f;
}
//----------------------------------------------------------------------------
void Application::MeasureTime ()
{
    // start performance measurements
    if ( m_fLastTime == -1.0f )
    {
        m_fLastTime = GetTimeInSeconds();
        m_fAccumulatedTime = 0.0f;
        m_fFrameRate = 0.0f;
        m_iClicks = 0;
        m_iTimer = m_iMaxTimer;
    }

    // measure time
    float fCurrentTime = GetTimeInSeconds();
    float fDelta = fCurrentTime - m_fLastTime;
    m_fLastTime = fCurrentTime;
    m_fAccumulatedTime += fDelta;
}
//----------------------------------------------------------------------------
void Application::UpdateClicks ()
{
    m_iClicks++;
}
//----------------------------------------------------------------------------
void Application::DrawFrameRate (int iX, int iY, const ColorRGB& rkColor)
{
    if ( --m_iTimer == 0 )
    {
        if ( m_fAccumulatedTime > 0.0f )
            m_fFrameRate = m_iClicks/m_fAccumulatedTime;
        else
            m_fFrameRate = 0.0f;

        m_iTimer = m_iMaxTimer;
    }

    char acMessage[256];
    sprintf(acMessage,"fps: %.1lf",(double)m_fFrameRate);
    ms_spkRenderer->Draw(iX,iY,rkColor,acMessage);
}
//----------------------------------------------------------------------------
