// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

//----------------------------------------------------------------------------
inline Application* Application::GetApplication ()
{
    return ms_pkApplication;
}
//----------------------------------------------------------------------------
inline char* Application::GetWindowTitle ()
{
    return ms_acWindowTitle;
}
//----------------------------------------------------------------------------
inline int Application::GetXPosition ()
{
    return ms_iXPos;
}
//----------------------------------------------------------------------------
inline int Application::GetYPosition ()
{
    return ms_iYPos;
}
//----------------------------------------------------------------------------
inline int Application::GetWidth ()
{
    return ms_iWidth;
}
//----------------------------------------------------------------------------
inline int Application::GetHeight ()
{
    return ms_iHeight;
}
//----------------------------------------------------------------------------
inline const ColorRGB& Application::GetBackgroundColor ()
{
    return ms_kBackgroundColor;
}
//----------------------------------------------------------------------------
inline bool Application::UseCLI ()
{
    return ms_bUseCLI;
}
//----------------------------------------------------------------------------
inline void Application::SetCommand (Command* pkCommand)
{
    ms_pkCommand = pkCommand;
}
//----------------------------------------------------------------------------
inline Command* Application::GetCommand ()
{
    return ms_pkCommand;
}
//----------------------------------------------------------------------------
inline void Application::SetWindowID (int iWindowID)
{
    ms_iWindowID = iWindowID;
}
//----------------------------------------------------------------------------
inline int Application::GetWindowID ()
{
    return ms_iWindowID;
}
//----------------------------------------------------------------------------
inline void Application::SetRenderer (Renderer* pkRenderer)
{
    ms_spkRenderer = pkRenderer;
}
//----------------------------------------------------------------------------
inline void Application::SetCamera (Camera* pkCamera)
{
    ms_spkCamera = pkCamera;
}
//----------------------------------------------------------------------------
