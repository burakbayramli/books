// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLOPENGLCAMERA_H
#define WMLOPENGLCAMERA_H

#include "WmlCamera.h"
#include "WmlRendererLibType.h"

namespace Wml
{

class WML_RENDERER_ITEM OpenGLCamera : public Camera
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    OpenGLCamera (float fWidth, float fHeight);

protected:
    OpenGLCamera ();

    virtual void OnResize (int iWidth, int iHeight);
    virtual void OnFrustumChange ();
    virtual void OnViewPortChange ();
    virtual void OnFrameChange ();

    float m_fWidth, m_fHeight;
};

WmlRegisterStream(OpenGLCamera);

}

#endif
