// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

// WARNING.  The order of inclusion here is important for CodeWarrior 7.0
// on the Macintosh.  In the other order, the MSL header <cmath.macos.h>
// would be included before the Carbon header <fp.h>.  Both headers define
// certain functions (like fabs), but <fp.h> first checks to see if
// <cmath.macos.h> has defined them before it redefines them, but
// <cmath.macos.h> does not check before redefining them.
#include "WmlOpenGLCamera.h"
#include "WmlOpenGLIncludes.h"
using namespace Wml;

WmlImplementRTTI(OpenGLCamera,Camera);
WmlImplementStream(OpenGLCamera);

//----------------------------------------------------------------------------
OpenGLCamera::OpenGLCamera (float fWidth, float fHeight)
{
    m_fWidth = fWidth;
    m_fHeight = fHeight;

    OnFrustumChange();
    OnViewPortChange();
    OnFrameChange();
}
//----------------------------------------------------------------------------
OpenGLCamera::OpenGLCamera ()
{
    m_fWidth = 0.0f;
    m_fHeight = 0.0f;
}
//----------------------------------------------------------------------------
void OpenGLCamera::OnResize (int iWidth, int iHeight)
{
    Camera::OnResize(iWidth,iHeight);
    m_fWidth = (float)iWidth;
    m_fHeight = (float)iHeight;
    OnViewPortChange();
}
//----------------------------------------------------------------------------
void OpenGLCamera::OnFrustumChange ()
{
    Camera::OnFrustumChange();

    // set projection matrix
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    if ( GetUsePerspective() )
    {
        glFrustum(m_fFrustumL,m_fFrustumR,m_fFrustumB,m_fFrustumT,m_fFrustumN,
            m_fFrustumF);
    }
    else
    {
        glOrtho(m_fFrustumL,m_fFrustumR,m_fFrustumB,m_fFrustumT,m_fFrustumN,
            m_fFrustumF);
    }
}
//----------------------------------------------------------------------------
void OpenGLCamera::OnViewPortChange ()
{
    Camera::OnViewPortChange();

    // set view port
    GLint iX = (GLint)(m_fPortL*m_fWidth);
    GLint iY = (GLint)(m_fPortB*m_fHeight);
    GLsizei iW = (GLsizei)((m_fPortR - m_fPortL)*m_fWidth);
    GLsizei iH = (GLsizei)((m_fPortT - m_fPortB)*m_fHeight);
    glViewport(iX,iY,iW,iH);
}
//----------------------------------------------------------------------------
void OpenGLCamera::OnFrameChange ()
{
    Camera::OnFrameChange();

    // set view matrix
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    Vector3f kLookAt = m_kLocation + m_kDirection;
    gluLookAt(m_kLocation.X(),m_kLocation.Y(),m_kLocation.Z(),kLookAt.X(),
        kLookAt.Y(),kLookAt.Z(),m_kUp.X(),m_kUp.Y(),m_kUp.Z());
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* OpenGLCamera::Factory (Stream& rkStream)
{
    OpenGLCamera* pkObject = new OpenGLCamera;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void OpenGLCamera::Load (Stream& rkStream, Stream::Link* pkLink)
{
    Camera::Load(rkStream,pkLink);

    // native data
    StreamRead(rkStream,m_fWidth);
    StreamRead(rkStream,m_fHeight);
}
//----------------------------------------------------------------------------
void OpenGLCamera::Link (Stream& rkStream, Stream::Link* pkLink)
{
    Camera::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool OpenGLCamera::Register (Stream& rkStream)
{
    return Camera::Register(rkStream);
}
//----------------------------------------------------------------------------
void OpenGLCamera::Save (Stream& rkStream)
{
    Camera::Save(rkStream);

    // native data
    StreamWrite(rkStream,m_fWidth);
    StreamWrite(rkStream,m_fHeight);
}
//----------------------------------------------------------------------------
StringTree* OpenGLCamera::SaveStrings ()
{
    // TO DO.  Finish implementation.
    StringTree* pkTree = new StringTree(1,0,1,0);
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetChild(0,Camera::SaveStrings());
    return pkTree;
}
//----------------------------------------------------------------------------
int OpenGLCamera::GetMemoryUsed () const
{
    int iBaseSize = sizeof(OpenGLCamera) - sizeof(Camera);
    int iTotalSize = iBaseSize + Camera::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int OpenGLCamera::GetDiskUsed () const
{
    return Camera::GetDiskUsed() +
        sizeof(m_fWidth) +
        sizeof(m_fHeight);
}
//----------------------------------------------------------------------------
