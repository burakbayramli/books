// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef DRAWIMPLICITSURFACE_H
#define DRAWIMPLICITSURFACE_H

#include "WmlApplication2.h"
#include "RayTracer.h"

class DrawImplicitSurface : public Application2
{
public:
    DrawImplicitSurface ();
    virtual ~DrawImplicitSurface ();

    virtual bool OnInitialize ();
    virtual void OnTerminate ();
    virtual void OnDisplay ();
    virtual void OnKeyDown (unsigned char ucKey, int iX, int iY);
    virtual void OnSpecialKeyDown (int iKey, int iX, int iY);

protected:
    RayTracer m_kRT;
    int m_iMaxSample;
    bool m_bBlur;
};

#endif
