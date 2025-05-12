// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef CLODPOLYLINE_H
#define CLODPOLYLINE_H

#include "WmlApplication2.h"
#include "Polyline3.h"

class ClodPolyline : public Application2
{
public:
    ClodPolyline ();
    virtual ~ClodPolyline ();

    virtual bool OnInitialize ();
    virtual void OnTerminate ();
    virtual void OnDisplay ();
    virtual void OnKeyDown (unsigned char ucKey, int iX, int iY);

protected:
    Polyline3* m_pkPolyline;
    int m_iLOD;
};

#endif
