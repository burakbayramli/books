// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLPOLYGONOFFSETSTATE_H
#define WMLPOLYGONOFFSETSTATE_H

#include "WmlColorRGB.h"
#include "WmlRenderState.h"

namespace Wml
{

WmlSmartPointer(PolygonOffsetState);

class WML_ITEM PolygonOffsetState : public RenderState
{
    WmlDeclareDefaultState(PolygonOffsetState);
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    PolygonOffsetState ();
    virtual Type GetType () const;

    // Set whether offset should be enabled for the various polygon drawing
    // modes (fill, line, point).
    bool& FillEnabled ();   // default: false
    bool& LineEnabled ();   // default: false
    bool& PointEnabled ();  // default: false

    // The offset is Scale()*dZ + Bias()*r where dZ is the change in depth
    // relative to the screen space area of the poly, and r is the smallest
    // resolvable depth difference.  Negative values move polygons closer to
    // the eye.
    float& Scale ();  // default: -1.0
    float& Bias ();   // default: -2.0

protected:
    bool m_bFillEnabled, m_bLineEnabled, m_bPointEnabled;
    float m_fScale, m_fBias;
};

WmlRegisterStream(PolygonOffsetState);
#include "WmlPolygonOffsetState.inl"

}

#endif
