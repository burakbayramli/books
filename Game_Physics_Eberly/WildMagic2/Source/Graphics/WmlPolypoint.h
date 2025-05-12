// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLPOLYPOINT_H
#define WMLPOLYPOINT_H

#include "WmlGeometry.h"

namespace Wml
{

class WML_ITEM Polypoint : public Geometry
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    // Construction.  Polypoint accepts responsibility for deleting the
    // input arrays.
    Polypoint (int iVertexQuantity, Vector3f* akVertex, Vector3f* akNormal,
        ColorRGB* akColor, Vector2f* akTexture);

    virtual ~Polypoint ();

    // member access
    void SetActiveQuantity (int iActiveQuantity);
    int GetActiveQuantity () const;
    const int* Indices () const;

    // updates
    virtual void UpdateModelNormals ();

protected:
    Polypoint ();

    // drawing
    virtual void Draw (Renderer& rkRenderer);

    // Allow application to specify fewer than the maximum number of vertices
    // to draw.
    int m_iActiveQuantity;

    // support for drawing vertex arrays
    int* m_aiIndex;
};

WmlSmartPointer(Polypoint);
WmlRegisterStream(Polypoint);
#include "WmlPolypoint.inl"

}

#endif
