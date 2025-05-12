// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLPOLYLINE_H
#define WMLPOLYLINE_H

#include "WmlGeometry.h"

namespace Wml
{

class WML_ITEM Polyline : public Geometry
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    // Construction and destruction.  Polyline accepts responsibility for
    // deleting the input arrays.
    Polyline (int iVertexQuantity, Vector3f* akVertex, Vector3f* akNormal,
        ColorRGB* akColor, Vector2f* akTexture, bool bClosed);

    virtual ~Polyline ();

    void Reconstruct (int iVertexQuantity, Vector3f* akVertex,
        Vector3f* akNormal, ColorRGB* akColor, Vector2f* akTexture,
        bool bClosed);

    // member access
    void SetActiveQuantity (int iActiveQuantity);
    int GetActiveQuantity () const;
    bool& Closed () const;
    int GetSegmentQuantity () const;
    void GetSegment (int i, Vector3f& rkV0, Vector3f& rkV1) const;
    const int* Indices () const;

    // updates
    virtual void UpdateModelNormals ();
    
    // Use contiguous vertices (default true).
    const bool Contiguous () const;
    bool& Contiguous ();

protected:
    Polyline ();

    // drawing
    virtual void Draw (Renderer& rkRenderer);

    // polyline is open or closed, contiguous or not
    bool m_bClosed;
    bool m_bContiguous;

    // supports drawing vertex arrays
    int* m_aiIndex;

    // Allow application to specify fewer than the maximum number of vertices
    // to draw.
    int m_iActiveQuantity;
};

WmlSmartPointer(Polyline);
WmlRegisterStream(Polyline);
#include "WmlPolyline.inl"

}

#endif
