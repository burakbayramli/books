// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLPORTAL_H
#define WMLPORTAL_H

#include "WmlSpatial.h"

namespace Wml
{

class Camera;
class ConvexRegion;
class Renderer;

class WML_ITEM Portal : public Object
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    // The portal is a unidirectional connector between two regions.  The
    // vertices of the portal must satisfy the following constraints:
    // 1. They must form a planar convex polygon (quantity >= 3 is implied).
    // 2. They must be counterclockwise ordered when looking through the
    //    portal to the adjacent region.
    // 3. They must be in the model space coordinates for the region that
    //    contains the portal.

    // Construction and destruction.  Portal accepts responsibility for
    // deleting the input array.
    Portal (int iVertexQuantity, Vector3f* akModelVertex,
        ConvexRegion* pkAdjacentRegion, bool bOpen);

    virtual ~Portal ();

    // member access
    ConvexRegion*& AdjacentRegion ();
    bool& Open ();

protected:
    // streaming
    Portal ();

    // updates
    friend class ConvexRegion;
    void UpdateWorldData (const Matrix3f& rkRot, const Vector3f& rkTrn,
        float fScale);

    // drawing
    void Draw (Renderer& rkRenderer);

    // portal vertices
    int m_iVertexQuantity;
    Vector3f* m_akModelVertex;
    Vector3f* m_akWorldVertex;

    // region to which portal leads
    ConvexRegion* m_pkAdjacentRegion;

    // portals can be open or closed
    bool m_bOpen;
};

WmlSmartPointer(Portal);
WmlRegisterStream(Portal);
#include "WmlPortal.inl"

}

#endif
