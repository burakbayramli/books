// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLCONVEXREGIONMANAGER_H
#define WMLCONVEXREGIONMANAGER_H

#include "WmlBspNode.h"

namespace Wml
{

class Camera;
class ConvexRegion;

class WML_ITEM ConvexRegionManager : public BspNode
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    // Construction.  The BSP tree should be built so that the leaf nodes are
    // where the ConvexRegion objects are located.
    ConvexRegionManager (bool bUseEyePlusNear = false);

    // The middle child of ConvexRegionManager is where the representation
    // of the outside of the set of regions is stored.  This can be an
    // arbitrary subgraph, not just drawable geometry.
    SpatialPtr AttachOutside (Spatial* pkOutside);
    SpatialPtr DetachOutside ();
    SpatialPtr GetOutside ();

    // Determine region that contains the point.  If the point is outside
    // the set of regions, the return values is null.
    ConvexRegion* GetContainingRegion (const Vector3f& rkPoint);

    // GetContainingRegion is called in Draw to determine in which
    // room the eye point is.  When transitions are allowed between inside
    // and outside, there is the problem when the eye point is inside
    // (outside) and the near face of the view frustum is outside (inside).
    // In this case the wrong contents are drawn.  To avoid this, set
    // the eye_plus_near Boolean value to 'true', in which case the point
    // E+n*D is used instead of E (E = eye point, n = near distance, D =
    // camera direction).
    bool& UseEyePlusNear ();

protected:
    // drawing
    virtual void Draw (Renderer& rkRenderer);

    bool m_bUseEyePlusNear;
};

WmlSmartPointer(ConvexRegionManager);
WmlRegisterStream(ConvexRegionManager);
#include "WmlConvexRegionManager.inl"

}

#endif
