// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLCONVEXREGION_H
#define WMLCONVEXREGION_H

#include "WmlBspNode.h"

namespace Wml
{

class Portal;

class WML_ITEM ConvexRegion : public BspNode
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    // Construction and destruction.  ConvexRegion accepts responsibility
    // for deleting the input array.
    ConvexRegion (int iPortalQuantity, Portal** apkPortal,
        Spatial* pkRepresentation);

    virtual ~ConvexRegion ();

    // The left and right children of ConvexRegion must be null.  The
    // middle child is where the representation of the region is stored.
    // This can be an arbitrary subgraph, not just drawable geometry.
    SpatialPtr AttachRepresentation (Spatial* pkRepresentation);
    SpatialPtr DetachRepresentation ();
    SpatialPtr GetRepresentation ();

    // portal access
    int GetPortalQuantity () const;
    Portal* GetPortal (int i) const;

    virtual Object* GetObjectByName (const char* acName);
    virtual void GetAllObjectsByName (const char* acName,
        std::vector<Object*>& rkObjects);

protected:
    // streaming
    ConvexRegion ();

    // geometric updates
    virtual void UpdateWorldData (float fAppTime);

    // Drawing.  ConvexRegionManager starts the region graph traversal
    // with the region containing the eye point.  Portal continues the
    // traversal.
    friend class ConvexRegionManager;
    friend class Portal;
    virtual void Draw (Renderer& rkRenderer);

    // portals of the region (these are not set up to be shared)
    int m_iPortalQuantity;
    Portal** m_apkPortal;

    // for region graph traversal
    bool m_bVisited;
};

WmlSmartPointer(ConvexRegion);
WmlRegisterStream(ConvexRegion);
#include "WmlConvexRegion.inl"

}

#endif
