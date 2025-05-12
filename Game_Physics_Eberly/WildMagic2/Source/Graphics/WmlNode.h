// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLNODE_H
#define WMLNODE_H

#include "WmlSpatial.h"
#include <vector>

namespace Wml
{

class WML_ITEM Node : public Spatial
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    // construction and destruction
    Node (int iQuantity = 1, int iGrowBy = 1);
    virtual ~Node ();

    // children
    int GetQuantity () const;
    int GetUsed () const;
    int AttachChild (Spatial* pkChild);
    int DetachChild (Spatial* pkChild);
    SpatialPtr DetachChildAt (int i);
    SpatialPtr SetChild (int i, Spatial* pkChild);
    SpatialPtr GetChild (int i);

    // support for searching by name
    virtual Object* GetObjectByName (const char* acName);
    virtual void GetAllObjectsByName (const char* acName,
        std::vector<Object*>& rkObjects);

    // Picking support.  The origin and direction of the ray must be in world
    // coordinates.  The application is responsible for deleting the pick
    // records in the array.
    virtual void DoPick (const Vector3f& rkOrigin,
        const Vector3f& rkDirection, PickArray& rkResults);

protected:
    // allow internal calls to update functions and drawing
    friend class Spatial;
    friend class Renderer;

    // geometric updates
    virtual void UpdateWorldData (float fAppTime);
    virtual void UpdateWorldBound ();

    // render state update
    virtual void UpdateRenderState (RenderState::Stack* pkStack);

    // drawing
    virtual void Draw (Renderer& rkRenderer);


    // children
    std::vector<SpatialPtr> m_kChild;
    int m_iGrowBy, m_iUsed;
};

WmlSmartPointer(Node);
WmlRegisterStream(Node);
#include "WmlNode.inl"

}

#endif


