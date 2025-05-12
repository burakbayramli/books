// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLBSPNODE_H
#define WMLBSPNODE_H

#include "WmlNode.h"

namespace Wml
{

class WML_ITEM BspNode : public Node
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    // Construction.  The base class Node has *three* children and is not
    // allowed to grow.  The first and last children (indices 0 and 2) are
    // the left and right children of the binary tree.  The middle child slot
    // is where a derived class can attach additional geometry such as the
    // triangles that are coplanar with the splitting plane.
    BspNode ();

    // These methods should be used instead of the attach/detach methods in
    // the Node base class.  The left child corresponds to the positive
    // side of the separating plane.  The right child corresponds to the
    // negative side of the plane.
    SpatialPtr AttachLeftChild (Spatial* pkChild);
    SpatialPtr AttachRightChild (Spatial* pkChild);
    SpatialPtr DetachLeftChild ();
    SpatialPtr DetachRightChild ();
    SpatialPtr GetLeftChild ();
    SpatialPtr GetRightChild ();

    // plane access
    Plane3f& ModelPlane ();
    const Plane3f& ModelPlane () const;
    const Plane3f& GetWorldPlane () const;

    // determine BSP node whose represented region contains the point
    BspNode* GetContainingNode (const Vector3f& rkPoint);

    // An application can attach relevant data to each BSP node.  The
    // callback is executed in the Draw pass when it is determined that the
    // node is potentially visible.  The application is responsible for
    // memory management associated with the data.
    typedef void (*CallbackFunction)(void);
    CallbackFunction& Callback ();
    void*& Data ();

protected:
    // geometric updates
    virtual void UpdateWorldData (float fAppTime);

    // drawing
    virtual void Draw (Renderer& rkRenderer);

    Plane3f m_kModelPlane;
    Plane3f m_kWorldPlane;
    CallbackFunction m_oCallback;
    void* m_pvData;
};

WmlSmartPointer(BspNode);
WmlRegisterStream(BspNode);
#include "WmlBspNode.inl"

}

#endif
