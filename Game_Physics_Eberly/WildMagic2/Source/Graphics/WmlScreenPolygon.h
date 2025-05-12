// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLSCREENPOLYGON_H
#define WMLSCREENPOLYGON_H

#include "WmlTriMesh.h"

namespace Wml
{

class WML_ITEM ScreenPolygon : public Object
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    // Construction and destruction.  ScreenPolygon accepts responsibility
    // for deleting the input arrays.  The polygon must be convex and vertices
    // stored in counterclockwise order.
    ScreenPolygon (int iVertexQuantity, Vector3f* akVertex,
        Vector3f* akNormal, ColorRGB* akColor, Vector2f* akTexture0,
        Vector2f* akTexture1 = NULL, Vector2f* akTexture2 = NULL,
        Vector2f* akTexture3 = NULL, Vector2f* akTextureBump = NULL);

    virtual ~ScreenPolygon ();

    // Triangle mesh that represents the screen polygon.  Use this object
    // to attach/detach render states.
    TriMeshPtr Mesh ();
    TriMeshPtr Mesh () const;

    // support for searching by name
    virtual Object* GetObjectByName (const char* acName);
    virtual void GetAllObjectsByName (const char* acName,
        std::vector<Object*>& rkObjects);

protected:
    ScreenPolygon ();

    friend class Renderer;
    void OnDraw (Renderer& rkRenderer);

    TriMeshPtr m_spkMesh;
};

WmlSmartPointer(ScreenPolygon);
WmlRegisterStream(ScreenPolygon);
#include "WmlScreenPolygon.inl"

}

#endif
