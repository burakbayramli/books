// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLBSPTREE_H
#define WMLBSPTREE_H

// Creation of a BSP tree from a list of triangles.  The internal nodes of
// the tree are BspNode objects and store the coplanar triangles (if any)
// of the splitting planes.

#include "WmlBspNode.h"
#include "WmlColorRGB.h"
#include "WmlVector2.h"
#include "WmlVector3.h"
#include <list>

namespace Wml
{

class WML_ITEM BspTree : public BspNode
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    class WML_ITEM Triangle
    {
    public:
        Triangle ();

        Vector3f m_akVertex[3];
        Vector3f* m_apkNormal[3];
        ColorRGB* m_apkColor[3];
        Vector2f* m_apkTexture[3];
    };

    typedef std::list<Triangle*> TriangleList;

    // Construction.  The input list is consumed by the construction, so the
    // application should keep a copy of the list if it needs to be used
    // elsewhere.
    BspTree (TriangleList& rkList);

protected:
    BspTree ();

    void CreateTree (TriangleList& rkList);

    void SplitTriangle (Triangle* pkTri, TriangleList& rkPositive,
        TriangleList& rkNegative, TriangleList& rkCoincident);

    void ClipTriangle (int i0, int i1, int i2, Triangle* pkTri,
        float afDistance[3], TriangleList& rkPositive,
        TriangleList& rkNegative);

    void AddTriangle (TriangleList& rkList, const Vector3f& rkV0,
        const Vector3f& rkV1, const Vector3f& rkV2, bool bHasNormals,
        const Vector3f* pkN0, const Vector3f* pkN1, const Vector3f* pkN2,
        bool bHasColors, const ColorRGB* pkC0, const ColorRGB* pkC1,
        const ColorRGB* pkC2, bool bHasTextures, const Vector2f* pkT0,
        const Vector2f* pkT1, const Vector2f* pkT2);
};

WmlSmartPointer(BspTree);
WmlRegisterStream(BspTree);

}

#endif
