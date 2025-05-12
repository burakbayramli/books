// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLBEZIERTRIANGLE2_H
#define WMLBEZIERTRIANGLE2_H

#include "WmlBezierTriangle.h"
#include "WmlColorRGB.h"
#include "WmlVector2.h"
#include "WmlVector3.h"

namespace Wml
{

class WML_ITEM BezierTriangle2 : public BezierTriangle
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    // Construction.  BezierTriangle2 accepts responsibility for deleting
    // the input array.
    BezierTriangle2 (int* aiIndex);

    // tessellation
    virtual void Tessellate (int iLevel, const Vector3f* akCtrlPoint,
        const ColorRGB* akCtrlColor, const Vector2f* akCtrlTexture,
        TriMesh* pkMesh, int& riVertexStart, int& riTriangleStart);

protected:
    BezierTriangle2 ();

    // precomputation
    class WML_ITEM BlockParameters
    {
    public:
        Vector3f m_kXuu, m_kXvv, m_kXhh;
        ColorRGB m_kCuu, m_kCvv, m_kChh;
        Vector2f m_kTuu, m_kTvv, m_kThh;
    };

    void InitializePoints (const Vector3f* akCtrlPoint,
        BlockParameters& rkBP);

    void InitializeColors (const ColorRGB* akCtrlColor,
        BlockParameters& rkBP);

    void InitializeTextures (const Vector2f* akCtrlTexture,
        BlockParameters& rkBP);

    // tessellation by recursive subdivision

    // subdivide triangle with right angle at Lower Left corner
    void SubdivideLL (int iLevel, float fDSqr, Vector3f* akX, Vector3f* akXu,
        Vector3f* akXv, ColorRGB* akColor, Vector2f* akTexture, int i0,
        int i1, int i2, BlockParameters& rkBP);

    // subdivide triangle with right angle at Upper Right corner
    void SubdivideUR (int iLevel, float fDSqr, Vector3f* akX, Vector3f* akXu,
        Vector3f* akXv, ColorRGB* akColor, Vector2f* akTexture, int i0,
        int i1, int i2, BlockParameters& rkBP);
};

WmlSmartPointer(BezierTriangle2);
WmlRegisterStream(BezierTriangle2);

}

#endif
