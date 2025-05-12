// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLBEZIERRECTANGLE2_H
#define WMLBEZIERRECTANGLE2_H

#include "WmlBezierRectangle.h"
#include "WmlColorRGB.h"
#include "WmlVector2.h"
#include "WmlVector3.h"

namespace Wml
{

class WML_ITEM BezierRectangle2 : public BezierRectangle
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    // Construction.  BezierRectangle2 accepts responsibility for deleting
    // the input array.
    BezierRectangle2 (int* aiIndex);

    // tessellation
    virtual void Tessellate (int iLevel, const Vector3f* akCtrlPoint,
        const ColorRGB* akCtrlColor, const Vector2f* akCtrlTexture,
        TriMesh* pkMesh, int& riVertexStart, int& riTriangleStart);

protected:
    BezierRectangle2 ();

    // precomputation
    class WML_ITEM BlockParameters
    {
    public:
        int m_i00, m_i01, m_i10, m_i11;

        // Vss(0,0), Vss(0,1), Vss(1,0) = Vss(0,0), Vss(1,1) = Vss(0,1)
        Vector3f m_akXss[2];
        ColorRGB m_akCss[2];
        Vector2f m_akTss[2];

        // Vtt(0,0), Vtt(1,0), Vtt(0,1) = Vtt(0,0), Vtt(1,1) = Vtt(1,0)
        Vector3f m_akXtt[2];
        ColorRGB m_akCtt[2];
        Vector2f m_akTtt[2];

        // Xsst(0,0), Xsst(0,1), Xsst(1,0) = Xsst(0,0), Xsst(1,1) = Xsst(0,1)
        Vector3f m_akXsst[2];

        // Xstt(0,0), Xstt(1,0), Xstt(0,1) = Xstt(0,0), Xstt(1,1) = Xstt(1,0)
        Vector3f m_akXstt[2];
    };

    void InitializePoints (const Vector3f* akCtrlPoint, Vector3f& rkXsstt,
        BlockParameters& rkBP);

    void InitializeNormals (const Vector3f* akCtrlPoint,
        BlockParameters& rkBP);

    void InitializeColors (const ColorRGB* akCtrlColor, ColorRGB& rkCsstt,
        BlockParameters& rkBP);

    void InitializeTextures (const Vector2f* akCtrlTexture, Vector2f& rkTsstt,
        BlockParameters& rkBP);

    // tessellation by recursive subdivision
    void Subdivide (int iLevel, float fDSqr, Vector3f* akX, Vector3f* akXs,
        Vector3f* akXt, ColorRGB* akColor, Vector2f* akTexture,
        Vector3f& rkXsstt, ColorRGB& rkCsstt, Vector2f& rkTsstt,
        BlockParameters& rkBP);
};

WmlSmartPointer(BezierRectangle2);
WmlRegisterStream(BezierRectangle2);

}

#endif
