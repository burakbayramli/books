// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLBEZIERRECTANGLE3_H
#define WMLBEZIERRECTANGLE3_H

#include "WmlBezierRectangle.h"
#include "WmlColorRGB.h"
#include "WmlVector2.h"
#include "WmlVector3.h"

namespace Wml
{

class WML_ITEM BezierRectangle3 : public BezierRectangle
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    // Construction.  BezierRectangle3 accepts responsibility for deleting
    // the input array.
    BezierRectangle3 (int* aiIndex);

    // tessellation
    virtual void Tessellate (int iLevel, const Vector3f* akCtrlPoint,
        const ColorRGB* akCtrlColor, const Vector2f* akCtrlTexture,
        TriMesh* pkMesh, int& riVertexStart, int& riTriangleStart);

protected:
    BezierRectangle3 ();

    // precomputation
    class WML_ITEM BlockParameters
    {
    public:
        int m_i00, m_i01, m_i10, m_i11;

        // Xsss(0,0), Xsss(0,1), Xsss(1,0) = Xsss(0,0), Xsss(1,1) = Xsss(0,1)
        Vector3f m_akXsss[2];

        // Xttt(0,0), Xttt(1,0), Xttt(0,1) = Xttt(0,0), Xttt(1,1) = Xttt(1,0)
        Vector3f m_akXttt[2];

        // Xssstt(0,0), Xssstt(0,1), Xssstt(1,0) = Xssstt(0,0),
        //   Xssstt(1,1) = Xssstt(0,1)
        Vector3f m_akXssstt[2];

        // Xssttt(0,0), Xssttt(1,0), Xssttt(0,1) = Xssttt(0,0),
        //   Xssttt(1,1) = Xssttt(1,0)
        Vector3f m_akXssttt[2];

        Vector3f m_aakXss[2][2], m_aakXtt[2][2], m_aakXsstt[2][2],
            m_aakXsst[2][2], m_aakXstt[2][2];
        ColorRGB m_aakCss[2][2], m_aakCtt[2][2], m_aakCsstt[2][2];
        Vector2f m_aakTss[2][2], m_aakTtt[2][2], m_aakTsstt[2][2];
    };

    void InitializePoints (const Vector3f* akCtrlPoint,
        BlockParameters& rkBP);

    void InitializeNormals (const Vector3f* akCtrlPoint,
        BlockParameters& rkBP);

    void InitializeColors (const ColorRGB* akCtrlColor,
        BlockParameters& rkBP);

    void InitializeTextures (const Vector2f* akCtrlTexture,
        BlockParameters& rkBP);

    // tessellation by recursive subdivision
    void Subdivide (int iLevel, float fDSqr, Vector3f* akX, Vector3f* akXs,
        Vector3f* akXt, ColorRGB* akColor, Vector2f* akTexture,
        BlockParameters& rkBP);
};

WmlSmartPointer(BezierRectangle3);
WmlRegisterStream(BezierRectangle3);

}

#endif
