// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLBEZIERCYLINDER3_H
#define WMLBEZIERCYLINDER3_H

#include "WmlBezierCylinder.h"
#include "WmlColorRGB.h"
#include "WmlVector2.h"
#include "WmlVector3.h"

namespace Wml
{

class WML_ITEM BezierCylinder3 : public BezierCylinder
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    // Construction.  BezierCylinder3 accepts responsibility for deleting
    // the input array.
    BezierCylinder3 (int* aiIndex);

    // tessellation
    virtual void Tessellate (int iLevel, const Vector3f* akCtrlPoint,
        const ColorRGB* akCtrlColor, const Vector2f* akCtrlTexture,
        TriMesh* pkMesh, int& riVertexStart, int& riTriangleStart);

protected:
    BezierCylinder3 ();

    // precomputation
    class WML_ITEM IntervalParameters
    {
    public:
        // s = curve direction, t = cylinder direction
        int m_i00, m_i01, m_i10, m_i11;
        Vector3f m_aakXss[2][2], m_akXsss[2];
        ColorRGB m_aakCss[2][2];
        Vector2f m_aakTss[2][2];
    };

    void InitializePoints (const Vector3f* akCtrlPoint,
        IntervalParameters& rkIP);

    void InitializeNormals (const Vector3f* akCtrlPoint,
        Vector3f akXsss[2]);

    void InitializeColors (const ColorRGB* akCtrlColor,
        IntervalParameters& rkIP);

    void InitializeTextures (const Vector2f* akCtrlTexture,
        IntervalParameters& rkIP);

    // tessellate cylinder curves by recursive subdivision
    void SubdivideCylinder (int iCLevel, Vector3f* akX, Vector3f* akXs,
        Vector3f* akXt, ColorRGB* akColor, Vector2f* akTexture, int i0,
        int i1, int iTwoPowL);

    // tessellate boundary curves by recursive subdivision
    void SubdivideBoundary (int iLevel, float fDSqr, Vector3f* akX,
        Vector3f* akXs, Vector3f* akXt, Vector3f akXsss[2],
        ColorRGB* akColor, Vector2f* akTexture, IntervalParameters& rkIP);
};

WmlSmartPointer(BezierCylinder3);
WmlRegisterStream(BezierCylinder3);

}

#endif
