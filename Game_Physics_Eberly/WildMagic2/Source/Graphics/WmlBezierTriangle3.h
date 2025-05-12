// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLBEZIERTRIANGLE3_H
#define WMLBEZIERTRIANGLE3_H

#include "WmlBezierTriangle.h"
#include "WmlColorRGB.h"
#include "WmlVector2.h"
#include "WmlVector3.h"

namespace Wml
{

class WML_ITEM BezierTriangle3 : public BezierTriangle
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    // Construction.  BezierTriangle3 accepts responsibility for deleting
    // the input array.
    BezierTriangle3 (int* aiIndex);

    // tessellation
    virtual void Tessellate (int iLevel, const Vector3f* akCtrlPoint,
        const ColorRGB* akCtrlColor, const Vector2f* akCtrlTexture,
        TriMesh* pkMesh, int& riVertexStart, int& riTriangleStart);

protected:
    BezierTriangle3 ();

    // precomputation
    class WML_ITEM BlockParameters
    {
    public:
        int m_i0, m_i1, m_i2;
        Vector3f m_aakXuu[3], m_aakXvv[3], m_aakXhh[3], m_kXuuu, m_kXuuv,
            m_kXuvv, m_kXvvv, m_kXhhu, m_kXhhv;
        ColorRGB m_aakCuu[3], m_aakCvv[3], m_aakChh[3];
        Vector2f m_aakTuu[3], m_aakTvv[3], m_aakThh[3];
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

    // subdivide triangle with right angle at Lower Left corner
    void SubdivideLL (int iLevel, float fDSqr, Vector3f* akX, Vector3f* akXu,
        Vector3f* akXv, ColorRGB* akColor, Vector2f* akTexture,
        BlockParameters& rkBP);

    // subdivide triangle with right angle at Upper Right corner
    void SubdivideUR (int iLevel, float fDSqr, Vector3f* akX, Vector3f* akXu,
        Vector3f* akXv, ColorRGB* akColor, Vector2f* akTexture,
        BlockParameters& rkBP);
};

WmlSmartPointer(BezierTriangle3);
WmlRegisterStream(BezierTriangle3);

}

#endif
