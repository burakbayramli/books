// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlBezierTriangle3.h"
#include "WmlTriMesh.h"
using namespace Wml;

WmlImplementRTTI(BezierTriangle3,BezierTriangle);
WmlImplementStream(BezierTriangle3);

// macros for initialization in the subdivision
#define XA(i) akCtrlPoint[m_aiIndex[i]]
#define CA(i) akCtrlColor[m_aiIndex[i]]
#define TA(i) akCtrlTexture[m_aiIndex[i]]

//----------------------------------------------------------------------------
BezierTriangle3::BezierTriangle3 (int* aiIndex)
    :
    BezierTriangle(3,10,aiIndex)
{
}
//----------------------------------------------------------------------------
BezierTriangle3::BezierTriangle3 ()
{
}
//----------------------------------------------------------------------------
void BezierTriangle3::InitializePoints (const Vector3f* akCtrlPoint,
    BlockParameters& rkBP)
{
    // Xuu
    rkBP.m_aakXuu[0] = 6.0f*(XA(0) - 2.0f*XA(1) + XA(2));
    rkBP.m_aakXuu[1] = 6.0f*(XA(1) - 2.0f*XA(2) + XA(3));
    rkBP.m_aakXuu[2] = 6.0f*(XA(4) - 2.0f*XA(5) + XA(6));

    // Xvv
    rkBP.m_aakXvv[0] = 6.0f*(XA(0) - 2.0f*XA(4) + XA(7));
    rkBP.m_aakXvv[1] = 6.0f*(XA(1) - 2.0f*XA(5) + XA(8));
    rkBP.m_aakXvv[2] = 6.0f*(XA(4) - 2.0f*XA(7) + XA(9));

    // Xhh = Xuu - 2*Xuv + Xvv (d/dh = d/ds - d/dt)
    rkBP.m_aakXhh[0] = 6.0f*(XA(2) - 2.0f*XA(5) + XA(7));
    rkBP.m_aakXhh[1] = 6.0f*(XA(3) - 2.0f*XA(6) + XA(8));
    rkBP.m_aakXhh[2] = 6.0f*(XA(6) - 2.0f*XA(8) + XA(9));
}
//----------------------------------------------------------------------------
void BezierTriangle3::InitializeNormals (const Vector3f* akCtrlPoint,
    BlockParameters& rkBP)
{
    // Xuuu
    rkBP.m_kXuuu = 6.0f*(XA(3) - XA(0) + 2.0f*(XA(1) - XA(2)));

    // Xuuv
    rkBP.m_kXuuv = 6.0f*(XA(4) + XA(6) - XA(0) - XA(2) + 2.0f*(XA(1) -
        XA(5)));

    // Xuvv
    rkBP.m_kXuvv = 6.0f*(XA(1) + XA(8) - XA(0) - XA(7) + 2.0f*(XA(4) - 
        XA(5)));

    // Xvvv
    rkBP.m_kXvvv = 6.0f*(XA(9) - XA(0) + 2.0f*(XA(4) - XA(7)));

    // Xhhu
    rkBP.m_kXhhu = 6.0f*(XA(3) + XA(8) - XA(2) - XA(7) + 2.0f*(XA(5) -
        XA(6)));

    // Xhhv
    rkBP.m_kXhhv = 6.0f*(XA(6) + XA(9) - XA(2) - XA(7) + 2.0f*(XA(5) -
        XA(8)));
}
//----------------------------------------------------------------------------
void BezierTriangle3::InitializeColors (const ColorRGB* akCtrlColor,
    BlockParameters& rkBP)
{
    // Cuu
    rkBP.m_aakCuu[0] = 6.0f*(CA(0) - 2.0f*CA(1) + CA(2));
    rkBP.m_aakCuu[1] = 6.0f*(CA(1) - 2.0f*CA(2) + CA(3));
    rkBP.m_aakCuu[2] = 6.0f*(CA(4) - 2.0f*CA(5) + CA(6));

    // Cvv
    rkBP.m_aakCvv[0] = 6.0f*(CA(0) - 2.0f*CA(4) + CA(7));
    rkBP.m_aakCvv[1] = 6.0f*(CA(1) - 2.0f*CA(5) + CA(8));
    rkBP.m_aakCvv[2] = 6.0f*(CA(4) - 2.0f*CA(7) + CA(9));

    // Chh = Cuu - 2*Cuv + Cvv (d/dh = d/ds - d/dt)
    rkBP.m_aakChh[0] = 6.0f*(CA(2) - 2.0f*CA(5) + CA(7));
    rkBP.m_aakChh[1] = 6.0f*(CA(3) - 2.0f*CA(6) + CA(8));
    rkBP.m_aakChh[2] = 6.0f*(CA(6) - 2.0f*CA(8) + CA(9));
}
//----------------------------------------------------------------------------
void BezierTriangle3::InitializeTextures (const Vector2f* akCtrlTexture,
    BlockParameters& rkBP)
{
    // Tuu
    rkBP.m_aakTuu[0] = 6.0f*(TA(0) - 2.0f*TA(1) + TA(2));
    rkBP.m_aakTuu[1] = 6.0f*(TA(1) - 2.0f*TA(2) + TA(3));
    rkBP.m_aakTuu[2] = 6.0f*(TA(4) - 2.0f*TA(5) + TA(6));

    // Tvv
    rkBP.m_aakTvv[0] = 6.0f*(TA(0) - 2.0f*TA(4) + TA(7));
    rkBP.m_aakTvv[1] = 6.0f*(TA(1) - 2.0f*TA(5) + TA(8));
    rkBP.m_aakTvv[2] = 6.0f*(TA(4) - 2.0f*TA(7) + TA(9));

    // Thh = Tuu - 2*Tuv + Tvv (d/dh = d/ds - d/dt)
    rkBP.m_aakThh[0] = 6.0f*(TA(2) - 2.0f*TA(5) + TA(7));
    rkBP.m_aakThh[1] = 6.0f*(TA(3) - 2.0f*TA(6) + TA(8));
    rkBP.m_aakThh[2] = 6.0f*(TA(6) - 2.0f*TA(8) + TA(9));
}
//----------------------------------------------------------------------------
void BezierTriangle3::Tessellate (int iLevel, const Vector3f* akCtrlPoint,
    const ColorRGB* akCtrlColor, const Vector2f* akCtrlTexture,
    TriMesh* pkMesh, int& riVertexStart, int& riTriangleStart)
{
    GenerateConnectivity(iLevel,pkMesh,riTriangleStart);

    // number of vertices in the subdivision
    int iQuantity = GetVerticesPerPatch(iLevel);

    // indices of three corners of patch, I0 (w=1), I1 (u=1), I2 (v=1)
    int iTwoPowL = (1 << iLevel);
    BlockParameters kBP;
    kBP.m_i0 = 0;
    kBP.m_i1 = iTwoPowL;
    kBP.m_i2 = iTwoPowL*(iTwoPowL + 3) >> 1;

    // vertices for subdivision
    Vector3f* akX = pkMesh->Vertices() + riVertexStart;
    akX[kBP.m_i0] = XA(0);
    akX[kBP.m_i1] = XA(3);
    akX[kBP.m_i2] = XA(9);

    // derivatives for subdivision (for normal vectors)
    Vector3f* akXu;
    Vector3f* akXv;
    if ( pkMesh->Normals() )
    {
        akXu = new Vector3f[iQuantity];
        akXu[kBP.m_i0] = 3.0f*(XA(1) - XA(0));
        akXu[kBP.m_i1] = 3.0f*(XA(3) - XA(2));
        akXu[kBP.m_i2] = 3.0f*(XA(8) - XA(7));

        akXv = new Vector3f[iQuantity];
        akXv[kBP.m_i0] = 3.0f*(XA(4) - XA(0));
        akXv[kBP.m_i1] = 3.0f*(XA(6) - XA(2));
        akXv[kBP.m_i2] = 3.0f*(XA(9) - XA(7));
    }
    else
    {
        akXu = NULL;
        akXv = NULL;
    }

    // colors for subdivision
    ColorRGB* akColor;
    if ( pkMesh->Colors() )
    {
        akColor = pkMesh->Colors() + riVertexStart;
        akColor[kBP.m_i0] = CA(0);
        akColor[kBP.m_i1] = CA(3);
        akColor[kBP.m_i2] = CA(9);
    }
    else
    {
        akColor = NULL;
    }

    // textures for subdivision
    Vector2f* akTexture;
    if ( pkMesh->Textures() )
    {
        akTexture = pkMesh->Textures() + riVertexStart;
        akTexture[kBP.m_i0] = TA(0);
        akTexture[kBP.m_i1] = TA(3);
        akTexture[kBP.m_i2] = TA(9);
    }
    else
    {
        akTexture = NULL;
    }

    // recursive subdivision
    if ( iLevel > 0 )
    {
        InitializePoints(akCtrlPoint,kBP);

        if ( akXu )
            InitializeNormals(akCtrlPoint,kBP);

        if ( akCtrlColor )
            InitializeColors(akCtrlColor,kBP);

        if ( akCtrlTexture )
            InitializeTextures(akCtrlTexture,kBP);

        SubdivideLL(--iLevel,0.25f,akX,akXu,akXv,akColor,akTexture,kBP);
    }

    // calculate unit-length normals from derivative vectors
    if ( pkMesh->Normals() )
    {
        Vector3f* akNormal = pkMesh->Normals() + riVertexStart;
        for (int i = 0; i < iQuantity; i++)
            akNormal[i] = akXu[i].UnitCross(akXv[i]);
        delete[] akXu;
        delete[] akXv;
    }

    riVertexStart += iQuantity;
}
//----------------------------------------------------------------------------
void BezierTriangle3::SubdivideLL (int iLevel, float fDSqr, Vector3f* akX,
    Vector3f* akXu, Vector3f* akXv, ColorRGB* akColor, Vector2f* akTexture,
    BlockParameters& rkBP)
{
    /*
     i2
     +
     | \
     |   \
     +----+
     i0    i1
    */

    // subdivision indices
    int i01 = (rkBP.m_i0 + rkBP.m_i1) >> 1;
    int iD10 = rkBP.m_i1 - rkBP.m_i0;
    int i02 = (((rkBP.m_i0+rkBP.m_i2) << 2)+iD10*iD10) >> 3;
    int i12 = i02 + (iD10 >> 1);

    // vertices

    // bottom u-edge subdivision
    Vector3f kXuu01 = 0.5f*(rkBP.m_aakXuu[0] + rkBP.m_aakXuu[1]);
    Vector3f kXvv01 = 0.5f*(rkBP.m_aakXvv[0] + rkBP.m_aakXvv[1]);
    Vector3f kXhh01 = 0.5f*(rkBP.m_aakXhh[0] + rkBP.m_aakXhh[1]);
    akX[i01] = 0.5f*(akX[rkBP.m_i0] + akX[rkBP.m_i1] - fDSqr*kXuu01);

    // left v-edge subdivision
    Vector3f kXuu02 = 0.5f*(rkBP.m_aakXuu[0] + rkBP.m_aakXuu[2]);
    Vector3f kXvv02 = 0.5f*(rkBP.m_aakXvv[0] + rkBP.m_aakXvv[2]);
    Vector3f kXhh02 = 0.5f*(rkBP.m_aakXhh[0] + rkBP.m_aakXhh[2]);
    akX[i02] = 0.5f*(akX[rkBP.m_i0] + akX[rkBP.m_i2] - fDSqr*kXvv02);

    // hypotenuse edge subdivision
    Vector3f kXuu12 = 0.5f*(rkBP.m_aakXuu[1] + rkBP.m_aakXuu[2]);
    Vector3f kXvv12 = 0.5f*(rkBP.m_aakXvv[1] + rkBP.m_aakXvv[2]);
    Vector3f kXhh12 = 0.5f*(rkBP.m_aakXhh[1] + rkBP.m_aakXhh[2]);
    akX[i12] = 0.5f*(akX[rkBP.m_i1] + akX[rkBP.m_i2] - fDSqr*kXhh12);

    // derivatives (for normal vectors)
    if ( akXu )
    {
        // bottom u-edge subdivision
        akXu[i01] = 0.5f*(akXu[rkBP.m_i0] + akXu[rkBP.m_i1] -
            fDSqr*rkBP.m_kXuuu);
        akXv[i01] = 0.5f*(akXv[rkBP.m_i0] + akXv[rkBP.m_i1] -
            fDSqr*rkBP.m_kXuuv);

        // left v-edge subdivision
        akXu[i02] = 0.5f*(akXu[rkBP.m_i0] + akXu[rkBP.m_i2] -
            fDSqr*rkBP.m_kXuvv);
        akXv[i02] = 0.5f*(akXv[rkBP.m_i0] + akXv[rkBP.m_i2] -
            fDSqr*rkBP.m_kXvvv);

        // hypotenuse edge subdivision
        akXu[i12] = 0.5f*(akXu[rkBP.m_i1] + akXu[rkBP.m_i2] -
            fDSqr*rkBP.m_kXhhu);
        akXv[i12] = 0.5f*(akXv[rkBP.m_i1] + akXv[rkBP.m_i2] -
            fDSqr*rkBP.m_kXhhv);
    }

    // colors
    ColorRGB kCuu01, kCvv01, kChh01, kCuu02, kCvv02, kChh02, kCuu12, kCvv12,
        kChh12;
    if ( akColor )
    {
        // bottom u-edge subdivision
        kCuu01 = 0.5f*(rkBP.m_aakCuu[0] + rkBP.m_aakCuu[1]);
        kCvv01 = 0.5f*(rkBP.m_aakCvv[0] + rkBP.m_aakCvv[1]);
        kChh01 = 0.5f*(rkBP.m_aakChh[0] + rkBP.m_aakChh[1]);
        akColor[i01] = 0.5f*(akColor[rkBP.m_i0] + akColor[rkBP.m_i1] -
            fDSqr*kCuu01);

        // left v-edge subdivision
        kCuu02 = 0.5f*(rkBP.m_aakCuu[0] + rkBP.m_aakCuu[2]);
        kCvv02 = 0.5f*(rkBP.m_aakCvv[0] + rkBP.m_aakCvv[2]);
        kChh02 = 0.5f*(rkBP.m_aakChh[0] + rkBP.m_aakChh[2]);
        akColor[i02] = 0.5f*(akColor[rkBP.m_i0] + akColor[rkBP.m_i2] -
            fDSqr*kCvv02);

        // hypotenuse edge subdivision
        kCuu12 = 0.5f*(rkBP.m_aakCuu[1] + rkBP.m_aakCuu[2]);
        kCvv12 = 0.5f*(rkBP.m_aakCvv[1] + rkBP.m_aakCvv[2]);
        kChh12 = 0.5f*(rkBP.m_aakChh[1] + rkBP.m_aakChh[2]);
        akColor[i12] = 0.5f*(akColor[rkBP.m_i1] + akColor[rkBP.m_i2] -
            fDSqr*kChh12);
    }

    // textures
    Vector2f kTuu01, kTvv01, kThh01, kTuu02, kTvv02, kThh02, kTuu12, kTvv12,
        kThh12;
    if ( akTexture )
    {
        // bottom u-edge subdivision
        kTuu01 = 0.5f*(rkBP.m_aakTuu[0] + rkBP.m_aakTuu[1]);
        kTvv01 = 0.5f*(rkBP.m_aakTvv[0] + rkBP.m_aakTvv[1]);
        kThh01 = 0.5f*(rkBP.m_aakThh[0] + rkBP.m_aakThh[1]);
        akTexture[i01] = 0.5f*(akTexture[rkBP.m_i0]+akTexture[rkBP.m_i1]
            - fDSqr*kTuu01);

        // left v-edge subdivision
        kTuu02 = 0.5f*(rkBP.m_aakTuu[0] + rkBP.m_aakTuu[2]);
        kTvv02 = 0.5f*(rkBP.m_aakTvv[0] + rkBP.m_aakTvv[2]);
        kThh02 = 0.5f*(rkBP.m_aakThh[0] + rkBP.m_aakThh[2]);
        akTexture[i02] = 0.5f*(akTexture[rkBP.m_i0]+akTexture[rkBP.m_i2]
            - fDSqr*kTvv02);

        // hypotenuse edge subdivision
        kTuu12 = 0.5f*(rkBP.m_aakTuu[1] + rkBP.m_aakTuu[2]);
        kTvv12 = 0.5f*(rkBP.m_aakTvv[1] + rkBP.m_aakTvv[2]);
        kThh12 = 0.5f*(rkBP.m_aakThh[1] + rkBP.m_aakThh[2]);
        akTexture[i12] = 0.5f*(akTexture[rkBP.m_i1]+akTexture[rkBP.m_i2]
            - fDSqr*kThh12);
    }

    // recurse on four children
    if ( iLevel > 0 )
    {
        iLevel--;
        fDSqr *= 0.25f;

        BlockParameters kSubBP;

        // subtriangle <(s0,t0),(sm,t0),(s0,tm)>
        kSubBP.m_i0 = rkBP.m_i0;
        kSubBP.m_i1 = i01;
        kSubBP.m_i2 = i02;
        kSubBP.m_aakXuu[0] = rkBP.m_aakXuu[0];
        kSubBP.m_aakXuu[1] = kXuu01;
        kSubBP.m_aakXuu[2] = kXuu02;
        kSubBP.m_aakXvv[0] = rkBP.m_aakXvv[0];
        kSubBP.m_aakXvv[1] = kXvv01;
        kSubBP.m_aakXvv[2] = kXvv02;
        kSubBP.m_aakXhh[0] = rkBP.m_aakXhh[0];
        kSubBP.m_aakXhh[1] = kXhh01;
        kSubBP.m_aakXhh[2] = kXhh02;

        if ( akColor )
        {
            kSubBP.m_aakCuu[0] = rkBP.m_aakCuu[0];
            kSubBP.m_aakCuu[1] = kCuu01;
            kSubBP.m_aakCuu[2] = kCuu02;
            kSubBP.m_aakCvv[0] = rkBP.m_aakCvv[0];
            kSubBP.m_aakCvv[1] = kCvv01;
            kSubBP.m_aakCvv[2] = kCvv02;
            kSubBP.m_aakChh[0] = rkBP.m_aakChh[0];
            kSubBP.m_aakChh[1] = kChh01;
            kSubBP.m_aakChh[2] = kChh02;
        }

        if ( akTexture )
        {
            kSubBP.m_aakTuu[0] = rkBP.m_aakTuu[0];
            kSubBP.m_aakTuu[1] = kTuu01;
            kSubBP.m_aakTuu[2] = kTuu02;
            kSubBP.m_aakTvv[0] = rkBP.m_aakTvv[0];
            kSubBP.m_aakTvv[1] = kTvv01;
            kSubBP.m_aakTvv[2] = kTvv02;
            kSubBP.m_aakThh[0] = rkBP.m_aakThh[0];
            kSubBP.m_aakThh[1] = kThh01;
            kSubBP.m_aakThh[2] = kThh02;
        }

        SubdivideLL(iLevel,fDSqr,akX,akXu,akXv,akColor,akTexture,kSubBP);

        // subtriangle <(sm,t0),(s1,t0),(sm,tm)>
        kSubBP.m_i0 = i01;
        kSubBP.m_i1 = rkBP.m_i1;
        kSubBP.m_i2 = i12;
        kSubBP.m_aakXuu[0] = kXuu01;
        kSubBP.m_aakXuu[1] = rkBP.m_aakXuu[1];
        kSubBP.m_aakXuu[2] = kXuu12;
        kSubBP.m_aakXvv[0] = kXvv01;
        kSubBP.m_aakXvv[1] = rkBP.m_aakXvv[1];
        kSubBP.m_aakXvv[2] = kXvv12;
        kSubBP.m_aakXhh[0] = kXhh01;
        kSubBP.m_aakXhh[1] = rkBP.m_aakXhh[1];
        kSubBP.m_aakXhh[2] = kXhh12;

        if ( akColor )
        {
            kSubBP.m_aakCuu[0] = kCuu01;
            kSubBP.m_aakCuu[1] = rkBP.m_aakCuu[1];
            kSubBP.m_aakCuu[2] = kCuu12;
            kSubBP.m_aakCvv[0] = kCvv01;
            kSubBP.m_aakCvv[1] = rkBP.m_aakCvv[1];
            kSubBP.m_aakCvv[2] = kCvv12;
            kSubBP.m_aakChh[0] = kChh01;
            kSubBP.m_aakChh[1] = rkBP.m_aakChh[1];
            kSubBP.m_aakChh[2] = kChh12;
        }

        if ( akTexture )
        {
            kSubBP.m_aakTuu[0] = kTuu01;
            kSubBP.m_aakTuu[1] = rkBP.m_aakTuu[1];
            kSubBP.m_aakTuu[2] = kTuu12;
            kSubBP.m_aakTvv[0] = kTvv01;
            kSubBP.m_aakTvv[1] = rkBP.m_aakTvv[1];
            kSubBP.m_aakTvv[2] = kTvv12;
            kSubBP.m_aakThh[0] = kThh01;
            kSubBP.m_aakThh[1] = rkBP.m_aakThh[1];
            kSubBP.m_aakThh[2] = kThh12;
        }

        SubdivideLL(iLevel,fDSqr,akX,akXu,akXv,akColor,akTexture,kSubBP);

        // subtriangle <(s0,tm),(sm,tm),(s0,t1)>
        kSubBP.m_i0 = i02;
        kSubBP.m_i1 = i12;
        kSubBP.m_i2 = rkBP.m_i2;
        kSubBP.m_aakXuu[0] = kXuu02;
        kSubBP.m_aakXuu[1] = kXuu12;
        kSubBP.m_aakXuu[2] = rkBP.m_aakXuu[2];
        kSubBP.m_aakXvv[0] = kXvv02;
        kSubBP.m_aakXvv[1] = kXvv12;
        kSubBP.m_aakXvv[2] = rkBP.m_aakXvv[2];
        kSubBP.m_aakXhh[0] = kXhh02;
        kSubBP.m_aakXhh[1] = kXhh12;
        kSubBP.m_aakXhh[2] = rkBP.m_aakXhh[2];

        if ( akColor )
        {
            kSubBP.m_aakCuu[0] = kCuu02;
            kSubBP.m_aakCuu[1] = kCuu12;
            kSubBP.m_aakCuu[2] = rkBP.m_aakCuu[2];
            kSubBP.m_aakCvv[0] = kCvv02;
            kSubBP.m_aakCvv[1] = kCvv12;
            kSubBP.m_aakCvv[2] = rkBP.m_aakCvv[2];
            kSubBP.m_aakChh[0] = kChh02;
            kSubBP.m_aakChh[1] = kChh12;
            kSubBP.m_aakChh[2] = rkBP.m_aakChh[2];
        }

        if ( akTexture )
        {
            kSubBP.m_aakTuu[0] = kTuu02;
            kSubBP.m_aakTuu[1] = kTuu12;
            kSubBP.m_aakTuu[2] = rkBP.m_aakTuu[2];
            kSubBP.m_aakTvv[0] = kTvv02;
            kSubBP.m_aakTvv[1] = kTvv12;
            kSubBP.m_aakTvv[2] = rkBP.m_aakTvv[2];
            kSubBP.m_aakThh[0] = kThh02;
            kSubBP.m_aakThh[1] = kThh12;
            kSubBP.m_aakThh[2] = rkBP.m_aakThh[2];
        }

        SubdivideLL(iLevel,fDSqr,akX,akXu,akXv,akColor,akTexture,kSubBP);

        // subtriangle <(sm,t0),(sm,tm),(s0,tm)>
        kSubBP.m_i0 = i01;
        kSubBP.m_i1 = i12;
        kSubBP.m_i2 = i02;
        kSubBP.m_aakXuu[0] = kXuu01;
        kSubBP.m_aakXuu[1] = kXuu12;
        kSubBP.m_aakXuu[2] = kXuu02;
        kSubBP.m_aakXvv[0] = kXvv01;
        kSubBP.m_aakXvv[1] = kXvv12;
        kSubBP.m_aakXvv[2] = kXvv02;
        kSubBP.m_aakXhh[0] = kXhh01;
        kSubBP.m_aakXhh[1] = kXhh12;
        kSubBP.m_aakXhh[2] = kXhh02;

        if ( akColor )
        {
            kSubBP.m_aakCuu[0] = kCuu01;
            kSubBP.m_aakCuu[1] = kCuu12;
            kSubBP.m_aakCuu[2] = kCuu02;
            kSubBP.m_aakCvv[0] = kCvv01;
            kSubBP.m_aakCvv[1] = kCvv12;
            kSubBP.m_aakCvv[2] = kCvv02;
            kSubBP.m_aakChh[0] = kChh01;
            kSubBP.m_aakChh[1] = kChh12;
            kSubBP.m_aakChh[2] = kChh02;
        }

        if ( akTexture )
        {
            kSubBP.m_aakTuu[0] = kTuu01;
            kSubBP.m_aakTuu[1] = kTuu12;
            kSubBP.m_aakTuu[2] = kTuu02;
            kSubBP.m_aakTvv[0] = kTvv01;
            kSubBP.m_aakTvv[1] = kTvv12;
            kSubBP.m_aakTvv[2] = kTvv02;
            kSubBP.m_aakThh[0] = kThh01;
            kSubBP.m_aakThh[1] = kThh12;
            kSubBP.m_aakThh[2] = kThh02;
        }

        SubdivideUR(iLevel,fDSqr,akX,akXu,akXv,akColor,akTexture,kSubBP);
    }
}
//----------------------------------------------------------------------------
void BezierTriangle3::SubdivideUR (int iLevel, float fDSqr, Vector3f* akX,
    Vector3f* akXu, Vector3f* akXv, ColorRGB* akColor, Vector2f* akTexture,
    BlockParameters& rkBP)
{
    /*
     i2   i1
     +----+
       \  |
         \|
          +
          i0
    */

    // subdivision indices
    int i12 = (rkBP.m_i1 + rkBP.m_i2) >> 1;
    int iD12 = rkBP.m_i1 - rkBP.m_i2;
    int i01 = (((rkBP.m_i0+rkBP.m_i1) << 2)+iD12*iD12) >> 3;
    int i02 = i01 - (iD12 >> 1);

    // vertices

    // top u-edge subdivision
    Vector3f kXuu12 = 0.5f*(rkBP.m_aakXuu[1] + rkBP.m_aakXuu[2]);
    Vector3f kXvv12 = 0.5f*(rkBP.m_aakXvv[1] + rkBP.m_aakXvv[2]);
    Vector3f kXhh12 = 0.5f*(rkBP.m_aakXhh[1] + rkBP.m_aakXhh[2]);
    akX[i12] = 0.5f*(akX[rkBP.m_i1] + akX[rkBP.m_i2] - fDSqr*kXuu12);

    // right v-edge subdivision
    Vector3f kXuu01 = 0.5f*(rkBP.m_aakXuu[0] + rkBP.m_aakXuu[1]);
    Vector3f kXvv01 = 0.5f*(rkBP.m_aakXvv[0] + rkBP.m_aakXvv[1]);
    Vector3f kXhh01 = 0.5f*(rkBP.m_aakXhh[0] + rkBP.m_aakXhh[1]);
    akX[i01] = 0.5f*(akX[rkBP.m_i0] + akX[rkBP.m_i1] - fDSqr*kXvv01);

    // hypotenuse edge subdivision
    Vector3f kXuu02 = 0.5f*(rkBP.m_aakXuu[0] + rkBP.m_aakXuu[2]);
    Vector3f kXvv02 = 0.5f*(rkBP.m_aakXvv[0] + rkBP.m_aakXvv[2]);
    Vector3f kXhh02 = 0.5f*(rkBP.m_aakXhh[0] + rkBP.m_aakXhh[2]);
    akX[i02] = 0.5f*(akX[rkBP.m_i0] + akX[rkBP.m_i2] - fDSqr*kXhh02);

    // derivatives (for normal vectors)
    if ( akXu )
    {
        // top u-edge subdivision
        akXu[i12] = 0.5f*(akXu[rkBP.m_i1] + akXu[rkBP.m_i2] -
            fDSqr*rkBP.m_kXuuu);
        akXv[i12] = 0.5f*(akXv[rkBP.m_i1] + akXv[rkBP.m_i2] -
            fDSqr*rkBP.m_kXuuv);

        // right v-edge subdivision
        akXu[i01] = 0.5f*(akXu[rkBP.m_i0] + akXu[rkBP.m_i1] - 
            fDSqr*rkBP.m_kXuvv);
        akXv[i01] = 0.5f*(akXv[rkBP.m_i0] + akXv[rkBP.m_i1] -
            fDSqr*rkBP.m_kXvvv);

        // hypotenuse edge subdivision
        akXu[i02] = 0.5f*(akXu[rkBP.m_i0] + akXu[rkBP.m_i2] -
            fDSqr*rkBP.m_kXhhu);
        akXv[i02] = 0.5f*(akXv[rkBP.m_i0] + akXv[rkBP.m_i2] -
            fDSqr*rkBP.m_kXhhv);
    }

    // colors
    ColorRGB kCuu12, kCvv12, kChh12, kCuu01, kCvv01, kChh01, kCuu02, kCvv02,
        kChh02;
    if ( akColor )
    {
        // top u-edge subdivision
        kCuu12 = 0.5f*(rkBP.m_aakCuu[1] + rkBP.m_aakCuu[2]);
        kCvv12 = 0.5f*(rkBP.m_aakCvv[1] + rkBP.m_aakCvv[2]);
        kChh12 = 0.5f*(rkBP.m_aakChh[1] + rkBP.m_aakChh[2]);
        akColor[i12] = 0.5f*(akColor[rkBP.m_i1] + akColor[rkBP.m_i2] -
            fDSqr*kCuu12);

        // right v-edge subdivision
        kCuu01 = 0.5f*(rkBP.m_aakCuu[0] + rkBP.m_aakCuu[1]);
        kCvv01 = 0.5f*(rkBP.m_aakCvv[0] + rkBP.m_aakCvv[1]);
        kChh01 = 0.5f*(rkBP.m_aakChh[0] + rkBP.m_aakChh[1]);
        akColor[i01] = 0.5f*(akColor[rkBP.m_i0] + akColor[rkBP.m_i1] -
            fDSqr*kCvv01);

        // hypotenuse edge subdivision
        kCuu02 = 0.5f*(rkBP.m_aakCuu[0] + rkBP.m_aakCuu[2]);
        kCvv02 = 0.5f*(rkBP.m_aakCvv[0] + rkBP.m_aakCvv[2]);
        kChh02 = 0.5f*(rkBP.m_aakChh[0] + rkBP.m_aakChh[2]);
        akColor[i02] = 0.5f*(akColor[rkBP.m_i0] + akColor[rkBP.m_i2] -
            fDSqr*kChh02);
    }

    // textures
    Vector2f kTuu12, kTvv12, kThh12, kTuu01, kTvv01, kThh01, kTuu02, kTvv02,
        kThh02;
    if ( akTexture )
    {
        // top u-edge subdivision
        kTuu12 = 0.5f*(rkBP.m_aakTuu[1] + rkBP.m_aakTuu[2]);
        kTvv12 = 0.5f*(rkBP.m_aakTvv[1] + rkBP.m_aakTvv[2]);
        kThh12 = 0.5f*(rkBP.m_aakThh[1] + rkBP.m_aakThh[2]);
        akTexture[i12] = 0.5f*(akTexture[rkBP.m_i1]+akTexture[rkBP.m_i2]
            - fDSqr*kTuu12);

        // right v-edge subdivision
        kTuu01 = 0.5f*(rkBP.m_aakTuu[0] + rkBP.m_aakTuu[1]);
        kTvv01 = 0.5f*(rkBP.m_aakTvv[0] + rkBP.m_aakTvv[1]);
        kThh01 = 0.5f*(rkBP.m_aakThh[0] + rkBP.m_aakThh[1]);
        akTexture[i01] = 0.5f*(akTexture[rkBP.m_i0]+akTexture[rkBP.m_i1]
            - fDSqr*kTvv01);

        // hypotenuse edge subdivision
        kTuu02 = 0.5f*(rkBP.m_aakTuu[0] + rkBP.m_aakTuu[2]);
        kTvv02 = 0.5f*(rkBP.m_aakTvv[0] + rkBP.m_aakTvv[2]);
        kThh02 = 0.5f*(rkBP.m_aakThh[0] + rkBP.m_aakThh[2]);
        akTexture[i02] = 0.5f*(akTexture[rkBP.m_i0]+akTexture[rkBP.m_i2]
            - fDSqr*kThh02);
    }

    // recurse on four children
    if ( iLevel > 0 )
    {
        iLevel--;
        fDSqr *= 0.25f;

        BlockParameters kSubBP;

        // subtriangle <(sm,tm),(sm,t1),(s0,t1)>
        kSubBP.m_i0 = i02;
        kSubBP.m_i1 = i12;
        kSubBP.m_i2 = rkBP.m_i2;
        kSubBP.m_aakXuu[0] = kXuu02;
        kSubBP.m_aakXuu[1] = kXuu12;
        kSubBP.m_aakXuu[2] = rkBP.m_aakXuu[2];
        kSubBP.m_aakXvv[0] = kXvv02;
        kSubBP.m_aakXvv[1] = kXvv12;
        kSubBP.m_aakXvv[2] = rkBP.m_aakXvv[2];
        kSubBP.m_aakXhh[0] = kXhh02;
        kSubBP.m_aakXhh[1] = kXhh12;
        kSubBP.m_aakXhh[2] = rkBP.m_aakXhh[2];

        if ( akColor )
        {
            kSubBP.m_aakCuu[0] = kCuu02;
            kSubBP.m_aakCuu[1] = kCuu12;
            kSubBP.m_aakCuu[2] = rkBP.m_aakCuu[2];
            kSubBP.m_aakCvv[0] = kCvv02;
            kSubBP.m_aakCvv[1] = kCvv12;
            kSubBP.m_aakCvv[2] = rkBP.m_aakCvv[2];
            kSubBP.m_aakChh[0] = kChh02;
            kSubBP.m_aakChh[1] = kChh12;
            kSubBP.m_aakChh[2] = rkBP.m_aakChh[2];
        }

        if ( akTexture )
        {
            kSubBP.m_aakTuu[0] = kTuu02;
            kSubBP.m_aakTuu[1] = kTuu12;
            kSubBP.m_aakTuu[2] = rkBP.m_aakTuu[2];
            kSubBP.m_aakTvv[0] = kTvv02;
            kSubBP.m_aakTvv[1] = kTvv12;
            kSubBP.m_aakTvv[2] = rkBP.m_aakTvv[2];
            kSubBP.m_aakThh[0] = kThh02;
            kSubBP.m_aakThh[1] = kThh12;
            kSubBP.m_aakThh[2] = rkBP.m_aakThh[2];
        }

        SubdivideUR(iLevel,fDSqr,akX,akXu,akXv,akColor,akTexture,kSubBP);

        // subtriangle <(s1,tm),(s1,t1),(sm,t1)>
        kSubBP.m_i0 = i01;
        kSubBP.m_i1 = rkBP.m_i1;
        kSubBP.m_i2 = i12;
        kSubBP.m_aakXuu[0] = kXuu01;
        kSubBP.m_aakXuu[1] = rkBP.m_aakXuu[1];
        kSubBP.m_aakXuu[2] = kXuu12;
        kSubBP.m_aakXvv[0] = kXvv01;
        kSubBP.m_aakXvv[1] = rkBP.m_aakXvv[1];
        kSubBP.m_aakXvv[2] = kXvv12;
        kSubBP.m_aakXhh[0] = kXhh01;
        kSubBP.m_aakXhh[1] = rkBP.m_aakXhh[1];
        kSubBP.m_aakXhh[2] = kXhh12;

        if ( akColor )
        {
            kSubBP.m_aakCuu[0] = kCuu01;
            kSubBP.m_aakCuu[1] = rkBP.m_aakCuu[1];
            kSubBP.m_aakCuu[2] = kCuu12;
            kSubBP.m_aakCvv[0] = kCvv01;
            kSubBP.m_aakCvv[1] = rkBP.m_aakCvv[1];
            kSubBP.m_aakCvv[2] = kCvv12;
            kSubBP.m_aakChh[0] = kChh01;
            kSubBP.m_aakChh[1] = rkBP.m_aakChh[1];
            kSubBP.m_aakChh[2] = kChh12;
        }

        if ( akTexture )
        {
            kSubBP.m_aakTuu[0] = kTuu01;
            kSubBP.m_aakTuu[1] = rkBP.m_aakTuu[1];
            kSubBP.m_aakTuu[2] = kTuu12;
            kSubBP.m_aakTvv[0] = kTvv01;
            kSubBP.m_aakTvv[1] = rkBP.m_aakTvv[1];
            kSubBP.m_aakTvv[2] = kTvv12;
            kSubBP.m_aakThh[0] = kThh01;
            kSubBP.m_aakThh[1] = rkBP.m_aakThh[1];
            kSubBP.m_aakThh[2] = kThh12;
        }

        SubdivideUR(iLevel,fDSqr,akX,akXu,akXv,akColor,akTexture,kSubBP);

        // subtriangle <(s1,t0),(s1,tm),(sm,tm)>
        kSubBP.m_i0 = rkBP.m_i0;
        kSubBP.m_i1 = i01;
        kSubBP.m_i2 = i02;
        kSubBP.m_aakXuu[0] = rkBP.m_aakXuu[0];
        kSubBP.m_aakXuu[1] = kXuu01;
        kSubBP.m_aakXuu[2] = kXuu02;
        kSubBP.m_aakXvv[0] = rkBP.m_aakXvv[0];
        kSubBP.m_aakXvv[1] = kXvv01;
        kSubBP.m_aakXvv[2] = kXvv02;
        kSubBP.m_aakXhh[0] = rkBP.m_aakXhh[0];
        kSubBP.m_aakXhh[1] = kXhh01;
        kSubBP.m_aakXhh[2] = kXhh02;

        if ( akColor )
        {
            kSubBP.m_aakCuu[0] = rkBP.m_aakCuu[0];
            kSubBP.m_aakCuu[1] = kCuu01;
            kSubBP.m_aakCuu[2] = kCuu02;
            kSubBP.m_aakCvv[0] = rkBP.m_aakCvv[0];
            kSubBP.m_aakCvv[1] = kCvv01;
            kSubBP.m_aakCvv[2] = kCvv02;
            kSubBP.m_aakChh[0] = rkBP.m_aakChh[0];
            kSubBP.m_aakChh[1] = kChh01;
            kSubBP.m_aakChh[2] = kChh02;
        }

        if ( akTexture )
        {
            kSubBP.m_aakTuu[0] = rkBP.m_aakTuu[0];
            kSubBP.m_aakTuu[1] = kTuu01;
            kSubBP.m_aakTuu[2] = kTuu02;
            kSubBP.m_aakTvv[0] = rkBP.m_aakTvv[0];
            kSubBP.m_aakTvv[1] = kTvv01;
            kSubBP.m_aakTvv[2] = kTvv02;
            kSubBP.m_aakThh[0] = rkBP.m_aakThh[0];
            kSubBP.m_aakThh[1] = kThh01;
            kSubBP.m_aakThh[2] = kThh02;
        }

        SubdivideUR(iLevel,fDSqr,akX,akXu,akXv,akColor,akTexture,kSubBP);

        // subtriangle <(sm,tm),(s1,tm),(sm,t1)>
        kSubBP.m_i0 = i02;
        kSubBP.m_i1 = i01;
        kSubBP.m_i2 = i12;
        kSubBP.m_aakXuu[0] = kXuu02;
        kSubBP.m_aakXuu[1] = kXuu01;
        kSubBP.m_aakXuu[2] = kXuu12;
        kSubBP.m_aakXvv[0] = kXvv02;
        kSubBP.m_aakXvv[1] = kXvv01;
        kSubBP.m_aakXvv[2] = kXvv12;
        kSubBP.m_aakXhh[0] = kXhh02;
        kSubBP.m_aakXhh[1] = kXhh01;
        kSubBP.m_aakXhh[2] = kXhh12;

        if ( akColor )
        {
            kSubBP.m_aakCuu[0] = kCuu02;
            kSubBP.m_aakCuu[1] = kCuu01;
            kSubBP.m_aakCuu[2] = kCuu12;
            kSubBP.m_aakCvv[0] = kCvv02;
            kSubBP.m_aakCvv[1] = kCvv01;
            kSubBP.m_aakCvv[2] = kCvv12;
            kSubBP.m_aakChh[0] = kChh02;
            kSubBP.m_aakChh[1] = kChh01;
            kSubBP.m_aakChh[2] = kChh12;
        }

        if ( akTexture )
        {
            kSubBP.m_aakTuu[0] = kTuu02;
            kSubBP.m_aakTuu[1] = kTuu01;
            kSubBP.m_aakTuu[2] = kTuu12;
            kSubBP.m_aakTvv[0] = kTvv02;
            kSubBP.m_aakTvv[1] = kTvv01;
            kSubBP.m_aakTvv[2] = kTvv12;
            kSubBP.m_aakThh[0] = kThh02;
            kSubBP.m_aakThh[1] = kThh01;
            kSubBP.m_aakThh[2] = kThh12;
        }

        SubdivideLL(iLevel,fDSqr,akX,akXu,akXv,akColor,akTexture,kSubBP);
    }
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* BezierTriangle3::Factory (Stream& rkStream)
{
    BezierTriangle3* pkObject = new BezierTriangle3;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void BezierTriangle3::Load (Stream& rkStream, Stream::Link* pkLink)
{
    BezierTriangle::Load(rkStream,pkLink);
}
//----------------------------------------------------------------------------
void BezierTriangle3::Link (Stream& rkStream, Stream::Link* pkLink)
{
    BezierTriangle::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool BezierTriangle3::Register (Stream& rkStream)
{
    return BezierTriangle::Register(rkStream);
}
//----------------------------------------------------------------------------
void BezierTriangle3::Save (Stream& rkStream)
{
    BezierTriangle::Save(rkStream);
}
//----------------------------------------------------------------------------
StringTree* BezierTriangle3::SaveStrings ()
{
    StringTree* pkTree = new StringTree(1,0,1,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));

    // children
    pkTree->SetChild(0,BezierTriangle::SaveStrings());

    return pkTree;
}
//----------------------------------------------------------------------------
int BezierTriangle3::GetMemoryUsed () const
{
    int iBaseSize = sizeof(BezierTriangle3) - sizeof(BezierTriangle);
    int iTotalSize = iBaseSize + BezierTriangle::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int BezierTriangle3::GetDiskUsed () const
{
    return BezierTriangle::GetDiskUsed();
}
//----------------------------------------------------------------------------
