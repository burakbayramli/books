// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlBezierRectangle3.h"
#include "WmlTriMesh.h"
using namespace Wml;

WmlImplementRTTI(BezierRectangle3,BezierRectangle);
WmlImplementStream(BezierRectangle3);

// macros for initialization in the subdivision
#define XA(i) akCtrlPoint[m_aiIndex[i]]
#define CA(i) akCtrlColor[m_aiIndex[i]]
#define TA(i) akCtrlTexture[m_aiIndex[i]]

//----------------------------------------------------------------------------
BezierRectangle3::BezierRectangle3 (int* aiIndex)
    :
    BezierRectangle(3,16,aiIndex)
{
}
//----------------------------------------------------------------------------
BezierRectangle3::BezierRectangle3 ()
{
}
//----------------------------------------------------------------------------
void BezierRectangle3::InitializePoints (const Vector3f* akCtrlPoint,
    BlockParameters& rkBP)
{
    // Xss[s][t]
    rkBP.m_aakXss[0][0] = 6.0f*(XA( 0) - 2.0f*XA( 1) + XA( 2));
    rkBP.m_aakXss[0][1] = 6.0f*(XA(12) - 2.0f*XA(13) + XA(14));
    rkBP.m_aakXss[1][0] = 6.0f*(XA( 1) - 2.0f*XA( 2) + XA( 3));
    rkBP.m_aakXss[1][1] = 6.0f*(XA(13) - 2.0f*XA(14) + XA(15));

    // Xtt[s][t]
    rkBP.m_aakXtt[0][0] = 6.0f*(XA( 0) - 2.0f*XA( 4) + XA( 8));
    rkBP.m_aakXtt[0][1] = 6.0f*(XA( 4) - 2.0f*XA( 8) + XA(12));
    rkBP.m_aakXtt[1][0] = 6.0f*(XA( 3) - 2.0f*XA( 7) + XA(11));
    rkBP.m_aakXtt[1][1] = 6.0f*(XA( 7) - 2.0f*XA(11) + XA(15));

    // Xsstt[s][t]
    rkBP.m_aakXsstt[0][0] = 36.0f*(XA(0) + XA(2) + XA(8) + XA(10) -
        2.0f*(XA(1) + XA(4) + XA(6) + XA(9)) + 4.0f*XA(5));
    rkBP.m_aakXsstt[0][1] = 36.0f*(XA(4) + XA(6) + XA(12) + XA(14) -
        2.0f*(XA(5) + XA(8) + XA(10) + XA(13)) + 4.0f*XA(9));
    rkBP.m_aakXsstt[1][0] = 36.0f*(XA(1) + XA(3) + XA(9) + XA(11) -
        2.0f*(XA(2) + XA(5) + XA(7) + XA(10)) + 4.0f*XA(6));
    rkBP.m_aakXsstt[1][1] = 36.0f*(XA(5) + XA(7) + XA(13) + XA(15) -
        2.0f*(XA(6) + XA(9) + XA(11) + XA(14)) + 4.0f*XA(10));
}
//----------------------------------------------------------------------------
void BezierRectangle3::InitializeNormals (const Vector3f* akCtrlPoint,
    BlockParameters& rkBP)
{
    // Xsst[s][t]
    rkBP.m_aakXsst[0][0] = 18.0f*(XA(4) + XA(6) - XA(0) - XA(2) +
        2.0f*(XA(1) - XA(5)));
    rkBP.m_aakXsst[0][1] = 18.0f*(XA(5) + XA(7) - XA(1) - XA(3) +
        2.0f*(XA(2) - XA(6)));
    rkBP.m_aakXsst[1][0] = 18.0f*(XA(12) + XA(14) - XA(8) - XA(10) +
        2.0f*(XA(9) - XA(13)));
    rkBP.m_aakXsst[1][1] = 18.0f*(XA(13) + XA(15) - XA(9) - XA(11) +
        2.0f*(XA(10) - XA(14)));

    // Xstt[s][t]
    rkBP.m_aakXstt[0][0] = 18.0f*(XA(1) + XA(9) - XA(0) - XA(8) +
        2.0f*(XA(4) - XA(5)));
    rkBP.m_aakXstt[0][1] = 18.0f*(XA(5) + XA(13) - XA(4) - XA(12) +
        2.0f*(XA(8) - XA(9)));
    rkBP.m_aakXstt[1][0] = 18.0f*(XA(3) + XA(11) - XA(2) - XA(10) +
        2.0f*(XA(6) - XA(7)));
    rkBP.m_aakXstt[1][1] = 18.0f*(XA(7) + XA(15) - XA(6) - XA(14) +
        2.0f*(XA(10) - XA(11)));

    // Xsss
    rkBP.m_akXsss[0] = 6.0f*(XA(3) + 3.0f*(XA(1) - XA(2)) - XA(0));
    rkBP.m_akXsss[1] = 6.0f*(XA(15) + 3.0f*(XA(13) - XA(14)) - XA(12));

    // Xttt
    rkBP.m_akXttt[0] = 6.0f*(XA(12) + 3.0f*(XA(4) - XA(8)) - XA(0));
    rkBP.m_akXttt[1] = 6.0f*(XA(15) + 3.0f*(XA(7) - XA(11)) - XA(3));

    // Xssstt
    rkBP.m_akXssstt[0] = 36.0f*(XA(3) + XA(11) - XA(0) - XA(8) +
        2.0f*(XA(4) - XA(7)) + 3.0f*(XA(1) + XA(9) - XA(2) - XA(10)) +
        6.0f*(XA(6) - XA(5)));
    rkBP.m_akXssstt[1] = 36.0f*(XA(7) + XA(15) - XA(4) - XA(12) +
        2.0f*(XA(8) - XA(11)) + 3.0f*(XA(5) + XA(13) - XA(6) - XA(14)) +
        6.0f*(XA(10) - XA(9)));

    // Xtttss
    rkBP.m_akXssttt[0] = 36.0f*(XA(12) + XA(14) - XA(0) - XA(2) +
        2.0f*(XA(1) - XA(13)) + 3.0f*(XA(4) + XA(6) - XA(8) - XA(10)) +
        6.0f*(XA(9) - XA(5)));
    rkBP.m_akXssttt[1] = 36.0f*(XA(13) + XA(15) - XA(1) - XA(3) +
        2.0f*(XA(2) - XA(14)) + 3.0f*(XA(5) + XA(7) - XA(9) - XA(11)) +
        6.0f*(XA(10) - XA(6)));
}
//----------------------------------------------------------------------------
void BezierRectangle3::InitializeColors (const ColorRGB* akCtrlColor,
    BlockParameters& rkBP)
{
    // Css[s][t]
    rkBP.m_aakCss[0][0] = 6.0f*(CA( 0) - 2.0f*CA( 1) + CA( 2));
    rkBP.m_aakCss[0][1] = 6.0f*(CA(12) - 2.0f*CA(13) + CA(14));
    rkBP.m_aakCss[1][0] = 6.0f*(CA( 1) - 2.0f*CA( 2) + CA( 3));
    rkBP.m_aakCss[1][1] = 6.0f*(CA(13) - 2.0f*CA(14) + CA(15));

    // Ctt[s][t]
    rkBP.m_aakCtt[0][0] = 6.0f*(CA( 0) - 2.0f*CA( 4) + CA( 8));
    rkBP.m_aakCtt[0][1] = 6.0f*(CA( 4) - 2.0f*CA( 8) + CA(12));
    rkBP.m_aakCtt[1][0] = 6.0f*(CA( 3) - 2.0f*CA( 7) + CA(11));
    rkBP.m_aakCtt[1][1] = 6.0f*(CA( 7) - 2.0f*CA(11) + CA(15));

    // Csstt[s][t]
    rkBP.m_aakCsstt[0][0] = 36.0f*(CA(0) + CA(2) + CA(8) + CA(10) -
        2.0f*(CA(1) + CA(4) + CA(6) + CA(9)) + 4.0f*CA(5));
    rkBP.m_aakCsstt[0][1] = 36.0f*(CA(4) + CA(6) + CA(12) + CA(14) -
        2.0f*(CA(5) + CA(8) + CA(10) + CA(13)) + 4.0f*CA(9));
    rkBP.m_aakCsstt[1][0] = 36.0f*(CA(1) + CA(3) + CA(9) + CA(11) -
        2.0f*(CA(2) + CA(5) + CA(7) + CA(10)) + 4.0f*CA(6));
    rkBP.m_aakCsstt[1][1] = 36.0f*(CA(5) + CA(7) + CA(13) + CA(15) -
        2.0f*(CA(6) + CA(9) + CA(11) + CA(14)) + 4.0f*CA(10));
}
//----------------------------------------------------------------------------
void BezierRectangle3::InitializeTextures (const Vector2f* akCtrlTexture,
    BlockParameters& rkBP)
{
    // Tss[s][t]
    rkBP.m_aakTss[0][0] = 6.0f*(TA( 0) - 2.0f*TA( 1) + TA( 2));
    rkBP.m_aakTss[0][1] = 6.0f*(TA(12) - 2.0f*TA(13) + TA(14));
    rkBP.m_aakTss[1][0] = 6.0f*(TA( 1) - 2.0f*TA( 2) + TA( 3));
    rkBP.m_aakTss[1][1] = 6.0f*(TA(13) - 2.0f*TA(14) + TA(15));

    // Ttt[s][t]
    rkBP.m_aakTtt[0][0] = 6.0f*(TA( 0) - 2.0f*TA( 4) + TA( 8));
    rkBP.m_aakTtt[0][1] = 6.0f*(TA( 4) - 2.0f*TA( 8) + TA(12));
    rkBP.m_aakTtt[1][0] = 6.0f*(TA( 3) - 2.0f*TA( 7) + TA(11));
    rkBP.m_aakTtt[1][1] = 6.0f*(TA( 7) - 2.0f*TA(11) + TA(15));

    // Tsstt[s][t]
    rkBP.m_aakTsstt[0][0] = 36.0f*(TA(0) + TA(2) + TA(8) + TA(10) -
        2.0f*(TA(1) + TA(4) + TA(6) + TA(9)) + 4.0f*TA(5));
    rkBP.m_aakTsstt[0][1] = 36.0f*(TA(4) + TA(6) + TA(12) + TA(14) -
        2.0f*(TA(5) + TA(8) + TA(10) + TA(13)) + 4.0f*TA(9));
    rkBP.m_aakTsstt[1][0] = 36.0f*(TA(1) + TA(3) + TA(9) + TA(11) -
        2.0f*(TA(2) + TA(5) + TA(7) + TA(10)) + 4.0f*TA(6));
    rkBP.m_aakTsstt[1][1] = 36.0f*(TA(5) + TA(7) + TA(13) + TA(15) -
        2.0f*(TA(6) + TA(9) + TA(11) + TA(14)) + 4.0f*TA(10));
}
//----------------------------------------------------------------------------
void BezierRectangle3::Tessellate (int iLevel, const Vector3f* akCtrlPoint,
    const ColorRGB* akCtrlColor, const Vector2f* akCtrlTexture,
    TriMesh* pkMesh, int& riVertexStart, int& riTriangleStart)
{
    GenerateConnectivity(iLevel,pkMesh,riTriangleStart);

    // number of vertices in the subdivision
    int iQuantity = GetVerticesPerPatch(iLevel);

    // indices of four corners of patch, I[s][t]
    int iTwoPowL = (1 << iLevel);
    BlockParameters kBP;
    kBP.m_i00 = 0;
    kBP.m_i01 = iTwoPowL*(iTwoPowL + 1);
    kBP.m_i10 = iTwoPowL;
    kBP.m_i11 = kBP.m_i01 + iTwoPowL;

    // vertices for subdivision
    Vector3f* akX = pkMesh->Vertices() + riVertexStart;
    akX[kBP.m_i00] = XA( 0);
    akX[kBP.m_i01] = XA(12);
    akX[kBP.m_i10] = XA( 3);
    akX[kBP.m_i11] = XA(15);

    // derivatives for subdivision (for normal vectors)
    Vector3f* akXs;
    Vector3f* akXt;
    if ( pkMesh->Normals() )
    {
        akXs = new Vector3f[iQuantity];
        akXs[kBP.m_i00] = 3.0f*(XA( 1) - XA( 0));
        akXs[kBP.m_i01] = 3.0f*(XA(13) - XA(12));
        akXs[kBP.m_i10] = 3.0f*(XA( 3) - XA( 2));
        akXs[kBP.m_i11] = 3.0f*(XA(15) - XA(14));

        akXt = new Vector3f[iQuantity];
        akXt[kBP.m_i00] = 3.0f*(XA( 4) - XA( 0));
        akXt[kBP.m_i01] = 3.0f*(XA(12) - XA( 8));
        akXt[kBP.m_i10] = 3.0f*(XA( 7) - XA( 3));
        akXt[kBP.m_i11] = 3.0f*(XA(15) - XA(11));
    }
    else
    {
        akXs = NULL;
        akXt = NULL;
    }

    // colors for subdivision
    ColorRGB* akColor;
    if ( pkMesh->Colors() )
    {
        akColor = pkMesh->Colors() + riVertexStart;
        akColor[kBP.m_i00] = CA( 0);
        akColor[kBP.m_i01] = CA(12);
        akColor[kBP.m_i10] = CA( 3);
        akColor[kBP.m_i11] = CA(15);
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
        akTexture[kBP.m_i00] = TA( 0);
        akTexture[kBP.m_i01] = TA(12);
        akTexture[kBP.m_i10] = TA( 3);
        akTexture[kBP.m_i11] = TA(15);
    }
    else
    {
        akTexture = NULL;
    }

    // recursive subdivision
    if ( iLevel > 0 )
    {
        InitializePoints(akCtrlPoint,kBP);

        if ( akXs )
            InitializeNormals(akCtrlPoint,kBP);

        if ( akCtrlColor )
            InitializeColors(akCtrlColor,kBP);

        if ( akCtrlTexture )
            InitializeTextures(akCtrlTexture,kBP);

        Subdivide(--iLevel,0.25f,akX,akXs,akXt,akColor,akTexture,kBP);
    }

    // calculate unit-length normals from derivative vectors
    if ( pkMesh->Normals() )
    {
        Vector3f* akNormal = pkMesh->Normals() + riVertexStart;
        for (int i = 0; i < iQuantity; i++)
            akNormal[i] = akXs[i].UnitCross(akXt[i]);
        delete[] akXs;
        delete[] akXt;
    }

    riVertexStart += iQuantity;
}
//----------------------------------------------------------------------------
void BezierRectangle3::Subdivide (int iLevel, float fDSqr, Vector3f* akX,
    Vector3f* akXs, Vector3f* akXt, ColorRGB* akColor, Vector2f* akTexture,
    BlockParameters& rkBP)
{
    // subdivision indices
    int iM0 = (rkBP.m_i00 + rkBP.m_i10) >> 1;
    int iM1 = (rkBP.m_i01 + rkBP.m_i11) >> 1;
    int i0M = (rkBP.m_i00 + rkBP.m_i01) >> 1;
    int i1M = (rkBP.m_i10 + rkBP.m_i11) >> 1;
    int iMM = (i0M + i1M) >> 1;

    // vertices

    // top and bottom s-edge subdivision
    Vector3f kXssM0 = 0.5f*(rkBP.m_aakXss[0][0] + rkBP.m_aakXss[1][0]);
    Vector3f kXssM1 = 0.5f*(rkBP.m_aakXss[0][1] + rkBP.m_aakXss[1][1]);
    Vector3f kXssttM0 = 0.5f*(rkBP.m_aakXsstt[0][0] + rkBP.m_aakXsstt[1][0]);
    Vector3f kXssttM1 = 0.5f*(rkBP.m_aakXsstt[0][1] + rkBP.m_aakXsstt[1][1]);
    Vector3f kXttM0 = 0.5f*(rkBP.m_aakXtt[0][0] + rkBP.m_aakXtt[1][0] -
        fDSqr*kXssttM0);
    Vector3f kXttM1 = 0.5f*(rkBP.m_aakXtt[0][1] + rkBP.m_aakXtt[1][1] -
        fDSqr*kXssttM1);
    akX[iM0] = 0.5f*(akX[rkBP.m_i00] + akX[rkBP.m_i10] - fDSqr*kXssM0);
    akX[iM1] = 0.5f*(akX[rkBP.m_i01] + akX[rkBP.m_i11] - fDSqr*kXssM1);

    // left and right t-edge subdivision
    Vector3f kXtt0M = 0.5f*(rkBP.m_aakXtt[0][0] + rkBP.m_aakXtt[0][1]);
    Vector3f kXtt1M = 0.5f*(rkBP.m_aakXtt[1][0] + rkBP.m_aakXtt[1][1]);
    Vector3f kXsstt0M = 0.5f*(rkBP.m_aakXsstt[0][0] + rkBP.m_aakXsstt[0][1]);
    Vector3f kXsstt1M = 0.5f*(rkBP.m_aakXsstt[1][0] + rkBP.m_aakXsstt[1][1]);
    Vector3f kXss0M = 0.5f*(rkBP.m_aakXss[0][0] + rkBP.m_aakXss[0][1] -
        fDSqr*kXsstt0M);
    Vector3f kXss1M = 0.5f*(rkBP.m_aakXss[1][0] + rkBP.m_aakXss[1][1] -
        fDSqr*kXsstt1M);
    akX[i0M] = 0.5f*(akX[rkBP.m_i00] + akX[rkBP.m_i01] - fDSqr*kXtt0M);
    akX[i1M] = 0.5f*(akX[rkBP.m_i10] + akX[rkBP.m_i11] - fDSqr*kXtt1M);

    // center subdivision
    Vector3f kXssMM = 0.5f*(kXss0M + kXss1M);
    Vector3f kXttMM = 0.5f*(kXttM0 + kXttM1);
    Vector3f kXssttMM = 0.5f*(kXsstt0M + kXsstt1M);
    akX[iMM] = 0.5f*(akX[i0M] + akX[i1M] - fDSqr*kXssMM);

    // derivatives (for normal vectors)
    Vector3f kXsss0M, kXsttM0, kXsttM1, kXstt0M, kXstt1M, kXssstt0M, kXsttMM;
    Vector3f kXsstM0, kXsstM1, kXsst0M, kXsst1M, kXtttM0, kXsstttM0, kXsstMM;
    if ( akXs )
    {
        // top and bottom s-edge subdivision
        kXsstM0 = 0.5f*(rkBP.m_aakXsst[0][0] + rkBP.m_aakXsst[1][0]);
        kXsstM1 = 0.5f*(rkBP.m_aakXsst[0][1] + rkBP.m_aakXsst[1][1]);
        akXs[iM0] = 0.5f*(akXs[rkBP.m_i00] + akXs[rkBP.m_i10] -
            fDSqr*rkBP.m_akXsss[0]);
        akXt[iM0] = 0.5f*(akXt[rkBP.m_i00] + akXt[rkBP.m_i10] -
            fDSqr*kXsstM0);
        akXs[iM1] = 0.5f*(akXs[rkBP.m_i01] + akXs[rkBP.m_i11] -
            fDSqr*rkBP.m_akXsss[1]);
        akXt[iM1] = 0.5f*(akXt[rkBP.m_i01] + akXt[rkBP.m_i11] -
            fDSqr*kXsstM1);

        kXsttM0 = 0.5f*(rkBP.m_aakXstt[0][0] + rkBP.m_aakXstt[1][0] -
            fDSqr*rkBP.m_akXssstt[0]);
        kXsstttM0 = 0.5f*(rkBP.m_akXssttt[0] + rkBP.m_akXssttt[1]);
        kXtttM0 = 0.5f*(rkBP.m_akXttt[0] + rkBP.m_akXttt[1] -
            fDSqr*kXsstttM0);
        kXsttM1 = 0.5f*(rkBP.m_aakXstt[0][1] + rkBP.m_aakXstt[1][1] -
            fDSqr*rkBP.m_akXssstt[1]);

        // left and right t-edge subdivision
        kXstt0M = 0.5f*(rkBP.m_aakXstt[0][0] + rkBP.m_aakXstt[0][1]);
        kXstt1M = 0.5f*(rkBP.m_aakXstt[1][0] + rkBP.m_aakXstt[1][1]);
        akXs[i0M] = 0.5f*(akXs[rkBP.m_i00] + akXs[rkBP.m_i01] -
            fDSqr*kXstt0M);
        akXt[i0M] = 0.5f*(akXt[rkBP.m_i00] + akXt[rkBP.m_i01] -
            fDSqr*rkBP.m_akXttt[0]);
        akXs[i1M] = 0.5f*(akXs[rkBP.m_i10] + akXs[rkBP.m_i11] -
            fDSqr*kXstt1M);
        akXt[i1M] = 0.5f*(akXt[rkBP.m_i10] + akXt[rkBP.m_i11] -
            fDSqr*rkBP.m_akXttt[1]);

        kXsst0M = 0.5f*(rkBP.m_aakXsst[0][0] + rkBP.m_aakXsst[0][1] -
            fDSqr*rkBP.m_akXssttt[0]);
        kXssstt0M = 0.5f*(rkBP.m_akXssstt[0] + rkBP.m_akXssstt[1]);
        kXsss0M = 0.5f*(rkBP.m_akXsss[0] + rkBP.m_akXsss[1] -
            fDSqr*kXssstt0M);
        kXsst1M = 0.5f*(rkBP.m_aakXsst[1][0] + rkBP.m_aakXsst[1][1] -
            fDSqr*rkBP.m_akXssttt[1]);

        // center subdivision
        kXsstMM = 0.5f*(kXsst0M + kXsst1M);
        kXsttMM = 0.5f*(kXsttM0 + kXsttM1);
        akXs[iMM] = 0.5f*(akXs[i0M] + akXs[i1M] - fDSqr*kXsss0M);
        akXt[iMM] = 0.5f*(akXt[iM0] + akXt[iM1] - fDSqr*kXtttM0);
    }

    // colors
    ColorRGB kCssM0, kCssM1, kCss0M, kCss1M, kCttM0, kCttM1, kCtt0M, kCtt1M,
        kCssttM0, kCssttM1, kCsstt0M, kCsstt1M, kCssMM, kCttMM, kCssttMM;
    if ( akColor )
    {
        // top and bottom s-edge subdivision
        kCssM0 = 0.5f*(rkBP.m_aakCss[0][0] + rkBP.m_aakCss[1][0]);
        kCssM1 = 0.5f*(rkBP.m_aakCss[0][1] + rkBP.m_aakCss[1][1]);
        kCssttM0 = 0.5f*(rkBP.m_aakCsstt[0][0] + rkBP.m_aakCsstt[1][0]);
        kCssttM1 = 0.5f*(rkBP.m_aakCsstt[0][1] + rkBP.m_aakCsstt[1][1]);
        kCttM0 = 0.5f*(rkBP.m_aakCtt[0][0] + rkBP.m_aakCtt[1][0] -
            fDSqr*kCssttM0);
        kCttM1 = 0.5f*(rkBP.m_aakCtt[0][1] + rkBP.m_aakCtt[1][1] -
            fDSqr*kCssttM1);
        akColor[iM0] = 0.5f*(akColor[rkBP.m_i00] + akColor[rkBP.m_i10] -
            fDSqr*kCssM0);
        akColor[iM1] = 0.5f*(akColor[rkBP.m_i01] + akColor[rkBP.m_i11] -
            fDSqr*kCssM1);

        // left and right t-edge subdivision
        kCtt0M = 0.5f*(rkBP.m_aakCtt[0][0] + rkBP.m_aakCtt[0][1]);
        kCtt1M = 0.5f*(rkBP.m_aakCtt[1][0] + rkBP.m_aakCtt[1][1]);
        kCsstt0M = 0.5f*(rkBP.m_aakCsstt[0][0] + rkBP.m_aakCsstt[0][1]);
        kCsstt1M = 0.5f*(rkBP.m_aakCsstt[1][0] + rkBP.m_aakCsstt[1][1]);
        kCss0M = 0.5f*(rkBP.m_aakCss[0][0] + rkBP.m_aakCss[0][1] -
            fDSqr*kCsstt0M);
        kCss1M = 0.5f*(rkBP.m_aakCss[1][0] + rkBP.m_aakCss[1][1] -
            fDSqr*kCsstt1M);
        akColor[i0M] = 0.5f*(akColor[rkBP.m_i00] + akColor[rkBP.m_i01] -
            fDSqr*kCtt0M);
        akColor[i1M] = 0.5f*(akColor[rkBP.m_i10] + akColor[rkBP.m_i11] -
            fDSqr*kCtt1M);

        // center subdivision
        kCssMM = 0.5f*(kCss0M + kCss1M);
        kCttMM = 0.5f*(kCttM0 + kCttM1);
        kCssttMM = 0.5f*(kCsstt0M + kCsstt1M);
        akColor[iMM] = 0.5f*(akColor[i0M] + akColor[i1M] - fDSqr*kCssMM);
    }

    // textures
    Vector2f kTssM0, kTssM1, kTss0M, kTss1M, kTttM0, kTttM1, kTtt0M, kTtt1M,
        kTssttM0, kTssttM1, kTsstt0M, kTsstt1M, kTssMM, kTttMM, kTssttMM;
    if ( akTexture )
    {
        // top and bottom s-edge subdivision
        kTssM0 = 0.5f*(rkBP.m_aakTss[0][0] + rkBP.m_aakTss[1][0]);
        kTssM1 = 0.5f*(rkBP.m_aakTss[0][1] + rkBP.m_aakTss[1][1]);
        kTssttM0 = 0.5f*(rkBP.m_aakTsstt[0][0] + rkBP.m_aakTsstt[1][0]);
        kTssttM1 = 0.5f*(rkBP.m_aakTsstt[0][1] + rkBP.m_aakTsstt[1][1]);
        kTttM0 = 0.5f*(rkBP.m_aakTtt[0][0] + rkBP.m_aakTtt[1][0] -
            fDSqr*kTssttM0);
        kTttM1 = 0.5f*(rkBP.m_aakTtt[0][1] + rkBP.m_aakTtt[1][1] -
            fDSqr*kTssttM1);
        akTexture[iM0] = 0.5f*(akTexture[rkBP.m_i00] +
            akTexture[rkBP.m_i10] - fDSqr*kTssM0);
        akTexture[iM1] = 0.5f*(akTexture[rkBP.m_i01] +
            akTexture[rkBP.m_i11] - fDSqr*kTssM1);

        // left and right t-edge subdivision
        kTtt0M = 0.5f*(rkBP.m_aakTtt[0][0] + rkBP.m_aakTtt[0][1]);
        kTtt1M = 0.5f*(rkBP.m_aakTtt[1][0] + rkBP.m_aakTtt[1][1]);
        kTsstt0M = 0.5f*(rkBP.m_aakTsstt[0][0] + rkBP.m_aakTsstt[0][1]);
        kTsstt1M = 0.5f*(rkBP.m_aakTsstt[1][0] + rkBP.m_aakTsstt[1][1]);
        kTss0M = 0.5f*(rkBP.m_aakTss[0][0] + rkBP.m_aakTss[0][1] -
            fDSqr*kTsstt0M);
        kTss1M = 0.5f*(rkBP.m_aakTss[1][0] + rkBP.m_aakTss[1][1] -
            fDSqr*kTsstt1M);
        akTexture[i0M] = 0.5f*(akTexture[rkBP.m_i00] +
            akTexture[rkBP.m_i01] - fDSqr*kTtt0M);
        akTexture[i1M] = 0.5f*(akTexture[rkBP.m_i10] +
            akTexture[rkBP.m_i11] - fDSqr*kTtt1M);

        // center subdivision
        kTssMM = 0.5f*(kTss0M + kTss1M);
        kTttMM = 0.5f*(kTttM0 + kTttM1);
        kTssttMM = 0.5f*(kTsstt0M + kTsstt1M);
        akTexture[iMM] = 0.5f*(akTexture[i0M] + akTexture[i1M] -
            fDSqr*kTssMM);
    }

    // recurse on four children
    if ( iLevel > 0 )
    {
        iLevel--;
        fDSqr *= 0.25f;

        BlockParameters kSubBP;

        // subblock [s0,sM]x[t0,tM]
        kSubBP.m_i00 = rkBP.m_i00;
        kSubBP.m_i01 = i0M;
        kSubBP.m_i10 = iM0;
        kSubBP.m_i11 = iMM;

        kSubBP.m_aakXss[0][0] = rkBP.m_aakXss[0][0];
        kSubBP.m_aakXss[0][1] = kXss0M;
        kSubBP.m_aakXss[1][0] = kXssM0;
        kSubBP.m_aakXss[1][1] = kXssMM;
        kSubBP.m_aakXtt[0][0] = rkBP.m_aakXtt[0][0];
        kSubBP.m_aakXtt[0][1] = kXtt0M;
        kSubBP.m_aakXtt[1][0] = kXttM0;
        kSubBP.m_aakXtt[1][1] = kXttMM;
        kSubBP.m_aakXsstt[0][0] = rkBP.m_aakXsstt[0][0];
        kSubBP.m_aakXsstt[0][1] = kXsstt0M;
        kSubBP.m_aakXsstt[1][0] = kXssttM0;
        kSubBP.m_aakXsstt[1][1] = kXssttMM;

        if ( akXs )
        {
            kSubBP.m_akXsss[0] = rkBP.m_akXsss[0];
            kSubBP.m_akXsss[1] = kXsss0M;
            kSubBP.m_aakXsst[0][0] = rkBP.m_aakXsst[0][0];
            kSubBP.m_aakXsst[0][1] = kXsst0M;
            kSubBP.m_aakXsst[1][0] = kXsstM0;
            kSubBP.m_aakXsst[1][1] = kXsstMM;
            kSubBP.m_aakXstt[0][0] = rkBP.m_aakXstt[0][0];
            kSubBP.m_aakXstt[0][1] = kXstt0M;
            kSubBP.m_aakXstt[1][0] = kXsttM0;
            kSubBP.m_aakXstt[1][1] = kXsttMM;
            kSubBP.m_akXttt[0] = rkBP.m_akXttt[0];
            kSubBP.m_akXttt[1] = kXtttM0;
            kSubBP.m_akXssstt[0] = rkBP.m_akXssstt[0];
            kSubBP.m_akXssstt[1] = kXssstt0M;
            kSubBP.m_akXssttt[0] = rkBP.m_akXssttt[0];
            kSubBP.m_akXssttt[1] = kXsstttM0;
        }

        if ( akColor )
        {
            kSubBP.m_aakCss[0][0] = rkBP.m_aakCss[0][0];
            kSubBP.m_aakCss[0][1] = kCss0M;
            kSubBP.m_aakCss[1][0] = kCssM0;
            kSubBP.m_aakCss[1][1] = kCssMM;
            kSubBP.m_aakCtt[0][0] = rkBP.m_aakCtt[0][0];
            kSubBP.m_aakCtt[0][1] = kCtt0M;
            kSubBP.m_aakCtt[1][0] = kCttM0;
            kSubBP.m_aakCtt[1][1] = kCttMM;
            kSubBP.m_aakCsstt[0][0] = rkBP.m_aakCsstt[0][0];
            kSubBP.m_aakCsstt[0][1] = kCsstt0M;
            kSubBP.m_aakCsstt[1][0] = kCssttM0;
            kSubBP.m_aakCsstt[1][1] = kCssttMM;
        }

        if ( akTexture )
        {
            kSubBP.m_aakTss[0][0] = rkBP.m_aakTss[0][0];
            kSubBP.m_aakTss[0][1] = kTss0M;
            kSubBP.m_aakTss[1][0] = kTssM0;
            kSubBP.m_aakTss[1][1] = kTssMM;
            kSubBP.m_aakTtt[0][0] = rkBP.m_aakTtt[0][0];
            kSubBP.m_aakTtt[0][1] = kTtt0M;
            kSubBP.m_aakTtt[1][0] = kTttM0;
            kSubBP.m_aakTtt[1][1] = kTttMM;
            kSubBP.m_aakTsstt[0][0] = rkBP.m_aakTsstt[0][0];
            kSubBP.m_aakTsstt[0][1] = kTsstt0M;
            kSubBP.m_aakTsstt[1][0] = kTssttM0;
            kSubBP.m_aakTsstt[1][1] = kTssttMM;
        }

        Subdivide(iLevel,fDSqr,akX,akXs,akXt,akColor,akTexture,kSubBP);

        // subblock [s0,sM]x[tM,t1]
        kSubBP.m_i00 = i0M;
        kSubBP.m_i01 = rkBP.m_i01;
        kSubBP.m_i10 = iMM;
        kSubBP.m_i11 = iM1;

        kSubBP.m_aakXss[0][0] = kXss0M;
        kSubBP.m_aakXss[0][1] = rkBP.m_aakXss[0][1];
        kSubBP.m_aakXss[1][0] = kXssMM;
        kSubBP.m_aakXss[1][1] = kXssM1;
        kSubBP.m_aakXtt[0][0] = kXtt0M;
        kSubBP.m_aakXtt[0][1] = rkBP.m_aakXtt[0][1];
        kSubBP.m_aakXtt[1][0] = kXttMM;
        kSubBP.m_aakXtt[1][1] = kXttM1;
        kSubBP.m_aakXsstt[0][0] = kXsstt0M;
        kSubBP.m_aakXsstt[0][1] = rkBP.m_aakXsstt[0][1];
        kSubBP.m_aakXsstt[1][0] = kXssttMM;
        kSubBP.m_aakXsstt[1][1] = kXssttM1;

        if ( akXs )
        {
            kSubBP.m_akXsss[0] = kXsss0M;
            kSubBP.m_akXsss[1] = rkBP.m_akXsss[1];
            kSubBP.m_aakXsst[0][0] = kXsst0M;
            kSubBP.m_aakXsst[0][1] = rkBP.m_aakXsst[0][1];
            kSubBP.m_aakXsst[1][0] = kXsstMM;
            kSubBP.m_aakXsst[1][1] = kXsstM1;
            kSubBP.m_aakXstt[0][0] = kXstt0M;
            kSubBP.m_aakXstt[0][1] = rkBP.m_aakXstt[0][1];
            kSubBP.m_aakXstt[1][0] = kXsttMM;
            kSubBP.m_aakXstt[1][1] = kXsttM1;
            kSubBP.m_akXttt[0] = rkBP.m_akXttt[0];
            kSubBP.m_akXttt[1] = kXtttM0;
            kSubBP.m_akXssstt[0] = kXssstt0M;
            kSubBP.m_akXssstt[1] = rkBP.m_akXssstt[1];
            kSubBP.m_akXssttt[0] = rkBP.m_akXssttt[0];
            kSubBP.m_akXssttt[1] = kXsstttM0;
        }

        if ( akColor )
        {
            kSubBP.m_aakCss[0][0] = kCss0M;
            kSubBP.m_aakCss[0][1] = rkBP.m_aakCss[0][1];
            kSubBP.m_aakCss[1][0] = kCssMM;
            kSubBP.m_aakCss[1][1] = kCssM1;
            kSubBP.m_aakCtt[0][0] = kCtt0M;
            kSubBP.m_aakCtt[0][1] = rkBP.m_aakCtt[0][1];
            kSubBP.m_aakCtt[1][0] = kCttMM;
            kSubBP.m_aakCtt[1][1] = kCttM1;
            kSubBP.m_aakCsstt[0][0] = kCsstt0M;
            kSubBP.m_aakCsstt[0][1] = rkBP.m_aakCsstt[0][1];
            kSubBP.m_aakCsstt[1][0] = kCssttMM;
            kSubBP.m_aakCsstt[1][1] = kCssttM1;
        }

        if ( akTexture )
        {
            kSubBP.m_aakTss[0][0] = kTss0M;
            kSubBP.m_aakTss[0][1] = rkBP.m_aakTss[0][1];
            kSubBP.m_aakTss[1][0] = kTssMM;
            kSubBP.m_aakTss[1][1] = kTssM1;
            kSubBP.m_aakTtt[0][0] = kTtt0M;
            kSubBP.m_aakTtt[0][1] = rkBP.m_aakTtt[0][1];
            kSubBP.m_aakTtt[1][0] = kTttMM;
            kSubBP.m_aakTtt[1][1] = kTttM1;
            kSubBP.m_aakTsstt[0][0] = kTsstt0M;
            kSubBP.m_aakTsstt[0][1] = rkBP.m_aakTsstt[0][1];
            kSubBP.m_aakTsstt[1][0] = kTssttMM;
            kSubBP.m_aakTsstt[1][1] = kTssttM1;
        }

        Subdivide(iLevel,fDSqr,akX,akXs,akXt,akColor,akTexture,kSubBP);

        // subblock [sM,s1]x[t0,tM]
        kSubBP.m_i00 = iM0;
        kSubBP.m_i01 = iMM;
        kSubBP.m_i10 = rkBP.m_i10;
        kSubBP.m_i11 = i1M;

        kSubBP.m_aakXss[0][0] = kXssM0;
        kSubBP.m_aakXss[0][1] = kXssMM;
        kSubBP.m_aakXss[1][0] = rkBP.m_aakXss[1][0];
        kSubBP.m_aakXss[1][1] = kXss1M;
        kSubBP.m_aakXtt[0][0] = kXttM0;
        kSubBP.m_aakXtt[0][1] = kXttMM;
        kSubBP.m_aakXtt[1][0] = rkBP.m_aakXtt[1][0];
        kSubBP.m_aakXtt[1][1] = kXtt1M;
        kSubBP.m_aakXsstt[0][0] = kXssttM0;
        kSubBP.m_aakXsstt[0][1] = kXssttMM;
        kSubBP.m_aakXsstt[1][0] = rkBP.m_aakXsstt[1][0];
        kSubBP.m_aakXsstt[1][1] = kXsstt1M;

        if ( akXs )
        {
            kSubBP.m_akXsss[0] = rkBP.m_akXsss[0];
            kSubBP.m_akXsss[1] = kXsss0M;
            kSubBP.m_aakXsst[0][0] = kXsstM0;
            kSubBP.m_aakXsst[0][1] = kXsstMM;
            kSubBP.m_aakXsst[1][0] = rkBP.m_aakXsst[1][0];
            kSubBP.m_aakXsst[1][1] = kXsst1M;
            kSubBP.m_aakXstt[0][0] = kXsttM0;
            kSubBP.m_aakXstt[0][1] = kXsttMM;
            kSubBP.m_aakXstt[1][0] = rkBP.m_aakXstt[1][0];
            kSubBP.m_aakXstt[1][1] = kXstt1M;
            kSubBP.m_akXttt[0] = kXtttM0;
            kSubBP.m_akXttt[1] = rkBP.m_akXttt[1];
            kSubBP.m_akXssstt[0] = rkBP.m_akXssstt[0];
            kSubBP.m_akXssstt[1] = kXssstt0M;
            kSubBP.m_akXssttt[0] = kXsstttM0;
            kSubBP.m_akXssttt[1] = rkBP.m_akXssttt[1];
        }

        if ( akColor )
        {
            kSubBP.m_aakCss[0][0] = kCssM0;
            kSubBP.m_aakCss[0][1] = kCssMM;
            kSubBP.m_aakCss[1][0] = rkBP.m_aakCss[1][0];
            kSubBP.m_aakCss[1][1] = kCss1M;
            kSubBP.m_aakCtt[0][0] = kCttM0;
            kSubBP.m_aakCtt[0][1] = kCttMM;
            kSubBP.m_aakCtt[1][0] = rkBP.m_aakCtt[1][0];
            kSubBP.m_aakCtt[1][1] = kCtt1M;
            kSubBP.m_aakCsstt[0][0] = kCssttM0;
            kSubBP.m_aakCsstt[0][1] = kCssttMM;
            kSubBP.m_aakCsstt[1][0] = rkBP.m_aakCsstt[1][0];
            kSubBP.m_aakCsstt[1][1] = kCsstt1M;
        }

        if ( akTexture )
        {
            kSubBP.m_aakTss[0][0] = kTssM0;
            kSubBP.m_aakTss[0][1] = kTssMM;
            kSubBP.m_aakTss[1][0] = rkBP.m_aakTss[1][0];
            kSubBP.m_aakTss[1][1] = kTss1M;
            kSubBP.m_aakTtt[0][0] = kTttM0;
            kSubBP.m_aakTtt[0][1] = kTttMM;
            kSubBP.m_aakTtt[1][0] = rkBP.m_aakTtt[1][0];
            kSubBP.m_aakTtt[1][1] = kTtt1M;
            kSubBP.m_aakTsstt[0][0] = kTssttM0;
            kSubBP.m_aakTsstt[0][1] = kTssttMM;
            kSubBP.m_aakTsstt[1][0] = rkBP.m_aakTsstt[1][0];
            kSubBP.m_aakTsstt[1][1] = kTsstt1M;
        }

        Subdivide(iLevel,fDSqr,akX,akXs,akXt,akColor,akTexture,kSubBP);

        // subblock [sM,s1]x[tM,t1]
        kSubBP.m_i00 = iMM;
        kSubBP.m_i01 = iM1;
        kSubBP.m_i10 = i1M;
        kSubBP.m_i11 = rkBP.m_i11;

        kSubBP.m_aakXss[0][0] = kXssMM;
        kSubBP.m_aakXss[0][1] = kXssM1;
        kSubBP.m_aakXss[1][0] = kXss1M;
        kSubBP.m_aakXss[1][1] = rkBP.m_aakXss[1][1];
        kSubBP.m_aakXtt[0][0] = kXttMM;
        kSubBP.m_aakXtt[0][1] = kXttM1;
        kSubBP.m_aakXtt[1][0] = kXtt1M;
        kSubBP.m_aakXtt[1][1] = rkBP.m_aakXtt[1][1];
        kSubBP.m_aakXsstt[0][0] = kXssttMM;
        kSubBP.m_aakXsstt[0][1] = kXssttM1;
        kSubBP.m_aakXsstt[1][0] = kXsstt1M;
        kSubBP.m_aakXsstt[1][1] = rkBP.m_aakXsstt[1][1];

        if ( akXs )
        {
            kSubBP.m_akXsss[0] = kXsss0M;
            kSubBP.m_akXsss[1] = rkBP.m_akXsss[1];
            kSubBP.m_aakXsst[0][0] = kXsstMM;
            kSubBP.m_aakXsst[0][1] = kXsstM1;
            kSubBP.m_aakXsst[1][0] = kXsst1M;
            kSubBP.m_aakXsst[1][1] = rkBP.m_aakXsst[1][1];
            kSubBP.m_aakXstt[0][0] = kXsttMM;
            kSubBP.m_aakXstt[0][1] = kXsttM1;
            kSubBP.m_aakXstt[1][0] = kXstt1M;
            kSubBP.m_aakXstt[1][1] = rkBP.m_aakXstt[1][1];
            kSubBP.m_akXttt[0] = kXtttM0;
            kSubBP.m_akXttt[1] = rkBP.m_akXttt[1];
            kSubBP.m_akXssstt[0] = kXssstt0M;
            kSubBP.m_akXssstt[1] = rkBP.m_akXssstt[1];
            kSubBP.m_akXssttt[0] = kXsstttM0;
            kSubBP.m_akXssttt[1] = rkBP.m_akXssttt[1];
        }

        if ( akColor )
        {
            kSubBP.m_aakCss[0][0] = kCssMM;
            kSubBP.m_aakCss[0][1] = kCssM1;
            kSubBP.m_aakCss[1][0] = kCss1M;
            kSubBP.m_aakCss[1][1] = rkBP.m_aakCss[1][1];
            kSubBP.m_aakCtt[0][0] = kCttMM;
            kSubBP.m_aakCtt[0][1] = kCttM1;
            kSubBP.m_aakCtt[1][0] = kCtt1M;
            kSubBP.m_aakCtt[1][1] = rkBP.m_aakCtt[1][1];
            kSubBP.m_aakCsstt[0][0] = kCssttMM;
            kSubBP.m_aakCsstt[0][1] = kCssttM1;
            kSubBP.m_aakCsstt[1][0] = kCsstt1M;
            kSubBP.m_aakCsstt[1][1] = rkBP.m_aakCsstt[1][1];
        }

        if ( akTexture )
        {
            kSubBP.m_aakTss[0][0] = kTssMM;
            kSubBP.m_aakTss[0][1] = kTssM1;
            kSubBP.m_aakTss[1][0] = kTss1M;
            kSubBP.m_aakTss[1][1] = rkBP.m_aakTss[1][1];
            kSubBP.m_aakTtt[0][0] = kTttMM;
            kSubBP.m_aakTtt[0][1] = kTttM1;
            kSubBP.m_aakTtt[1][0] = kTtt1M;
            kSubBP.m_aakTtt[1][1] = rkBP.m_aakTtt[1][1];
            kSubBP.m_aakTsstt[0][0] = kTssttMM;
            kSubBP.m_aakTsstt[0][1] = kTssttM1;
            kSubBP.m_aakTsstt[1][0] = kTsstt1M;
            kSubBP.m_aakTsstt[1][1] = rkBP.m_aakTsstt[1][1];
        }

        Subdivide(iLevel,fDSqr,akX,akXs,akXt,akColor,akTexture,kSubBP);
    }
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* BezierRectangle3::Factory (Stream& rkStream)
{
    BezierRectangle3* pkObject = new BezierRectangle3;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void BezierRectangle3::Load (Stream& rkStream, Stream::Link* pkLink)
{
    BezierRectangle::Load(rkStream,pkLink);
}
//----------------------------------------------------------------------------
void BezierRectangle3::Link (Stream& rkStream, Stream::Link* pkLink)
{
    BezierRectangle::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool BezierRectangle3::Register (Stream& rkStream)
{
    return BezierRectangle::Register(rkStream);
}
//----------------------------------------------------------------------------
void BezierRectangle3::Save (Stream& rkStream)
{
    BezierRectangle::Save(rkStream);
}
//----------------------------------------------------------------------------
StringTree* BezierRectangle3::SaveStrings ()
{
    StringTree* pkTree = new StringTree(1,0,1,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));

    // children
    pkTree->SetChild(0,BezierRectangle::SaveStrings());

    return pkTree;
}
//----------------------------------------------------------------------------
int BezierRectangle3::GetMemoryUsed () const
{
    int iBaseSize = sizeof(BezierRectangle3) - sizeof(BezierRectangle);
    int iTotalSize = iBaseSize + BezierRectangle::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int BezierRectangle3::GetDiskUsed () const
{
    return BezierRectangle::GetDiskUsed();
}
//----------------------------------------------------------------------------
