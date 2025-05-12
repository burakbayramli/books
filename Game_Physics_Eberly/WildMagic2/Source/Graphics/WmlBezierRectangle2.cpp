// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlBezierRectangle2.h"
#include "WmlTriMesh.h"
using namespace Wml;

WmlImplementRTTI(BezierRectangle2,BezierRectangle);
WmlImplementStream(BezierRectangle2);

// macros for initialization in the subdivision
#define XA(i) akCtrlPoint[m_aiIndex[i]]
#define CA(i) akCtrlColor[m_aiIndex[i]]
#define TA(i) akCtrlTexture[m_aiIndex[i]]

//----------------------------------------------------------------------------
BezierRectangle2::BezierRectangle2 (int* aiIndex)
    :
    BezierRectangle(2,9,aiIndex)
{
}
//----------------------------------------------------------------------------
BezierRectangle2::BezierRectangle2 ()
{
}
//----------------------------------------------------------------------------
void BezierRectangle2::InitializePoints (const Vector3f* akCtrlPoint,
    Vector3f& rkXsstt, BlockParameters& rkBP)
{
    // Xss
    rkBP.m_akXss[0] = 2.0f*(XA(0) - 2.0f*XA(1) + XA(2));
    rkBP.m_akXss[1] = 2.0f*(XA(6) - 2.0f*XA(7) + XA(8));

    // Xtt
    rkBP.m_akXtt[0] = 2.0f*(XA(0) - 2.0f*XA(3) + XA(6));
    rkBP.m_akXtt[1] = 2.0f*(XA(2) - 2.0f*XA(5) + XA(8));

    // Xsstt
    rkXsstt = 4.0f*(XA(0) + XA(2) + XA(6) + XA(8) -
        2.0f*(XA(1) + XA(3) + XA(5) + XA(7)) + 4.0f*XA(4));
}
//----------------------------------------------------------------------------
void BezierRectangle2::InitializeNormals (const Vector3f* akCtrlPoint,
    BlockParameters& rkBP)
{
    // Xsst
    rkBP.m_akXsst[0] = 4.0f*(XA(3)+XA(5)-XA(0)-XA(2)+2.0f*(XA(1)-XA(4)));
    rkBP.m_akXsst[1] = 4.0f*(XA(6)+XA(8)-XA(3)-XA(5)+2.0f*(XA(4)-XA(7)));

    // Xstt
    rkBP.m_akXstt[0] = 4.0f*(XA(1)+XA(7)-XA(0)-XA(6)+2.0f*(XA(3)-XA(4)));
    rkBP.m_akXstt[1] = 4.0f*(XA(2)+XA(8)-XA(1)-XA(7)+2.0f*(XA(4)-XA(5)));
}
//----------------------------------------------------------------------------
void BezierRectangle2::InitializeColors (const ColorRGB* akCtrlColor,
    ColorRGB& rkCsstt, BlockParameters& rkBP)
{
    // Css
    rkBP.m_akCss[0] = 2.0f*(CA(0) - 2.0f*CA(1) + CA(2));
    rkBP.m_akCss[1] = 2.0f*(CA(6) - 2.0f*CA(7) + CA(8));

    // Ctt
    rkBP.m_akCtt[0] = 2.0f*(CA(0) - 2.0f*CA(3) + CA(6));
    rkBP.m_akCtt[1] = 2.0f*(CA(2) - 2.0f*CA(5) + CA(8));

    // Csstt
    rkCsstt = 4.0f*(CA(0) + CA(2) + CA(6) + CA(8) -
        2.0f*(CA(1) + CA(3) + CA(5) + CA(7)) + 4.0f*CA(4));
}
//----------------------------------------------------------------------------
void BezierRectangle2::InitializeTextures (const Vector2f* akCtrlTexture,
    Vector2f& rkTsstt, BlockParameters& rkBP)
{
    // Tss
    rkBP.m_akTss[0] = 2.0f*(TA(0) - 2.0f*TA(1) + TA(2));
    rkBP.m_akTss[1] = 2.0f*(TA(6) - 2.0f*TA(7) + TA(8));

    // Ttt
    rkBP.m_akTtt[0] = 2.0f*(TA(0) - 2.0f*TA(3) + TA(6));
    rkBP.m_akTtt[1] = 2.0f*(TA(2) - 2.0f*TA(5) + TA(8));

    // Tsstt
    rkTsstt = 4.0f*(TA(0) + TA(2) + TA(6) + TA(8) -
        2.0f*(TA(1) + TA(3) + TA(5) + TA(7)) + 4.0f*TA(4));
}
//----------------------------------------------------------------------------
void BezierRectangle2::Tessellate (int iLevel, const Vector3f* akCtrlPoint,
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
    akX[kBP.m_i00] = XA(0);
    akX[kBP.m_i01] = XA(6);
    akX[kBP.m_i10] = XA(2);
    akX[kBP.m_i11] = XA(8);

    // derivatives for subdivision (for normal vectors)
    Vector3f* akXs;
    Vector3f* akXt;
    if ( pkMesh->Normals() )
    {
        akXs = new Vector3f[iQuantity];
        akXs[kBP.m_i00] = 3.0f*(XA(1) - XA(0));
        akXs[kBP.m_i01] = 3.0f*(XA(7) - XA(6));
        akXs[kBP.m_i10] = 3.0f*(XA(2) - XA(1));
        akXs[kBP.m_i11] = 3.0f*(XA(8) - XA(7));

        akXt = new Vector3f[iQuantity];
        akXt[kBP.m_i00] = 3.0f*(XA(3) - XA(0));
        akXt[kBP.m_i01] = 3.0f*(XA(6) - XA(3));
        akXt[kBP.m_i10] = 3.0f*(XA(5) - XA(2));
        akXt[kBP.m_i11] = 3.0f*(XA(8) - XA(5));
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
        akColor[kBP.m_i00] = CA(0);
        akColor[kBP.m_i01] = CA(6);
        akColor[kBP.m_i10] = CA(2);
        akColor[kBP.m_i11] = CA(8);
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
        akTexture[kBP.m_i00] = TA(0);
        akTexture[kBP.m_i01] = TA(6);
        akTexture[kBP.m_i10] = TA(2);
        akTexture[kBP.m_i11] = TA(8);
    }
    else
    {
        akTexture = NULL;
    }

    // recursive subdivision
    if ( iLevel > 0 )
    {
        Vector3f kXsstt;
        ColorRGB kCsstt;
        Vector2f kTsstt;

        InitializePoints(akCtrlPoint,kXsstt,kBP);

        if ( akXs )
            InitializeNormals(akCtrlPoint,kBP);

        if ( akCtrlColor )
            InitializeColors(akCtrlColor,kCsstt,kBP);

        if ( akCtrlTexture )
            InitializeTextures(akCtrlTexture,kTsstt,kBP);

        Subdivide(--iLevel,0.25f,akX,akXs,akXt,akColor,akTexture,kXsstt,
            kCsstt,kTsstt,kBP);
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
void BezierRectangle2::Subdivide (int iLevel, float fDSqr, Vector3f* akX,
    Vector3f* akXs, Vector3f* akXt, ColorRGB* akColor, Vector2f* akTexture,
    Vector3f& rkXsstt, ColorRGB& rkCsstt, Vector2f& rkTsstt,
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
    Vector3f kXttM0 = 0.5f*(rkBP.m_akXtt[0]+rkBP.m_akXtt[1]-fDSqr*rkXsstt);
    akX[iM0] = 0.5f*(akX[rkBP.m_i00] + akX[rkBP.m_i10] -
        fDSqr*rkBP.m_akXss[0]);
    akX[iM1] = 0.5f*(akX[rkBP.m_i01] + akX[rkBP.m_i11] -
        fDSqr*rkBP.m_akXss[1]);

    // left and right t-edge subdivision
    Vector3f kXss0M = 0.5f*(rkBP.m_akXss[0]+rkBP.m_akXss[1]-fDSqr*rkXsstt);
    akX[i0M] = 0.5f*(akX[rkBP.m_i00] + akX[rkBP.m_i01] -
        fDSqr*rkBP.m_akXtt[0]);
    akX[i1M] = 0.5f*(akX[rkBP.m_i10] + akX[rkBP.m_i11] -
        fDSqr*rkBP.m_akXtt[1]);

    // center subdivision
    akX[iMM] = 0.5f*(akX[i0M] + akX[i1M] - fDSqr*kXss0M);

    // derivatives (for normal vectors)
    Vector3f kXsst0M, kXsttM0;
    if ( akXs )
    {
        // top and bottom s-edge subdivision
        akXs[iM0] = 0.5f*(akXs[rkBP.m_i00] + akXs[rkBP.m_i10]);
        akXt[iM0] = 0.5f*(akXt[rkBP.m_i00] + akXt[rkBP.m_i10] -
            fDSqr*rkBP.m_akXsst[0]);
        akXs[iM1] = 0.5f*(akXs[rkBP.m_i01] + akXs[rkBP.m_i11]);
        akXt[iM1] = 0.5f*(akXt[rkBP.m_i01] + akXt[rkBP.m_i11] -
            fDSqr*rkBP.m_akXsst[1]);

        kXsttM0 = 0.5f*(rkBP.m_akXstt[0] + rkBP.m_akXstt[1]);

        // left and right t-edge subdivision
        akXs[i0M] = 0.5f*(akXs[rkBP.m_i00] + akXs[rkBP.m_i01] -
            fDSqr*rkBP.m_akXstt[0]);
        akXt[i0M] = 0.5f*(akXt[rkBP.m_i00] + akXt[rkBP.m_i01]);
        akXs[i1M] = 0.5f*(akXs[rkBP.m_i10] + akXs[rkBP.m_i11] -
            fDSqr*rkBP.m_akXstt[1]);
        akXt[i1M] = 0.5f*(akXt[rkBP.m_i10] + akXt[rkBP.m_i11]);

        kXsst0M = 0.5f*(rkBP.m_akXsst[0] + rkBP.m_akXsst[1]);

        // center subdivision
        akXs[iMM] = 0.5f*(akXs[i0M] + akXs[i1M]);
        akXt[iMM] = 0.5f*(akXt[iM0] + akXt[iM1]);
    }

    // colors
    ColorRGB kCss0M, kCttM0;
    if ( akColor )
    {
        // top and bottom s-edge subdivision
        kCttM0 = 0.5f*(rkBP.m_akCtt[0]+rkBP.m_akCtt[1]-fDSqr*rkCsstt);
        akColor[iM0] = 0.5f*(akColor[rkBP.m_i00] + akColor[rkBP.m_i10] -
            fDSqr*rkBP.m_akCss[0]);
        akColor[iM1] = 0.5f*(akColor[rkBP.m_i01] + akColor[rkBP.m_i11] -
            fDSqr*rkBP.m_akCss[1]);

        // left and right t-edge subdivision
        kCss0M = 0.5f*(rkBP.m_akCss[0]+rkBP.m_akCss[1]-fDSqr*rkCsstt);
        akColor[i0M] = 0.5f*(akColor[rkBP.m_i00] + akColor[rkBP.m_i01] -
            fDSqr*rkBP.m_akCtt[0]);
        akColor[i1M] = 0.5f*(akColor[rkBP.m_i10] + akColor[rkBP.m_i11] -
            fDSqr*rkBP.m_akCtt[1]);

        // center subdivision
        akColor[iMM] = 0.5f*(akColor[i0M] + akColor[i1M] - fDSqr*kCss0M);
    }

    // textures
    Vector2f kTss0M, kTttM0;
    if ( akTexture )
    {
        // top and bottom s-edge subdivision
        kTttM0 = 0.5f*(rkBP.m_akTtt[0]+rkBP.m_akTtt[1]-fDSqr*rkTsstt);
        akTexture[iM0] = 0.5f*(akTexture[rkBP.m_i00] +
            akTexture[rkBP.m_i10] - fDSqr*rkBP.m_akTss[0]);
        akTexture[iM1] = 0.5f*(akTexture[rkBP.m_i01] +
            akTexture[rkBP.m_i11] - fDSqr*rkBP.m_akTss[1]);

        // left and right t-edge subdivision
        kTss0M = 0.5f*(rkBP.m_akTss[0]+rkBP.m_akTss[1]-fDSqr*rkTsstt);
        akTexture[i0M] = 0.5f*(akTexture[rkBP.m_i00] +
            akTexture[rkBP.m_i01] - fDSqr*rkBP.m_akTtt[0]);
        akTexture[i1M] = 0.5f*(akTexture[rkBP.m_i10] +
            akTexture[rkBP.m_i11] - fDSqr*rkBP.m_akTtt[1]);

        // center subdivision
        akTexture[iMM] = 0.5f*(akTexture[i0M] + akTexture[i1M] -
            fDSqr*kTss0M);
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

        kSubBP.m_akXss[0] = rkBP.m_akXss[0];
        kSubBP.m_akXss[1] = kXss0M;
        kSubBP.m_akXtt[0] = rkBP.m_akXtt[0];
        kSubBP.m_akXtt[1] = kXttM0;

        if ( akXs )
        {
            kSubBP.m_akXsst[0] = rkBP.m_akXsst[0];
            kSubBP.m_akXsst[1] = kXsst0M;
            kSubBP.m_akXstt[0] = rkBP.m_akXstt[0];
            kSubBP.m_akXstt[1] = kXsttM0;
        }

        if ( akColor )
        {
            kSubBP.m_akCss[0] = rkBP.m_akCss[0];
            kSubBP.m_akCss[1] = kCss0M;
            kSubBP.m_akCtt[0] = rkBP.m_akCtt[0];
            kSubBP.m_akCtt[1] = kCttM0;
        }

        if ( akTexture )
        {
            kSubBP.m_akTss[0] = rkBP.m_akTss[0];
            kSubBP.m_akTss[1] = kTss0M;
            kSubBP.m_akTtt[0] = rkBP.m_akTtt[0];
            kSubBP.m_akTtt[1] = kTttM0;
        }

        Subdivide(iLevel,fDSqr,akX,akXs,akXt,akColor,akTexture,rkXsstt,
            rkCsstt,rkTsstt,kSubBP);

        // subblock [s0,sM]x[tM,t1]
        kSubBP.m_i00 = i0M;
        kSubBP.m_i01 = rkBP.m_i01;
        kSubBP.m_i10 = iMM;
        kSubBP.m_i11 = iM1;

        kSubBP.m_akXss[0] = kXss0M;
        kSubBP.m_akXss[1] = rkBP.m_akXss[1];
        kSubBP.m_akXtt[0] = rkBP.m_akXtt[0];
        kSubBP.m_akXtt[1] = kXttM0;

        if ( akXs )
        {
            kSubBP.m_akXsst[0] = kXsst0M;
            kSubBP.m_akXsst[1] = rkBP.m_akXsst[1];
            kSubBP.m_akXstt[0] = rkBP.m_akXstt[0];
            kSubBP.m_akXstt[1] = kXsttM0;
        }

        if ( akColor )
        {
            kSubBP.m_akCss[0] = kCss0M;
            kSubBP.m_akCss[1] = rkBP.m_akCss[1];
            kSubBP.m_akCtt[0] = rkBP.m_akCtt[0];
            kSubBP.m_akCtt[1] = kCttM0;
        }

        if ( akTexture )
        {
            kSubBP.m_akTss[0] = kTss0M;
            kSubBP.m_akTss[1] = rkBP.m_akTss[1];
            kSubBP.m_akTtt[0] = rkBP.m_akTtt[0];
            kSubBP.m_akTtt[1] = kTttM0;
        }

        Subdivide(iLevel,fDSqr,akX,akXs,akXt,akColor,akTexture,rkXsstt,
            rkCsstt,rkTsstt,kSubBP);

        // subblock [sM,s1]x[t0,tM]
        kSubBP.m_i00 = iM0;
        kSubBP.m_i01 = iMM;
        kSubBP.m_i10 = rkBP.m_i10;
        kSubBP.m_i11 = i1M;

        kSubBP.m_akXss[0] = rkBP.m_akXss[0];
        kSubBP.m_akXss[1] = kXss0M;
        kSubBP.m_akXtt[0] = kXttM0;
        kSubBP.m_akXtt[1] = rkBP.m_akXtt[1];

        if ( akXs )
        {
            kSubBP.m_akXsst[0] = rkBP.m_akXsst[0];
            kSubBP.m_akXsst[1] = kXsst0M;
            kSubBP.m_akXstt[0] = kXsttM0;
            kSubBP.m_akXstt[1] = rkBP.m_akXstt[1];
        }

        if ( akColor )
        {
            kSubBP.m_akCss[0] = rkBP.m_akCss[0];
            kSubBP.m_akCss[1] = kCss0M;
            kSubBP.m_akCtt[0] = kCttM0;
            kSubBP.m_akCtt[1] = rkBP.m_akCtt[1];
        }

        if ( akTexture )
        {
            kSubBP.m_akTss[0] = rkBP.m_akTss[0];
            kSubBP.m_akTss[1] = kTss0M;
            kSubBP.m_akTtt[0] = kTttM0;
            kSubBP.m_akTtt[1] = rkBP.m_akTtt[1];
        }

        Subdivide(iLevel,fDSqr,akX,akXs,akXt,akColor,akTexture,rkXsstt,
            rkCsstt,rkTsstt,kSubBP);

        // subblock [sM,s1]x[tM,t1]
        kSubBP.m_i00 = iMM;
        kSubBP.m_i01 = iM1;
        kSubBP.m_i10 = i1M;
        kSubBP.m_i11 = rkBP.m_i11;

        kSubBP.m_akXss[0] = kXss0M;
        kSubBP.m_akXss[1] = rkBP.m_akXss[1];
        kSubBP.m_akXtt[0] = kXttM0;
        kSubBP.m_akXtt[1] = rkBP.m_akXtt[1];

        if ( akXs )
        {
            kSubBP.m_akXsst[0] = kXsst0M;
            kSubBP.m_akXsst[1] = rkBP.m_akXsst[1];
            kSubBP.m_akXstt[0] = kXsttM0;
            kSubBP.m_akXstt[1] = rkBP.m_akXstt[1];
        }

        if ( akColor )
        {
            kSubBP.m_akCss[0] = kCss0M;
            kSubBP.m_akCss[1] = rkBP.m_akCss[1];
            kSubBP.m_akCtt[0] = kCttM0;
            kSubBP.m_akCtt[1] = rkBP.m_akCtt[1];
        }

        if ( akTexture )
        {
            kSubBP.m_akTss[0] = kTss0M;
            kSubBP.m_akTss[1] = rkBP.m_akTss[1];
            kSubBP.m_akTtt[0] = kTttM0;
            kSubBP.m_akTtt[1] = rkBP.m_akTtt[1];
        }

        Subdivide(iLevel,fDSqr,akX,akXs,akXt,akColor,akTexture,rkXsstt,
            rkCsstt,rkTsstt,kSubBP);
    }
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* BezierRectangle2::Factory (Stream& rkStream)
{
    BezierRectangle2* pkObject = new BezierRectangle2;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void BezierRectangle2::Load (Stream& rkStream, Stream::Link* pkLink)
{
    BezierRectangle::Load(rkStream,pkLink);
}
//----------------------------------------------------------------------------
void BezierRectangle2::Link (Stream& rkStream, Stream::Link* pkLink)
{
    BezierRectangle::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool BezierRectangle2::Register (Stream& rkStream)
{
    return BezierRectangle::Register(rkStream);
}
//----------------------------------------------------------------------------
void BezierRectangle2::Save (Stream& rkStream)
{
    BezierRectangle::Save(rkStream);
}
//----------------------------------------------------------------------------
StringTree* BezierRectangle2::SaveStrings ()
{
    StringTree* pkTree = new StringTree(1,0,1,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));

    // children
    pkTree->SetChild(0,BezierRectangle::SaveStrings());

    return pkTree;
}
//----------------------------------------------------------------------------
int BezierRectangle2::GetMemoryUsed () const
{
    int iBaseSize = sizeof(BezierRectangle2) - sizeof(BezierRectangle);
    int iTotalSize = iBaseSize + BezierRectangle::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int BezierRectangle2::GetDiskUsed () const
{
    return BezierRectangle::GetDiskUsed();
}
//----------------------------------------------------------------------------
