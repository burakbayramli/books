// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlBezierCylinder3.h"
#include "WmlTriMesh.h"
using namespace Wml;

WmlImplementRTTI(BezierCylinder3,BezierCylinder);
WmlImplementStream(BezierCylinder3);

// macros for initialization in the subdivision
#define XA(i) akCtrlPoint[m_aiIndex[i]]
#define CA(i) akCtrlColor[m_aiIndex[i]]
#define TA(i) akCtrlTexture[m_aiIndex[i]]

//----------------------------------------------------------------------------
BezierCylinder3::BezierCylinder3 (int* aiIndex)
    :
    BezierCylinder(3,8,aiIndex)
{
}
//----------------------------------------------------------------------------
BezierCylinder3::BezierCylinder3 ()
{
}
//----------------------------------------------------------------------------
void BezierCylinder3::InitializePoints (const Vector3f* akCtrlPoint,
    IntervalParameters& rkIP)
{
    // Xss[s][t]
    rkIP.m_aakXss[0][0] = 6.0f*(XA(0) - 2.0f*XA(1) + XA(2));
    rkIP.m_aakXss[0][1] = 6.0f*(XA(4) - 2.0f*XA(5) + XA(6));
    rkIP.m_aakXss[1][0] = 6.0f*(XA(1) - 2.0f*XA(2) + XA(3));
    rkIP.m_aakXss[1][1] = 6.0f*(XA(5) - 2.0f*XA(6) + XA(7));
}
//----------------------------------------------------------------------------
void BezierCylinder3::InitializeNormals (const Vector3f* akCtrlPoint,
    Vector3f akXsss[2])
{
    // Xsss[t]
    akXsss[0] = 6.0f*(XA(3) + 3.0f*(XA(1) - XA(2)) - XA(0));
    akXsss[1] = 6.0f*(XA(7) + 3.0f*(XA(5) - XA(6)) - XA(4));
}
//----------------------------------------------------------------------------
void BezierCylinder3::InitializeColors (const ColorRGB* akCtrlColor,
    IntervalParameters& rkIP)
{
    // Css[s][t]
    rkIP.m_aakCss[0][0] = 6.0f*(CA(0) - 2.0f*CA(1) + CA(2));
    rkIP.m_aakCss[0][1] = 6.0f*(CA(4) - 2.0f*CA(5) + CA(6));
    rkIP.m_aakCss[1][0] = 6.0f*(CA(1) - 2.0f*CA(2) + CA(3));
    rkIP.m_aakCss[1][1] = 6.0f*(CA(5) - 2.0f*CA(6) + CA(7));
}
//----------------------------------------------------------------------------
void BezierCylinder3::InitializeTextures (const Vector2f* akCtrlTexture,
    IntervalParameters& rkIP)
{
    // Tss[s][t]
    rkIP.m_aakTss[0][0] = 6.0f*(TA(0) - 2.0f*TA(1) + TA(2));
    rkIP.m_aakTss[0][1] = 6.0f*(TA(4) - 2.0f*TA(5) + TA(6));
    rkIP.m_aakTss[1][0] = 6.0f*(TA(1) - 2.0f*TA(2) + TA(3));
    rkIP.m_aakTss[1][1] = 6.0f*(TA(5) - 2.0f*TA(6) + TA(7));
}
//----------------------------------------------------------------------------
void BezierCylinder3::Tessellate (int iLevel, const Vector3f* akCtrlPoint,
    const ColorRGB* akCtrlColor, const Vector2f* akCtrlTexture,
    TriMesh* pkMesh, int& riVertexStart, int& riTriangleStart)
{
    GenerateConnectivity(iLevel,pkMesh,riTriangleStart);

    // number of vertices in the subdivision
    int iQuantity = GetVerticesPerPatch(iLevel);

    // indices of four corners of patch, I[s][t]
    int iTwoPowL = (1 << iLevel);
    int iTwoPowC = (1 << m_iCylinderLevel);
    IntervalParameters kIP;
    kIP.m_i00 = 0;
    kIP.m_i01 = iTwoPowC*(iTwoPowL + 1);
    kIP.m_i10 = iTwoPowL;
    kIP.m_i11 = kIP.m_i01 + iTwoPowL;

    // vertices for subdivision
    Vector3f* akX = pkMesh->Vertices() + riVertexStart;
    akX[kIP.m_i00] = XA(0);
    akX[kIP.m_i01] = XA(4);
    akX[kIP.m_i10] = XA(3);
    akX[kIP.m_i11] = XA(7);

    // derivatives for subdivision (for normal vectors)
    Vector3f* akXs;
    Vector3f* akXt;
    if ( pkMesh->Normals() )
    {
        akXs = new Vector3f[iQuantity];
        akXs[kIP.m_i00] = 3.0f*(XA(1) - XA(0));
        akXs[kIP.m_i01] = 3.0f*(XA(5) - XA(4));
        akXs[kIP.m_i10] = 3.0f*(XA(3) - XA(2));
        akXs[kIP.m_i11] = 3.0f*(XA(7) - XA(6));

        akXt = new Vector3f[iQuantity];
        akXt[kIP.m_i00] = 3.0f*(XA(4) - XA(0));
        akXt[kIP.m_i01] = akXt[kIP.m_i00];
        akXt[kIP.m_i10] = 3.0f*(XA(7) - XA(3));
        akXt[kIP.m_i11] = akXt[kIP.m_i10];
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
        akColor[kIP.m_i00] = CA(0);
        akColor[kIP.m_i01] = CA(4);
        akColor[kIP.m_i10] = CA(3);
        akColor[kIP.m_i11] = CA(7);
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
        akTexture[kIP.m_i00] = TA(0);
        akTexture[kIP.m_i01] = TA(4);
        akTexture[kIP.m_i10] = TA(3);
        akTexture[kIP.m_i11] = TA(7);
    }
    else
    {
        akTexture = NULL;
    }

    // recursive subdivision
    Vector3f akXsss[2];
    if ( iLevel > 0 || m_iCylinderLevel > 0 )
    {
        InitializePoints(akCtrlPoint,kIP);

        if ( akXs )
            InitializeNormals(akCtrlPoint,akXsss);

        if ( akCtrlColor )
            InitializeColors(akCtrlColor,kIP);

        if ( akCtrlTexture )
            InitializeTextures(akCtrlTexture,kIP);
    }

    if ( iLevel > 0 )
    {
        SubdivideBoundary(--iLevel,0.25f,akX,akXs,akXt,akXsss,akColor,
            akTexture,kIP);
    }

    if ( m_iCylinderLevel > 0 )
    {
        SubdivideCylinder(m_iCylinderLevel-1,akX,akXs,akXt,akColor,
            akTexture,0,kIP.m_i01,iTwoPowL);
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
void BezierCylinder3::SubdivideBoundary (int iLevel, float fDSqr,
    Vector3f* akX, Vector3f* akXs, Vector3f* akXt, Vector3f akXsss[2],
    ColorRGB* akColor, Vector2f* akTexture, IntervalParameters& rkIP)
{
    // subdivision indices
    int iM0 = (rkIP.m_i00 + rkIP.m_i10) >> 1;
    int iM1 = (rkIP.m_i01 + rkIP.m_i11) >> 1;

    // vertices
    Vector3f kXssM0 = 0.5f*(rkIP.m_aakXss[0][0] + rkIP.m_aakXss[1][0]);
    Vector3f kXssM1 = 0.5f*(rkIP.m_aakXss[0][1] + rkIP.m_aakXss[1][1]);
    akX[iM0] = 0.5f*(akX[rkIP.m_i00] + akX[rkIP.m_i10] - fDSqr*kXssM0);
    akX[iM1] = 0.5f*(akX[rkIP.m_i01] + akX[rkIP.m_i11] - fDSqr*kXssM1);

    // derivatives (for normal vectors)
    if ( akXs )
    {
        akXs[iM0] = 0.5f*(akXs[rkIP.m_i00] + akXs[rkIP.m_i10] -
            fDSqr*akXsss[0]);
        akXs[iM1] = 0.5f*(akXs[rkIP.m_i01] + akXs[rkIP.m_i11] -
            fDSqr*akXsss[1]);
        akXt[iM0] = akX[iM1] - akX[iM0];
        akXt[iM1] = akXt[iM0];
    }

    // colors
    ColorRGB kCssM0, kCssM1;
    if ( akColor )
    {
        kCssM0 = 0.5f*(rkIP.m_aakCss[0][0] + rkIP.m_aakCss[1][0]);
        kCssM1 = 0.5f*(rkIP.m_aakCss[0][1] + rkIP.m_aakCss[1][1]);
        akColor[iM0] = 0.5f*(akColor[rkIP.m_i00] + akColor[rkIP.m_i10] -
            fDSqr*kCssM0);
        akColor[iM1] = 0.5f*(akColor[rkIP.m_i01] + akColor[rkIP.m_i11] -
            fDSqr*kCssM1);
    }

    // textures
    Vector2f kTssM0, kTssM1;
    if ( akTexture )
    {
        kTssM0 = 0.5f*(rkIP.m_aakTss[0][0] + rkIP.m_aakTss[1][0]);
        kTssM1 = 0.5f*(rkIP.m_aakTss[0][1] + rkIP.m_aakTss[1][1]);
        akTexture[iM0] = 0.5f*(akTexture[rkIP.m_i00] +
            akTexture[rkIP.m_i10] - fDSqr*kTssM0);
        akTexture[iM1] = 0.5f*(akTexture[rkIP.m_i01] +
            akTexture[rkIP.m_i11] - fDSqr*kTssM1);
    }

    // recurse on two children
    if ( iLevel > 0 )
    {
        iLevel--;
        fDSqr *= 0.25f;

        IntervalParameters kSubIP;

        // subinterval [s0,sM]
        kSubIP.m_i00 = rkIP.m_i00;
        kSubIP.m_i01 = rkIP.m_i01;
        kSubIP.m_i10 = iM0;
        kSubIP.m_i11 = iM1;

        kSubIP.m_aakXss[0][0] = rkIP.m_aakXss[0][0];
        kSubIP.m_aakXss[0][1] = rkIP.m_aakXss[0][1];
        kSubIP.m_aakXss[1][0] = kXssM0;
        kSubIP.m_aakXss[1][1] = kXssM1;

        if ( akColor )
        {
            kSubIP.m_aakCss[0][0] = rkIP.m_aakCss[0][0];
            kSubIP.m_aakCss[0][1] = rkIP.m_aakCss[0][1];
            kSubIP.m_aakCss[1][0] = kCssM0;
            kSubIP.m_aakCss[1][1] = kCssM1;
        }

        if ( akTexture )
        {
            kSubIP.m_aakTss[0][0] = rkIP.m_aakTss[0][0];
            kSubIP.m_aakTss[0][1] = rkIP.m_aakTss[0][1];
            kSubIP.m_aakTss[1][0] = kTssM0;
            kSubIP.m_aakTss[1][1] = kTssM1;
        }

        SubdivideBoundary(iLevel,fDSqr,akX,akXs,akXt,akXsss,akColor,
            akTexture,kSubIP);

        // subinterval [sM,s1]
        kSubIP.m_i00 = iM0;
        kSubIP.m_i01 = iM1;
        kSubIP.m_i10 = rkIP.m_i10;
        kSubIP.m_i11 = rkIP.m_i11;

        kSubIP.m_aakXss[0][0] = kXssM0;
        kSubIP.m_aakXss[0][1] = kXssM1;
        kSubIP.m_aakXss[1][0] = rkIP.m_aakXss[1][0];
        kSubIP.m_aakXss[1][1] = rkIP.m_aakXss[1][1];

        if ( akColor )
        {
            kSubIP.m_aakCss[0][0] = kCssM0;
            kSubIP.m_aakCss[0][1] = kCssM1;
            kSubIP.m_aakCss[1][0] = rkIP.m_aakCss[1][0];
            kSubIP.m_aakCss[1][1] = rkIP.m_aakCss[1][1];
        }

        if ( akTexture )
        {
            kSubIP.m_aakTss[0][0] = kTssM0;
            kSubIP.m_aakTss[0][1] = kTssM1;
            kSubIP.m_aakTss[1][0] = rkIP.m_aakTss[1][0];
            kSubIP.m_aakTss[1][1] = rkIP.m_aakTss[1][1];
        }

        SubdivideBoundary(iLevel,fDSqr,akX,akXs,akXt,akXsss,akColor,
            akTexture,kSubIP);
    }
}
//----------------------------------------------------------------------------
void BezierCylinder3::SubdivideCylinder (int iCLevel, Vector3f* akX,
    Vector3f* akXs, Vector3f* akXt, ColorRGB* akColor, Vector2f* akTexture,
    int i0, int i1, int iTwoPowL)
{
    // subdivision index
    int iM = (i0 + i1) >> 1;

    int j0 = i0, jM = iM, j1 = i1;
    int iJLast = jM + iTwoPowL;
    for (/**/; jM <= iJLast; j0++, jM++, j1++)
    {
        // vertices
        akX[jM] = 0.5f*(akX[j0] + akX[j1]);

        // derivatives (for normal vectors)
        if ( akXs )
        {
            akXs[jM] = 0.5f*(akXs[j0] + akXs[j1]);
            akXt[jM] = akXt[j0];
        }

        // colors
        if ( akColor )
            akColor[jM] = 0.5f*(akColor[j0] + akColor[j1]);

        // textures
        if ( akTexture )
            akTexture[jM] = 0.5f*(akTexture[j0] + akTexture[j1]);
    }

    // recurse on two children
    if ( iCLevel > 0 )
    {
        iCLevel--;

        // subinterval [t0,tM]
        SubdivideCylinder(iCLevel,akX,akXs,akXt,akColor,akTexture,i0,iM,
            iTwoPowL);

        // subinterval [tM,t1]
        SubdivideCylinder(iCLevel,akX,akXs,akXt,akColor,akTexture,iM,i1,
            iTwoPowL);
    }
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* BezierCylinder3::Factory (Stream& rkStream)
{
    BezierCylinder3* pkObject = new BezierCylinder3;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void BezierCylinder3::Load (Stream& rkStream, Stream::Link* pkLink)
{
    BezierCylinder::Load(rkStream,pkLink);
}
//----------------------------------------------------------------------------
void BezierCylinder3::Link (Stream& rkStream, Stream::Link* pkLink)
{
    BezierCylinder::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool BezierCylinder3::Register (Stream& rkStream)
{
    return BezierCylinder::Register(rkStream);
}
//----------------------------------------------------------------------------
void BezierCylinder3::Save (Stream& rkStream)
{
    BezierCylinder::Save(rkStream);
}
//----------------------------------------------------------------------------
StringTree* BezierCylinder3::SaveStrings ()
{
    StringTree* pkTree = new StringTree(1,0,1,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));

    // children
    pkTree->SetChild(0,BezierCylinder::SaveStrings());

    return pkTree;
}
//----------------------------------------------------------------------------
int BezierCylinder3::GetMemoryUsed () const
{
    int iBaseSize = sizeof(BezierCylinder3) - sizeof(BezierCylinder);
    int iTotalSize = iBaseSize + BezierCylinder::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int BezierCylinder3::GetDiskUsed () const
{
    return BezierCylinder::GetDiskUsed();
}
//----------------------------------------------------------------------------
