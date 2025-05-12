// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlBezierTriangle2.h"
#include "WmlTriMesh.h"
using namespace Wml;

WmlImplementRTTI(BezierTriangle2,BezierTriangle);
WmlImplementStream(BezierTriangle2);

// macros for initialization in the subdivision
#define XA(i) akCtrlPoint[m_aiIndex[i]]
#define CA(i) akCtrlColor[m_aiIndex[i]]
#define TA(i) akCtrlTexture[m_aiIndex[i]]

//----------------------------------------------------------------------------
BezierTriangle2::BezierTriangle2 (int* aiIndex)
    :
    BezierTriangle(2,6,aiIndex)
{
}
//----------------------------------------------------------------------------
BezierTriangle2::BezierTriangle2 ()
{
}
//----------------------------------------------------------------------------
void BezierTriangle2::InitializePoints (const Vector3f* akCtrlPoint,
    BlockParameters& rkBP)
{
    // Xuu
    rkBP.m_kXuu = 2.0f*(XA(0) - 2.0f*XA(1) + XA(2));

    // Xvv
    rkBP.m_kXvv = 2.0f*(XA(0) - 2.0f*XA(3) + XA(5));

    // Xhh = Xuu - 2*Xuv + Xvv (d/dh = d/ds - d/dt)
    rkBP.m_kXhh = 2.0f*(XA(0) + XA(4) - XA(1) - XA(3));
}
//----------------------------------------------------------------------------
void BezierTriangle2::InitializeColors (const ColorRGB* akCtrlColor,
    BlockParameters& rkBP)
{
    // Cuu
    rkBP.m_kCuu = 2.0f*(CA(0) - 2.0f*CA(1) + CA(2));

    // Cvv
    rkBP.m_kCvv = 2.0f*(CA(0) - 2.0f*CA(3) + CA(5));

    // Chh = Cuu - 2*Cuv + Cvv (d/dh = d/ds - d/dt)
    rkBP.m_kChh = 2.0f*(CA(0) + CA(4) - CA(1) - CA(3));
}
//----------------------------------------------------------------------------
void BezierTriangle2::InitializeTextures (const Vector2f* akCtrlTexture,
    BlockParameters& rkBP)
{
    // Tuu
    rkBP.m_kTuu = 2.0f*(TA(0) - 2.0f*TA(1) + TA(2));

    // Tvv
    rkBP.m_kTvv = 2.0f*(TA(0) - 2.0f*TA(3) + TA(5));

    // Thh = Tuu - 2*Tuv + Tvv (d/dh = d/ds - d/dt)
    rkBP.m_kThh = 2.0f*(TA(0) + TA(4) - TA(1) - TA(3));
}
//----------------------------------------------------------------------------
void BezierTriangle2::Tessellate (int iLevel, const Vector3f* akCtrlPoint,
    const ColorRGB* akCtrlColor, const Vector2f* akCtrlTexture,
    TriMesh* pkMesh, int& riVertexStart, int& riTriangleStart)
{
    GenerateConnectivity(iLevel,pkMesh,riTriangleStart);

    // number of vertices in the subdivision
    int iQuantity = GetVerticesPerPatch(iLevel);

    // indices of three corners of patch, I0 (w=1), I1 (u=1), I2 (v=1)
    int iTwoPowL = (1 << iLevel);
    int i0 = 0;
    int i1 = iTwoPowL;
    int i2 = iTwoPowL*(iTwoPowL + 3) >> 1;

    BlockParameters kBP;

    // vertices for subdivision
    Vector3f* akX = pkMesh->Vertices() + riVertexStart;
    akX[i0] = XA(0);
    akX[i1] = XA(2);
    akX[i2] = XA(5);

    // derivatives for subdivision (for normal vectors)
    Vector3f* akXu;
    Vector3f* akXv;
    if ( pkMesh->Normals() )
    {
        akXu = new Vector3f[iQuantity];
        akXu[i0] = 2.0f*(XA(1) - XA(0));
        akXu[i1] = 2.0f*(XA(2) - XA(1));
        akXu[i2] = 2.0f*(XA(4) - XA(3));

        akXv = new Vector3f[iQuantity];
        akXv[i0] = 2.0f*(XA(3) - XA(0));
        akXv[i1] = 2.0f*(XA(5) - XA(3));
        akXv[i2] = 2.0f*(XA(4) - XA(1));
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
        akColor[i0] = CA(0);
        akColor[i1] = CA(2);
        akColor[i2] = CA(5);
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
        akTexture[i0] = TA(0);
        akTexture[i1] = TA(2);
        akTexture[i2] = TA(5);
    }
    else
    {
        akTexture = NULL;
    }

    // recursive subdivision
    if ( iLevel > 0 )
    {
        InitializePoints(akCtrlPoint,kBP);

        if ( akCtrlColor )
            InitializeColors(akCtrlColor,kBP);

        if ( akCtrlTexture )
            InitializeTextures(akCtrlTexture,kBP);

        SubdivideLL(--iLevel,0.25f,akX,akXu,akXv,akColor,akTexture,i0,i1,i2,
            kBP);
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
void BezierTriangle2::SubdivideLL (int iLevel, float fDSqr, Vector3f* akX,
    Vector3f* akXu, Vector3f* akXv, ColorRGB* akColor, Vector2f* akTexture,
    int i0, int i1, int i2, BlockParameters& rkBP)
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
    int i01 = (i0 + i1) >> 1;
    int iD10 = i1 - i0;
    int i02 = (((i0 + i2) << 2) + iD10*iD10) >> 3;
    int i12 = i02 + (iD10 >> 1);

    // vertices

    // bottom u-edge subdivision
    akX[i01] = 0.5f*(akX[i0] + akX[i1] - fDSqr*rkBP.m_kXuu);

    // left v-edge subdivision
    akX[i02] = 0.5f*(akX[i0] + akX[i2] - fDSqr*rkBP.m_kXvv);

    // hypotenuse edge subdivision
    akX[i12] = 0.5f*(akX[i1] + akX[i2] - fDSqr*rkBP.m_kXhh);

    // derivatives (for normal vectors)
    if ( akXu )
    {
        // bottom u-edge subdivision
        akXu[i01] = 0.5f*(akXu[i0] + akXu[i1]);
        akXv[i01] = 0.5f*(akXv[i0] + akXv[i1]);

        // left v-edge subdivision
        akXu[i02] = 0.5f*(akXu[i0] + akXu[i2]);
        akXv[i02] = 0.5f*(akXv[i0] + akXv[i2]);

        // hypotenuse edge subdivision
        akXu[i12] = 0.5f*(akXu[i1] + akXu[i2]);
        akXv[i12] = 0.5f*(akXv[i1] + akXv[i2]);
    }

    // colors
    if ( akColor )
    {
        // bottom u-edge subdivision
        akColor[i01] = 0.5f*(akColor[i0] + akColor[i1] - fDSqr*rkBP.m_kCuu);

        // left v-edge subdivision
        akColor[i02] = 0.5f*(akColor[i0] + akColor[i2] - fDSqr*rkBP.m_kCvv);

        // hypotenuse edge subdivision
        akColor[i12] = 0.5f*(akColor[i1] + akColor[i2] - fDSqr*rkBP.m_kChh);
    }

    // textures
    if ( akTexture )
    {
        // bottom u-edge subdivision
        akTexture[i01] = 0.5f*(akTexture[i0]+akTexture[i1]-fDSqr*rkBP.m_kTuu);

        // left v-edge subdivision
        akTexture[i02] = 0.5f*(akTexture[i0]+akTexture[i2]-fDSqr*rkBP.m_kTvv);

        // hypotenuse edge subdivision
        akTexture[i12] = 0.5f*(akTexture[i1]+akTexture[i2]-fDSqr*rkBP.m_kThh);
    }

    // recurse on four children
    if ( iLevel > 0 )
    {
        iLevel--;
        fDSqr *= 0.25f;

        // subtriangle <(s0,t0),(sm,t0),(s0,tm)>
        SubdivideLL(iLevel,fDSqr,akX,akXu,akXv,akColor,akTexture,i0,i01,i02,
            rkBP);

        // subtriangle <(sm,t0),(s1,t0),(sm,tm)>
        SubdivideLL(iLevel,fDSqr,akX,akXu,akXv,akColor,akTexture,i01,i1,i12,
            rkBP);

        // subtriangle <(s0,tm),(sm,tm),(s0,t1)>
        SubdivideLL(iLevel,fDSqr,akX,akXu,akXv,akColor,akTexture,i02,i12,i2,
            rkBP);

        // subtriangle <(sm,t0),(sm,tm),(s0,tm)>
        SubdivideUR(iLevel,fDSqr,akX,akXu,akXv,akColor,akTexture,i01,i12,i02,
            rkBP);
    }
}
//----------------------------------------------------------------------------
void BezierTriangle2::SubdivideUR (int iLevel, float fDSqr, Vector3f* akX,
    Vector3f* akXu, Vector3f* akXv, ColorRGB* akColor, Vector2f* akTexture,
    int i0, int i1, int i2, BlockParameters& rkBP)
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
    int i12 = (i1 + i2) >> 1;
    int iD12 = i1 - i2;
    int i01 = (((i0 + i1) << 2) + iD12*iD12) >> 3;
    int i02 = i01 - (iD12 >> 1);

    // vertices

    // top u-edge subdivision
    akX[i12] = 0.5f*(akX[i1] + akX[i2] - fDSqr*rkBP.m_kXuu);

    // right v-edge subdivision
    akX[i01] = 0.5f*(akX[i0] + akX[i1] - fDSqr*rkBP.m_kXvv);

    // hypotenuse edge subdivision
    akX[i02] = 0.5f*(akX[i0] + akX[i2] - fDSqr*rkBP.m_kXhh);

    // colors
    if ( akColor )
    {
        // top u-edge subdivision
        akColor[i12] = 0.5f*(akColor[i1] + akColor[i2] - fDSqr*rkBP.m_kCuu);

        // right v-edge subdivision
        akColor[i01] = 0.5f*(akColor[i0] + akColor[i1] - fDSqr*rkBP.m_kCvv);

        // hypotenuse edge subdivision
        akColor[i02] = 0.5f*(akColor[i0] + akColor[i2] - fDSqr*rkBP.m_kChh);
    }

    // textures
    if ( akTexture )
    {
        // top u-edge subdivision
        akTexture[i12] = 0.5f*(akTexture[i1]+akTexture[i2]-fDSqr*rkBP.m_kTuu);

        // right v-edge subdivision
        akTexture[i01] = 0.5f*(akTexture[i0]+akTexture[i1]-fDSqr*rkBP.m_kTvv);

        // hypotenuse edge subdivision
        akTexture[i02] = 0.5f*(akTexture[i0]+akTexture[i2]-fDSqr*rkBP.m_kThh);
    }

    // recurse on four children
    if ( iLevel > 0 )
    {
        iLevel--;
        fDSqr *= 0.25f;

        // subtriangle <(sm,tm),(sm,t1),(s0,t1)>
        SubdivideUR(iLevel,fDSqr,akX,akXu,akXv,akColor,akTexture,i02,i12,i2,
            rkBP);

        // subtriangle <(s1,tm),(s1,t1),(sm,t1)>
        SubdivideUR(iLevel,fDSqr,akX,akXu,akXv,akColor,akTexture,i01,i1,i12,
            rkBP);

        // subtriangle <(s1,t0),(s1,tm),(sm,tm)>
        SubdivideUR(iLevel,fDSqr,akX,akXu,akXv,akColor,akTexture,i0,i01,i02,
            rkBP);

        // subtriangle <(sm,tm),(s1,tm),(sm,t1)>
        SubdivideLL(iLevel,fDSqr,akX,akXu,akXv,akColor,akTexture,i02,i01,i12,
            rkBP);
    }
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* BezierTriangle2::Factory (Stream& rkStream)
{
    BezierTriangle2* pkObject = new BezierTriangle2;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void BezierTriangle2::Load (Stream& rkStream, Stream::Link* pkLink)
{
    BezierTriangle::Load(rkStream,pkLink);
}
//----------------------------------------------------------------------------
void BezierTriangle2::Link (Stream& rkStream, Stream::Link* pkLink)
{
    BezierTriangle::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool BezierTriangle2::Register (Stream& rkStream)
{
    return BezierTriangle::Register(rkStream);
}
//----------------------------------------------------------------------------
void BezierTriangle2::Save (Stream& rkStream)
{
    BezierTriangle::Save(rkStream);
}
//----------------------------------------------------------------------------
StringTree* BezierTriangle2::SaveStrings ()
{
    StringTree* pkTree = new StringTree(1,0,1,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));

    // children
    pkTree->SetChild(0,BezierTriangle::SaveStrings());

    return pkTree;
}
//----------------------------------------------------------------------------
int BezierTriangle2::GetMemoryUsed () const
{
    int iBaseSize = sizeof(BezierTriangle2) - sizeof(BezierTriangle);
    int iTotalSize = iBaseSize + BezierTriangle::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int BezierTriangle2::GetDiskUsed () const
{
    return BezierTriangle::GetDiskUsed();
}
//----------------------------------------------------------------------------
