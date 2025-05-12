// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlBezierTriangle.h"
#include "WmlTriMesh.h"
using namespace Wml;

WmlImplementRTTI(BezierTriangle,BezierPatch);
WmlImplementStream(BezierTriangle);

//----------------------------------------------------------------------------
BezierTriangle::BezierTriangle (int iDegree, int iIndexQuantity, int* aiIndex)
    :
    BezierPatch(iDegree,iIndexQuantity,aiIndex)
{
}
//----------------------------------------------------------------------------
BezierTriangle::BezierTriangle ()
{
}
//----------------------------------------------------------------------------
int BezierTriangle::GetVerticesPerPatch (int iLevel) const
{
    // V = (2^L+1)*(2^L+2)/2
    int iTwoPowLPlusOne = (1 << iLevel) + 1;
    return iTwoPowLPlusOne*(iTwoPowLPlusOne+1) >> 1;
}
//----------------------------------------------------------------------------
int BezierTriangle::GetTrianglesPerPatch (int iLevel) const
{
    // T = 4^L
    return 1 << 2*iLevel;
}
//----------------------------------------------------------------------------
void BezierTriangle::GenerateConnectivity (int iLevel, TriMesh* pkMesh,
    int& riTriangleStart)
{
    // generate connectivity array and put in proper location in trimesh

    int iTwoPowL = 1 << iLevel;
    int iTwiceTwoPowLPlusOne = (iTwoPowL + 1) << 1;
    int* piConnect = pkMesh->Connectivity() + 3*riTriangleStart;

    for (int iY = 0, iYStart = 0; iY < iTwoPowL; iY++)
    {
        int i0 = iYStart;
        int i1 = i0 + 1;
        iYStart = ((iY + 1)*(iTwiceTwoPowLPlusOne - iY)) >> 1;
        int i2 = iYStart;
        int iSum;
        for (int iX = 0; (iSum = iX + iY) < iTwoPowL; iX++)
        {
            *piConnect++ = i0;
            *piConnect++ = i1;
            *piConnect++ = i2;
            riTriangleStart++;

            if ( iSum + 1 < iTwoPowL )
            {
                *piConnect++ = i1;
                *piConnect++ = i2 + 1;
                *piConnect++ = i2;
                riTriangleStart++;
            }

            i0++;
            i1++;
            i2++;
        }
    }
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* BezierTriangle::Factory (Stream&)
{
    // BezierTriangle is abstract, Factory never called
    return NULL;
}
//----------------------------------------------------------------------------
void BezierTriangle::Load (Stream& rkStream, Stream::Link* pkLink)
{
    BezierPatch::Load(rkStream,pkLink);
}
//----------------------------------------------------------------------------
void BezierTriangle::Link (Stream& rkStream, Stream::Link* pkLink)
{
    BezierPatch::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool BezierTriangle::Register (Stream& rkStream)
{
    return BezierPatch::Register(rkStream);
}
//----------------------------------------------------------------------------
void BezierTriangle::Save (Stream& rkStream)
{
    BezierPatch::Save(rkStream);
}
//----------------------------------------------------------------------------
StringTree* BezierTriangle::SaveStrings ()
{
    StringTree* pkTree = new StringTree(1,0,1,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));

    // children
    pkTree->SetChild(0,BezierPatch::SaveStrings());

    return pkTree;
}
//----------------------------------------------------------------------------
int BezierTriangle::GetMemoryUsed () const
{
    int iBaseSize = sizeof(BezierTriangle) - sizeof(BezierPatch);
    int iTotalSize = iBaseSize + BezierPatch::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int BezierTriangle::GetDiskUsed () const
{
    return BezierPatch::GetDiskUsed();
}
//----------------------------------------------------------------------------
