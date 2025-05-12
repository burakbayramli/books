// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlBezierRectangle.h"
#include "WmlTriMesh.h"
using namespace Wml;

WmlImplementRTTI(BezierRectangle,BezierPatch);
WmlImplementStream(BezierRectangle);

//----------------------------------------------------------------------------
BezierRectangle::BezierRectangle (int iDegree, int iIndexQuantity,
    int* aiIndex)
    :
    BezierPatch(iDegree,iIndexQuantity,aiIndex)
{
}
//----------------------------------------------------------------------------
BezierRectangle::BezierRectangle ()
{
}
//----------------------------------------------------------------------------
int BezierRectangle::GetVerticesPerPatch (int iLevel) const
{
    // V = (2^L+1)^2
    int iTwoPowLPlusOne = (1 << iLevel) + 1;
    return iTwoPowLPlusOne*iTwoPowLPlusOne;
}
//----------------------------------------------------------------------------
int BezierRectangle::GetTrianglesPerPatch (int iLevel) const
{
    // T = 2*4^L
    return 2*(1 << 2*iLevel);
}
//----------------------------------------------------------------------------
void BezierRectangle::GenerateConnectivity (int iLevel, TriMesh* pkMesh,
    int& riTriangleStart)
{
    // generate connectivity array and put in proper location in trimesh

    int iTwoPowL = (1 << iLevel);
    int iTwoPowLPlusOne = iTwoPowL + 1;
    int* piConnect = pkMesh->Connectivity() + 3*riTriangleStart;

    for (int iY = 0, iYStart = 0; iY < iTwoPowL; iY++)
    {
        int i0 = iYStart;
        iYStart += iTwoPowLPlusOne;
        int i1 = i0 + 1;
        int i2 = iYStart;
        int i3 = i2 + 1;
        for (int iX = 0; iX < iTwoPowL; iX++)
        {
            *piConnect++ = i0;
            *piConnect++ = i1;
            *piConnect++ = i2;
            *piConnect++ = i1;
            *piConnect++ = i3;
            *piConnect++ = i2;
            i0++;
            i1++;
            i2++;
            i3++;
            riTriangleStart += 2;
        }
    }
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* BezierRectangle::Factory (Stream&)
{
    // BezierRectangle is abstract, Factory never called
    return NULL;
}
//----------------------------------------------------------------------------
void BezierRectangle::Load (Stream& rkStream, Stream::Link* pkLink)
{
    BezierPatch::Load(rkStream,pkLink);
}
//----------------------------------------------------------------------------
void BezierRectangle::Link (Stream& rkStream, Stream::Link* pkLink)
{
    BezierPatch::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool BezierRectangle::Register (Stream& rkStream)
{
    return BezierPatch::Register(rkStream);
}
//----------------------------------------------------------------------------
void BezierRectangle::Save (Stream& rkStream)
{
    BezierPatch::Save(rkStream);
}
//----------------------------------------------------------------------------
StringTree* BezierRectangle::SaveStrings ()
{
    StringTree* pkTree = new StringTree(1,0,1,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));

    // children
    pkTree->SetChild(0,BezierPatch::SaveStrings());

    return pkTree;
}
//----------------------------------------------------------------------------
int BezierRectangle::GetMemoryUsed () const
{
    int iBaseSize = sizeof(BezierRectangle) - sizeof(BezierPatch);
    int iTotalSize = iBaseSize + BezierPatch::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int BezierRectangle::GetDiskUsed () const
{
    return BezierPatch::GetDiskUsed();
}
//----------------------------------------------------------------------------
