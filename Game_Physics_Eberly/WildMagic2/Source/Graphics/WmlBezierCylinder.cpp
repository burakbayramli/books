// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlBezierCylinder.h"
#include "WmlTriMesh.h"
using namespace Wml;

WmlImplementRTTI(BezierCylinder,BezierPatch);
WmlImplementStream(BezierCylinder);

//----------------------------------------------------------------------------
BezierCylinder::BezierCylinder (int iDegree, int iIndexQuantity,
    int* aiIndex)
    :
    BezierPatch(iDegree,iIndexQuantity,aiIndex)
{
    m_iCylinderLevel = 0;
}
//----------------------------------------------------------------------------
BezierCylinder::BezierCylinder ()
{
}
//----------------------------------------------------------------------------
int BezierCylinder::GetVerticesPerPatch (int iLevel) const
{
    // V = (2^L+1)*(2^C+1)
    int iTwoPowLPlusOne = (1 << iLevel) + 1;
    int iTwoPowCPlusOne = (1 << m_iCylinderLevel) + 1;
    return iTwoPowLPlusOne*iTwoPowCPlusOne;
}
//----------------------------------------------------------------------------
int BezierCylinder::GetTrianglesPerPatch (int iLevel) const
{
    // T = 2*2^L*2^C
    int iTwoPowL = (1 << iLevel);
    int iTwoPowC = (1 << m_iCylinderLevel);
    return 2*iTwoPowL*iTwoPowC;
}
//----------------------------------------------------------------------------
void BezierCylinder::GenerateConnectivity (int iLevel, TriMesh* pkMesh,
    int& riTriangleStart)
{
    // generate connectivity array and put in proper location in trimesh

    int iTwoPowL = (1 << iLevel);
    int iTwoPowLPlusOne = iTwoPowL + 1;
    int iTwoPowC = (1 << m_iCylinderLevel);
    int* piConnect = pkMesh->Connectivity() + 3*riTriangleStart;
    for (int iY = 0, iYStart = 0; iY < iTwoPowC; iY++)
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
        }
    }
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* BezierCylinder::Factory (Stream&)
{
    // BezierCylinder is abstract, Factory never called
    return NULL;
}
//----------------------------------------------------------------------------
void BezierCylinder::Load (Stream& rkStream, Stream::Link* pkLink)
{
    BezierPatch::Load(rkStream,pkLink);

    // native data
    StreamRead(rkStream,m_iCylinderLevel);
}
//----------------------------------------------------------------------------
void BezierCylinder::Link (Stream& rkStream, Stream::Link* pkLink)
{
    BezierPatch::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool BezierCylinder::Register (Stream& rkStream)
{
    return BezierPatch::Register(rkStream);
}
//----------------------------------------------------------------------------
void BezierCylinder::Save (Stream& rkStream)
{
    BezierPatch::Save(rkStream);

    // native data
    StreamWrite(rkStream,m_iCylinderLevel);
}
//----------------------------------------------------------------------------
StringTree* BezierCylinder::SaveStrings ()
{
    StringTree* pkTree = new StringTree(2,0,1,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetString(1,MakeString("cyln level =",m_iCylinderLevel));

    // children
    pkTree->SetChild(0,BezierPatch::SaveStrings());

    return pkTree;
}
//----------------------------------------------------------------------------
int BezierCylinder::GetMemoryUsed () const
{
    int iBaseSize = sizeof(BezierCylinder) - sizeof(BezierPatch);
    int iTotalSize = iBaseSize + BezierPatch::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int BezierCylinder::GetDiskUsed () const
{
    return BezierPatch::GetDiskUsed() +
        sizeof(m_iCylinderLevel);
}
//----------------------------------------------------------------------------
