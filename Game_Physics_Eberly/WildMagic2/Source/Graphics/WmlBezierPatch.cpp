// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlBezierPatch.h"
using namespace Wml;

WmlImplementRTTI(BezierPatch,Object);
WmlImplementStream(BezierPatch);

//----------------------------------------------------------------------------
BezierPatch::BezierPatch (int iDegree, int iIndexQuantity, int* aiIndex)
{
    assert( iDegree > 0 && iIndexQuantity >= 3 && aiIndex );

    m_iDegree = iDegree;
    m_iIndexQuantity = iIndexQuantity;
    m_aiIndex = aiIndex;
}
//----------------------------------------------------------------------------
BezierPatch::BezierPatch ()
{
    m_iDegree = 0;
    m_iIndexQuantity = 0;
    m_aiIndex = NULL;
}
//----------------------------------------------------------------------------
BezierPatch::~BezierPatch ()
{
    delete[] m_aiIndex;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* BezierPatch::Factory (Stream&)
{
    // BezierPatch is abstract, Factory never called
    return NULL;
}
//----------------------------------------------------------------------------
void BezierPatch::Load (Stream& rkStream, Stream::Link* pkLink)
{
    Object::Load(rkStream,pkLink);

    // native data
    StreamRead(rkStream,m_iDegree);
    StreamRead(rkStream,m_iIndexQuantity);
    m_aiIndex = new int[m_iIndexQuantity];
    StreamRead(rkStream,m_aiIndex,m_iIndexQuantity);
}
//----------------------------------------------------------------------------
void BezierPatch::Link (Stream& rkStream, Stream::Link* pkLink)
{
    Object::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool BezierPatch::Register (Stream& rkStream)
{
    return Object::Register(rkStream);
}
//----------------------------------------------------------------------------
void BezierPatch::Save (Stream& rkStream)
{
    Object::Save(rkStream);

    // native data
    StreamWrite(rkStream,m_iDegree);
    StreamWrite(rkStream,m_iIndexQuantity);
    StreamWrite(rkStream,m_aiIndex,m_iIndexQuantity);
}
//----------------------------------------------------------------------------
StringTree* BezierPatch::SaveStrings ()
{
    StringTree* pkTree = new StringTree(4,0,1,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetString(1,MakeString("degree =",m_iDegree));
    pkTree->SetString(2,MakeString("index quantity =",m_iIndexQuantity));

    char acString[512];  // big enough to hold 16 patch indices
    acString[0] = 0;
    char acDummy[32];
    for (int i = 0; i < m_iIndexQuantity; i++)
    {
        sprintf(acDummy,"%d",m_aiIndex[i]);
        strcat(acString,acDummy);
    }

    pkTree->SetString(3,MakeString("indices =",acString));

    // children
    pkTree->SetChild(0,Object::SaveStrings());

    return pkTree;
}
//----------------------------------------------------------------------------
int BezierPatch::GetMemoryUsed () const
{
    int iBaseSize = sizeof(BezierPatch) - sizeof(Object);
    int iDynaSize = m_iIndexQuantity*sizeof(m_aiIndex[0]);
    int iTotalSize = iBaseSize + iDynaSize + Object::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int BezierPatch::GetDiskUsed () const
{
    return Object::GetDiskUsed() +
        sizeof(m_iDegree) +
        sizeof(m_iIndexQuantity) +
        m_iIndexQuantity*sizeof(m_aiIndex[0]);
}
//----------------------------------------------------------------------------
