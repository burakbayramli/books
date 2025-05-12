// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIKGoal.h"
#include "WmlSpatial.h"
using namespace Wml;

WmlImplementRTTI(IKGoal,Object);
WmlImplementStream(IKGoal);

//----------------------------------------------------------------------------
IKGoal::IKGoal (Spatial* pkTarget, Spatial* pkEffector, float fWeight)
{
    m_pkTarget = pkTarget;
    m_pkEffector = pkEffector;
    m_fWeight = fWeight;
}
//----------------------------------------------------------------------------
IKGoal::IKGoal ()
{
    m_pkTarget = NULL;
    m_pkEffector = NULL;
    m_fWeight = 1.0f;
}
//----------------------------------------------------------------------------
Vector3f IKGoal::GetPosition () const
{
    return m_pkTarget->WorldTranslate();
}
//----------------------------------------------------------------------------
Vector3f IKGoal::GetEffectorPosition () const
{
    return m_pkEffector->WorldTranslate();
}
//----------------------------------------------------------------------------
float IKGoal::GetNorm () const
{
    Vector3f kDiff = m_pkTarget->WorldTranslate() -
        m_pkEffector->WorldTranslate();
    return kDiff.SquaredLength();
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* IKGoal::Factory (Stream& rkStream)
{
    IKGoal* pkObject = new IKGoal;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void IKGoal::Load (Stream& rkStream, Stream::Link* pkLink)
{
    Object::Load(rkStream,pkLink);

    // native data
    StreamRead(rkStream,m_fWeight);

    // link data
    Spatial* pkTmp;
    StreamRead(rkStream,pkTmp);
    pkLink->Add(pkTmp);
    StreamRead(rkStream,pkTmp);
    pkLink->Add(pkTmp);
}
//----------------------------------------------------------------------------
void IKGoal::Link (Stream& rkStream, Stream::Link* pkLink)
{
    Object::Link(rkStream,pkLink);

    Object* pkLinkID = pkLink->GetLinkID();
    m_pkTarget = (Spatial*)rkStream.GetFromMap(pkLinkID);
    pkLinkID = pkLink->GetLinkID();
    m_pkEffector = (Spatial*)rkStream.GetFromMap(pkLinkID);
}
//----------------------------------------------------------------------------
bool IKGoal::Register (Stream& rkStream)
{
    if ( !Object::Register(rkStream) )
        return false;

    if ( m_pkTarget )
        m_pkTarget->Register(rkStream);

    if ( m_pkEffector )
        m_pkEffector->Register(rkStream);

    return true;
}
//----------------------------------------------------------------------------
void IKGoal::Save (Stream& rkStream)
{
    Object::Save(rkStream);

    // native data
    StreamWrite(rkStream,m_fWeight);

    // link data
    StreamWrite(rkStream,m_pkTarget);
    StreamWrite(rkStream,m_pkEffector);
}
//----------------------------------------------------------------------------
StringTree* IKGoal::SaveStrings ()
{
    StringTree* pkTree = new StringTree(2,0,1,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetString(1,MakeString("weight =",m_fWeight));

    // children
    pkTree->SetChild(0,Object::SaveStrings());

    return pkTree;
}
//----------------------------------------------------------------------------
int IKGoal::GetMemoryUsed () const
{
    int iBaseSize = sizeof(IKGoal) - sizeof(Object);
    int iTotalSize = iBaseSize + Object::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int IKGoal::GetDiskUsed () const
{
    return Object::GetDiskUsed() +
        sizeof(m_fWeight) +
        sizeof(m_pkTarget) +
        sizeof(m_pkEffector);
}
//----------------------------------------------------------------------------
