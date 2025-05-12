// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlController.h"
using namespace Wml;

WmlImplementRTTI(Controller,Object);
WmlImplementStream(Controller);

//----------------------------------------------------------------------------
Controller::Controller ()
{
    m_eRepeat = RT_CLAMP;
    m_fMinTime = 0.0f;
    m_fMaxTime = 0.0f;
    m_fPhase = 0.0f;
    m_fFrequency = 1.0f;
    m_pkObject = NULL;
    m_spkNext = NULL;
    m_bActive = true;
}
//----------------------------------------------------------------------------
Controller::~Controller ()
{
    m_spkNext = NULL;
}
//----------------------------------------------------------------------------
void Controller::SetObject (Object* pkObject)
{
    m_pkObject = pkObject;
}
//----------------------------------------------------------------------------
float Controller::GetControlTime (float fAppTime)
{
    float fCtrlTime = m_fFrequency*fAppTime + m_fPhase;

    if ( m_eRepeat == RT_CLAMP )
    {
        if ( fCtrlTime < m_fMinTime )
            return m_fMinTime;
        if ( fCtrlTime > m_fMaxTime )
            return m_fMaxTime;
        return fCtrlTime;
    }

    float fRange = m_fMaxTime - m_fMinTime;
    if ( fRange > 0.0f )
    {
        float fMultiples = (fCtrlTime - m_fMinTime)/fRange;
        float fIntTime = floorf(fMultiples);
        float fFrcTime = fMultiples - fIntTime;
        if ( m_eRepeat == RT_WRAP )
            return m_fMinTime + fFrcTime*fRange;

        // m_eRepeat == RT_CYCLE
        if ( int(fIntTime) & 1 )
        {
            // backward time
            return m_fMaxTime - fFrcTime*fRange;
        }
        else
        {
            // forward time
            return m_fMinTime + fFrcTime*fRange;
        }
    }
    else
    {
        return m_fMinTime;
    }
}
//----------------------------------------------------------------------------
Object* Controller::GetObjectByName (const char* acName)
{
    Object* pkFound = Object::GetObjectByName(acName);
    if ( pkFound )
        return pkFound;

    pkFound = m_pkObject->GetObjectByName(acName);
    if ( pkFound )
        return pkFound;

    if ( m_spkNext )
    {
        pkFound = m_spkNext->GetObjectByName(acName);
        if ( pkFound )
            return pkFound;
    }

    return NULL;
}
//----------------------------------------------------------------------------
void Controller::GetAllObjectsByName (const char* acName,
    std::vector<Object*>& rkObjects)
{
    Object::GetAllObjectsByName(acName,rkObjects);
    m_pkObject->GetAllObjectsByName(acName,rkObjects);

    if ( m_spkNext )
        m_spkNext->GetAllObjectsByName(acName,rkObjects);
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* Controller::Factory (Stream&)
{
    // Controller is abstract, Factory never called
    return NULL;
}
//----------------------------------------------------------------------------
void Controller::Load (Stream& rkStream, Stream::Link* pkLink)
{
    Object::Load(rkStream,pkLink);

    // native data
    StreamReadEnum(rkStream,m_eRepeat);
    StreamRead(rkStream,m_fMinTime);
    StreamRead(rkStream,m_fMaxTime);
    StreamRead(rkStream,m_fPhase);
    StreamRead(rkStream,m_fFrequency);

    // link data
    Object* pkObject;
    StreamRead(rkStream,pkObject);
    pkLink->Add(pkObject);

    Controller* pkNext;
    StreamRead(rkStream,pkNext);
    pkLink->Add(pkNext);
}
//----------------------------------------------------------------------------
void Controller::Link (Stream& rkStream, Stream::Link* pkLink)
{
    Object::Link(rkStream,pkLink);

    Object* pkLinkID = pkLink->GetLinkID();
    m_pkObject = rkStream.GetFromMap(pkLinkID);

    pkLinkID = pkLink->GetLinkID();
    m_spkNext = (Controller*)rkStream.GetFromMap(pkLinkID);
}
//----------------------------------------------------------------------------
bool Controller::Register (Stream& rkStream)
{
    if ( !Object::Register(rkStream) )
        return false;

    if ( m_pkObject )
        m_pkObject->Register(rkStream);

    if ( m_spkNext )
        m_spkNext->Register(rkStream);

    return true;
}
//----------------------------------------------------------------------------
void Controller::Save (Stream& rkStream)
{
    Object::Save(rkStream);

    // native data
    StreamWriteEnum(rkStream,m_eRepeat);
    StreamWrite(rkStream,m_fMinTime);
    StreamWrite(rkStream,m_fMaxTime);
    StreamWrite(rkStream,m_fPhase);
    StreamWrite(rkStream,m_fFrequency);

    // link data
    StreamWrite(rkStream,m_pkObject);
    StreamWrite(rkStream,m_spkNext);
}
//----------------------------------------------------------------------------
StringTree* Controller::SaveStrings ()
{
    StringTree* pkTree = new StringTree(5,0,1,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetString(1,MakeString("min time =",m_fMinTime));
    pkTree->SetString(2,MakeString("max time =",m_fMaxTime));
    pkTree->SetString(3,MakeString("phase =",m_fPhase));
    pkTree->SetString(4,MakeString("frequency =",m_fFrequency));

    // children
    pkTree->SetChild(0,Object::SaveStrings());

    // Object will iterate over controllers to save strings

    return pkTree;
}
//----------------------------------------------------------------------------
int Controller::GetMemoryUsed () const
{
    int iBaseSize = sizeof(Controller) - sizeof(Object);
    int iTotalSize = iBaseSize + Object::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int Controller::GetDiskUsed () const
{
    return Object::GetDiskUsed() +
        StreamBytesEnum(m_eRepeat) +
        sizeof(m_fMinTime) +
        sizeof(m_fMaxTime) +
        sizeof(m_fPhase) +
        sizeof(m_fFrequency) +
        sizeof(m_pkObject) +
        sizeof(m_spkNext);
}
//----------------------------------------------------------------------------
