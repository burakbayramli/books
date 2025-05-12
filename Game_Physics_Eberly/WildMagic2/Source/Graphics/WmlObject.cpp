// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlObject.h"
#include "WmlController.h"
using namespace Wml;

WmlImplementRootRTTI;
WmlImplementRootStream;

unsigned int Object::ms_uiTotalObjects = 0;
unsigned int Object::ms_uiNextID = 0;

//----------------------------------------------------------------------------
Object::Object ()
{
    m_uiReferences = 0;
    m_spkControl = NULL;
    m_acName = NULL;
    m_uiID = ms_uiNextID++;
    ms_uiTotalObjects++;
}
//----------------------------------------------------------------------------
Object::~Object ()
{
    m_spkControl = NULL;
    delete[] m_acName;
    ms_uiTotalObjects--;
}
//----------------------------------------------------------------------------
void Object::SetName (const char* acName)
{
    delete[] m_acName;

    if ( acName )
    {
        m_acName = new char[strlen(acName)+1];
        strcpy(m_acName,acName);
    }
    else
    {
        m_acName = NULL;
    }
}
//----------------------------------------------------------------------------
Object* Object::GetObjectByName (const char* acName)
{
    if ( m_acName && acName && strcmp(m_acName,acName) == 0 )
        return this;
    else
        return NULL;
}
//----------------------------------------------------------------------------
void Object::GetAllObjectsByName (const char* acName,
    std::vector<Object*>& rkObjects)
{
    if ( m_acName && acName && strcmp(m_acName,acName) == 0 )
        rkObjects.push_back(this);
}
//----------------------------------------------------------------------------
bool Object::AttachControl (Controller* pkControl)
{
    if ( pkControl && !pkControl->m_pkObject && !pkControl->GetNext() )
    {

        pkControl->SetObject(this);
        Controller* pkRootControl =
            WmlSmartPointerCast(Controller,m_spkControl);
        pkControl->SetNext(pkRootControl);
        m_spkControl = pkControl;
        return true;
    }
    return false;
}
//----------------------------------------------------------------------------
bool Object::DetachControl (Controller* pkControl)
{
    if ( pkControl && m_spkControl )
    {
        if ( pkControl == m_spkControl )
        {
            // controller at front of list, remove it
            pkControl->SetObject(NULL);
            ControllerPtr spkSave = pkControl;  // prevent delete
            m_spkControl = pkControl->GetNext();
            pkControl->SetNext(NULL);

            // TO DO. If the reference count for pkControl is 1, when the
            // function returns, spkSave's destructor will delete pkControl.
            // Warn the application about this?
            return true;
        }

        // search for controller
        Controller* pkPrevious = WmlSmartPointerCast(Controller,m_spkControl);
        Controller* pkCurrent = pkPrevious->GetNext();
        while ( pkCurrent && pkCurrent != pkControl )
        {
            pkPrevious = pkCurrent;
            pkCurrent = pkCurrent->GetNext();
        }
        if ( pkCurrent )
        {
            // found the controller, remove it
            pkControl->SetObject(NULL);
            ControllerPtr spkSave = pkControl;  // prevent delete
            pkPrevious->SetNext(pkControl->GetNext());
            pkControl->SetNext(NULL);

            // TO DO. If the reference count for pkControl is 1, when the
            // function returns, spkSave's destructor will delete pkControl.
            // Warn the application about this?
            return true;
        }
    }
    return false;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* Object::Factory (Stream&)
{
    // Object is abstract, Factory never called
    return NULL;
}
//----------------------------------------------------------------------------
void Object::Load (Stream& rkStream, Stream::Link* pkLink)
{
    // get old address of object, save it for linking phase
    Object* pkLinkID;
    StreamRead(rkStream,pkLinkID);
    rkStream.InsertInMap(pkLinkID,pkLink);

    // native data
    rkStream.Read(m_acName);

    // link data
    Object* pkControl;
    StreamRead(rkStream,pkControl);
    pkLink->Add(pkControl);
}
//----------------------------------------------------------------------------
void Object::Link (Stream& rkStream, Stream::Link* pkLink)
{
    Object* pkLinkID = pkLink->GetLinkID();
    m_spkControl = (Controller*)rkStream.GetFromMap(pkLinkID);
}
//----------------------------------------------------------------------------
bool Object::Register (Stream& rkStream)
{
    if ( rkStream.InsertInMap(this,NULL) )
    {
        // Used to ensure the objects are saved in the order corresponding to
        // a depth-first traversal of the scene.
        rkStream.InsertInOrdered(this);

        if ( m_spkControl )
            m_spkControl->Register(rkStream);
        return true;
    }

    return false;
}
//----------------------------------------------------------------------------
void Object::Save (Stream& rkStream)
{
    // run-time class name for factory lookup
    rkStream.Write(GetRTTI()->GetName());

    // address for unique link ID
    StreamWrite(rkStream,this);

    // native data
    rkStream.Write(m_acName);

    // link data
    StreamWrite(rkStream,m_spkControl);
}
//----------------------------------------------------------------------------
StringTree* Object::SaveStrings ()
{
    int iCQuantity = ( m_spkControl ? 1 : 0 );
    StringTree* pkTree = new StringTree(4,0,iCQuantity,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetString(1,MakeString("this =",this));
    pkTree->SetString(2,MakeString("ID   =",m_uiID));
    pkTree->SetString(3,MakeString("refs =",m_uiReferences));

    // children
    if ( m_spkControl )
    {
        Controller* pkControl = WmlSmartPointerCast(Controller,m_spkControl);
        iCQuantity = 0;
        while ( pkControl )
        {
            iCQuantity++;
            pkControl = pkControl->GetNext();
        }

        StringTree* pkCTree = new StringTree(1,0,iCQuantity,0);
        pkTree->SetChild(0,pkCTree);
        pkCTree->SetString(0,MakeString("controllers"));
        int iSlot = 0;
        pkControl = WmlSmartPointerCast(Controller,m_spkControl);
        while ( pkControl )
        {
            pkCTree->SetChild(iSlot++,pkControl->SaveStrings());
            pkControl = pkControl->GetNext();
        }
    }

    return pkTree;
}
//----------------------------------------------------------------------------
int Object::GetMemoryUsed () const
{
    int iBaseSize = sizeof(Object);
    int iDynaSize = ( m_acName ? (int)strlen(m_acName)+1 : 0 );
    return iBaseSize + iDynaSize;
}
//----------------------------------------------------------------------------
int Object::GetDiskUsed () const
{
    return
        StreamBytesString(GetRTTI()->GetName()) +
        sizeof(this) +
        StreamBytesString(m_acName) +
        sizeof(m_spkControl);
}
//----------------------------------------------------------------------------
