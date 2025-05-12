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
#include "WmlStream.h"
using namespace Wml;
using namespace std;

const char Stream::ms_acTopLevel[] = "Top Level";

//----------------------------------------------------------------------------
Stream::Stream ()
{
    m_iBufferSize = 0;
    m_iBufferNext = 0;
    m_acBuffer = NULL;
}
//----------------------------------------------------------------------------
Stream::~Stream ()
{
    RemoveAll();
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// top-level object management
//----------------------------------------------------------------------------
bool Stream::Insert (Object* pkObject)
{
    if ( pkObject  )
    {
        // An object can only be inserted once.
        for (int i = 0; i < (int)m_apkTopLevel.size(); i++)
        {
            if ( pkObject == m_apkTopLevel[i] )
                return false;
        }

        m_apkTopLevel.push_back(pkObject);
        pkObject->IncrementReferences();
        return true;
    }

    return false;
}
//----------------------------------------------------------------------------
bool Stream::Remove (Object* pkObject)
{
    if ( pkObject )
    {
        vector<Object*>::iterator pkIter = m_apkTopLevel.begin();
        while ( pkIter != m_apkTopLevel.end() )
        {
            if ( pkObject == *pkIter )
            {
                m_apkTopLevel.erase(pkIter);
                pkObject->DecrementReferences();
                return true;
            }

            pkIter++;
        }
    }

    return false;
}
//----------------------------------------------------------------------------
void Stream::RemoveAll ()
{
    for (int i = 0; i < (int)m_apkTopLevel.size(); i++)
        m_apkTopLevel[i]->DecrementReferences();

    m_apkTopLevel.clear();
}
//----------------------------------------------------------------------------
int Stream::GetObjectCount ()
{
    return (int)m_apkTopLevel.size();
}
//----------------------------------------------------------------------------
Object* Stream::GetObjectAt (int i) const
{
    if ( i < (int)m_apkTopLevel.size() )
        return m_apkTopLevel[i];
    else
        return NULL;
}
//----------------------------------------------------------------------------
bool Stream::IsTopLevel (Object* pkObject)
{
    for (int i = 0; i < (int)m_apkTopLevel.size(); i++)
    {
        if ( pkObject == m_apkTopLevel[i] )
            return true;
    }
    return false;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// memory streaming
//----------------------------------------------------------------------------
bool Stream::Load (char* acBuffer, int iSize, int iNext)
{
    assert( m_kMap.empty() && m_kOrdered.empty() );

    // clear out all previous top level objects
    RemoveAll();

    // Memory blocks are always current version.  The file loader calls
    // Stream::Load, but with iNext = Version::LENGTH > 0, so iNext == 0
    // means this is a memory block load.  This means the caller of
    // Stream::Load for a memory block should *not* set iNext.
    if ( iNext == 0 )
        m_kVersion = Version(Version::MAJOR,Version::MINOR);

    // initialize the internal Stream buffer
    m_iBufferSize = iSize;
    m_iBufferNext = iNext;
    m_acBuffer = acBuffer;

    // load list of unique objects
    Object* pkObject;
    while ( m_iBufferNext < m_iBufferSize )
    {
        // read "Top Level" or RTTI name
        char* acString;
        Read(acString);
        bool bTopLevel = ( strcmp(acString,ms_acTopLevel) == 0 );
        if ( bTopLevel )
        {
            // read RTTI name
            delete[] acString;
            Read(acString);
        }

        // get the factory function for the type of object about to be read
        map<string,FactoryFunction>::iterator kFIter =
            Object::ms_pkFactory->find(string(acString));
        assert( kFIter != Object::ms_pkFactory->end() );
        if ( kFIter == Object::ms_pkFactory->end() )
        {
            // cannot find factory function
            delete[] acString;
            return false;
        }

        // load the object
        FactoryFunction oFactory = kFIter->second;
        pkObject = oFactory(*this);

        // keep track of all top level objects for application use
        if ( bTopLevel )
            Insert(pkObject);

        delete[] acString;
    }

    // link the objects
    Link* pkLink;
    map<Object*,void*>::iterator kOIter;
    for (kOIter = m_kMap.begin(); kOIter != m_kMap.end(); kOIter++)
    {
        pkLink = (Link*)kOIter->second;
        pkObject = pkLink->GetObject();
        pkObject->Link(*this,pkLink);
    }

    // delete the stream link records
    for (kOIter = m_kMap.begin(); kOIter != m_kMap.end(); kOIter++)
    {
        pkLink = (Link*)kOIter->second;
        delete pkLink;
    }

    m_kMap.clear();
    m_kOrdered.clear();

    return true;
}
//----------------------------------------------------------------------------
bool Stream::Save (char*& racBuffer, int& riSize)
{
    assert( m_kMap.empty() && m_kOrdered.empty() );

    // build list of unique objects
    int i;
    for (i = 0; i < (int)m_apkTopLevel.size(); i++)
    {
        assert( m_apkTopLevel[i] );
        m_apkTopLevel[i]->Register(*this);
    }

    // accumulate object bytes used
    m_iBufferSize = ((int)m_apkTopLevel.size()) *
        StreamBytesString(ms_acTopLevel);
    for (i = 0; i < (int)m_kOrdered.size(); i++)
        m_iBufferSize += m_kOrdered[i]->GetDiskUsed();

    m_acBuffer = new char[m_iBufferSize];
    m_iBufferNext = 0;

    // save list of unique objects
    for (i = 0; i < (int)m_kOrdered.size(); i++)
    {
        Object* pkObject = m_kOrdered[i];
        if ( IsTopLevel(pkObject) )
            Write(ms_acTopLevel);
        pkObject->Save(*this);
    }

    // reduce memory, prepare for other calls
    m_kMap.clear();
    m_kOrdered.clear();

    // transfer ownership of buffer to caller
    racBuffer = m_acBuffer;
    riSize = m_iBufferSize;
    m_acBuffer = NULL;
    m_iBufferSize = 0;
    return true;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// file streaming
//----------------------------------------------------------------------------
bool Stream::Load (const char* acFilename)
{
    struct stat kStat;
    int iResult = stat(acFilename,&kStat);
    if ( iResult != 0 || kStat.st_size < Version::LENGTH )
    {
        // file does not exist or not large enough to store version string
        return false;
    }

    FILE* pkFile = fopen(acFilename,"rb");
    assert( pkFile );

    int iSize = kStat.st_size;
    char* acBuffer = new char[iSize];
    int iRead = (int)fread(acBuffer,sizeof(char),iSize,pkFile);
    assert( iRead == iSize );

    // get the file version
    m_kVersion = Version(acBuffer);
    if ( !m_kVersion.IsValid() )
    {
        delete[] acBuffer;
        fclose(pkFile);
        return false;
    }

    Load(acBuffer,iSize,Version::LENGTH);

    delete[] acBuffer;
    fclose(pkFile);
    return true;
}
//----------------------------------------------------------------------------
bool Stream::Save (const char* acFilename)
{
    FILE* pkFile = fopen(acFilename,"wb");
    if ( !pkFile )
        return false;

    int iSize;
    char* acBuffer;
    Save(acBuffer,iSize);

    // set the file version
    int iWrite = (int)fwrite(Version::CURRENT,sizeof(char),Version::LENGTH,
        pkFile);
    assert( iWrite == Version::LENGTH );

    // write the scene graph
    iWrite = (int)fwrite(acBuffer,sizeof(char),iSize,pkFile);
    assert( iWrite == iSize );

    delete[] acBuffer;
    fclose(pkFile);
    return true;
}
//----------------------------------------------------------------------------
bool Stream::SaveText (const char* acFilename, int iTabSize)
{
    int iCQuantity = GetObjectCount();
    StringTree kRoot(1,0,iCQuantity,0);
    kRoot.SetString(0,MakeString(acFilename));

    int iSlot = 0;
    for (int i = 0; i < iCQuantity; i++)
    {
        Object* pkObject = m_apkTopLevel[i];
        assert( pkObject );
        kRoot.SetChild(iSlot++,pkObject->SaveStrings());
    }

    return kRoot.Save(acFilename,iTabSize);
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// support functions
//----------------------------------------------------------------------------
bool Stream::InsertInMap (Object* pkKey, void* pvValue)
{
    pair<map<Object*,void*>::iterator,bool> kIter = m_kMap.insert(
        make_pair(pkKey,pvValue));

    return kIter.second;
}
//----------------------------------------------------------------------------
Object* Stream::GetFromMap (Object* pkLinkID)
{
    map<Object*,void*>::iterator kOIter = m_kMap.find(pkLinkID);
    if ( kOIter != m_kMap.end() )
    {
        Link* pkLink = (Link*) kOIter->second;
        return pkLink->GetObject();
    }
    else
    {
        return NULL;
    }
}
//----------------------------------------------------------------------------
void Stream::InsertInOrdered (Object* pkObject)
{
    m_kOrdered.push_back(pkObject);
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// memory and disk usage
//----------------------------------------------------------------------------
int Stream::GetMemoryUsed ()
{
    // build list of unique objects
    int i;
    for (i = 0; i < (int)m_apkTopLevel.size(); i++)
    {
        if ( m_apkTopLevel[i] )
            m_apkTopLevel[i]->Register(*this);
    }

    // accumulate object bytes used
    int iSize = 0;
    for (i = 0; i < (int)m_kOrdered.size(); i++)
        iSize += m_kOrdered[i]->GetMemoryUsed();

    // clear out the structures (reduce memory, prepare for other calls)
    m_kMap.clear();
    m_kOrdered.clear();

    return iSize;
}
//----------------------------------------------------------------------------
int Stream::GetDiskUsed ()
{
    // build list of unique objects
    int i;
    for (i = 0; i < (int)m_apkTopLevel.size(); i++)
    {
        if ( m_apkTopLevel[i] )
            m_apkTopLevel[i]->Register(*this);
    }

    // accumulate object bytes used
    int iSize = 0;
    for (i = 0; i < (int)m_kOrdered.size(); i++)
        iSize += m_kOrdered[i]->GetDiskUsed();

    // clear out the structures (reduce memory, prepare for other calls)
    m_kMap.clear();
    m_kOrdered.clear();

    return iSize;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// string read/write
//----------------------------------------------------------------------------
void Stream::Read (char*& racString)
{
    int iLength;
    StreamRead(*this,iLength);

    if ( iLength > 0 )
    {
        assert( m_iBufferNext + iLength <= m_iBufferSize );
        racString = new char[iLength+1];
        char* acSrc = m_acBuffer + m_iBufferNext;
        memcpy(racString,acSrc,iLength);
        m_iBufferNext += iLength;
        racString[iLength] = 0;
    }
    else
    {
        racString = NULL;
    }
}
//----------------------------------------------------------------------------
void Stream::Write (const char* acString)
{
    int iLength = ( acString ? (int)strlen(acString) : 0 );
    StreamWrite(*this,iLength);

    if ( iLength > 0 )
    {
        assert( m_iBufferNext + iLength <= m_iBufferSize );
        char* acDest = m_acBuffer + m_iBufferNext;
        memcpy(acDest,acString,iLength);
        m_iBufferNext += iLength;
    }
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// Stream::Link member functions
//----------------------------------------------------------------------------
Stream::Link::Link (Object* pkObject)
{
    m_pkObject = pkObject;
    m_iCurrent = 0;
}
//----------------------------------------------------------------------------
void Stream::Link::Add (Object* pkLinkID)
{
    m_akLinkID.push_back(pkLinkID);
}
//----------------------------------------------------------------------------
void Stream::Link::SetObject (Object* pkObject)
{
    m_pkObject = pkObject;
}
//----------------------------------------------------------------------------
Object* Stream::Link::GetObject ()
{
    return m_pkObject;
}
//----------------------------------------------------------------------------
int Stream::Link::GetQuantity () const
{
    return (int)m_akLinkID.size();
}
//----------------------------------------------------------------------------
Object* Stream::Link::GetLinkID ()
{
    assert( m_iCurrent < (int)m_akLinkID.size() );
    return m_akLinkID[m_iCurrent++];
}
//----------------------------------------------------------------------------


