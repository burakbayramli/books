// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlKeyframeController.h"
#include "WmlSpatial.h"
using namespace Wml;

WmlImplementRTTI(KeyframeController,Controller);
WmlImplementStream(KeyframeController);

//----------------------------------------------------------------------------
KeyframeController::KeyframeController ()
{
    m_iTQuantity = 0;
    m_afTTime = NULL;
    m_akTData = NULL;
    m_iTLastIndex = 0;

    m_iRQuantity = 0;
    m_afRTime = NULL;
    m_akRData = NULL;
    m_iRLastIndex = 0;

    m_iSQuantity = 0;
    m_afSTime = NULL;
    m_afSData = NULL;
    m_iSLastIndex = 0;

    m_iSharedQuantity = 0;
    m_afSharedTime = NULL;
    m_iSharedLastIndex = 0;
}
//----------------------------------------------------------------------------
KeyframeController::~KeyframeController ()
{
    delete[] m_akTData;
    delete[] m_akRData;
    delete[] m_afSData;

    if ( m_iSharedQuantity > 0 )
    {
        delete[] m_afSharedTime;
    }
    else
    {
        delete[] m_afTTime;
        delete[] m_afRTime;
        delete[] m_afSTime;
    }
}
//----------------------------------------------------------------------------
void KeyframeController::SetSharedQuantity (int iQuantity)
{
    m_iSharedQuantity = iQuantity;
    m_iTQuantity = iQuantity;
    m_iRQuantity = iQuantity;
    m_iSQuantity = iQuantity;
}
//----------------------------------------------------------------------------
void KeyframeController::SetSharedTimes (float* afTime)
{
    m_afSharedTime = afTime;
    m_afTTime = afTime;
    m_afRTime = afTime;
    m_afSTime = afTime;
}
//----------------------------------------------------------------------------
void KeyframeController::GetKeyInfo (float fCtrlTime, int iQuantity,
    float* afTime, int& riLastIndex, float& rfTime, int& ri0, int& ri1)
{
    if ( fCtrlTime <= afTime[0] )
    {
        rfTime = 0.0f;
        riLastIndex = 0;
        ri0 = 0;
        ri1 = 0;
        return;
    }

    if ( fCtrlTime >= afTime[iQuantity-1] )
    {
        rfTime = 0.0f;
        riLastIndex = iQuantity - 1;
        ri0 = riLastIndex;
        ri1 = riLastIndex;
        return;
    }

    int iNextIndex;
    if ( fCtrlTime > afTime[riLastIndex] )
    {
        iNextIndex = riLastIndex + 1;
        while ( fCtrlTime >= afTime[iNextIndex] )
        {
            riLastIndex = iNextIndex;
            iNextIndex++;
        }

        ri0 = riLastIndex;
        ri1 = iNextIndex;
        rfTime = (fCtrlTime - afTime[ri0])/(afTime[ri1] - afTime[ri0]);
    }
    else if ( fCtrlTime < afTime[riLastIndex] )
    {
        iNextIndex = riLastIndex - 1;
        while ( fCtrlTime <= afTime[iNextIndex] )
        {
            riLastIndex = iNextIndex;
            iNextIndex--;
        }

        ri0 = iNextIndex;
        ri1 = riLastIndex;
        rfTime = (fCtrlTime - afTime[ri0])/(afTime[ri1] - afTime[ri0]);
    }
    else
    {
        rfTime = 0.0f;
        ri0 = riLastIndex;
        ri1 = riLastIndex;
    }
}
//----------------------------------------------------------------------------
Vector3f KeyframeController::GetTranslation (float fNormTime, int i0, int i1)
{
    return m_akTData[i0] + fNormTime*(m_akTData[i1] - m_akTData[i0]);
}
//----------------------------------------------------------------------------
Matrix3f KeyframeController::GetRotation (float fNormTime, int i0, int i1)
{
    Quaternionf kQ = Quaternionf::Slerp(fNormTime,m_akRData[i0],
        m_akRData[i1]);

    Matrix3f kRot;
    kQ.ToRotationMatrix(kRot);
    return kRot;
}
//----------------------------------------------------------------------------
float KeyframeController::GetScale (float fNormTime, int i0, int i1)
{
    return m_afSData[i0] + fNormTime*(m_afSData[i1] - m_afSData[i0]);
}
//----------------------------------------------------------------------------
bool KeyframeController::Update (float fAppTime)
{
    if ( !Active() )
    {
        // controller does not compute world transform
        return false;
    }

    Spatial* pkSpatial = (Spatial*) m_pkObject;
    float fCtrlTime = GetControlTime(fAppTime);
    float fNormTime;
    int i0, i1;

    if ( m_iSharedQuantity > 0 )
    {
        GetKeyInfo(fCtrlTime,m_iSharedQuantity,m_afSharedTime,
            m_iSharedLastIndex,fNormTime,i0,i1);

        if ( m_iTQuantity > 0 )
            pkSpatial->Translate() = GetTranslation(fNormTime,i0,i1);

        if ( m_iRQuantity > 0 )
            pkSpatial->Rotate() = GetRotation(fNormTime,i0,i1);

        if ( m_iSQuantity > 0 )
            pkSpatial->Scale() = GetScale(fNormTime,i0,i1);
    }
    else
    {
        if ( m_iTQuantity > 0 )
        {
            GetKeyInfo(fCtrlTime,m_iTQuantity,m_afTTime,m_iTLastIndex,
                fNormTime,i0,i1);
            pkSpatial->Translate() = GetTranslation(fNormTime,i0,i1);
        }

        if ( m_iRQuantity > 0 )
        {
            GetKeyInfo(fCtrlTime,m_iRQuantity,m_afRTime,m_iRLastIndex,
                fNormTime,i0,i1);
            pkSpatial->Rotate() = GetRotation(fNormTime,i0,i1);
        }

        if ( m_iSQuantity > 0 )
        {
            GetKeyInfo(fCtrlTime,m_iSQuantity,m_afSTime,m_iSLastIndex,
                fNormTime,i0,i1);
            pkSpatial->Scale() = GetScale(fNormTime,i0,i1);
        }
    }

    // controller does not compute world transform
    return false;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* KeyframeController::Factory (Stream& rkStream)
{
    KeyframeController* pkObject = new KeyframeController;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void KeyframeController::Load (Stream& rkStream,
    Stream::Link* pkLink)
{
    Controller::Load(rkStream,pkLink);

    bool bShared;
    StreamReadBool(rkStream,bShared);

    if ( bShared )
    {
        StreamRead(rkStream,m_iSharedQuantity);
        if ( m_iSharedQuantity > 0 )
        {
            m_afSharedTime = new float[m_iSharedQuantity];
            m_akTData = new Vector3f[m_iSharedQuantity];
            m_akRData = new Quaternionf[m_iSharedQuantity];
            m_afSData = new float[m_iSharedQuantity];

            StreamRead(rkStream,m_afSharedTime,m_iSharedQuantity);
            StreamRead(rkStream,m_akTData,m_iSharedQuantity);
            StreamRead(rkStream,m_akRData,m_iSharedQuantity);
            StreamRead(rkStream,m_afSData,m_iSharedQuantity);

            SetSharedQuantity(m_iSharedQuantity);
        }
    }
    else
    {
        StreamRead(rkStream,m_iTQuantity);
        if ( m_iTQuantity > 0 )
        {
            m_afTTime = new float[m_iTQuantity];
            m_akTData = new Vector3f[m_iTQuantity];
            StreamRead(rkStream,m_afTTime,m_iTQuantity);
            StreamRead(rkStream,m_akTData,m_iTQuantity);
        }

        StreamRead(rkStream,m_iRQuantity);
        if ( m_iRQuantity > 0 )
        {
            m_afRTime = new float[m_iRQuantity];
            m_akRData = new Quaternionf[m_iRQuantity];
            StreamRead(rkStream,m_afRTime,m_iRQuantity);
            StreamRead(rkStream,m_akRData,m_iRQuantity);
        }

        StreamRead(rkStream,m_iSQuantity);
        if ( m_iSQuantity > 0 )
        {
            m_afSTime = new float[m_iSQuantity];
            m_afSData = new float[m_iSQuantity];
            StreamRead(rkStream,m_afSTime,m_iSQuantity);
            StreamRead(rkStream,m_afSData,m_iSQuantity);
        }
    }
}
//----------------------------------------------------------------------------
void KeyframeController::Link (Stream& rkStream,
    Stream::Link* pkLink)
{
    Controller::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool KeyframeController::Register (Stream& rkStream)
{
    return Controller::Register(rkStream);
}
//----------------------------------------------------------------------------
void KeyframeController::Save (Stream& rkStream)
{
    Controller::Save(rkStream);

    bool bShared = m_iSharedQuantity > 0;
    StreamWriteBool(rkStream,bShared);

    if ( bShared  )
    {
        StreamWrite(rkStream,m_iSharedQuantity);
        StreamWrite(rkStream,m_afSharedTime,m_iSharedQuantity);
        StreamWrite(rkStream,m_akTData,m_iSharedQuantity);
        StreamWrite(rkStream,m_akRData,m_iSharedQuantity);
        StreamWrite(rkStream,m_afSData,m_iSharedQuantity);
    }
    else
    {
        StreamWrite(rkStream,m_iTQuantity);
        if ( m_iTQuantity > 0 )
        {
            StreamWrite(rkStream,m_afTTime,m_iTQuantity);
            StreamWrite(rkStream,m_akTData,m_iTQuantity);
        }

        StreamWrite(rkStream,m_iRQuantity);
        if ( m_iRQuantity > 0 )
        {
            StreamWrite(rkStream,m_afRTime,m_iRQuantity);
            StreamWrite(rkStream,m_akRData,m_iRQuantity);
        }

        StreamWrite(rkStream,m_iSQuantity);
        if ( m_iSQuantity > 0 )
        {
            StreamWrite(rkStream,m_afSTime,m_iSQuantity);
            StreamWrite(rkStream,m_afSData,m_iSQuantity);
        }
    }
}
//----------------------------------------------------------------------------
StringTree* KeyframeController::SaveStrings ()
{
    int iSQuantity, iCQuantity;
    if ( m_iSharedQuantity > 0 )
    {
        iSQuantity = 2;
        iCQuantity = 5;
    }
    else
    {
        iSQuantity = 4;
        iCQuantity = 7;
    }

    StringTree* pkTree = new StringTree(iSQuantity,0,iCQuantity,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    if ( m_iSharedQuantity > 0 )
    {
        pkTree->SetString(1,MakeString("quantity =",m_iSharedQuantity));
    }
    else
    {
        pkTree->SetString(1,MakeString("translate quantity =",m_iTQuantity));
        pkTree->SetString(2,MakeString("rotate quantity =",m_iRQuantity));
        pkTree->SetString(3,MakeString("scale quantity =",m_iSQuantity));
    }

    // children
    pkTree->SetChild(0,Controller::SaveStrings());

    StringTree* pkAttrTree;
    char acDummy[16];
    int i;

    if ( m_iSharedQuantity > 0 )
    {
        pkAttrTree = new StringTree(m_iSharedQuantity+1,0,0,0);
        pkTree->SetChild(1,pkAttrTree);
        pkAttrTree->SetString(0,MakeString("times"));
        for (i = 0; i < m_iSharedQuantity; i++)
        {
            sprintf(acDummy,"%d:",i);
            pkAttrTree->SetString(i+1,MakeString(acDummy,m_afSharedTime[i]));
        }

        pkAttrTree = new StringTree(m_iSharedQuantity+1,0,0,0);
        pkTree->SetChild(2,pkAttrTree);
        pkAttrTree->SetString(0,MakeString("translate keys"));
        for (i = 0; i < m_iSharedQuantity; i++)
        {
            sprintf(acDummy,"%d:",i);
            pkAttrTree->SetString(i+1,MakeString(acDummy,m_akTData[i]));
        }

        pkAttrTree = new StringTree(m_iSharedQuantity+1,0,0,0);
        pkTree->SetChild(3,pkAttrTree);
        pkAttrTree->SetString(0,MakeString("rotate keys"));
        for (i = 0; i < m_iSharedQuantity; i++)
        {
            sprintf(acDummy,"%d:",i);
            pkAttrTree->SetString(i+1,MakeString(acDummy,m_akRData[i]));
        }

        pkAttrTree = new StringTree(m_iSharedQuantity+1,0,0,0);
        pkTree->SetChild(4,pkAttrTree);
        pkAttrTree->SetString(0,MakeString("scale keys"));
        for (i = 0; i < m_iSharedQuantity; i++)
        {
            sprintf(acDummy,"%d:",i);
            pkAttrTree->SetString(i+1,MakeString(acDummy,m_afSData[i]));
        }
    }
    else
    {
        pkAttrTree = new StringTree(m_iTQuantity+1,0,0,0);
        pkTree->SetChild(1,pkAttrTree);
        pkAttrTree->SetString(0,MakeString("translate times"));
        for (i = 0; i < m_iTQuantity; i++)
        {
            sprintf(acDummy,"%d:",i);
            pkAttrTree->SetString(i+1,MakeString(acDummy,m_afTTime[i]));
        }
        pkAttrTree = new StringTree(m_iTQuantity+1,0,0,0);
        pkTree->SetChild(2,pkAttrTree);
        pkAttrTree->SetString(0,MakeString("translate keys"));
        for (i = 0; i < m_iTQuantity; i++)
        {
            sprintf(acDummy,"%d:",i);
            pkAttrTree->SetString(i+1,MakeString(acDummy,m_akTData[i]));
        }

        pkAttrTree = new StringTree(m_iRQuantity+1,0,0,0);
        pkTree->SetChild(3,pkAttrTree);
        pkAttrTree->SetString(0,MakeString("rotate times"));
        for (i = 0; i < m_iRQuantity; i++)
        {
            sprintf(acDummy,"%d:",i);
            pkAttrTree->SetString(i+1,MakeString(acDummy,m_afRTime[i]));
        }
        pkAttrTree = new StringTree(m_iRQuantity+1,0,0,0);
        pkTree->SetChild(4,pkAttrTree);
        pkAttrTree->SetString(0,MakeString("rotate keys"));
        for (i = 0; i < m_iRQuantity; i++)
        {
            sprintf(acDummy,"%d:",i);
            pkAttrTree->SetString(i+1,MakeString(acDummy,m_akRData[i]));
        }

        pkAttrTree = new StringTree(m_iSQuantity+1,0,0,0);
        pkTree->SetChild(5,pkAttrTree);
        pkAttrTree->SetString(0,MakeString("scale times"));
        for (i = 0; i < m_iSQuantity; i++)
        {
            sprintf(acDummy,"%d:",i);
            pkAttrTree->SetString(i+1,MakeString(acDummy,m_afSTime[i]));
        }
        pkAttrTree = new StringTree(m_iSQuantity+1,0,0,0);
        pkTree->SetChild(6,pkAttrTree);
        pkAttrTree->SetString(0,MakeString("scale keys"));
        for (i = 0; i < m_iSQuantity; i++)
        {
            sprintf(acDummy,"%d:",i);
            pkAttrTree->SetString(i+1,MakeString(acDummy,
                m_afSData[i]));
        }
    }

    return pkTree;
}
//----------------------------------------------------------------------------
int KeyframeController::GetMemoryUsed () const
{
    int iBaseSize = sizeof(KeyframeController) - sizeof(Controller);

    int iDynaSize = 0;
    if ( m_iSharedQuantity > 0 )
    {
        iDynaSize += m_iSharedQuantity*sizeof(m_afSharedTime[0]);
        iDynaSize += m_iSharedQuantity*sizeof(m_akTData[0]);
        iDynaSize += m_iSharedQuantity*sizeof(m_akRData[0]);
        iDynaSize += m_iSharedQuantity*sizeof(m_afSData[0]);
    }
    else
    {
        iDynaSize += m_iTQuantity*(sizeof(m_afTTime[0])+sizeof(m_akTData[0]));
        iDynaSize += m_iRQuantity*(sizeof(m_afRTime[0])+sizeof(m_akRData[0]));
        iDynaSize += m_iSQuantity*(sizeof(m_afSTime[0])+sizeof(m_afSData[0]));
    }

    int iTotalSize = iBaseSize + iDynaSize + Controller::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int KeyframeController::GetDiskUsed () const
{
    int iSize = Controller::GetDiskUsed();

    bool bShared = m_iSharedQuantity > 0;
    iSize += StreamBytesBool(bShared);

    if ( bShared  )
    {
        iSize += sizeof(m_iSharedQuantity);
        iSize += m_iSharedQuantity*sizeof(m_afSharedTime[0]);
        iSize += m_iSharedQuantity*sizeof(m_akTData[0]);
        iSize += m_iSharedQuantity*sizeof(m_akRData[0]);
        iSize += m_iSharedQuantity*sizeof(m_afSData[0]);
    }
    else
    {
        iSize += sizeof(m_iTQuantity);
        if ( m_iTQuantity > 0 )
        {
            iSize += m_iTQuantity*sizeof(m_afTTime[0]);
            iSize += m_iTQuantity*sizeof(m_akTData[0]);
        }

        iSize += sizeof(m_iRQuantity);
        if ( m_iRQuantity > 0 )
        {
            iSize += m_iRQuantity*sizeof(m_afRTime[0]);
            iSize += m_iRQuantity*sizeof(m_akRData[0]);
        }

        iSize += sizeof(m_iSQuantity);
        if ( m_iSQuantity > 0 )
        {
            iSize += m_iSQuantity*sizeof(m_afSTime[0]);
            iSize += m_iSQuantity*sizeof(m_afSData[0]);
        }
    }

    return iSize;
}
//----------------------------------------------------------------------------
