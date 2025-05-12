// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIKController.h"
#include "WmlIKGoal.h"
#include "WmlIKJoint.h"
using namespace Wml;

WmlImplementRTTI(IKController,Controller);
WmlImplementStream(IKController);

//----------------------------------------------------------------------------
IKController::IKController (int iJointQuantity, IKJoint** apkJoint,
    int iGoalQuantity, IKGoal** apkGoal, int iIterations, ProcessOrder eOrder)
{
    m_iJointQuantity = iJointQuantity;
    m_apkJoint = apkJoint;
    m_iGoalQuantity = iGoalQuantity;
    m_apkGoal = apkGoal;
    m_iIterations = iIterations;
    m_eOrder = eOrder;
}
//----------------------------------------------------------------------------
IKController::IKController ()
{
    m_iJointQuantity = 0;
    m_apkJoint = NULL;
    m_iGoalQuantity = 0;
    m_apkGoal = NULL;
    m_iIterations = 0;
    m_eOrder = PO_END_TO_ROOT;
}
//----------------------------------------------------------------------------
IKController::~IKController ()
{
    int i;

    if ( m_apkJoint )
    {
        for (i = 0; i < m_iJointQuantity; i++)
            delete m_apkJoint[i];
        delete[] m_apkJoint;
    }

    if ( m_apkGoal )
    {
        for (i = 0; i < m_iGoalQuantity; i++)
            delete m_apkGoal[i];
        delete[] m_apkGoal;
    }
}
//----------------------------------------------------------------------------
bool IKController::Update (float)
{
    if ( !Active() )
    {
        // controller does not compute world transform
        return false;
    }

    // Make sure effectors are all current in world space.  It is assumed
    // that the joints form a chain, so the world transforms of joint I
    // are the parent transforms for the joint I+1.
    int iJoint;
    for (iJoint = 0; iJoint < m_iJointQuantity; iJoint++)
        m_apkJoint[iJoint]->UpdateWorldSRT();

    // Compute current norm for controlling when an update is accepted.
    float fNorm = 0.0f;
    int iGoal;
    for (iGoal = 0; iGoal < m_iGoalQuantity; iGoal++)
        fNorm += m_apkGoal[iGoal]->GetNorm();

    // Update joints one-at-a-time to meet goals.  As each joint is updated,
    // the nodes occurring in the chain after that joint must be made current
    // in world space.
    int iIter, i, j;
    IKJoint* pkJoint;
    if ( m_eOrder == PO_END_TO_ROOT )
    {
        for (iIter = 0; iIter < m_iIterations; iIter++)
        {
            for (iJoint = 0; iJoint < m_iJointQuantity; iJoint++)
            {
                int iRJoint = m_iJointQuantity - 1 - iJoint;
                pkJoint = m_apkJoint[iRJoint];

                for (i = 0; i < 3; i++)
                {
                    if ( pkJoint->AllowTranslation(i) )
                    {
                        if ( pkJoint->UpdateLocalT(i,m_iGoalQuantity,
                             m_apkGoal,fNorm) )
                        {
                            for (j = iRJoint; j < m_iJointQuantity; j++)
                                m_apkJoint[j]->UpdateWorldRT();
                        }
                    }
                }

                for (i = 0; i < 3; i++)
                {
                    if ( pkJoint->AllowRotation(i) )
                    {
                        if ( pkJoint->UpdateLocalR(i,m_iGoalQuantity,
                             m_apkGoal,fNorm) )
                        {
                            for (j = iRJoint; j < m_iJointQuantity; j++)
                                m_apkJoint[j]->UpdateWorldRT();
                        }
                    }
                }
            }
        }
    }
    else  // m_eOrder == PO_ROOT_TO_END
    {
        for (iIter = 0; iIter < m_iIterations; iIter++)
        {
            for (iJoint = 0; iJoint < m_iJointQuantity; iJoint++)
            {
                pkJoint = m_apkJoint[iJoint];

                for (i = 0; i < 3; i++)
                {
                    if ( pkJoint->AllowTranslation(i) )
                    {
                        if ( pkJoint->UpdateLocalT(i,m_iGoalQuantity,
                             m_apkGoal,fNorm) )
                        {
                            for (j = iJoint; j < m_iJointQuantity; j++)
                                m_apkJoint[j]->UpdateWorldRT();
                        }
                    }
                }

                for (i = 0; i < 3; i++)
                {
                    if ( pkJoint->AllowRotation(i) )
                    {
                        if ( pkJoint->UpdateLocalR(i,m_iGoalQuantity,
                             m_apkGoal,fNorm) )
                        {
                            for (j = iJoint; j < m_iJointQuantity; j++)
                                m_apkJoint[j]->UpdateWorldRT();
                        }
                    }
                }
            }
        }
    }

    // controller does not compute world transform
    return false;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* IKController::Factory (Stream& rkStream)
{
    IKController* pkObject = new IKController;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void IKController::Load (Stream& rkStream, Stream::Link* pkLink)
{
    Controller::Load(rkStream,pkLink);

    // native data
    StreamRead(rkStream,m_iJointQuantity);
    StreamRead(rkStream,m_iGoalQuantity);
    StreamRead(rkStream,m_iIterations);
    StreamReadEnum(rkStream,m_eOrder);

    // link data
    int i;
    if ( m_iJointQuantity > 0 )
    {
        m_apkJoint = new IKJoint*[m_iJointQuantity];
        for (i = 0; i < m_iJointQuantity; i++)
        {
            IKJoint* pkJoint;
            StreamRead(rkStream,pkJoint);
            pkLink->Add(pkJoint);
        }
    }
    else
    {
        m_apkJoint = NULL;
    }

    if ( m_iGoalQuantity > 0 )
    {
        m_apkGoal = new IKGoal*[m_iGoalQuantity];
        for (i = 0; i < m_iGoalQuantity; i++)
        {
            IKGoal* pkGoal;
            StreamRead(rkStream,pkGoal);
            pkLink->Add(pkGoal);
        }
    }
    else
    {
        m_apkGoal = NULL;
    }
}
//----------------------------------------------------------------------------
void IKController::Link (Stream& rkStream, Stream::Link* pkLink)
{
    Controller::Link(rkStream,pkLink);

    Object* pkLinkID;
    int i;
    for (i = 0; i < m_iJointQuantity; i++)
    {
        pkLinkID = pkLink->GetLinkID();
        m_apkJoint[i] = (IKJoint*)rkStream.GetFromMap(pkLinkID);
    }

    for (i = 0; i < m_iGoalQuantity; i++)
    {
        pkLinkID = pkLink->GetLinkID();
        m_apkGoal[i] = (IKGoal*)rkStream.GetFromMap(pkLinkID);
    }
}
//----------------------------------------------------------------------------
bool IKController::Register (Stream& rkStream)
{
    if ( !Controller::Register(rkStream) )
        return false;

    int i;
    for (i = 0; i < m_iJointQuantity; i++)
    {
        assert( m_apkJoint[i] );
        m_apkJoint[i]->Register(rkStream);
    }

    for (i = 0; i < m_iGoalQuantity; i++)
    {
        assert( m_apkGoal[i] );
        m_apkGoal[i]->Register(rkStream);
    }

    return true;
}
//----------------------------------------------------------------------------
void IKController::Save (Stream& rkStream)
{
    Controller::Save(rkStream);

    // native data
    StreamWrite(rkStream,m_iJointQuantity);
    StreamWrite(rkStream,m_iGoalQuantity);
    StreamWrite(rkStream,m_iIterations);
    StreamWriteEnum(rkStream,m_eOrder);

    // link data
    int i;
    for (i = 0; i < m_iJointQuantity; i++)
    {
        assert( m_apkJoint[i] );
        StreamWrite(rkStream,m_apkJoint[i]);
    }

    for (i = 0; i < m_iGoalQuantity; i++)
    {
        assert( m_apkGoal[i] );
        StreamWrite(rkStream,m_apkGoal[i]);
    }
}
//----------------------------------------------------------------------------
StringTree* IKController::SaveStrings ()
{
    StringTree* pkTree = new StringTree(4,0,3,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetString(1,MakeString("joint quantity =",m_iJointQuantity));
    pkTree->SetString(2,MakeString("goal quantity =",m_iGoalQuantity));
    pkTree->SetString(3,MakeString("iterations =",m_iIterations));

    switch ( m_eOrder )
    {
    case PO_END_TO_ROOT:
        pkTree->SetString(3,MakeString("order = END_TO_ROOT"));
        break;
    case PO_ROOT_TO_END:
        pkTree->SetString(3,MakeString("order = ROOT_TO_END"));
        break;
    }

    // children
    pkTree->SetChild(0,Controller::SaveStrings());

    StringTree* pkJTree = new StringTree(1,0,m_iJointQuantity,0);
    pkJTree->SetString(0,MakeString("joints"));
    int i;
    for (i = 0; i < m_iJointQuantity; i++)
        pkJTree->SetChild(i,m_apkJoint[i]->SaveStrings());

    StringTree* pkGTree = new StringTree(1,0,m_iGoalQuantity,0);
    pkGTree->SetString(0,MakeString("goals"));
    for (i = 0; i < m_iGoalQuantity; i++)
        pkGTree->SetChild(i,m_apkGoal[i]->SaveStrings());

    pkTree->SetChild(1,pkJTree);
    pkTree->SetChild(2,pkGTree);

    return pkTree;
}
//----------------------------------------------------------------------------
int IKController::GetMemoryUsed () const
{
    int iBaseSize = sizeof(IKController) - sizeof(Controller);

    int iDynaSize = m_iJointQuantity*sizeof(m_apkJoint[0]) +
        m_iGoalQuantity*sizeof(m_apkGoal[0]);

    int iTotalSize = iBaseSize + iDynaSize + Controller::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int IKController::GetDiskUsed () const
{
    return Controller::GetDiskUsed() +
        sizeof(m_iJointQuantity) +
        sizeof(m_iGoalQuantity) +
        sizeof(m_iIterations) +
        StreamBytesEnum(m_eOrder) +
        m_iJointQuantity*sizeof(m_apkJoint[0]) +
        m_iGoalQuantity*sizeof(m_apkGoal[0]);
}
//----------------------------------------------------------------------------
