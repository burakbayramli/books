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
#include "WmlIKJoint.h"
#include "WmlNode.h"
#include "WmlMath.h"
#include "WmlMatrix3.h"
using namespace Wml;

WmlImplementRTTI(IKJoint,Object);
WmlImplementStream(IKJoint);

//----------------------------------------------------------------------------
IKJoint::IKJoint (Spatial* pkObject)
{
    m_pkObject = pkObject;

    for (int i = 0; i < 3; i++)
    {
        m_abAllowTrn[i] = false;
        m_afMinTrn[i] = -FLT_MAX;
        m_afMaxTrn[i] = FLT_MAX;
        m_afDampTrn[i] = 1.0f;

        m_abAllowRot[i] = false;
        m_afMinRot[i] = -Mathf::PI;
        m_afMaxRot[i] = Mathf::PI;
        m_afDampRot[i] = 1.0f;
    }
}
//----------------------------------------------------------------------------
void IKJoint::SetTranslationRange (int i, float fMin, float fMax)
{
    assert( 0 <= i && i < 3 );
    assert( fMin <= fMax );

    m_afMinTrn[i] = fMin;
    m_afMaxTrn[i] = fMax;
}
//----------------------------------------------------------------------------
void IKJoint::SetRotationRange (int i, float fMin, float fMax)
{
    assert( 0 <= i && i < 3 );
    assert( -Mathf::PI <= fMin && fMin <= fMax && fMax <= Mathf::PI );

    m_afMinRot[i] = fMin;
    m_afMaxRot[i] = fMax;
}
//----------------------------------------------------------------------------
void IKJoint::UpdateWorldSRT ()
{
    float fWorldScale;
    Matrix3f kWorldRotate;
    Vector3f kWorldTranslate;

    Node* pkParent = m_pkObject->GetParent();
    if ( pkParent )
    {
        fWorldScale = pkParent->WorldScale()*m_pkObject->Scale();
        kWorldRotate = pkParent->WorldRotate()*m_pkObject->Rotate();
        kWorldTranslate = pkParent->WorldTranslate() +
            pkParent->WorldScale()*(pkParent->WorldRotate() *
            m_pkObject->Translate());
    }
    else
    {
        fWorldScale = m_pkObject->Scale();
        kWorldRotate = m_pkObject->Rotate();
        kWorldTranslate = m_pkObject->Translate();
    }

    m_pkObject->WorldScale() = fWorldScale;
    m_pkObject->WorldRotate() = kWorldRotate;
    m_pkObject->WorldTranslate() = kWorldTranslate;
}
//----------------------------------------------------------------------------
void IKJoint::UpdateWorldRT ()
{
    Matrix3f kWorldRotate;
    Vector3f kWorldTranslate;

    Node* pkParent = m_pkObject->GetParent();
    if ( pkParent )
    {
        kWorldRotate = pkParent->WorldRotate()*m_pkObject->Rotate();
        kWorldTranslate = pkParent->WorldTranslate() +
            pkParent->WorldScale()*(pkParent->WorldRotate() *
            m_pkObject->Translate());
    }
    else
    {
        kWorldRotate = m_pkObject->Rotate();
        kWorldTranslate = m_pkObject->Translate();
    }

    m_pkObject->WorldRotate() = kWorldRotate;
    m_pkObject->WorldTranslate() = kWorldTranslate;
}
//----------------------------------------------------------------------------
Vector3f IKJoint::GetAxis (int i)
{
    Node* pkParent = m_pkObject->GetParent();
    if ( pkParent )
        return pkParent->WorldRotate().GetColumn(i);
    else if ( i == 0 )
        return Vector3f::UNIT_X;
    else if ( i == 1 )
        return Vector3f::UNIT_Y;
    else  //  i == 2
        return Vector3f::UNIT_Z;
}
//----------------------------------------------------------------------------
bool IKJoint::UpdateLocalT (int i, int iGoalQuantity, IKGoal** apkGoal,
    float& rfCurrentNorm)
{
    Vector3f kAxis = GetAxis(i);
    float fNumer = 0.0f;
    float fDenom = 0.0f;

    IKGoal* pkGoal;
    int iG;
    for (iG = 0; iG < iGoalQuantity; iG++)
    {
        pkGoal = apkGoal[iG];
        Vector3f kGmE = pkGoal->GetPosition() - pkGoal->GetEffectorPosition();
        fNumer += kAxis.Dot(kGmE)*pkGoal->Weight();
        fDenom += pkGoal->Weight();
    }

    if ( Mathf::FAbs(fDenom) <= Mathf::EPSILON )
    {
        // weights were too small, no translation
        return false;
    }

    float fDelta = fNumer/fDenom;

    // apply damping
    if ( m_afDampTrn[i] < 1.0f )
        fDelta *= m_afDampTrn[i];

    // get temporary hold on translation
    Vector3f kTrn = m_pkObject->Translate();

    // clamp to range
    float fDesired = kTrn[i] + fDelta;
    if ( fDesired > m_afMinTrn[i] )
    {
        if ( fDesired < m_afMaxTrn[i] )
        {
            kTrn[i] = fDesired;
        }
        else
        {
            fDelta = m_afMaxTrn[i] - kTrn[i];
            kTrn[i] = m_afMaxTrn[i];
        }
    }
    else
    {
        fDelta = m_afMinTrn[i] - kTrn[i];
        kTrn[i] = m_afMinTrn[i];
    }

    // test if step should be taken
    Vector3f kStep = fDelta*kAxis;

    float fNorm = 0.0f;
    for (iG = 0; iG < iGoalQuantity; iG++)
    {
        pkGoal = apkGoal[iG];
        Vector3f kNewE = pkGoal->GetEffectorPosition() + kStep;
        Vector3f kDiff = pkGoal->GetPosition() - kNewE;
        fNorm += kDiff.SquaredLength();
    }

    if ( fNorm >= rfCurrentNorm )
    {
        // translation does not get effector closer to goal
        return false;
    }

    rfCurrentNorm = fNorm;

    // update the local translation
    m_pkObject->Translate() = kTrn;
    return true;
}
//----------------------------------------------------------------------------
bool IKJoint::UpdateLocalR (int i, int iGoalQuantity, IKGoal** apkGoal,
    float& rfCurrentNorm)
{
    Vector3f kAxis = GetAxis(i);
    float fNumer = 0.0f;
    float fDenom = 0.0f;

    IKGoal* pkGoal;
    int iG;
    for (iG = 0; iG < iGoalQuantity; iG++)
    {
        pkGoal = apkGoal[iG];
        Vector3f kEmP = pkGoal->GetEffectorPosition() -
            m_pkObject->WorldTranslate();
        Vector3f kGmP = pkGoal->GetPosition() - m_pkObject->WorldTranslate();
        Vector3f kAxEmP = kAxis.Cross(kEmP);
        Vector3f kAxAxEmP = kAxis.Cross(kAxEmP);
        fNumer += kGmP.Dot(kAxEmP)*pkGoal->Weight();
        fDenom -= kGmP.Dot(kAxAxEmP)*pkGoal->Weight();
    }

    if ( fNumer*fNumer + fDenom*fDenom <= Mathf::EPSILON )
    {
        // undefined atan2, no rotation
        return false;
    }

    // desired angle to rotate about axis(i)
    float fDelta = Mathf::ATan2(fNumer,fDenom);

    // apply damping
    if ( m_afDampRot[i] < 1.0f )
        fDelta *= m_afDampRot[i];

    // factor local rotation into Euler angles
    float afEuler[3];
    m_pkObject->Rotate().ToEulerAnglesZYX(afEuler[2],afEuler[1],afEuler[0]);

    // clamp to range
    float fDesired = afEuler[i] + fDelta;
    if ( fDesired > m_afMinRot[i] )
    {
        if ( fDesired < m_afMaxRot[i] )
        {
            afEuler[i] = fDesired;
        }
        else
        {
            fDelta = m_afMaxRot[i] - afEuler[i];
            afEuler[i] = m_afMaxRot[i];
        }
    }
    else
    {
        fDelta = m_afMinRot[i] - afEuler[i];
        afEuler[i] = m_afMinRot[i];
    }

    // test if step should be taken
    Matrix3f kRot(kAxis,fDelta);

    float fNorm = 0.0f;
    for (iG = 0; iG < iGoalQuantity; iG++)
    {
        pkGoal = apkGoal[iG];
        Vector3f kEmP = pkGoal->GetEffectorPosition() -
            m_pkObject->WorldTranslate();
        Vector3f kNewE = m_pkObject->WorldTranslate() + kRot*kEmP;
        Vector3f kDiff = pkGoal->GetPosition() - kNewE;
        fNorm += kDiff.SquaredLength();
    }

    if ( fNorm >= rfCurrentNorm )
    {
        // rotation does not get effector closer to goal
        return false;
    }

    rfCurrentNorm = fNorm;

    // update the local rotation
    m_pkObject->Rotate().FromEulerAnglesZYX(afEuler[2],afEuler[1],afEuler[0]);
    return true;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* IKJoint::Factory (Stream& rkStream)
{
    IKJoint* pkObject = new IKJoint;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void IKJoint::Load (Stream& rkStream, Stream::Link* pkLink)
{
    Object::Load(rkStream,pkLink);

    // native data
    StreamReadBool(rkStream,m_abAllowTrn,3);
    StreamRead(rkStream,m_afMinTrn,3);
    StreamRead(rkStream,m_afMaxTrn,3);
    StreamRead(rkStream,m_afDampTrn,3);

    StreamReadBool(rkStream,m_abAllowRot,3);
    StreamRead(rkStream,m_afMinRot,3);
    StreamRead(rkStream,m_afMaxRot,3);
    StreamRead(rkStream,m_afDampRot,3);

    // link data
    Spatial* pkObject;
    StreamRead(rkStream,pkObject);
    pkLink->Add(pkObject);
}
//----------------------------------------------------------------------------
void IKJoint::Link (Stream& rkStream, Stream::Link* pkLink)
{
    Object::Link(rkStream,pkLink);

    Object* pkLinkID = pkLink->GetLinkID();
    m_pkObject = (Spatial*)rkStream.GetFromMap(pkLinkID);
}
//----------------------------------------------------------------------------
bool IKJoint::Register (Stream& rkStream)
{
    if ( !Object::Register(rkStream) ) 
        return false;

    if ( m_pkObject )
        m_pkObject->Register(rkStream);

    return true;
}
//----------------------------------------------------------------------------
void IKJoint::Save (Stream& rkStream)
{
    Object::Save(rkStream);

    // native data
    StreamWriteBool(rkStream,m_abAllowTrn,3);
    StreamWrite(rkStream,m_afMinTrn,3);
    StreamWrite(rkStream,m_afMaxTrn,3);
    StreamWrite(rkStream,m_afDampTrn,3);

    StreamWriteBool(rkStream,m_abAllowRot,3);
    StreamWrite(rkStream,m_afMinRot,3);
    StreamWrite(rkStream,m_afMaxRot,3);
    StreamWrite(rkStream,m_afDampRot,3);

    // link data
    StreamWrite(rkStream,m_pkObject);
}
//----------------------------------------------------------------------------
StringTree* IKJoint::SaveStrings ()
{
    StringTree* pkTree = new StringTree(9,0,1,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));

    char acDummy[128];
    int i;
    Vector3f kTmp;

    strcpy(acDummy,"( ");
    for (i = 0; i < 3; i++)
    {
        if ( m_abAllowTrn[i] )
            strcat(acDummy,"true ");
        else
            strcat(acDummy,"false ");
    }
    strcat(acDummy,")");

    pkTree->SetString(1,MakeString("allow trn =",acDummy));

    kTmp = Vector3f(m_afMinTrn[0],m_afMinTrn[1],m_afMinTrn[2]);
    pkTree->SetString(2,MakeString("min trn =",kTmp));
    kTmp = Vector3f(m_afMaxTrn[0],m_afMaxTrn[1],m_afMaxTrn[2]);
    pkTree->SetString(3,MakeString("max trn =",kTmp));
    kTmp = Vector3f(m_afDampTrn[0],m_afDampTrn[1],m_afDampTrn[2]);
    pkTree->SetString(4,MakeString("damp trn =",kTmp));

    strcpy(acDummy,"( ");
    for (i = 0; i < 3; i++)
    {
        if ( m_abAllowRot[i] )
            strcat(acDummy,"true ");
        else
            strcat(acDummy,"false ");
    }
    strcat(acDummy,")");

    pkTree->SetString(5,MakeString("allow rot =",acDummy));

    kTmp = Vector3f(m_afMinRot[0],m_afMinRot[1],m_afMinRot[2]);
    pkTree->SetString(6,MakeString("min rot =",kTmp));
    kTmp = Vector3f(m_afMaxRot[0],m_afMaxRot[1],m_afMaxRot[2]);
    pkTree->SetString(7,MakeString("max rot =",kTmp));
    kTmp = Vector3f(m_afDampTrn[0],m_afDampRot[1],m_afDampRot[2]);
    pkTree->SetString(8,MakeString("damp rot =",kTmp));

    // children
    pkTree->SetChild(0,Object::SaveStrings());

    return pkTree;
}
//----------------------------------------------------------------------------
int IKJoint::GetMemoryUsed () const
{
    int iBaseSize = sizeof(IKJoint) - sizeof(Object);
    int iTotalSize = iBaseSize + Object::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int IKJoint::GetDiskUsed () const
{
    return Object::GetDiskUsed() +
        3*StreamBytesBool(m_abAllowTrn[0]) +
        3*sizeof(m_afMinTrn[0]) +
        3*sizeof(m_afMaxTrn[0]) +
        3*sizeof(m_afDampTrn[0]) +
        3*StreamBytesBool(m_abAllowRot[0]) +
        3*sizeof(m_afMinRot[0]) +
        3*sizeof(m_afMaxRot[0]) +
        3*sizeof(m_afDampRot[0]) +
        sizeof(m_pkObject);
}
//----------------------------------------------------------------------------
