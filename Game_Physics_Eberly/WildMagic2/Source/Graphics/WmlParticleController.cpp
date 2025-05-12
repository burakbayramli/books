// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlParticleController.h"
#include "WmlParticles.h"
using namespace Wml;

WmlImplementRTTI(ParticleController,Controller);
WmlImplementStream(ParticleController);

//----------------------------------------------------------------------------
ParticleController::ParticleController ()
{
    m_fSystemLinearSpeed = 0.0f;
    m_fSystemAngularSpeed = 0.0f;
    m_kSystemLinearAxis = Vector3f::UNIT_Z;
    m_kSystemAngularAxis = Vector3f::UNIT_Z;
    m_afPointLinearSpeed = NULL;
    m_afPointAngularSpeed = NULL;
    m_akPointLinearAxis = NULL;
    m_akPointAngularAxis = NULL;
    m_fSystemSizeChange = 0.0f;
    m_afPointSizeChange = NULL;
}
//----------------------------------------------------------------------------
ParticleController::~ParticleController ()
{
    delete[] m_afPointLinearSpeed;
    delete[] m_afPointAngularSpeed;
    delete[] m_akPointLinearAxis;
    delete[] m_akPointAngularAxis;
    delete[] m_afPointSizeChange;
}
//----------------------------------------------------------------------------
void ParticleController::Reallocate (int iVertexQuantity)
{
    m_fSystemLinearSpeed = 0.0f;
    m_fSystemAngularSpeed = 0.0f;
    m_kSystemLinearAxis = Vector3f::UNIT_Z;
    m_kSystemAngularAxis = Vector3f::UNIT_Z;
    m_fSystemSizeChange = 0.0f;

    delete[] m_afPointLinearSpeed;
    delete[] m_afPointAngularSpeed;
    delete[] m_akPointLinearAxis;
    delete[] m_akPointAngularAxis;
    delete[] m_afPointSizeChange;

    if ( iVertexQuantity > 0 )
    {
        m_afPointLinearSpeed = new float[iVertexQuantity];
        m_afPointAngularSpeed = new float[iVertexQuantity];
        m_akPointLinearAxis = new Vector3f[iVertexQuantity];
        m_akPointAngularAxis = new Vector3f[iVertexQuantity];
        m_afPointSizeChange = new float[iVertexQuantity];

        memset(m_afPointLinearSpeed,0,iVertexQuantity*sizeof(float));
        memset(m_afPointAngularSpeed,0,iVertexQuantity*sizeof(float));
        for (int i = 0; i < iVertexQuantity; i++)
        {
            m_akPointLinearAxis[i] = Vector3f::UNIT_Z;
            m_akPointAngularAxis[i] = Vector3f::UNIT_Z;
        }
        memset(m_afPointSizeChange,0,iVertexQuantity*sizeof(float));
    }
    else
    {
        m_afPointLinearSpeed = NULL;
        m_afPointAngularSpeed = NULL;
        m_akPointLinearAxis = NULL;
        m_akPointAngularAxis = NULL;
        m_afPointSizeChange = NULL;
    }
}
//----------------------------------------------------------------------------
void ParticleController::SetObject (Object* pkObject)
{
    assert( WmlIsDerivedFromClass(Particles,pkObject) );

    Controller::SetObject(pkObject);

    if ( pkObject )
        Reallocate(((Particles*)pkObject)->GetVertexQuantity());
    else
        Reallocate(0);
}
//----------------------------------------------------------------------------
void ParticleController::UpdateSystemMotion (float fCtrlTime)
{
    Particles* pkParticle = (Particles*) m_pkObject;

    float fDSize = fCtrlTime*m_fSystemSizeChange;
    pkParticle->SizeAdjust() += fDSize;
    if ( pkParticle->SizeAdjust() < 0.0f )
        pkParticle->SizeAdjust() = 0.0f;

    float fDistance = fCtrlTime*m_fSystemLinearSpeed;
    Vector3f kDTrn = fDistance*m_kSystemLinearAxis;
    pkParticle->Translate() += kDTrn;

    float fAngle = fCtrlTime*m_fSystemAngularSpeed;
    Matrix3f kDRot(m_kSystemAngularAxis,fAngle);
    pkParticle->Rotate() = kDRot*pkParticle->Rotate();
}
//----------------------------------------------------------------------------
void ParticleController::UpdatePointMotion (float fCtrlTime)
{
    Particles* pkParticle = (Particles*) m_pkObject;
    int i, iVertexQuantity = pkParticle->GetActiveQuantity();
    float* afSize = pkParticle->Sizes();

    for (i = 0; i < iVertexQuantity; i++)
    {
        float fDSize = fCtrlTime*m_afPointSizeChange[i];
        afSize[i] += fDSize;
    }

    Vector3f* akVertex = pkParticle->Vertices();
    for (i = 0; i < iVertexQuantity; i++)
    {
        float fDistance = fCtrlTime*m_afPointLinearSpeed[i];
        Vector3f kDTrn = fDistance*m_akPointLinearAxis[i];
        akVertex[i] += kDTrn;
    }

    Vector3f* akNormal = pkParticle->Normals();
    if ( akNormal )
    {
        for (i = 0; i < iVertexQuantity; i++)
        {
            float fAngle = fCtrlTime*m_afPointAngularSpeed[i];
            Matrix3f kDRot(m_akPointAngularAxis[i],fAngle);
            akNormal[i] = kDRot*akNormal[i];

            // TO DO.  Since the normals are being updated in-place, after a
            // few rotations the numerical errors might build up.  The
            // unitizing is designed to fix the errors in length.  Arrange
            // for this to happen on a less frequent basis.
            akNormal[i].Normalize();
        }
    }
}
//----------------------------------------------------------------------------
bool ParticleController::Update (float fAppTime)
{
    if ( !Active() )
    {
        // controller does not compute world transform
        return false;
    }

    float fCtrlTime = GetControlTime(fAppTime);

    UpdateSystemMotion(fCtrlTime);
    UpdatePointMotion(fCtrlTime);

    // controller does not compute world transform
    return false;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* ParticleController::Factory (Stream& rkStream)
{
    ParticleController* pkObject = new ParticleController;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void ParticleController::Load (Stream& rkStream,
    Stream::Link* pkLink)
{
    Controller::Load(rkStream,pkLink);

    // NOTE.  ParticleController was derived from PointController.  The
    // PointController::Save wrote various quantities to disk that are now
    // managed by ParticleController.  These quantities are written to disk
    // in the same order, so no special handling must occur here based on the
    // stream version number.
    // native data
    StreamRead(rkStream,m_fSystemLinearSpeed);
    StreamRead(rkStream,m_fSystemAngularSpeed);
    StreamRead(rkStream,m_kSystemLinearAxis);
    StreamRead(rkStream,m_kSystemAngularAxis);

    int iVertexQuantity;
    StreamRead(rkStream,iVertexQuantity);
    Reallocate(iVertexQuantity);
    StreamRead(rkStream,m_afPointLinearSpeed,iVertexQuantity);
    StreamRead(rkStream,m_afPointAngularSpeed,iVertexQuantity);
    StreamRead(rkStream,m_akPointLinearAxis,iVertexQuantity);
    StreamRead(rkStream,m_akPointAngularAxis,iVertexQuantity);
    StreamRead(rkStream,m_fSystemSizeChange);

    if ( rkStream.GetVersion() < Version(1,4) )
    {
        // The vertex quantity had been written to disk twice, once by
        // PointController and once by ParticleController.  Read it and
        // discard since it was loaded above.
        int iDiscard;
        StreamRead(rkStream,iDiscard);
    }

    StreamRead(rkStream,m_afPointSizeChange,iVertexQuantity);
}
//----------------------------------------------------------------------------
void ParticleController::Link (Stream& rkStream,
    Stream::Link* pkLink)
{
    Controller::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool ParticleController::Register (Stream& rkStream)
{
    return Controller::Register(rkStream);
}
//----------------------------------------------------------------------------
void ParticleController::Save (Stream& rkStream)
{
    Controller::Save(rkStream);

    // native data
    StreamWrite(rkStream,m_fSystemLinearSpeed);
    StreamWrite(rkStream,m_fSystemAngularSpeed);
    StreamWrite(rkStream,m_kSystemLinearAxis);
    StreamWrite(rkStream,m_kSystemAngularAxis);

    // Write this to disk so that Load does not have to wait until the
    // controlled object is loaded and linked in order to allocate the
    // arrays.
    Particles* pkParticle = (Particles*)m_pkObject;
    int iVertexQuantity = pkParticle->GetVertexQuantity();
    StreamWrite(rkStream,iVertexQuantity);

    StreamWrite(rkStream,m_afPointLinearSpeed,iVertexQuantity);
    StreamWrite(rkStream,m_afPointAngularSpeed,iVertexQuantity);
    StreamWrite(rkStream,m_akPointLinearAxis,iVertexQuantity);
    StreamWrite(rkStream,m_akPointAngularAxis,iVertexQuantity);
    StreamWrite(rkStream,m_fSystemSizeChange);
    StreamWrite(rkStream,m_afPointSizeChange,iVertexQuantity);
}
//----------------------------------------------------------------------------
StringTree* ParticleController::SaveStrings ()
{
    // TO DO.  Finish implementation.
    StringTree* pkTree = new StringTree(1,0,1,0);
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetChild(0,Controller::SaveStrings());
    return pkTree;
}
//----------------------------------------------------------------------------
int ParticleController::GetMemoryUsed () const
{
    int iBaseSize = sizeof(ParticleController) - sizeof(Controller);

    Particles* pkParticle = (Particles*) m_pkObject;
    int iVertexQuantity = pkParticle->GetVertexQuantity();
    int iDynaSize = iVertexQuantity*sizeof(m_afPointLinearSpeed[0]);
    iDynaSize += iVertexQuantity*sizeof(m_afPointAngularSpeed[0]);
    iDynaSize += iVertexQuantity*sizeof(m_akPointLinearAxis[0]);
    iDynaSize += iVertexQuantity*sizeof(m_akPointAngularAxis[0]);
    iDynaSize += iVertexQuantity*sizeof(m_afPointSizeChange[0]);

    int iTotalSize = iBaseSize + iDynaSize + Controller::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int ParticleController::GetDiskUsed () const
{
    int iSize = Controller::GetDiskUsed() +
        sizeof(m_fSystemLinearSpeed) +
        sizeof(m_fSystemAngularSpeed) +
        sizeof(m_kSystemLinearAxis) +
        sizeof(m_kSystemAngularAxis) +
        sizeof(m_fSystemSizeChange);

    Particles* pkParticle = (Particles*) m_pkObject;
    int iVertexQuantity = pkParticle->GetVertexQuantity();

    iSize += sizeof(iVertexQuantity);
    iSize += iVertexQuantity*sizeof(m_afPointLinearSpeed[0]);
    iSize += iVertexQuantity*sizeof(m_afPointAngularSpeed[0]);
    iSize += iVertexQuantity*sizeof(m_akPointLinearAxis[0]);
    iSize += iVertexQuantity*sizeof(m_akPointAngularAxis[0]);
    iSize += iVertexQuantity*sizeof(m_afPointSizeChange[0]);

    return iSize;
}
//----------------------------------------------------------------------------
