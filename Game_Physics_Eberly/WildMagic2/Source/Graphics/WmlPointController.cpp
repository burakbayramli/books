// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlPointController.h"
#include "WmlPolypoint.h"
using namespace Wml;

WmlImplementRTTI(PointController,Controller);
WmlImplementStream(PointController);

//----------------------------------------------------------------------------
PointController::PointController ()
{
    m_fSystemLinearSpeed = 0.0f;
    m_fSystemAngularSpeed = 0.0f;
    m_kSystemLinearAxis = Vector3f::UNIT_Z;
    m_kSystemAngularAxis = Vector3f::UNIT_Z;
    m_afPointLinearSpeed = NULL;
    m_afPointAngularSpeed = NULL;
    m_akPointLinearAxis = NULL;
    m_akPointAngularAxis = NULL;
}
//----------------------------------------------------------------------------
PointController::~PointController ()
{
    delete[] m_afPointLinearSpeed;
    delete[] m_afPointAngularSpeed;
    delete[] m_akPointLinearAxis;
    delete[] m_akPointAngularAxis;
}
//----------------------------------------------------------------------------
void PointController::Reallocate (int iVertexQuantity)
{
    m_fSystemLinearSpeed = 0.0f;
    m_fSystemAngularSpeed = 0.0f;
    m_kSystemLinearAxis = Vector3f::UNIT_Z;
    m_kSystemAngularAxis = Vector3f::UNIT_Z;

    delete[] m_afPointLinearSpeed;
    delete[] m_afPointAngularSpeed;
    delete[] m_akPointLinearAxis;
    delete[] m_akPointAngularAxis;

    if ( iVertexQuantity > 0 )
    {
        m_afPointLinearSpeed = new float[iVertexQuantity];
        m_afPointAngularSpeed = new float[iVertexQuantity];
        m_akPointLinearAxis = new Vector3f[iVertexQuantity];
        m_akPointAngularAxis = new Vector3f[iVertexQuantity];

        memset(m_afPointLinearSpeed,0,iVertexQuantity*sizeof(float));
        memset(m_afPointAngularSpeed,0,iVertexQuantity*sizeof(float));
        for (int i = 0; i < iVertexQuantity; i++)
        {
            m_akPointLinearAxis[i] = Vector3f::UNIT_Z;
            m_akPointAngularAxis[i] = Vector3f::UNIT_Z;
        }
    }
    else
    {
        m_afPointLinearSpeed = NULL;
        m_afPointAngularSpeed = NULL;
        m_akPointLinearAxis = NULL;
        m_akPointAngularAxis = NULL;
    }
}
//----------------------------------------------------------------------------
void PointController::SetObject (Object* pkObject)
{
    assert( WmlIsDerivedFromClass(Polypoint,pkObject) );

    Controller::SetObject(pkObject);

    if ( pkObject )
        Reallocate(((Polypoint*)pkObject)->GetVertexQuantity());
    else
        Reallocate(0);
}
//----------------------------------------------------------------------------
void PointController::UpdateSystemMotion (float fCtrlTime)
{
    Polypoint* pkPoint = (Polypoint*) m_pkObject;

    float fDistance = fCtrlTime*m_fSystemLinearSpeed;
    Vector3f kDTrn = fDistance*m_kSystemLinearAxis;
    pkPoint->Translate() += kDTrn;

    float fAngle = fCtrlTime*m_fSystemAngularSpeed;
    Matrix3f kDRot(m_kSystemAngularAxis,fAngle);
    pkPoint->Rotate() = kDRot*pkPoint->Rotate();
}
//----------------------------------------------------------------------------
void PointController::UpdatePointMotion (float fCtrlTime)
{
    Polypoint* pkPoint = (Polypoint*) m_pkObject;
    int i, iVertexQuantity = pkPoint->GetActiveQuantity();
    Vector3f* akVertex = pkPoint->Vertices();

    for (i = 0; i < iVertexQuantity; i++)
    {
        float fDistance = fCtrlTime*m_afPointLinearSpeed[i];
        Vector3f kDTrn = fDistance*m_akPointLinearAxis[i];
        akVertex[i] += kDTrn;
    }

    Vector3f* akNormal = pkPoint->Normals();
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
bool PointController::Update (float fAppTime)
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
Object* PointController::Factory (Stream& rkStream)
{
    PointController* pkObject = new PointController;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void PointController::Load (Stream& rkStream, Stream::Link* pkLink)
{
    Controller::Load(rkStream,pkLink);

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
}
//----------------------------------------------------------------------------
void PointController::Link (Stream& rkStream, Stream::Link* pkLink)
{
    Controller::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool PointController::Register (Stream& rkStream)
{
    return Controller::Register(rkStream);
}
//----------------------------------------------------------------------------
void PointController::Save (Stream& rkStream)
{
    Controller::Save(rkStream);

    // native data
    StreamWrite(rkStream,m_fSystemLinearSpeed);
    StreamWrite(rkStream,m_fSystemAngularSpeed);
    StreamWrite(rkStream,m_kSystemLinearAxis);
    StreamWrite(rkStream,m_kSystemAngularAxis);

    Polypoint* pkPoint = (Polypoint*) m_pkObject;
    int iVertexQuantity = pkPoint->GetVertexQuantity();

    // Write this to disk so that Load does not have to wait until the
    // controlled object is loaded and linked in order to allocate the
    // arrays.
    StreamWrite(rkStream,iVertexQuantity);

    StreamWrite(rkStream,m_afPointLinearSpeed,iVertexQuantity);
    StreamWrite(rkStream,m_afPointAngularSpeed,iVertexQuantity);
    StreamWrite(rkStream,m_akPointLinearAxis,iVertexQuantity);
    StreamWrite(rkStream,m_akPointAngularAxis,iVertexQuantity);
}
//----------------------------------------------------------------------------
StringTree* PointController::SaveStrings ()
{
    // TO DO.  Finish implementation.
    StringTree* pkTree = new StringTree(1,0,1,0);
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetChild(0,Controller::SaveStrings());
    return pkTree;
}
//----------------------------------------------------------------------------
int PointController::GetMemoryUsed () const
{
    int iBaseSize = sizeof(PointController) - sizeof(Controller);

    Polypoint* pkPoint = (Polypoint*) m_pkObject;
    int iVertexQuantity = pkPoint->GetVertexQuantity();
    int iDynaSize = iVertexQuantity*sizeof(m_afPointLinearSpeed[0]);
    iDynaSize += iVertexQuantity*sizeof(m_afPointAngularSpeed[0]);
    iDynaSize += iVertexQuantity*sizeof(m_akPointLinearAxis[0]);
    iDynaSize += iVertexQuantity*sizeof(m_akPointAngularAxis[0]);

    int iTotalSize = iBaseSize + iDynaSize + Controller::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int PointController::GetDiskUsed () const
{
    int iSize = Controller::GetDiskUsed() +
        sizeof(m_fSystemLinearSpeed) +
        sizeof(m_fSystemAngularSpeed) +
        sizeof(m_kSystemLinearAxis) +
        sizeof(m_kSystemAngularAxis);

    Polypoint* pkPoint = (Polypoint*) m_pkObject;
    int iVertexQuantity = pkPoint->GetVertexQuantity();

    iSize += sizeof(iVertexQuantity);
    iSize += iVertexQuantity*sizeof(m_afPointLinearSpeed[0]);
    iSize += iVertexQuantity*sizeof(m_afPointAngularSpeed[0]);
    iSize += iVertexQuantity*sizeof(m_akPointLinearAxis[0]);
    iSize += iVertexQuantity*sizeof(m_akPointAngularAxis[0]);

    return iSize;
}
//----------------------------------------------------------------------------
