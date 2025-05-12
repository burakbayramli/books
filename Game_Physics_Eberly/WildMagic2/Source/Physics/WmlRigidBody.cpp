// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#include "WmlRigidBody.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
RigidBody<Real>::RigidBody ()
{
    // default body is immovable
    m_fMass = Math<Real>::MAX_REAL;
    m_fInvMass = (Real)0.0;
    m_kInertia = Matrix3<Real>::IDENTITY;
    m_kInvInertia = Matrix3<Real>::ZERO;
    m_kQOrient = Quaternion<Real>::IDENTITY;
    m_kLinMom = Vector3<Real>::ZERO;
    m_kAngMom = Vector3<Real>::ZERO;
    m_kROrient = Matrix3<Real>::IDENTITY;
    m_kLinVel = Vector3<Real>::ZERO;
    m_kAngVel = Vector3<Real>::ZERO;
}
//----------------------------------------------------------------------------
template <class Real>
RigidBody<Real>::~RigidBody ()
{
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real>& RigidBody<Real>::Position ()
{
    return m_kPos;
}
//----------------------------------------------------------------------------
template <class Real>
void RigidBody<Real>::SetMass (float fMass)
{
    if ( (Real)0.0 < fMass && fMass < Math<Real>::MAX_REAL )
    {
        m_fMass = fMass;
        m_fInvMass = ((Real)1.0)/fMass;
    }
    else
    {
        // assume the body as immovable
        m_fMass = Math<Real>::MAX_REAL;
        m_fInvMass = (Real)0.0;
        m_kInertia = Matrix3<Real>::IDENTITY;
        m_kInvInertia = Matrix3<Real>::ZERO;
        m_kQOrient = Quaternion<Real>::IDENTITY;
        m_kLinMom = Vector3<Real>::ZERO;
        m_kAngMom = Vector3<Real>::ZERO;
        m_kROrient = Matrix3<Real>::IDENTITY;
        m_kLinVel = Vector3<Real>::ZERO;
        m_kAngVel = Vector3<Real>::ZERO;
    }
}
//----------------------------------------------------------------------------
template <class Real>
void RigidBody<Real>::SetBodyInertia (const Matrix3<Real>& rkInertia)
{
    m_kInertia = rkInertia;
    m_kInvInertia = m_kInertia.Inverse();
}
//----------------------------------------------------------------------------
template <class Real>
void RigidBody<Real>::SetPosition (const Vector3<Real>& rkPos)
{
    m_kPos = rkPos;
}
//----------------------------------------------------------------------------
template <class Real>
void RigidBody<Real>::SetQOrientation (const Quaternion<Real>& rkQOrient)
{
    m_kQOrient = rkQOrient;
    m_kQOrient.ToRotationMatrix(m_kROrient);
}
//----------------------------------------------------------------------------
template <class Real>
void RigidBody<Real>::SetLinearMomentum (const Vector3<Real>& rkLinMom)
{
    m_kLinMom = rkLinMom;
    m_kLinVel = m_fInvMass*m_kLinMom;
}
//----------------------------------------------------------------------------
template <class Real>
void RigidBody<Real>::SetAngularMomentum (const Vector3<Real>& rkAngMom)
{
    m_kAngMom = rkAngMom;
    m_kAngVel = m_kROrient*m_kInvInertia*m_kROrient.Transpose()*m_kAngMom;
}
//----------------------------------------------------------------------------
template <class Real>
void RigidBody<Real>::SetROrientation (const Matrix3<Real>& rkROrient)
{
    m_kROrient = rkROrient;
    m_kQOrient.FromRotationMatrix(m_kROrient);
}
//----------------------------------------------------------------------------
template <class Real>
void RigidBody<Real>::SetLinearVelocity (const Vector3<Real>& rkLinVel)
{
    m_kLinVel = rkLinVel;
    m_kLinMom = m_fMass*m_kLinVel;
}
//----------------------------------------------------------------------------
template <class Real>
void RigidBody<Real>::SetAngularVelocity (const Vector3<Real>& rkAngVel)
{
    m_kAngVel = rkAngVel;
    m_kAngMom = m_kROrient*m_kInertia*m_kROrient.Transpose()*m_kAngVel;
}
//----------------------------------------------------------------------------
template <class Real>
Real RigidBody<Real>::GetMass () const
{
    return m_fMass;
}
//----------------------------------------------------------------------------
template <class Real>
Real RigidBody<Real>::GetInverseMass () const
{
    return m_fInvMass;
}
//----------------------------------------------------------------------------
template <class Real>
const Matrix3<Real>& RigidBody<Real>::GetBodyInertia () const
{
    return m_kInertia;
}
//----------------------------------------------------------------------------
template <class Real>
const Matrix3<Real>& RigidBody<Real>::GetBodyInverseInertia () const
{
    return m_kInvInertia;
}
//----------------------------------------------------------------------------
template <class Real>
Matrix3<Real> RigidBody<Real>::GetWorldInertia () const
{
    return m_kROrient*m_kInertia*m_kROrient.Transpose();
}
//----------------------------------------------------------------------------
template <class Real>
Matrix3<Real> RigidBody<Real>::GetWorldInverseInertia () const
{
    return m_kROrient*m_kInvInertia*m_kROrient.Transpose();
}
//----------------------------------------------------------------------------
template <class Real>
const Vector3<Real>& RigidBody<Real>::GetPosition () const
{
    return m_kPos;
}
//----------------------------------------------------------------------------
template <class Real>
const Quaternion<Real>& RigidBody<Real>::GetQOrientation () const
{
    return m_kQOrient;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector3<Real>& RigidBody<Real>::GetLinearMomentum () const
{
    return m_kLinMom;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector3<Real>& RigidBody<Real>::GetAngularMomentum () const
{
    return m_kAngMom;
}
//----------------------------------------------------------------------------
template <class Real>
const Matrix3<Real>& RigidBody<Real>::GetROrientation () const
{
    return m_kROrient;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector3<Real>& RigidBody<Real>::GetLinearVelocity () const
{
    return m_kLinVel;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector3<Real>& RigidBody<Real>::GetAngularVelocity () const
{
    return m_kAngVel;
}
//----------------------------------------------------------------------------
template <class Real>
void RigidBody<Real>::SetInternalForce (const Vector3<Real>& rkForce)
{
    m_kInternalForce = rkForce;
}
//----------------------------------------------------------------------------
template <class Real>
void RigidBody<Real>::SetInternalTorque (const Vector3<Real>& rkTorque)
{
    m_kInternalTorque = rkTorque;
}
//----------------------------------------------------------------------------
template <class Real>
void RigidBody<Real>::SetExternalForce (const Vector3<Real>& rkForce)
{
    m_kExternalForce = rkForce;
}
//----------------------------------------------------------------------------
template <class Real>
void RigidBody<Real>::SetExternalTorque (const Vector3<Real>& rkTorque)
{
    m_kExternalTorque = rkTorque;
}
//----------------------------------------------------------------------------
template <class Real>
void RigidBody<Real>::AppendInternalForce (const Vector3<Real>& rkForce)
{
    m_kInternalForce += rkForce;
}
//----------------------------------------------------------------------------
template <class Real>
void RigidBody<Real>::AppendInternalTorque (const Vector3<Real>& rkTorque)
{
    m_kInternalTorque += rkTorque;
}
//----------------------------------------------------------------------------
template <class Real>
void RigidBody<Real>::Update (Real fT, Real fDT)
{
    Real fHalfDT = ((Real)0.5)*fDT;
    Real fSixthDT = fDT/((Real)6.0);
    Real fTpHalfDT = fT + fHalfDT;
    Real fTpDT = fT + fDT;

    Vector3<Real> kNewPos, kNewLinMom, kNewAngMom, kNewLinVel, kNewAngVel;
    Quaternion<Real> kNewQOrient;
    Matrix3<Real> kNewROrient;

    // A1 = G(T,S0), B1 = S0 + (DT/2)*A1
    Vector3<Real> kA1DXDT = m_kLinVel;
    Quaternion<Real> kW = Quaternion<Real>((Real)0.0,m_kAngVel.X(),
        m_kAngVel.Y(),m_kAngVel.Z());
    Quaternion<Real> kA1DQDT = ((Real)0.5)*kW*m_kQOrient;
    Vector3<Real> kA1DPDT = m_kExternalForce + m_kInternalForce;
    Vector3<Real> kA1DLDT = m_kExternalTorque + m_kInternalTorque;
    m_kInternalForce = Vector3<Real>::ZERO;
    m_kInternalTorque = Vector3<Real>::ZERO;
    kNewPos = m_kPos + fHalfDT*kA1DXDT;
    kNewQOrient = m_kQOrient + fHalfDT*kA1DQDT;
    kNewLinMom = m_kLinMom + fHalfDT*kA1DPDT;
    kNewAngMom = m_kAngMom + fHalfDT*kA1DLDT;
    kNewQOrient.ToRotationMatrix(kNewROrient);
    kNewLinVel = m_fInvMass*kNewLinMom;
    kNewAngVel = kNewROrient*m_kInvInertia*kNewROrient.Transpose()*kNewAngMom;

    // A2 = G(T+DT/2,B1), B2 = S0 + (DT/2)*A2
    Vector3<Real> kA2DXDT = kNewLinVel;
    kW = Quaternion<Real>((Real)0.0,kNewAngVel.X(),kNewAngVel.Y(),
        kNewAngVel.Z());
    Quaternion<Real> kA2DQDT = ((Real)0.5)*kW*kNewQOrient;
    Vector3<Real> kA2DPDT = Force(fTpHalfDT,m_fMass,kNewPos,kNewQOrient,
        kNewLinMom,kNewAngMom,kNewROrient,kNewLinVel,kNewAngVel);
    Vector3<Real> kA2DLDT = Torque(fTpHalfDT,m_fMass,kNewPos,kNewQOrient,
        kNewLinMom,kNewAngMom,kNewROrient,kNewLinVel,kNewAngVel);
    kNewPos = m_kPos + fHalfDT*kA2DXDT;
    kNewQOrient = m_kQOrient + fHalfDT*kA2DQDT;
    kNewLinMom = m_kLinMom + fHalfDT*kA2DPDT;
    kNewAngMom = m_kAngMom + fHalfDT*kA2DLDT;
    kNewQOrient.ToRotationMatrix(kNewROrient);
    kNewLinVel = m_fInvMass*kNewLinMom;
    kNewAngVel = kNewROrient*m_kInvInertia*kNewROrient.Transpose()*kNewAngMom;

    // A3 = G(T+DT/2,B2), B3 = S0 + DT*A3
    Vector3<Real> kA3DXDT = kNewLinVel;
    kW = Quaternion<Real>((Real)0.0,kNewAngVel.X(),kNewAngVel.Y(),
        kNewAngVel.Z());
    Quaternion<Real> kA3DQDT = ((Real)0.5)*kW*kNewQOrient;
    Vector3<Real> kA3DPDT = Force(fTpHalfDT,m_fMass,kNewPos,kNewQOrient,
        kNewLinMom,kNewAngMom,kNewROrient,kNewLinVel,kNewAngVel);
    Vector3<Real> kA3DLDT = Torque(fTpHalfDT,m_fMass,kNewPos,kNewQOrient,
        kNewLinMom,kNewAngMom,kNewROrient,kNewLinVel,kNewAngVel);
    kNewPos = m_kPos + fDT*kA3DXDT;
    kNewQOrient = m_kQOrient + fDT*kA3DQDT;
    kNewLinMom = m_kLinMom + fDT*kA3DPDT;
    kNewAngMom = m_kAngMom + fDT*kA3DLDT;
    kNewQOrient.ToRotationMatrix(kNewROrient);
    kNewLinVel = m_fInvMass*kNewLinMom;
    kNewAngVel = kNewROrient*m_kInvInertia*kNewROrient.Transpose()*kNewAngMom;

    // A4 = G(T+DT,B3), S1 = S0 + (DT/6)*(A1+2*(A2+A3)+A4)
    Vector3<Real> kA4DXDT = kNewLinVel;
    kW = Quaternion<Real>((Real)0.0,kNewAngVel.X(),kNewAngVel.Y(),
        kNewAngVel.Z());
    Quaternion<Real> kA4DQDT = ((Real)0.5)*kW*kNewQOrient;
    Vector3<Real> kA4DPDT = Force(fTpDT,m_fMass,kNewPos,kNewQOrient,
        kNewLinMom,kNewAngMom,kNewROrient,kNewLinVel,kNewAngVel);
    Vector3<Real> kA4DLDT = Torque(fTpDT,m_fMass,kNewPos,kNewQOrient,
        kNewLinMom,kNewAngMom,kNewROrient,kNewLinVel,kNewAngVel);
    m_kPos = m_kPos + fSixthDT*(kA1DXDT + ((Real)2.0)*(kA2DXDT +
        kA3DXDT) + kA4DXDT);
    m_kQOrient = m_kQOrient + fSixthDT*(kA1DQDT + ((Real)2.0)*(kA2DQDT +
        kA3DQDT) + kA4DQDT);
    m_kLinMom = m_kLinMom + fSixthDT*(kA1DPDT + ((Real)2.0)*(kA2DPDT +
        kA3DPDT) + kA4DPDT);
    m_kAngMom = m_kAngMom + fSixthDT*(kA1DLDT + ((Real)2.0)*(kA2DLDT +
        kA3DLDT) + kA4DLDT);
    m_kQOrient.ToRotationMatrix(m_kROrient);
    m_kLinVel = m_fInvMass*m_kLinMom;
    m_kAngVel = m_kROrient*m_kInvInertia*m_kROrient.Transpose()*m_kAngMom;

    // make force and torque correspond to new time T+DT
    m_kExternalForce = Force(fTpDT,m_fMass,m_kPos,m_kQOrient,m_kLinMom,
        m_kAngMom,m_kROrient,m_kLinVel,m_kAngVel);
    m_kExternalTorque = Torque(fTpDT,m_fMass,m_kPos,m_kQOrient,m_kLinMom,
        m_kAngMom,m_kROrient,m_kLinVel,m_kAngVel);
}
//----------------------------------------------------------------------------
template <class Real>
bool& RigidBody<Real>::Moved ()
{
    return m_bMoved;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM RigidBody<float>;
template class WML_ITEM RigidBody<double>;
}
//----------------------------------------------------------------------------
