// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#include "PhysicsModule.h"

//----------------------------------------------------------------------------
PhysicsModule::PhysicsModule (int iRows, int iCols, float fStep,
    const Vector3f& rkGravity, const Vector3f& rkWind, float fViscosity,
    float fAmplitude)
    :
    MassSpringSurface3f(iRows,iCols,fStep),
    m_kGravity(rkGravity),
    m_kWind(rkWind)
{
    m_fViscosity = fViscosity;
    m_kDirection = m_kGravity.UnitCross(m_kWind);
    m_fAmplitude = fAmplitude;

    m_afPhase = new float[m_iNumParticles];
    for (int iRow = 0; iRow < m_iRows; iRow++)
    {
        for (int iCol = 0; iCol < m_iCols; iCol++)
            m_afPhase[GetIndex(iRow,iCol)] = Mathf::UnitRandom()*Mathf::PI;
    }
}
//----------------------------------------------------------------------------
PhysicsModule::~PhysicsModule ()
{
    delete[] m_afPhase;
}
//----------------------------------------------------------------------------
Vector3f PhysicsModule::ExternalImpulse (int i, float fTime,
    const Vector3f* akPosition, const Vector3f* akVelocity)
{
    // impulse due to gravity, wind, and viscosity
    Vector3f kImpulse = m_kGravity + m_kWind - m_fViscosity*akVelocity[i];

    // sinusoidal perturbation
    float fAmplitude = m_fAmplitude*Mathf::Sin(2.0f*fTime+m_afPhase[i]);
    kImpulse += fAmplitude*m_kDirection;

    return kImpulse;
}
//----------------------------------------------------------------------------
