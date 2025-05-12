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
PhysicsModule::PhysicsModule (int iSlices, int iRows, int iCols, float fStep,
    float fViscosity)
    :
    MassSpringVolume3f(iSlices,iRows,iCols,fStep)
{
    m_fViscosity = fViscosity;
}
//----------------------------------------------------------------------------
PhysicsModule::~PhysicsModule ()
{
}
//----------------------------------------------------------------------------
float& PhysicsModule::Viscosity ()
{
    return m_fViscosity;
}
//----------------------------------------------------------------------------
Vector3f PhysicsModule::ExternalImpulse (int i, float fTime,
    const Vector3f* akPosition, const Vector3f* akVelocity)
{
    Vector3f kImpulse = -m_fViscosity*akVelocity[i];
    return kImpulse;
}
//----------------------------------------------------------------------------
