// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#ifndef PHYSICSMODULE_H
#define PHYSICSMODULE_H

#include "WmlMassSpringVolume.h"
using namespace Wml;

class PhysicsModule : public MassSpringVolume3f
{
public:
    // construction and destruction
    PhysicsModule (int iSlices, int iRows, int iCols, float fStep,
        float fViscosity);

    virtual ~PhysicsModule ();

    float& Viscosity ();

    // external impulse is due to viscous forces
    virtual Vector3f ExternalImpulse (int i, float fTime,
        const Vector3f* akPosition, const Vector3f* akVelocity);

protected:
    float m_fViscosity;
};

#endif
