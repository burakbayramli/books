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

#include "WmlMassSpringSurface.h"
using namespace Wml;

class PhysicsModule : public MassSpringSurface3f
{
public:
    // Construction and destruction.
    //
    // Gravity is controlled by the input rkGravity.
    //
    // Mass-spring systems tend to exhibit "stiffness" (in the sense of
    // numerical stability).  To remedy this problem, a small amount of
    // viscous friction is added to the external force.  This term is
    // of the form -fViscosity*velocity where fViscosity > 0.
    //
    // The initial wind force is specified by the caller.  The wind remains
    // in effect throughout the simulation.  To simulate oscillatory behavior
    // locally, random forces are applied at each mass in the direction
    // perpendicular to the plane of the wind and gravity vectors.  The
    // amplitudes are sinusoidal.  The phases are randomly generated.

    PhysicsModule (int iRows, int iCols, float fStep,
        const Vector3f& rkGravity, const Vector3f& rkWind, float fViscosity,
        float fAmplitude);

    virtual ~PhysicsModule ();

    // External impulse is due to forces of gravitation, wind, and viscous
    // friction.  The wind forces are randomly generated.
    virtual Vector3f ExternalImpulse (int i, float fTime,
        const Vector3f* akPosition, const Vector3f* akVelocity);

protected:
    Vector3f m_kGravity, m_kWind;
    float m_fViscosity;

    // sinusoidal forces
    float m_fAmplitude;
    float* m_afPhase;
    Vector3f m_kDirection;
};

#endif
