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

#include "WmlMassSpringCurve.h"
using namespace Wml;

class PhysicsModule : public MassSpringCurve3f
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
    // The initial wind force is specified by the caller.  The application
    // of wind can be enabled/disabled by EnableWind().  The member
    // function EnableWindChange() allows the wind direction to change
    // randomly, but each new direction is nearby the old direction in order
    // to obtain some sense of continuity of direction.  The magnitude of
    // the wind force is constant, the length of the initial force.

    PhysicsModule (int iNumParticles, float fStep, const Vector3f& rkGravity,
        const Vector3f& rkWind, float fWindChangeAmplitude, float fViscosity);

    virtual ~PhysicsModule ();

    bool& EnableWind ();
    bool& EnableWindChange ();

    // External impulse is due to forces of gravitation, wind, and viscous
    // friction.  The wind forces are randomly generated.
    virtual Vector3f ExternalImpulse (int i, float fTime,
        const Vector3f* akPosition, const Vector3f* akVelocity);

protected:
    Vector3f m_kGravity, m_kWind;
    float m_fViscosity, m_fWindChangeAmplitude;
    bool m_bEnableWind, m_bEnableWindChange;
};

#endif
