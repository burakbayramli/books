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

#include "WmlVector2.h"

class PhysicsModule
{
public:
    // construction and destruction
    PhysicsModule ();
    ~PhysicsModule ();

    // initialize the system
    void Initialize (double dTime, double dDeltaTime,
        const Wml::Vector2d& rkInitPos, const Wml::Vector2d& rkInitVel);

    // take a single step of the simulation
    void Update ();

    // access the current state
    double GetTime () const;
    double GetDeltaTime () const;
    const Wml::Vector2d& GetPosition () const;
    const Wml::Vector2d& GetVelocity () const;
    double GetFrequency () const;

    // physical constants
    double SpringConstant;  // c
    double Mass;  // m

private:
    void Evaluate ();

    // state variables
    double m_dTime, m_dDeltaTime;
    Wml::Vector2d m_kPosition, m_kVelocity;

    // auxiliary variables
    Wml::Vector2d m_kInitPos;  // initial position
    double m_dFrequency;  // sqrt(c/m)
    Wml::Vector2d m_kVelDivFreq;  // initial_velocity/frequency
};

#include "PhysicsModule.inl"

#endif
