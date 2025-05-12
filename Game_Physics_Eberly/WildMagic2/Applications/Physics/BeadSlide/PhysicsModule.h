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

#include "WmlOdeSolver.h"

class PhysicsModule
{
public:
    // construction and destruction
    PhysicsModule ();
    ~PhysicsModule ();

    // initialize the differential equation solver
    void Initialize (double dTime, double dDeltaTime, double dQ,
        double dQDer);

    // take a single step of the solver
    void Update ();

    // access the current state
    double GetTime () const;
    double GetDeltaTime () const;
    double GetQ () const;
    double GetQDer () const;

    // physical constants
    double Gravity;
    double Mass;

private:
    // state and auxiliary variables
    double m_dTime, m_dDeltaTime, m_dQ, m_dQDer;
    double m_adState[2], m_adAux[1];

    // ODE solver (specific solver assigned in the cpp file)
    Wml::OdeSolverd* m_pkSolver;
    static double DQ (double dTime, double* adState, void* pvData);
    static double DQDer (double dTime, double* adState, void* pvData);
    static Wml::OdeSolverd::Function ms_aoFunction[2];
};

#include "PhysicsModule.inl"

#endif
