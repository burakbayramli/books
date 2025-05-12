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
    void Initialize (double dTime, double dDeltaTime, double dRadius,
        double dDRadius, double dTheta, double dDTheta);

    // take a single step of the solver
    void Update ();

    // access the current state
    double GetTime () const;
    double GetDeltaTime () const;
    double GetRadius () const;
    double GetDRadius () const;
    double GetTheta () const;
    double GetDTheta () const;

    // physical constants
    double Gravity;
    double Mass;

    // ellipse parameters
    double GetEccentricity () const;
    double GetMajorAxis () const;
    double GetMinorAxis () const;
    double GetPeriod () const;

private:
    // state and auxiliary variables
    double m_dTime, m_dDeltaTime, m_dRadius, m_dDRadius, m_dTheta, m_dDTheta;
    double m_adState[1], m_adAux[5];

    // ellipse parameters
    double m_dEccentricity, m_dRho, m_dMajorAxis, m_dMinorAxis;

    // ODE solver (specific solver assigned in the cpp file)
    Wml::OdeSolverd* m_pkSolver;
    static double DTheta (double dTime, double* adState, void* pvData);
    static Wml::OdeSolverd::Function ms_aoFunction[1];
};

#include "PhysicsModule.inl"

#endif
