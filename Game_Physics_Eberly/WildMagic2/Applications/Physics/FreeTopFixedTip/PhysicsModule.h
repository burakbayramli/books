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
#include "WmlMatrix3.h"

class PhysicsModule
{
public:
    // construction and destruction
    PhysicsModule ();
    ~PhysicsModule ();

    // Initialize the differential equation solver.  The inputs dTheta, dPhi,
    // and dPsi determine the body coordinate axes Xi1, Xi2, and Xi3.  The
    // angular velocity inputs are the coefficients in the body coordinate
    // system.
    void Initialize (double dTime, double dDeltaTime, double dTheta,
        double dPhi, double dPsi, double dAngVel1, double dAngVel2,
        double dAngVel3);

    // take a single step of the solver
    void Update ();

    double GetTheta () const;
    double GetPhi () const;
    double GetPsi () const;

    // Get the body coordinate axes in world coordinates.  The axes are
    // stored as the columns of a rotation matrix.
    Wml::Matrix3f GetBodyAxes () const;

    // physical constants
    double Gravity;
    double Mass;
    double Length;
    double Inertia1, Inertia3;  // Inertia2 = Inertia1

private:
    // state and auxiliary variables
    double m_dTime, m_dDeltaTime;
    double m_adState[3], m_adAux[8];

    // ODE solver (specific solver assigned in the cpp file)
    Wml::OdeSolverd* m_pkSolver;
    static double DTheta (double dTime, double* adState, void* pvData);
    static double DPhi (double dTime, double* adState, void* pvData);
    static double DPsi (double dTime, double* adState, void* pvData);
    static Wml::OdeSolverd::Function ms_aoFunction[3];
};

#endif
