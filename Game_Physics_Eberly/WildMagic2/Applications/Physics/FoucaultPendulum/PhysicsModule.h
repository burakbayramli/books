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

    // initialize the differential equation solver
    void Initialize (double dTime, double dDeltaTime, double dTheta,
        double dDTheta, double dPhi, double dDPhi);

    // the orientation of the pendulum
    Wml::Matrix3f GetOrientation () const;

    // take a single step of the solver
    void Update ();

    // pendulum parameters
    double AngularSpeed;  // w
    double Latitude;  // lat
    double GDivL;  // g/L

private:
    // The four state variables.
    //
    // state[0] = theta
    // state[1] = u (dtheta)
    // state[2] = phi
    // state[3] = v (dphi)
    //
    // Auxiliary variables that the caller of the Update function must
    // evaluate before passing to the update.
    //
    // aux[0] = sin(theta)
    // aux[1] = sin(phi)
    // aux[2] = cos(phi)
    // aux[3] = w*sin(lat)
    // aux[4] = w*cos(lat)
    // aux[5] = g/L

    // state and auxiliary variables
    double m_dTime, m_dDeltaTime, m_adState[4], m_adAux[6];

    // ODE solver (specific solver assigned in the cpp file)
    Wml::OdeSolverd* m_pkSolver;
    static double DTheta (double dTime, double* adState, void* pvData);
    static double DThetaDot (double dTime, double* adState, void* pvData);
    static double DPhi (double dTime, double* adState, void* pvData);
    static double DPhiDot (double dTime, double* adState, void* pvData);
    static Wml::OdeSolverd::Function ms_aoFunction[4];
};

#endif
