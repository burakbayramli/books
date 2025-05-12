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
    void Initialize (double dTime, double dDeltaTime, double dY1, double dDY1,
        double dY2, double dDY2);

    // get the ball center and incremental rotation (after update)
    void GetData (Wml::Vector3f& rkCenter, Wml::Matrix3f& rkIncrRot) const;

    // compute paraboloid height from an xy-plane position
    float GetHeight (float fX, float fY) const;

    // take a single step of the solver
    void Update ();

    // physical constants
    double Gravity;

    // paraboloid parameters
    double A1, A2, A3;

    // ball radius
    double Radius;

private:
    // The paraboloid is x3 = a3 - (x1/a1)^2 - (x2/a2)^2.  The equations of
    // motion are:
    //   x1"+(4*x1/a1^2)*((x1*x1"+(x1')^2)/a1^2+(x2*x2"+(x2')^2)/a2^2)
    //     = 2*g*x1/a1^2
    //   x2"+(4*x2/a2^2)*((x1*x1"+(x1')^2)/a1^2+(x2*x2"+(x2')^2)/a2^2)
    //     = 2*g*x2/a2^2
    // Make the change of variables y1 = x1/a2 and y2 = x2/a2.  The equations
    // of motion are:
    //   a1^2*y1"+4*y1*(y1*y1"+(y1')^2+y2*y2"+(y2')^2) = 2g*y1
    //   a2^2*y2"+4*y2*(y1*y1"+(y1')^2+y2*y2"+(y2')^2) = 2g*y2
    // The second derivatives y1" and y2" can be solved algebraically:
    //  +   +   +                       +^{-1} +                             +
    //  |y1"| = |a1^2+4*y1^2 4*y1*y2    |      |2*g*y1-4*y1*((y1')^2+(y2')^2)|
    //  |y2"|   |4*y1*y2     a2^2+4*y2^2|      |2*g*y2-4*y2*((y1')^2+(y2')^2)|
    //  +   +   +                       +      +                             +
    //
    // The four state variables for the RK4 solver.
    //
    // state[0] = y1
    // state[1] = y1'
    // state[2] = y2
    // state[3] = y2'
    //
    // Auxiliary variables that the caller of the RK4 Update function must
    // set before passing to the update.
    //
    // aux[0] = a1^2
    // aux[1] = a2^2
    // aux[2] = g
    // aux[3] = storage for return value of DY2Dot, calculated in DY1Dot

    // state and auxiliary variables
    double m_dTime, m_dDeltaTime, m_adState[4], m_adAux[4];

    // ODE solver (specific solver assigned in the cpp file)
    Wml::OdeSolverd* m_pkSolver;
    static double DY1 (double dTime, double* adState, void* pvData);
    static double DY1Dot (double dTime, double* adState, void* pvData);
    static double DY2 (double dTime, double* adState, void* pvData);
    static double DY2Dot (double dTime, double* adState, void* pvData);
    static Wml::OdeSolverd::Function ms_aoFunction[4];
};

#endif
