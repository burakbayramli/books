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
#include "WmlMath.h"
#include "WmlOdeRungeKutta4.h"
using namespace Wml;

OdeSolverd::Function PhysicsModule::ms_aoFunction[2] = { DQ, DQDer };

//----------------------------------------------------------------------------
PhysicsModule::PhysicsModule ()
{
    Gravity = 0.0;
    Mass = 0.0;
    m_dTime = 0.0;
    m_dDeltaTime = 0.0;
    m_dQ = 0.0;
    m_dQDer = 0.0;
    memset(m_adState,0,2*sizeof(double));
    memset(m_adAux,0,sizeof(double));
    m_pkSolver = NULL;
}
//----------------------------------------------------------------------------
PhysicsModule::~PhysicsModule ()
{
    delete m_pkSolver;
}
//----------------------------------------------------------------------------
void PhysicsModule::Initialize (double dTime, double dDeltaTime,
    double dQ, double dQDer)
{
    m_dTime = dTime;
    m_dDeltaTime = dDeltaTime;
    m_dQ = dQ;
    m_dQDer = dQDer;

    // state variables
    m_adState[0] = dQ;
    m_adState[1] = dQDer;

    // auxiliary variable
    m_adAux[0] = Gravity;

    // RK4 differential equation solver.  Since m_pkSolver is a base class
    // pointer, you can instead create a solver of whatever class you prefer.
    delete m_pkSolver;
    m_pkSolver = new OdeRungeKutta4d(2,m_dDeltaTime,ms_aoFunction,m_adAux);
}
//----------------------------------------------------------------------------
void PhysicsModule::Update ()
{
    assert( m_pkSolver );
    if ( !m_pkSolver )
        return;

    // take a single step in the ODE solver
    m_pkSolver->Update(m_dTime,m_adState,m_dTime,m_adState);

    // save theta for application access
    m_dQ = m_adState[0];
    m_dQDer = m_adState[1];
}
//----------------------------------------------------------------------------
double PhysicsModule::DQ (double dTime, double* adState, void*)
{
    return adState[1];
}
//----------------------------------------------------------------------------
double PhysicsModule::DQDer (double dTime, double* adState, void* pvData)
{
    double* adAux = (double*)pvData;

    double dQ = adState[0];
    double dQDer = adState[1];
    double dQ2 = dQ*dQ;
    double dQDer2 = dQDer*dQDer;
    double dNumer = -3.0*adAux[0]*dQ2 - 2.0*dQ*(2.0+9.0*dQ2)*dQDer2;
    double dDenom = 1.0 + dQ2*(4.0 + 9.0*dQ2);
    double dResult = dNumer/dDenom;

    return dResult;
}
//----------------------------------------------------------------------------
