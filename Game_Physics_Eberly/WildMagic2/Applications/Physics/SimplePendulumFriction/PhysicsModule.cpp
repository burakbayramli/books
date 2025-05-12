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
#include "WmlOdeRungeKutta4.h"
#include "WmlMath.h"
using namespace Wml;

OdeSolverd::Function PhysicsModule::ms_aoFunction[2] =
{
    DTheta, DThetaDot
};

//----------------------------------------------------------------------------
PhysicsModule::PhysicsModule ()
{
    memset(m_adState,0,2*sizeof(double));
    memset(m_adAux,0,2*sizeof(double));
    m_pkSolver = NULL;
}
//----------------------------------------------------------------------------
PhysicsModule::~PhysicsModule ()
{
    delete m_pkSolver;
}
//----------------------------------------------------------------------------
void PhysicsModule::Initialize (double dTime, double dDeltaTime,
    double dTheta, double dDTheta)
{
    // pendulum initial conditions
    m_dTime = 0.0;
    m_dDeltaTime = dDeltaTime;
    m_adState[0] = dTheta;
    m_adState[1] = dDTheta;

    // auxiliary variables
    m_adAux[0] = GDivL;
    m_adAux[1] = CDivM;

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
}
//----------------------------------------------------------------------------
double PhysicsModule::GetTheta () const
{
    return m_adState[0];
}
//----------------------------------------------------------------------------
double PhysicsModule::DTheta (double, double* adState, void*)
{
    double dDTheta = adState[1];
    return dDTheta;
}
//----------------------------------------------------------------------------
double PhysicsModule::DThetaDot (double, double* adState, void* pvData)
{
    double* adAux = (double*)pvData;
    double dSinTheta = Mathd::Sin(adState[0]);
    double dDThetaDot = -(adAux[0]*dSinTheta + adAux[1]*adState[1]);
    return dDThetaDot;
}
//----------------------------------------------------------------------------
