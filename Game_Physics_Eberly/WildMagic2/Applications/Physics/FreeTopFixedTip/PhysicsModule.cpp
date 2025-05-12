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

OdeSolverd::Function PhysicsModule::ms_aoFunction[3] =
{
    DTheta, DPhi, DPsi
};

//----------------------------------------------------------------------------
PhysicsModule::PhysicsModule ()
{
    Gravity = 0.0;
    Mass = 0.0;
    Length = 0.0;
    Inertia1 = 0.0;
    Inertia3 = 0.0;
    m_dTime = 0.0;
    m_dDeltaTime = 0.0;
    memset(m_adState,0,3*sizeof(double));
    memset(m_adAux,0,8*sizeof(double));
    m_pkSolver = NULL;
}
//----------------------------------------------------------------------------
PhysicsModule::~PhysicsModule ()
{
    delete m_pkSolver;
}
//----------------------------------------------------------------------------
void PhysicsModule::Initialize (double dTime, double dDeltaTime,
    double dTheta, double dPhi, double dPsi, double dAngVel1,
    double dAngVel2, double dAngVel3)
{
    // state variables
    m_dTime = dTime;
    m_dDeltaTime = dDeltaTime;
    m_adState[0] = dTheta;
    m_adState[1] = dPhi;
    m_adState[2] = dPsi;

    // auxiliary variables
    double dCosPhi = Mathd::Cos(dPhi), dSinPhi = Mathd::Sin(dPhi);
    double dCosPsi = Mathd::Cos(dPsi), dSinPsi = Mathd::Sin(dPsi);
    m_adAux[0] = Mass*Gravity*Length/Inertia1;  // alpha
    m_adAux[1] = dAngVel1*dAngVel1 + dAngVel2*dAngVel2 +
        2.0*dCosPhi*m_adAux[0];  // beta
    m_adAux[2] = dAngVel3*Inertia3/Inertia1;  // epsilon
    m_adAux[3] = dSinPhi*(dAngVel1*dSinPsi+dAngVel2*dCosPsi) +
        dCosPhi*m_adAux[2];  // delta
    m_adAux[4] = dAngVel3;

    // memoized values for optimization of the function evaluation
    m_adAux[5] = dCosPhi;  // last cos(phi)
    m_adAux[6] = dSinPhi*dSinPhi;  // last sin^2(phi)
    m_adAux[7] = 0.0;  // last dTheta/dt (initial value is irrelevant)

    // RK4 differential equation solver.  Since m_pkSolver is a base class
    // pointer, you can instead create a solver of whatever class you prefer.
    delete m_pkSolver;
    m_pkSolver = new OdeRungeKutta4d(3,m_dDeltaTime,ms_aoFunction,m_adAux);
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
double PhysicsModule::GetPhi () const
{
    return m_adState[1];
}
//----------------------------------------------------------------------------
double PhysicsModule::GetPsi () const
{
    return m_adState[2];
}
//----------------------------------------------------------------------------
Matrix3f PhysicsModule::GetBodyAxes () const
{
    float fCosTheta = (float)Mathd::Cos(m_adState[0]);
    float fSinTheta = (float)Mathd::Sin(m_adState[0]);
    float fCosPhi = (float)Mathd::Cos(m_adState[1]);
    float fSinPhi = (float)Mathd::Sin(m_adState[1]);
    float fCosPsi = (float)Mathd::Cos(m_adState[2]);
    float fSinPsi = (float)Mathd::Sin(m_adState[2]);

    Vector3f kN(fCosTheta,fSinTheta,0.0f);
    Vector3f kAxis3(fSinTheta*fSinPhi,-fCosTheta*fSinPhi,fCosPhi);
    Vector3f kAxis3xN = kAxis3.Cross(kN);
    Vector3f kAxis1 = fCosPsi*kN + fSinPsi*kAxis3xN;
    Vector3f kAxis2 = fCosPsi*kAxis3xN - fSinPsi*kN;

    return Matrix3f(kAxis1,kAxis2,kAxis3,true);
}
//----------------------------------------------------------------------------
double PhysicsModule::DTheta (double dTime, double* adState, void* pvData)
{
    double* adAux = (double*)pvData;
    adAux[5] = Mathd::Cos(adState[1]);
    adAux[6] = Mathd::Sin(adState[1]);
    double dNumer = adAux[3] - adAux[2]*adAux[5];
    adAux[7] = dNumer/adAux[6];
    double dDTheta = adAux[7]/adAux[6];
    return dDTheta;
}
//----------------------------------------------------------------------------
double PhysicsModule::DPhi (double dTime, double* adState, void* pvData)
{
    double* adAux = (double*)pvData;
    double dArg = adAux[1] - 2.0*adAux[0]*adAux[5] - adAux[7]*adAux[7];
    double dDPhi = Mathd::Sqrt(Mathd::FAbs(dArg));
    return dDPhi;
}
//----------------------------------------------------------------------------
double PhysicsModule::DPsi (double dTime, double* adState, void* pvData)
{
    double* adAux = (double*)pvData;
    double dDPsi = adAux[4] - adAux[7]*adAux[5]/adAux[6];
    return dDPsi;
}
//----------------------------------------------------------------------------
