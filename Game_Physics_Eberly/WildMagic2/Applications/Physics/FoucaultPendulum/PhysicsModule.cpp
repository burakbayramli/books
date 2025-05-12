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

OdeSolverd::Function PhysicsModule::ms_aoFunction[4] =
{
    DTheta, DThetaDot, DPhi, DPhiDot
};

//----------------------------------------------------------------------------
PhysicsModule::PhysicsModule ()
{
    memset(m_adState,0,4*sizeof(double));
    memset(m_adAux,0,6*sizeof(double));
    m_pkSolver = NULL;
}
//----------------------------------------------------------------------------
PhysicsModule::~PhysicsModule ()
{
    delete m_pkSolver;
}
//----------------------------------------------------------------------------
void PhysicsModule::Initialize (double dTime, double dDeltaTime,
    double dTheta, double dDTheta, double dPhi, double dDPhi)
{
    // pendulum initial conditions
    m_dTime = 0.0;
    m_dDeltaTime = dDeltaTime;
    m_adState[0] = dTheta;
    m_adState[1] = dDTheta;
    m_adState[2] = dPhi;
    m_adState[3] = dDPhi;

    // auxiliary variables (aux[0], aux[1], aux[2] evaluated by DTheta)
    m_adAux[0] = 0.0;  // sin(theta)
    m_adAux[1] = 0.0;  // sin(phi)
    m_adAux[2] = 0.0;  // cos(phi)
    m_adAux[3] = AngularSpeed*Mathd::Sin(Latitude);  // w*sin(lat)
    m_adAux[4] = AngularSpeed*Mathd::Cos(Latitude);  // w*cos(lat)
    m_adAux[5] = GDivL;

    // RK4 differential equation solver.  Since m_pkSolver is a base class
    // pointer, you can instead create a solver of whatever class you prefer.
    delete m_pkSolver;
    m_pkSolver = new OdeRungeKutta4d(4,m_dDeltaTime,ms_aoFunction,m_adAux);
}
//----------------------------------------------------------------------------
Matrix3f PhysicsModule::GetOrientation () const
{
    float fCosTheta = (float)Mathd::Cos(m_adState[0]);
    float fSinTheta = (float)Mathd::Sin(m_adState[0]);
    float fSinPhi = (float)Mathd::Sin(m_adState[2]);
    float fCosPhi = (float)Mathd::Cos(m_adState[2]);
    float fOmCosPhi = 1.0f - fCosPhi;

    Matrix3f rkRot;
    rkRot[0][0] = 1.0f - fOmCosPhi*fCosTheta*fCosTheta;
    rkRot[0][1] = -fOmCosPhi*fSinTheta*fCosTheta;
    rkRot[0][2] = -fSinPhi*fCosTheta;
    rkRot[1][0] = rkRot[0][1];
    rkRot[1][1] = 1.0f - fOmCosPhi*fSinTheta*fSinTheta;
    rkRot[1][2] = -fSinPhi*fSinTheta;
    rkRot[2][0] = -rkRot[0][2];
    rkRot[2][1] = -rkRot[1][2];
    rkRot[2][2] = fCosPhi;

    return rkRot;
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
double PhysicsModule::DTheta (double, double* adState, void*)
{
    double dU = adState[1];
    return dU;
}
//----------------------------------------------------------------------------
double PhysicsModule::DThetaDot (double, double* adState, void* pvData)
{
    double* adAux = (double*)pvData;
    double dSinTheta = Mathd::Sin(adState[0]);
    double dSinPhi = Mathd::Sin(adState[2]);
    double dCosPhi = Mathd::Cos(adState[2]);
    double dWSinLat = adAux[3];
    double dWCosLat = adAux[4];
    double dU = adState[1];
    double dV = adState[3];

    // store for use by other three RK4 functions
    adAux[0] = dSinTheta;
    adAux[1] = dSinPhi;
    adAux[2] = dCosPhi;

    // The RK4 function has a removable discontinuity at phi=0.  When
    // sin(phi) is nearly zero, switch to the function that is defined at
    // phi=0.
    if ( Mathd::FAbs(dSinPhi) < 1e-06 )
        return (2.0/3.0)*dWCosLat*dV*dSinTheta;

    return -2.0*dV*(-dWCosLat*dSinTheta + dCosPhi*(dU+dWSinLat)/dSinPhi);
}
//----------------------------------------------------------------------------
double PhysicsModule::DPhi (double, double* adState, void*)
{
    double dV = adState[3];
    return dV;
}
//----------------------------------------------------------------------------
double PhysicsModule::DPhiDot (double, double* adState, void* pvData)
{
    double* adAux = (double*)pvData;
    double dSinTheta = adAux[0];
    double dSinPhi = adAux[1];
    double dCosPhi = adAux[2];
    double dWSinLat = adAux[3];
    double dWCosLat = adAux[4];
    double dGDivL = adAux[5];
    double dU = adState[1];

    return dSinPhi*(dU*dU*dCosPhi + 2.0*dU*(dWCosLat*dSinTheta*dSinPhi -
        dWSinLat*dCosPhi) - dGDivL);
}
//----------------------------------------------------------------------------
