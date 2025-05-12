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

OdeSolverd::Function PhysicsModule::ms_aoFunction[1] = { DTheta };

//----------------------------------------------------------------------------
PhysicsModule::PhysicsModule ()
{
    Gravity = 0.0;
    Mass = 0.0;
    m_dTime = 0.0;
    m_dDeltaTime = 0.0;
    m_dRadius = 0.0;
    m_dDRadius = 0.0;
    m_dTheta = 0.0;
    m_dDTheta = 0.0;
    m_dEccentricity = 0.0;
    m_dRho = 0.0;
    m_dMajorAxis = 0.0;
    m_dMinorAxis = 0.0;
    memset(m_adState,0,sizeof(double));
    memset(m_adAux,0,5*sizeof(double));
    m_pkSolver = NULL;
}
//----------------------------------------------------------------------------
PhysicsModule::~PhysicsModule ()
{
    delete m_pkSolver;
}
//----------------------------------------------------------------------------
void PhysicsModule::Initialize (double dTime, double dDeltaTime,
    double dRadius, double dDRadius, double dTheta, double dDTheta)
{
    m_dTime = dTime;
    m_dDeltaTime = dDeltaTime;
    m_dRadius = dRadius;
    m_dDRadius = dDRadius;
    m_dTheta = dTheta;
    m_dDTheta = dDTheta;

    // state variable
    m_adState[0] = dTheta;

    // compute c0 and c1 in the potential energy function V(theta)
    double dGM = Gravity*Mass;
    double dGM2 = dGM*Mass;
    double dRadiusSqr = dRadius*dRadius;
    double dAlpha = Mass*dRadiusSqr*dDTheta;
    double dG2M4dA2 = dGM2*dGM2/(dAlpha*dAlpha);
    double dV0 = -dGM/dRadius;
    double dDV0 = dGM2*dDRadius/dAlpha;
    double dV0Plus = dV0 + dG2M4dA2;
    double dSinTheta0 = Mathd::Sin(dTheta);
    double dCosTheta0 = Mathd::Cos(dTheta);
    double dC0 = dV0Plus*dSinTheta0 + dDV0*dCosTheta0;
    double dC1 = dV0Plus*dCosTheta0 - dDV0*dSinTheta0;

    // auxiliary variables needed by function DTheta(...)
    m_adAux[0] = dC0;
    m_adAux[1] = dC1;
    m_adAux[2] = dG2M4dA2;
    m_adAux[3] = dAlpha/(dGM*dGM2);

    // ellipse parameters
    double dGamma0 = dRadiusSqr*Mathd::FAbs(dDTheta);
    double dTmp0 = dRadiusSqr*dRadius*dDTheta*dDTheta - dGM;
    double dTmp1 = dRadiusSqr*dDRadius*dDTheta;
    double dGamma1 = Mathd::Sqrt(dTmp0*dTmp0+dTmp1*dTmp1);
    m_dEccentricity = dGamma1/dGM;
    m_dRho = dGamma0*dGamma0/dGamma1;
    double dTmp2 = 1.0 - m_dEccentricity*m_dEccentricity;
    assert( dTmp2 > 0.0 );
    m_dMajorAxis = m_dRho*m_dEccentricity/dTmp2;
    m_dMinorAxis = m_dMajorAxis*Mathd::Sqrt(dTmp2);

    // RK4 differential equation solver.  Since m_pkSolver is a base class
    // pointer, you can instead create a solver of whatever class you prefer.
    delete m_pkSolver;
    m_pkSolver = new OdeRungeKutta4d(1,m_dDeltaTime,ms_aoFunction,m_adAux);
}
//----------------------------------------------------------------------------
double PhysicsModule::GetPeriod () const
{
    return Mathd::TWO_PI*Mathd::Pow(m_dMajorAxis,1.5) /
        Mathd::Sqrt(Gravity*Mass);
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
    m_dTheta = m_adState[0];

    // compute dtheta for application access
    double dSin = Mathd::Sin(m_dTheta);
    double dCos = Mathd::Cos(m_dTheta);
    double dV = m_adAux[0]*dSin + m_adAux[1]*dCos - m_adAux[2];
    m_dDTheta = m_adAux[3]*dV*dV;

    // compute radius for application access
    m_dRadius = m_dEccentricity*m_dRho/(1.0 + m_dEccentricity*dCos);

    // compute dradius for application access
    m_dDRadius = m_dRadius*m_dRadius*m_dDTheta*dSin/m_dRho;
}
//----------------------------------------------------------------------------
double PhysicsModule::DTheta (double dTime, double* adState, void* pvData)
{
    double* adAux = (double*)pvData;

    double dSin = Mathd::Sin(adState[0]);
    double dCos = Mathd::Cos(adState[0]);
    double dV = adAux[0]*dSin + adAux[1]*dCos - adAux[2];
    double dDTheta = adAux[3]*dV*dV;

    return dDTheta;
}
//----------------------------------------------------------------------------
