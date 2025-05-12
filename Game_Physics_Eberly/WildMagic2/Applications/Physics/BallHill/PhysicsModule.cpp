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
using namespace Wml;

OdeSolverd::Function PhysicsModule::ms_aoFunction[4] =
{
    DY1, DY1Dot, DY2, DY2Dot
};

//----------------------------------------------------------------------------
PhysicsModule::PhysicsModule ()
{
    Gravity = 0.0;
    A1 = 0.0;
    A2 = 0.0;
    A3 = 0.0;
    memset(m_adState,0,4*sizeof(double));
    memset(m_adAux,0,4*sizeof(double));
    m_pkSolver = NULL;
}
//----------------------------------------------------------------------------
PhysicsModule::~PhysicsModule ()
{
    delete m_pkSolver;
}
//----------------------------------------------------------------------------
void PhysicsModule::Initialize (double dTime, double dDeltaTime, double dY1,
    double dDY1, double dY2, double dDY2)
{
    m_dTime = dTime;
    m_dDeltaTime = dDeltaTime;

    // state variables
    m_adState[0] = dY1;   // y1(0)
    m_adState[1] = dDY1;  // y1'(0)
    m_adState[2] = dY2;   // y2(0)
    m_adState[3] = dDY2;  // y2'(0)

    // auxiliary variables
    m_adAux[0] = A1*A1;   // a1^2
    m_adAux[1] = A2*A2;   // a2^2
    m_adAux[2] = Gravity; // g

    // RK4 differential equation solver.  Since m_pkSolver is a base class
    // pointer, you can instead create a solver of whatever class you prefer.
    delete m_pkSolver;
    m_pkSolver = new OdeRungeKutta4d(4,m_dDeltaTime,ms_aoFunction,m_adAux);
}
//----------------------------------------------------------------------------
void PhysicsModule::GetData (Vector3f& rkCenter, Matrix3f& rkIncrRot) const
{
    // position is a point exactly on the hill
    Vector3f kPos;
    kPos.X() = (float)(A1*m_adState[0]);
    kPos.Y() = (float)(A2*m_adState[2]);
    kPos.Z() = (float)(A3 - m_adState[0]*m_adState[0] -
        m_adState[2]*m_adState[2]);

    // Lift this point off the hill in the normal direction by the radius of
    // the ball so that the ball just touches the hill.  The hill is
    // implicitly specified by F(x,y,z) = z - [a3 - (x/a1)^2 - (y/a2)^2]
    // where (x,y,z) is the position on the hill.  The gradient of F is a
    // normal vector, Grad(F) = (2*x/a1^2,2*y/a2^2,1).
    Vector3f kNor(2.0f*kPos.X()/(float)m_adAux[0],
        2.0f*kPos.Y()/(float)m_adAux[1],1.0f);
    kNor.Normalize();
    rkCenter = kPos + ((float)Radius)*kNor;

    // Let the ball rotate as it rolls down hill.  The axis of rotation is
    // the perpendicular to hill normal and ball velocity.  The angle of
    // rotation from the last position is A = speed*deltaTime/radius.
    Vector3f kVel;
    kVel.X() = (float)(A1*m_adState[1]);
    kVel.Y() = (float)(A1*m_adState[3]);
    kVel.Z() = -2.0f*(kVel.X()*(float)m_adState[0] +
        kVel.Y()*(float)m_adState[2]);
    float fSpeed = kVel.Normalize();
    float fAngle = fSpeed*((float)m_dDeltaTime)/((float)Radius);
    Vector3f kAxis = kNor.UnitCross(kVel);
    rkIncrRot = Matrix3f(kAxis,fAngle);
}
//----------------------------------------------------------------------------
float PhysicsModule::GetHeight (float fX, float fY) const
{
    double dXScaled = ((double)fX)/A1;
    double dYScaled = ((double)fY)/A2;
    return (float)(A3 - dXScaled*dXScaled - dYScaled*dYScaled);
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
double PhysicsModule::DY1 (double, double* adState, void*)
{
    double dDY1 = adState[1];
    return dDY1;
}
//----------------------------------------------------------------------------
double PhysicsModule::DY1Dot (double, double* adState, void* pvData)
{
    double* adAux = (double*)pvData;
    double dA1Sqr = adAux[0];
    double dA2Sqr = adAux[1];
    double dGravity = adAux[2];
    double dY1 = adState[0];
    double dDY1 = adState[1];
    double dY2 = adState[2];
    double dDY2 = adState[3];

    double dMat00 = dA1Sqr + 4.0*dY1*dY1;
    double dMat01 = 4.0*dY1*dY2;
    double dMat11 = dA2Sqr + 4.0*dY2*dY2;
    double dInvDet = 1.0/(dMat00*dMat11 - dMat01*dMat01);
    double dSqrLen = dDY1*dDY1 + dDY2*dDY2;
    double dRhs0 = 2.0*dY1*(dGravity-2.0*dY1*dSqrLen);
    double dRhs1 = 2.0*dY2*(dGravity-2.0*dY2*dSqrLen);

    double dDY1Dot = (dMat11*dRhs0 - dMat01*dRhs1)*dInvDet;
    double dDY2Dot = (dMat00*dRhs1 - dMat01*dRhs0)*dInvDet;

    adAux[3] = dDY2Dot;  // calculated now, return value of function DY2Dot
    return dDY1Dot;
}
//----------------------------------------------------------------------------
double PhysicsModule::DY2 (double, double* adState, void*)
{
    double dDY2 = adState[3];
    return dDY2;
}
//----------------------------------------------------------------------------
double PhysicsModule::DY2Dot (double, double* adState, void* pvData)
{
    // result already calculated in function DY1Dot
    double* adAux = (double*)pvData;
    return adAux[3];
}
//----------------------------------------------------------------------------
