// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlApprEllipsoidFit3.h"
#include "WmlContBox3.h"
#include "WmlDistVec3Eld3.h"
#include "WmlMinimizeN.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
EllipsoidFit3<Real>::EllipsoidFit3 (int iQuantity,
    const Vector3<Real>* akPoint, Vector3<Real>& rkU, Matrix3<Real>& rkR,
    Real afD[3], Real& rfError)
{
    // Energy function is E : R^9 -> R where
    // V = (V0,V1,V2,V3,V4,V5,V6,V7,V8)
    //   = (D[0],D[1],D[2],U.X(),U,y,U.Z(),A0,A1,A2). 
    // For really scattered data, you might need a search function

    m_iQuantity = iQuantity;
    m_akPoint = akPoint;
    m_akTemp = new Vector3<Real>[iQuantity];

    MinimizeN<Real> kMinimizer(9,Energy,8,8,32,this);

    InitialGuess(iQuantity,akPoint,rkU,rkR,afD);

    Real afAngle[3];
    MatrixToAngles(rkR,afAngle);

    Real afExtent[3] =
    {
        afD[0]*Math<Real>::FAbs(rkR[0][0]) +
            afD[1]*Math<Real>::FAbs(rkR[0][1]) +
            afD[2]*Math<Real>::FAbs(rkR[0][2]),
        afD[0]*Math<Real>::FAbs(rkR[1][0]) +
            afD[1]*Math<Real>::FAbs(rkR[1][1]) +
            afD[2]*Math<Real>::FAbs(rkR[1][2]),
        afD[0]*Math<Real>::FAbs(rkR[2][0]) +
            afD[1]*Math<Real>::FAbs(rkR[2][1]) +
            afD[2]*Math<Real>::FAbs(rkR[2][2])
    };

    Real afV0[9] =
    {
        ((Real)0.5)*afD[0],
        ((Real)0.5)*afD[1],
        ((Real)0.5)*afD[2],
        rkU.X() - afExtent[0],
        rkU.Y() - afExtent[1],
        rkU.Z() - afExtent[2],
        -Math<Real>::PI,
        (Real)0.0,
        (Real)0.0
    };

    Real afV1[9] =
    {
        ((Real)2.0)*afD[0],
        ((Real)2.0)*afD[1],
        ((Real)2.0)*afD[2],
        rkU.X() + afExtent[0],
        rkU.Y() + afExtent[1],
        rkU.Z() + afExtent[2],
        Math<Real>::PI,
        Math<Real>::PI,
        Math<Real>::PI
    };

    Real afVInitial[9] =
    {
        afD[0],
        afD[1],
        afD[2],
        rkU.X(),
        rkU.Y(),
        rkU.Z(),
        afAngle[0],
        afAngle[1],
        afAngle[2]
    };

    Real afVMin[9];
    kMinimizer.GetMinimum(afV0,afV1,afVInitial,afVMin,rfError);

    afD[0] = afVMin[0];
    afD[1] = afVMin[1];
    afD[2] = afVMin[2];
    rkU.X() = afVMin[3];
    rkU.Y() = afVMin[4];
    rkU.Z() = afVMin[5];

    AnglesToMatrix(&afVMin[6],rkR);

    delete[] m_akTemp;
}
//----------------------------------------------------------------------------
template <class Real>
void EllipsoidFit3<Real>::InitialGuess (int iQuantity,
    const Vector3<Real>* akPoint, Vector3<Real>& rkU,  Matrix3<Real>& rkR,
    Real afD[3])
{
    Box3<Real> kBox = ContOrientedBox(iQuantity,akPoint);

    rkU = kBox.Center();
    rkR[0][0] = kBox.Axis(0).X();
    rkR[0][1] = kBox.Axis(0).Y();
    rkR[0][2] = kBox.Axis(0).Z();
    rkR[1][0] = kBox.Axis(1).X();
    rkR[1][1] = kBox.Axis(1).Y();
    rkR[1][2] = kBox.Axis(1).Z();
    rkR[2][0] = kBox.Axis(2).X();
    rkR[2][1] = kBox.Axis(2).Y();
    rkR[2][2] = kBox.Axis(2).Z();
    afD[0] = kBox.Extent(0);
    afD[1] = kBox.Extent(1);
    afD[2] = kBox.Extent(2);
}
//----------------------------------------------------------------------------
template <class Real>
Real EllipsoidFit3<Real>::Energy (const Real* afV, void* pvData)
{
    EllipsoidFit3& rkSelf = *(EllipsoidFit3*)pvData;

    // build rotation matrix
    Matrix3<Real> kRot;
    AnglesToMatrix(&afV[6],kRot);

    // Uniformly scale the extents to keep reasonable floating point values
    // in the distance calculations.
    Real fMax = afV[0];
    if ( afV[1] > fMax )
        fMax = afV[1];
    if ( afV[2] > fMax )
        fMax = afV[2];

    Real fInvMax = ((Real)1.0)/fMax;
    EllipsoidStandard3<Real> kEllipsoid;
    kEllipsoid.Extent(0) = fInvMax*afV[0];
    kEllipsoid.Extent(1) = fInvMax*afV[1];
    kEllipsoid.Extent(2) = fInvMax*afV[2];
    Vector3<Real> kClosest;

    // transform the points to the coordinate system of U and R
    Real fEnergy = (Real)0.0;
    for (int i = 0; i < rkSelf.m_iQuantity; i++)
    {
        Vector3<Real> kDiff(
            rkSelf.m_akPoint[i].X() - afV[3],
            rkSelf.m_akPoint[i].Y() - afV[4],
            rkSelf.m_akPoint[i].Z() - afV[5]);

        rkSelf.m_akTemp[i] = fInvMax*(kDiff*kRot);

        Real fDist = Distance(kEllipsoid,rkSelf.m_akTemp[i],kClosest);
        fEnergy += fMax*fDist;
    }

    return fEnergy;
}
//----------------------------------------------------------------------------
template <class Real>
void EllipsoidFit3<Real>::MatrixToAngles (const Matrix3<Real>& rkR,
    Real* afAngle)
{
    // rotation axis = (cos(a0)sin(a1),sin(a0)sin(a1),cos(a1))
    // a0 in [-pi,pi], a1 in [0,pi], a2 in [0,pi]

    Vector3<Real> kAxis;
    rkR.ToAxisAngle(kAxis,afAngle[2]);

    if ( -(Real)1.0 < kAxis.Z() )
    {
        if ( kAxis.Z() < (Real)1.0 )
        {
            afAngle[0] = Math<Real>::ATan2(kAxis.Y(),kAxis.X());
            afAngle[1] = Math<Real>::ACos(kAxis.Z());
        }
        else
        {
            afAngle[0] = (Real)0.0;
            afAngle[1] = (Real)0.0;
        }
    }
    else
    {
        afAngle[0] = (Real)0.0;
        afAngle[1] = Math<Real>::PI;
    }
}
//----------------------------------------------------------------------------
template <class Real>
void EllipsoidFit3<Real>::AnglesToMatrix (const Real* afAngle,
    Matrix3<Real>& rkR)
{
    // rotation axis = (cos(a0)sin(a1),sin(a0)sin(a1),cos(a1))
    // a0 in [-pi,pi], a1 in [0,pi], a2 in [0,pi]

    Real fCos0 = Math<Real>::Cos(afAngle[0]);
    Real fSin0 = Math<Real>::Sin(afAngle[0]);
    Real fCos1 = Math<Real>::Cos(afAngle[1]);
    Real fSin1 = Math<Real>::Sin(afAngle[1]);
    Vector3<Real> kAxis(fCos0*fSin1,fSin0*fSin1,fCos1);
    rkR.FromAxisAngle(kAxis,afAngle[2]);
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM class EllipsoidFit3<float>;
template WML_ITEM class EllipsoidFit3<double>;
}
//----------------------------------------------------------------------------
