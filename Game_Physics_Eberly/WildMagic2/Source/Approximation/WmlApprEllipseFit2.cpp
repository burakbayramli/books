// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlApprEllipseFit2.h"
#include "WmlContBox2.h"
#include "WmlDistVec2Elp2.h"
#include "WmlMinimizeN.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
EllipseFit2<Real>::EllipseFit2 (int iQuantity, const Vector2<Real>* akPoint,
    Vector2<Real>& rkU, Matrix2<Real>& rkR, Real afD[2], Real& rfError)
{
    // Energy function is E : R^5 -> R where
    // V = (V0,V1,V2,V3,V4)
    //   = (D[0],D[1],U.x,U,y,atan2(R[1][0],R[1][1])).

    m_iQuantity = iQuantity;
    m_akPoint = akPoint;
    m_akTemp = new Vector2<Real>[iQuantity];

    MinimizeN<Real> kMinimizer(5,Energy,8,8,32,this);

    InitialGuess(iQuantity,akPoint,rkU,rkR,afD);
    Real fAngle = Math<Real>::ACos(rkR[0][0]);
    Real fE0 = afD[0]*Math<Real>::FAbs(rkR[0][0]) + 
        afD[1]*Math<Real>::FAbs(rkR[0][1]);
    Real fE1 = afD[0]*Math<Real>::FAbs(rkR[1][0]) +
        afD[1]*Math<Real>::FAbs(rkR[1][1]);

    Real afV0[5] =
    {
        ((Real)0.5)*afD[0],
        ((Real)0.5)*afD[1],
        rkU.X() - fE0,
        rkU.Y() - fE1,
        (Real)0.0
    };

    Real afV1[5] =
    {
        ((Real)2.0)*afD[0],
        ((Real)2.0)*afD[1],
        rkU.X() + fE0,
        rkU.Y() + fE1,
        Math<Real>::PI
    };

    Real afVInitial[5] =
    {
        afD[0],
        afD[1],
        rkU.X(),
        rkU.Y(),
        fAngle
    };

    Real afVMin[5];
    kMinimizer.GetMinimum(afV0,afV1,afVInitial,afVMin,rfError);

    afD[0] = afVMin[0];
    afD[1] = afVMin[1];
    rkU.X() = afVMin[2];
    rkU.Y() = afVMin[3];
    rkR.FromAngle(afVMin[4]);

    delete[] m_akTemp;
}
//----------------------------------------------------------------------------
template <class Real>
void EllipseFit2<Real>::InitialGuess (int iQuantity,
    const Vector2<Real>* akPoint, Vector2<Real>& rkU, Matrix2<Real>& rkR,
    Real afD[2])
{
    Box2<Real> kBox = ContOrientedBox(iQuantity,akPoint);

    rkU = kBox.Center();
    rkR[0][0] = kBox.Axis(0).X();
    rkR[0][1] = kBox.Axis(0).Y();
    rkR[1][0] = kBox.Axis(1).X();
    rkR[1][1] = kBox.Axis(1).Y();
    afD[0] = kBox.Extent(0);
    afD[1] = kBox.Extent(1);
}
//----------------------------------------------------------------------------
template <class Real>
Real EllipseFit2<Real>::Energy (const Real* afV, void* pvData)
{
    EllipseFit2& rkSelf = *(EllipseFit2*)pvData;

    // build rotation matrix
    Matrix2<Real> kRot(afV[4]);

    EllipseStandard2<Real> kEllipse;
    kEllipse.Extent(0) = afV[0];
    kEllipse.Extent(1) = afV[1];
    Vector2<Real> kClosest;

    // transform the points to the coordinate system of U and R
    Real fEnergy = (Real)0.0;
    for (int i = 0; i < rkSelf.m_iQuantity; i++)
    {
        Vector2<Real> kDiff(
            rkSelf.m_akPoint[i].X() - afV[2],
            rkSelf.m_akPoint[i].Y() - afV[3]);

        rkSelf.m_akTemp[i] = kDiff*kRot;
        Real fDist = Distance(kEllipse,rkSelf.m_akTemp[i],kClosest);
        fEnergy += fDist;
    }

    return fEnergy;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM class EllipseFit2<float>;
template WML_ITEM class EllipseFit2<double>;
}
//----------------------------------------------------------------------------
