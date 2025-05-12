// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlDistVec2Qdr2.h"
#include "WmlEigen.h"
#include "WmlPolynomial1.h"
#include "WmlPolynomialRoots.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
static void ComputeCoeff2 (Real afA[2], Real afB[2], Real afD[2],
    Real fC, Polynomial1<Real>& rkPoly)
{
    Real afBPad[2] = { afB[0]+afA[0]*afD[0], afB[1]+afA[1]*afD[1] };
    Real afBSqr[2] = { afB[0]*afB[0], afB[1]*afB[1] };
    Real afDSqr[2] = { afD[0]*afD[0], afD[1]*afD[1] };
    Real fDPrd = afD[0]*afD[1];
    Real fDSum = afD[0]+afD[1];

    rkPoly[0] = afA[0]*afBPad[0]+afA[1]*afBPad[1]+fC;
    rkPoly[1] = -afBSqr[0]-afBSqr[1]+((Real)4.0)*(afA[0]*afD[1]*afBPad[0]+
        afA[1]*afD[0]*afBPad[1]+fC*fDSum);
    rkPoly[2] = -afBSqr[0]*(afD[0]+((Real)4.0)*afD[1])-afBSqr[1]*(afD[1]+
        ((Real)4.0)*afD[0])+((Real)4.0)*(afA[0]*afDSqr[1]*afBPad[0]+
        afA[1]*afDSqr[0]*afBPad[1]+fC*(afDSqr[0]+afDSqr[1]+
        ((Real)4.0)*fDPrd));

    Real fTmp = -((Real)4.0)*(afBSqr[0]*afD[1]+afBSqr[1]*afD[0]-
        ((Real)4.0)*fC*fDPrd);
    rkPoly[3] = fDSum*fTmp;
    rkPoly[4] = fDPrd*fTmp;
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::SqrDistance (const Vector2<Real>& rkPoint, const Real afQuad[6],
    Vector2<Real>& rkClosest)
{
    // eigendecomposition
    Eigen<Real> kES(2);
    kES(0,0) = afQuad[3];
    kES(0,1) = ((Real)0.5)*afQuad[5];
    kES(1,0) = kES(0,1);
    kES(1,1) = afQuad[4];

    kES.IncrSortEigenStuff2();
    Vector2<Real> akEVec[2];
    kES.GetEigenvector(0,akEVec[0]);
    kES.GetEigenvector(1,akEVec[1]);

    Real afA[2], afB[2], afD[2], fC = afQuad[0];
    int i;
    for (i = 0; i < 2; i++)
    {
        afA[i] = akEVec[i].X()*rkPoint.X() + akEVec[i].Y()*rkPoint.Y();
        afB[i] = akEVec[i].X()*afQuad[1] + akEVec[i].Y()*afQuad[2];
        afD[i] = kES.GetEigenvalue(i);
    }

    Polynomial1<Real> kPoly(4);
    ComputeCoeff2(afA,afB,afD,fC,kPoly);

    PolynomialRoots<Real> kPR(Math<Real>::EPSILON);
    kPR.FindB(kPoly,6);
    int iCount = kPR.GetCount();
    const Real* afRoot = kPR.GetRoots();

    if ( iCount > 0 )
    {
        Real fMinDistSqr = Math<Real>::MAX_REAL;
        int iMinIndex = -1;
        Real afV[2], fDenom;
        for (int iIndex = 0; iIndex < iCount; iIndex++)
        {
            // compute closest point for this root
            for (i = 0; i < 2; i++)
            {
                fDenom = (Real)1.0 + ((Real)2.0)*afRoot[iIndex]*afD[i];
                afV[i] = (afA[i]-afRoot[iIndex]*afB[i])/fDenom;
            }

            rkClosest.X() = akEVec[0][0]*afV[0] + akEVec[1][0]*afV[1];
            rkClosest.Y() = akEVec[0][1]*afV[0] + akEVec[1][1]*afV[1];

            // compute squared distance from point to quadratic
            Vector2<Real> kDiff = rkClosest - rkPoint;
            Real fDistSqr = kDiff.SquaredLength();
            if ( fDistSqr < fMinDistSqr )
            {
                fMinDistSqr = fDistSqr;
                iMinIndex = iIndex;
            }
        }

        for (i = 0; i < 2; i++)
        {
            fDenom = (Real)1.0 + ((Real)2.0)*afRoot[iMinIndex]*afD[i];
            afV[i] = (afA[i]-afRoot[iMinIndex]*afB[i])/fDenom;
        }

        rkClosest.X() = akEVec[0][0]*afV[0] + akEVec[1][0]*afV[1];
        rkClosest.Y() = akEVec[0][1]*afV[0] + akEVec[1][1]*afV[1];
        return fMinDistSqr;
    }
    else
    {
        // should not happen
        assert( false );
        return -(Real)1.0;
    }
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::Distance (const Vector2<Real>& rkPoint, const Real afQuad[6],
    Vector2<Real>& rkClosest)
{
    return Math<Real>::Sqrt(SqrDistance(rkPoint,afQuad,rkClosest));
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM float SqrDistance<float> (const Vector2<float>&,
    const float[6], Vector2<float>&);
template WML_ITEM float Distance<float> (const Vector2<float>&,
    const float[6], Vector2<float>&);

template WML_ITEM double SqrDistance<double> (const Vector2<double>&,
    const double[6], Vector2<double>&);
template WML_ITEM double Distance<double> (const Vector2<double>&,
    const double[6], Vector2<double>&);
}
//----------------------------------------------------------------------------
