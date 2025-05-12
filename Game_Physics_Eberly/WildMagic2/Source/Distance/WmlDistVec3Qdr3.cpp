// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlDistVec3Qdr3.h"
#include "WmlEigen.h"
#include "WmlPolynomial1.h"
#include "WmlPolynomialRoots.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
static void ComputeCoeff3 (Real afA[3], Real afB[3], Real afD[3], Real fC,
    Polynomial1<Real>& rkPoly)
{
    Real afBPad[3] =
    {
        afB[0]+afA[0]*afD[0],
        afB[1]+afA[1]*afD[1],
        afB[2]+afA[2]*afD[2]
    };

    Real afBSqr[3] =
    {
        afB[0]*afB[0],
        afB[1]*afB[1],
        afB[2]*afB[2]
    };

    Real afDSum[4] =
    {
        afD[0]+afD[1],
        afD[0]+afD[2],
        afD[1]+afD[2],
        afD[0]+afD[1]+afD[2]
    };

    Real afDPrd[4] =
    {
        afD[0]*afD[1],
        afD[0]*afD[2],
        afD[1]*afD[2],
        afD[0]*afD[1]*afD[2]
    };

    Real afDSqr[3] =
    {
        afD[0]*afD[0],
        afD[1]*afD[1],
        afD[2]*afD[2]
    };

    rkPoly[0] = afA[0]*afBPad[0]+afA[1]*afBPad[1]+afA[2]*afBPad[2]+fC;

    rkPoly[1] = - afBSqr[0] - afBSqr[1] - afBSqr[2] + 4.0f*(
        afA[0]*afBPad[0]*afDSum[2] + afA[1]*afBPad[1]*afDSum[1] +
        afA[2]*afBPad[2]*afDSum[0] + fC*afDSum[3]);

    rkPoly[2] = - afBSqr[0]*(afD[0] + 4.0f*afDSum[2]) - afBSqr[1]*(afD[1] +
        4.0f*afDSum[1]) - afBSqr[2]*(afD[2] + 4.0f*afDSum[0]) + 4.0f*(
        afA[0]*afBPad[0]*(afDSum[2]*afDSum[2]+2*afDPrd[2]) +
        afA[1]*afBPad[1]*(afDSum[1]*afDSum[1]+2*afDPrd[1]) +
        afA[2]*afBPad[2]*(afDSum[0]*afDSum[0]+2*afDPrd[0]) +
        fC*(afDSqr[0]+afDSqr[1]+afDSqr[2]+4.0f*(
        afDPrd[0]+afDPrd[1]+afDPrd[2])));

    rkPoly[3] =
        - afBSqr[0]*(afD[1]*afDSum[0]+afD[2]*afDSum[1]+4.0f*afDPrd[2])
        - afBSqr[1]*(afD[0]*afDSum[0]+afD[2]*afDSum[2]+4.0f*afDPrd[1])
        - afBSqr[2]*(afD[0]*afDSum[1]+afD[1]*afDSum[2]+4.0f*afDPrd[0])
        + 4.0f*(afA[0]*afDPrd[2]*afBPad[0]*afDSum[2] +
        afA[1]*afDPrd[1]*afBPad[1]*afDSum[1] +
        afA[2]*afDPrd[0]*afBPad[2]*afDSum[0] +
        fC*(afDSqr[0]*afDSum[2]+afDSqr[1]*afDSum[1]+afDSqr[2]*afDSum[0]+
        4.0f*afDPrd[3]));

    rkPoly[3] *= 4.0f;

    rkPoly[4] =
        - afBSqr[0]*(afD[0]*(afDSqr[1]+afDSqr[2])+4.0f*afDPrd[2]*afDSum[3])
        - afBSqr[1]*(afD[1]*(afDSqr[0]+afDSqr[2])+4.0f*afDPrd[1]*afDSum[3])
        - afBSqr[2]*(afD[2]*(afDSqr[0]+afDSqr[1])+4.0f*afDPrd[0]*afDSum[3])
        + 4.0f*(afA[0]*afDSqr[1]*afDSqr[2]*afBPad[0] +
        afA[1]*afDSqr[0]*afDSqr[2]*afBPad[1] +
        afA[2]*afDSqr[0]*afDSqr[1]*afBPad[2] +
        fC*(afDSqr[0]*afDSqr[1]+afDSqr[0]*afDSqr[2]+afDSqr[1]*afDSqr[2]
        + 4.0f*afDPrd[3]*afDSum[3]));

    rkPoly[4] *= 4.0f;

    rkPoly[5] = 16.0f*(afDPrd[0]+afDPrd[1]+afDPrd[2])*(
        - afBSqr[0]*afDPrd[2] - afBSqr[1]*afDPrd[1] - afBSqr[2]*afDPrd[0] +
        4.0f*fC*afDPrd[3]);

    rkPoly[6] = 16.0f*afDPrd[3]*(- afBSqr[0]*afDPrd[2] - afBSqr[1]*afDPrd[1]
        - afBSqr[2]*afDPrd[0] + 4.0f*fC*afDPrd[3]);
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::SqrDistance (const Vector3<Real>& rkPoint, const Real afQuad[10],
    Vector3<Real>& rkClosest)
{
    // eigendecomposition
    Eigen<Real> kES(3);
    kES(0,0) = afQuad[4];
    kES(0,1) = ((Real)0.5)*afQuad[7];
    kES(0,2) = ((Real)0.5)*afQuad[8];
    kES(1,0) = kES(0,1);
    kES(1,1) = afQuad[5];
    kES(1,2) = ((Real)0.5)*afQuad[9];
    kES(2,0) = kES(0,2);
    kES(2,1) = kES(1,2);
    kES(2,2) = afQuad[6];

    kES.IncrSortEigenStuff3();
    Vector3<Real> akEVec[3];
    kES.GetEigenvector(0,akEVec[0]);
    kES.GetEigenvector(1,akEVec[1]);
    kES.GetEigenvector(2,akEVec[2]);

    Real afA[3], afB[3], afD[3], fC = afQuad[0];
    int i;
    for (i = 0; i < 3; i++)
    {
        afA[i] = akEVec[i].X()*rkPoint.X() + akEVec[i].Y()*rkPoint.Y() +
            akEVec[i].Z()*rkPoint.Z();
        afB[i] = akEVec[i].X()*afQuad[1] + akEVec[i].Y()*afQuad[2] +
            akEVec[i].Z()*afQuad[3];
        afD[i] = kES.GetEigenvalue(i);
    }

    Polynomial1<Real> kPoly(6);
    ComputeCoeff3(afA,afB,afD,fC,kPoly);

    PolynomialRoots<Real> kPR(Math<Real>::EPSILON);
    kPR.FindB(kPoly,6);
    int iCount = kPR.GetCount();
    const Real* afRoot = kPR.GetRoots();

    if ( iCount > 0 )
    {
        Real fMinDistSqr = Math<Real>::MAX_REAL;
        int iMinIndex = -1;
        Real afV[3], fDenom;
        for (int iIndex = 0; iIndex < iCount; iIndex++)
        {
            // compute closest point for this root
            for (i = 0; i < 3; i++)
            {
                fDenom = (Real)1.0 + ((Real)2.0)*afRoot[iIndex]*afD[i];
                afV[i] = (afA[i]-afRoot[iIndex]*afB[i])/fDenom;
            }

            rkClosest.X() = akEVec[0][0]*afV[0] + akEVec[1][0]*afV[1] +
                akEVec[2][0]*afV[2];
            rkClosest.Y() = akEVec[0][1]*afV[0] + akEVec[1][1]*afV[1] +
                akEVec[2][1]*afV[2];
            rkClosest.Z() = akEVec[0][2]*afV[0] + akEVec[1][2]*afV[1] +
                akEVec[2][2]*afV[2];

            // compute squared distance from point to quadric
            Vector3<Real> kDiff = rkClosest - rkPoint;
            Real fDistSqr = kDiff.SquaredLength();
            if ( fDistSqr < fMinDistSqr )
            {
                fMinDistSqr = fDistSqr;
                iMinIndex = iIndex;
            }
        }

        for (i = 0; i < 3; i++)
        {
            fDenom = (Real)1.0+((Real)2.0)*afRoot[iMinIndex]*afD[i];
            afV[i] = (afA[i]-afRoot[iMinIndex]*afB[i])/fDenom;
        }

        rkClosest.X() = akEVec[0][0]*afV[0] + akEVec[1][0]*afV[1] +
            akEVec[2][0]*afV[2];
        rkClosest.Y() = akEVec[0][1]*afV[0] + akEVec[1][1]*afV[1] +
            akEVec[2][1]*afV[2];
        rkClosest.Z() = akEVec[0][2]*afV[0] + akEVec[1][2]*afV[1] +
            akEVec[2][2]*afV[2];
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
Real Wml::Distance (const Vector3<Real>& rkPoint, const Real afQuad[10],
    Vector3<Real>& rkClosest)
{
    return Math<Real>::Sqrt(SqrDistance(rkPoint,afQuad,rkClosest));
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM float SqrDistance<float> (const Vector3<float>&,
    const float[10], Vector3<float>&);
template WML_ITEM float Distance<float> (const Vector3<float>&,
    const float[10], Vector3<float>&);

template WML_ITEM double SqrDistance<double> (const Vector3<double>&,
    const double[10], Vector3<double>&);
template WML_ITEM double Distance<double> (const Vector3<double>&,
    const double[10], Vector3<double>&);
}
//----------------------------------------------------------------------------
