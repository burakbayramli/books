// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntrElp2Elp2.h"
#include "WmlPolynomial1.h"
#include "WmlPolynomialRoots.h"
#include "WmlMatrix2.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
static void GetAABB (const Vector2<Real>& rkC, const Vector2<Real> akAxis[2],
    const Real afL[2], Real& rfXMin, Real& rfXMax, Real& rfYMin, Real& rfYMax)
{
    Real fRx = afL[0]*Math<Real>::FAbs(akAxis[0].X()) +
        afL[1]*Math<Real>::FAbs(akAxis[1].X());
    rfXMin = rkC.X() - fRx;
    rfXMax = rkC.X() + fRx;

    Real fRy = afL[0]*Math<Real>::FAbs(akAxis[0].Y()) +
        afL[1]*Math<Real>::FAbs(akAxis[1].Y());
    rfYMin = rkC.Y() - fRy;
    rfYMax = rkC.Y() + fRy;
}
//----------------------------------------------------------------------------
template <class Real>
static Real EvaluateQuadratic (const Real afQP[6], Real fX, Real fY)
{
    return (afQP[0]*fX + afQP[1]*fY + afQP[3])*fX + (afQP[2]*fY + afQP[4])*fY
        + afQP[5];
}
//----------------------------------------------------------------------------
template <class Real>
static Polynomial1<Real> GetQuartic (const Real afP0[6], const Real afP1[6])
{
    // polynomial is constructed as a Bezout determinant
    Real fAB = afP0[0]*afP1[1] - afP1[0]*afP0[1];
    Real fAC = afP0[0]*afP1[2] - afP1[0]*afP0[2];
    Real fAD = afP0[0]*afP1[3] - afP1[0]*afP0[3];
    Real fAE = afP0[0]*afP1[4] - afP1[0]*afP0[4];
    Real fAF = afP0[0]*afP1[5] - afP1[0]*afP0[5];
    Real fBC = afP0[1]*afP1[2] - afP1[1]*afP0[2];
    Real fBE = afP0[1]*afP1[4] - afP1[1]*afP0[4];
    Real fBF = afP0[1]*afP1[5] - afP1[1]*afP0[5];
    Real fCD = afP0[2]*afP1[3] - afP1[2]*afP0[3];
    Real fDE = afP0[3]*afP1[4] - afP1[3]*afP0[4];
    Real fDF = afP0[3]*afP1[5] - afP1[3]*afP0[5];
    Real fBFpDE = fBF+fDE;
    Real fBEmCD = fBE-fCD;

    Real afU[5] =
    {
        fAD*fDF-fAF*fAF,
        fAB*fDF+fAD*fBFpDE-((Real)2.0)*fAE*fAF,
        fAB*fBFpDE+fAD*fBEmCD-fAE*fAE-((Real)2.0)*fAC*fAF,
        fAB*fBEmCD+fAD*fBC-((Real)2.0)*fAC*fAE,
        fAB*fBC-fAC*fAC
    };

    // reduce degree if necessary
    int iDegree = 4;
    while ( iDegree > 0
    &&      Math<Real>::FAbs(afU[iDegree]) < Math<Real>::EPSILON )
    {
        iDegree--;
    }

    Polynomial1<Real> kPoly(iDegree);
    memcpy(&kPoly[0],afU,(iDegree+1)*sizeof(Real));
    return kPoly;
}
//----------------------------------------------------------------------------
template <class Real>
void Wml::ConvertEllipseToQuadratic (const Vector2<Real>& rkC,
    const Vector2<Real> akAxis[2], const Real afL[2], Real afQuad[6])
{
    Vector2<Real> akScaledAxis[2] =
    {
        akAxis[0]/afL[0],
        akAxis[1]/afL[1]
    };

    Matrix2<Real> kTensor0(akScaledAxis[0],akScaledAxis[0]);
    Matrix2<Real> kTensor1(akScaledAxis[1],akScaledAxis[1]);
    Matrix2<Real> kA = kTensor0 + kTensor1;
    Vector2<Real> kPrd = kA*rkC;
    Vector2<Real> kB = -((Real)2.0)*kPrd;
    Real fC = rkC.Dot(kPrd) - (Real)1.0;

    afQuad[0] = kA[0][0];
    afQuad[1] = ((Real)2.0)*kA[0][1];
    afQuad[2] = kA[1][1];
    afQuad[3] = kB[0];
    afQuad[4] = kB[1];
    afQuad[5] = fC;

    Real fMax = Math<Real>::FAbs(afQuad[0]);
    int i;
    for (i = 1; i < 6; i++)
    {
        Real fTest = Math<Real>::FAbs(afQuad[i]);
        if ( fTest > fMax )
            fMax = fTest;
    }

    Real fInvMax = ((Real)1.0)/fMax;
    for (i = 0; i < 6; i++)
        afQuad[i] *= fInvMax;
}
//----------------------------------------------------------------------------
template <class Real>
static void ScaleEllipsesToQuadratics (const Vector2<Real>& rkC0,
    const Vector2<Real> akAxis0[2], const Real afL0[2],
    const Vector2<Real>& rkC1, const Vector2<Real> akAxis1[2],
    const Real afL1[2], Real afQP0[6], Real afQP1[6], Real& rfMin,
    Real& rfHalfRange)
{
    // The ellipse E[i] is contained in the oriented rectangle with vertices
    //   C[i] + s0*L[i][0]*U[i] + s1*L[i][1]*V[i]
    // where |s0| = 1 and |s1| = 1 (four choices).  The implementation for
    // finding intersections first computes the smallest axis-aligned
    // rectangle that contains the oriented rectangles of the ellipses.  The
    // bounding rectangle is mapped to the square [-1,1]^2.  This portion of
    // the algorithm is designed to help keep the floating point calculations
    // robust in the root finding.

    // construct axis-aligned bounding rectangle for E0
    Real fXMin0, fXMax0, fYMin0, fYMax0;
    GetAABB(rkC0,akAxis0,afL0,fXMin0,fXMax0,fYMin0,fYMax0);

    // construct axis-aligned bounding rectangle for E1
    Real fXMin1, fXMax1, fYMin1, fYMax1;
    GetAABB(rkC1,akAxis1,afL1,fXMin1,fXMax1,fYMin1,fYMax1);

    // construct axis-aligned bounding square of bounding rectangles
    Real fMin = fXMin0, fMax = fXMax0;
    if ( fXMin1 < fMin ) fMin = fXMin1;
    if ( fXMax1 > fMax ) fMax = fXMax1;
    if ( fYMin0 < fMin ) fMin = fYMin0;
    if ( fYMax0 > fMax ) fMax = fYMax0;
    if ( fYMin1 < fMin ) fMin = fYMin1;
    if ( fYMax1 > fMax ) fMax = fYMax1;

    Real fHalfRange = ((Real)0.5)*(fMax - fMin);
    Real fInvHalfRange = ((Real)1.0)/fHalfRange;

    // map ellipses to bounding square [-1,1]^2
    Vector2<Real> kC0Tmp, kC1Tmp;
    Real afL0Tmp[2], afL1Tmp[2];

    kC0Tmp.X() = -(Real)1.0 + fInvHalfRange*(rkC0.X() - fMin);
    kC0Tmp.Y() = -(Real)1.0 + fInvHalfRange*(rkC0.Y() - fMin);
    kC1Tmp.X() = -(Real)1.0 + fInvHalfRange*(rkC1.X() - fMin);
    kC1Tmp.Y() = -(Real)1.0 + fInvHalfRange*(rkC1.Y() - fMin);
    afL0Tmp[0] = fInvHalfRange*afL0[0];
    afL0Tmp[1] = fInvHalfRange*afL0[1];
    afL1Tmp[0] = fInvHalfRange*afL1[0];
    afL1Tmp[1] = fInvHalfRange*afL1[1];

    // convert ellipses to quadratic equations
    ConvertEllipseToQuadratic(kC0Tmp,akAxis0,afL0Tmp,afQP0);
    ConvertEllipseToQuadratic(kC1Tmp,akAxis1,afL1Tmp,afQP1);

    rfMin = fMin;
    rfHalfRange = fHalfRange;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (const Vector2<Real>& rkC0,
    const Vector2<Real> akAxis0[2], const Real afL0[2],
    const Vector2<Real>& rkC1, const Vector2<Real> akAxis1[2],
    const Real afL1[2], int& riQuantity, Vector2<Real> akP[4])
{
    Real afQP0[6], afQP1[6], fMin, fHalfRange;
    ScaleEllipsesToQuadratics(rkC0,akAxis0,afL0,rkC1,akAxis1,afL1,afQP0,afQP1,
        fMin,fHalfRange);

    // Get the 4th-degree polynomial whose roots lead to intersections of the
    // ellipses.  The y-values are in the interval [-1,1], so we only need
    // to locate roots there.  The increase to [-1.5,1.5] is to catch roots
    // that are nearly 1 or -1 without having to worry about floating point
    // round-off errors.
    Polynomial1<Real> kPoly = GetQuartic(afQP0,afQP1);
    PolynomialRoots<Real> kPR(Math<Real>::EPSILON);
    kPR.FindB(kPoly,-(Real)1.5,(Real)1.5,6);
    int iYCount = kPR.GetCount();
    const Real* afY = kPR.GetRoots();
    if ( iYCount == 0 )
    {
        riQuantity = 0;
        return false;
    }

    // Adjustment for quadratics to allow for relative error testing when
    // eliminating extraneous roots.
    Real fNorm0 = afQP0[0]*afQP0[0] + ((Real)2.0)*afQP0[1]*afQP0[1] +
        afQP0[2]*afQP0[2];
    Real fNorm1 = afQP1[0]*afQP1[0] + ((Real)2.0)*afQP1[1]*afQP1[1] +
        afQP1[2]*afQP1[2];

    // test roots to eliminate extraneous ones that occurred due to "squaring"
    riQuantity = 0;
    int iXCount;
    const Real* afX;
    for (int iY = 0; iY < iYCount; iY++)
    {
        Polynomial1<Real> kAPoly(2);
        kAPoly[0] = afQP0[5]+afY[iY]*(afQP0[4]+afY[iY]*afQP0[2]);
        kAPoly[1] = afQP0[3]+afY[iY]*afQP0[1];
        kAPoly[2] = afQP0[0];
        kPR.FindB(kAPoly,6);
        iXCount = kPR.GetCount();
        afX = kPR.GetRoots();
        for (int iX = 0; iX < iXCount; iX++)
        {
            const Real fEpsilon = (Real)1e-03;
            Real fTest = EvaluateQuadratic(afQP0,afX[iX],afY[iY]);
            if ( Math<Real>::FAbs(fTest) < fEpsilon*fNorm0 )
            {
                fTest = EvaluateQuadratic(afQP1,afX[iX],afY[iY]);
                if ( Math<Real>::FAbs(fTest) < fEpsilon*fNorm1 )
                {
                    akP[riQuantity].X() = afX[iX];
                    akP[riQuantity].Y() = afY[iY];
                    riQuantity++;
                }
            }
        }
    }

    // map intersections back to original space
    for (int i = 0; i < riQuantity; i++)
    {
        akP[i].X() = fHalfRange*(akP[i].X()+(Real)1.0) + fMin;
        akP[i].Y() = fHalfRange*(akP[i].Y()+(Real)1.0) + fMin;
    }

    return riQuantity > 0;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Vector2<Real>& rkC0,
    const Vector2<Real> akAxis0[2], const Real afL0[2],
    const Vector2<Real>& rkC1, const Vector2<Real> akAxis1[2],
    const Real afL1[2])
{
    Real afQP0[6], afQP1[6], fMin, fHalfRange;
    ScaleEllipsesToQuadratics(rkC0,akAxis0,afL0,rkC1,akAxis1,afL1,afQP0,afQP1,
        fMin,fHalfRange);

    Polynomial1<Real> kPoly = GetQuartic(afQP0,afQP1);

    PolynomialRoots<Real> kPR(Math<Real>::EPSILON);
    int iCount = kPR.GetRootCount(kPoly,-Math<Real>::MAX_REAL,
        Math<Real>::MAX_REAL);

    return iCount > 0;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Real afQP0[6], const Real afQP1[6])
{
    Polynomial1<Real> kPoly = GetQuartic(afQP0,afQP1);

    PolynomialRoots<Real> kPR(Math<Real>::EPSILON);
    int iCount = kPR.GetRootCount(kPoly,-Math<Real>::MAX_REAL,
        Math<Real>::MAX_REAL);

    return iCount > 0;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM void ConvertEllipseToQuadratic<float> (
    const Vector2<float>&, const Vector2<float>[2], const float[2],
    float[6]);
template WML_ITEM bool FindIntersection<float> (
    const Vector2<float>&, const Vector2<float>[2], const float[2],
    const Vector2<float>&, const Vector2<float>[2],  const float[2],
    int&, Vector2<float>[4]);
template WML_ITEM bool TestIntersection<float> (
    const Vector2<float>&, const Vector2<float>[2], const float[2],
    const Vector2<float>&, const Vector2<float>[2], const float[2]);
template WML_ITEM bool TestIntersection<float> (const float[6],
    const float[6]);

template WML_ITEM void ConvertEllipseToQuadratic<double> (
    const Vector2<double>&, const Vector2<double>[2], const double[2],
    double[6]);
template WML_ITEM bool FindIntersection<double> (
    const Vector2<double>&, const Vector2<double>[2], const double[2],
    const Vector2<double>&, const Vector2<double>[2],  const double[2],
    int&, Vector2<double>[4]);
template WML_ITEM bool TestIntersection<double> (
    const Vector2<double>&, const Vector2<double>[2], const double[2],
    const Vector2<double>&, const Vector2<double>[2], const double[2]);
template WML_ITEM bool TestIntersection<double> (const double[6],
    const double[6]);
}
//----------------------------------------------------------------------------
