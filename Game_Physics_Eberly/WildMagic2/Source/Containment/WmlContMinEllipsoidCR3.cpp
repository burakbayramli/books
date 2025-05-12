// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlContMinEllipsoidCR3.h"
using namespace Wml;

// forward reference for recursive chain with FindEdgeMax
template <class Real>
static void FindFacetMax (int,Real*,Real*,Real*,int&,Real&,Real&,Real&);

//----------------------------------------------------------------------------
template <class Real>
static void FindEdgeMax (int iQuantity, Real* afA, Real* afB, Real* afC,
    int& riPlane0, int& riPlane1, Real& rfX0, Real& rfY0, Real& rfZ0)
{
    // compute direction to local maximum point on line of intersection
    Real fXDir = afB[riPlane0]*afC[riPlane1]-afB[riPlane1]*afC[riPlane0];
    Real fYDir = afC[riPlane0]*afA[riPlane1]-afC[riPlane1]*afA[riPlane0];
    Real fZDir = afA[riPlane0]*afB[riPlane1]-afA[riPlane1]*afB[riPlane0];

    // build quadratic Q'(t) = (d/dt)(x(t)y(t)z(t)) = a0+a1*t+a2*t^2
    Real fA0 = rfX0*rfY0*fZDir + rfX0*rfZ0*fYDir + rfY0*rfZ0*fXDir;
    Real fA1 = ((Real)2.0)*(rfZ0*fXDir*fYDir + rfY0*fXDir*fZDir +
        rfX0*fYDir*fZDir);
    Real fA2 = ((Real)3.0)*(fXDir*fYDir*fZDir);

    // find root to Q'(t) = 0 corresponding to maximum
    Real fTFinal;
    if ( fA2 != (Real)0.0 )
    {
        Real fDiscr = fA1*fA1 - ((Real)4.0)*fA0*fA2;
        assert( fDiscr >= (Real)0.0 );
        fDiscr = Math<Real>::Sqrt(fDiscr);
        fTFinal = -((Real)0.5)*(fA1 + fDiscr)/fA2;
        if ( fA1 + ((Real)2.0)*fA2*fTFinal > (Real)0.0 )
            fTFinal = ((Real)0.5)*(-fA1 + fDiscr)/fA2;
    }
    else if ( fA1 != (Real)0.0 )
    {
        fTFinal = -fA0/fA1;
    }
    else if ( fA0 != (Real)0.0 )
    {
        fTFinal = ( fA0 >= (Real)0.0 ? Math<Real>::MAX_REAL :
            -Math<Real>::MAX_REAL );
    }
    else
    {
        return;
    }

    if ( fTFinal < (Real)0.0 )
    {
        // make (xDir,yDir,zDir) point in direction of increase of Q
        fTFinal = -fTFinal;
        fXDir = -fXDir;
        fYDir = -fYDir;
        fZDir = -fZDir;
    }

    // sort remaining planes along line from current point to
    // local maximum
    Real fTMax = fTFinal;
    int iPlane2 = -1;
    for (int i = 0; i < iQuantity; i++)
    {
        if ( i == riPlane0 || i == riPlane1 )
            continue;

        Real fNorDotDir = afA[i]*fXDir + afB[i]*fYDir + afC[i]*fZDir;
        if ( fNorDotDir <= (Real)0.0 )
            continue;

        // Theoretically the numerator must be nonnegative since an
        // invariant in the algorithm is that (x0,y0,z0) is on the
        // convex hull of the constraints.  However, some numerical
        // error may make this a small negative number.  In that case
        // set tmax = 0 (no change in position).
        Real fNumer = (Real)1.0 - afA[i]*rfX0 - afB[i]*rfY0 - afC[i]*rfZ0;
        if ( fNumer < (Real)0.0 )
        {
            assert( fNumer >= -Math<Real>::EPSILON );
            iPlane2 = i;
            fTMax = (Real)0.0;
            break;
        }

        Real fT = fNumer/fNorDotDir;
        if ( 0 <= fT && fT < fTMax )
        {
            iPlane2 = i;
            fTMax = fT;
        }
    }

    rfX0 += fTMax*fXDir;
    rfY0 += fTMax*fYDir;
    rfZ0 += fTMax*fZDir;

    if ( fTMax == fTFinal )
        return;

    if ( fTMax > Math<Real>::EPSILON )
    {
        riPlane0 = iPlane2;
        FindFacetMax(iQuantity,afA,afB,afC,riPlane0,rfX0,rfY0,rfZ0);
        return;
    }

    // tmax == 0, return with x0, y0, z0 unchanged
}
//----------------------------------------------------------------------------
template <class Real>
static void FindFacetMax (int iQuantity, Real* afA, Real* afB, Real* afC,
    int& riPlane0, Real& rfX0, Real& rfY0, Real& rfZ0)
{
    Real fTFinal, fXDir, fYDir, fZDir;

    if ( afA[riPlane0] > Math<Real>::EPSILON
    &&   afB[riPlane0] > Math<Real>::EPSILON
    &&   afC[riPlane0] > Math<Real>::EPSILON )
    {
        // compute local maximum point on plane
        const Real fOneThird = (Real)(1.0/3.0);
        Real fX1 = fOneThird/afA[riPlane0];
        Real fY1 = fOneThird/afB[riPlane0];
        Real fZ1 = fOneThird/afC[riPlane0];
        
        // compute direction to local maximum point on plane
        fTFinal = (Real)1.0;
        fXDir = fX1 - rfX0;
        fYDir = fY1 - rfY0;
        fZDir = fZ1 - rfZ0;
    }
    else
    {
        fTFinal = Math<Real>::MAX_REAL;
        fXDir = (afA[riPlane0] > Math<Real>::EPSILON ? (Real)0.0 : (Real)1.0);
        fYDir = (afB[riPlane0] > Math<Real>::EPSILON ? (Real)0.0 : (Real)1.0);
        fZDir = (afC[riPlane0] > Math<Real>::EPSILON ? (Real)0.0 : (Real)1.0);
    }
    
    // sort remaining planes along line from current point
    Real fTMax = fTFinal;
    int iPlane1 = -1;
    for (int i = 0; i < iQuantity; i++)
    {
        if ( i == riPlane0 )
            continue;

        Real fNorDotDir = afA[i]*fXDir + afB[i]*fYDir + afC[i]*fZDir;
        if ( fNorDotDir <= (Real)0.0 )
            continue;

        // Theoretically the numerator must be nonnegative since an
        // invariant in the algorithm is that (x0,y0,z0) is on the
        // convex hull of the constraints.  However, some numerical
        // error may make this a small negative number.  In that case
        // set tmax = 0 (no change in position).
        Real fNumer = (Real)1.0 - afA[i]*rfX0 - afB[i]*rfY0 - afC[i]*rfZ0;
        if ( fNumer < (Real)0.0 )
        {
            assert( fNumer >= -Math<Real>::EPSILON );
            iPlane1 = i;
            fTMax = (Real)0.0;
            break;
        }

        Real fT = fNumer/fNorDotDir;
        if ( 0 <= fT && fT < fTMax )
        {
            iPlane1 = i;
            fTMax = fT;
        }
    }

    rfX0 += fTMax*fXDir;
    rfY0 += fTMax*fYDir;
    rfZ0 += fTMax*fZDir;

    if ( fTMax == (Real)1.0 )
        return;

    if ( fTMax > Math<Real>::EPSILON )
    {
        riPlane0 = iPlane1;
        FindFacetMax(iQuantity,afA,afB,afC,riPlane0,rfX0,rfY0,rfZ0);
        return;
    }

    FindEdgeMax(iQuantity,afA,afB,afC,riPlane0,iPlane1,rfX0,rfY0,rfZ0);
}
//----------------------------------------------------------------------------
template <class Real>
static void MaxProduct (int iQuantity, Real* afA, Real* afB, Real* afC,
    Real& rfX, Real& rfY, Real& rfZ)
{
    // Maximize x*y*z subject to x >= 0, y >= 0, z >= 0, and
    // A[i]*x+B[i]*y+C[i]*z <= 1 for 0 <= i < N where A[i] >= 0,
    // B[i] >= 0, and C[i] >= 0.

    // Jitter the lines to avoid cases where more than three planes
    // intersect at the same point.  Should also break parallelism
    // and planes parallel to the coordinate planes.
    const Real fMaxJitter = (Real)1e-12;
    int i;
    for (i = 0; i < iQuantity; i++)
    {
        afA[i] += fMaxJitter*Math<Real>::UnitRandom();
        afB[i] += fMaxJitter*Math<Real>::UnitRandom();
        afC[i] += fMaxJitter*Math<Real>::UnitRandom();
    }

    // sort lines along rfZ-axis (rfX=0 and rfY=0)
    int iPlane = -1;
    Real fCMax = (Real)0.0;
    for (i = 0; i < iQuantity; i++)
    {
        if ( afC[i] > fCMax )
        {
            fCMax = afC[i];
            iPlane = i;
        }
    }
    assert( iPlane != -1 );

    // walk along convex hull searching for maximum
    rfX = (Real)0.0;
    rfY = (Real)0.0;
    rfZ = ((Real)1.0)/fCMax;
    FindFacetMax(iQuantity,afA,afB,afC,iPlane,rfX,rfY,rfZ);
}
//----------------------------------------------------------------------------
template <class Real>
void Wml::MinEllipsoidCR3 (int iQuantity, const Vector3<Real>* akPoint,
    const Vector3<Real>& rkC, const Matrix3<Real>& rkR, Real afD[3])
{
    // Given center C and orientation R, finds minimum volume ellipsoid
    // (X-C)^t R^t D R (X-C) = 1 where D is a diagonal matrix whose diagonal
    // entries are positive.  The problem is equivalent to maximizing the
    // product D[0]*D[1]*D[2] given C and R and subject to the constraints
    // (P[i]-C)^t R^t D R (P[i]-C) <= 1 for all input points P[i] with
    // 0 <= i < N.  Each constraint has form a0*D[0]+a1*D[1]+a2*D[2] <= 1
    // where a0 >= 0, a1 >= 0, and a2 >= 0.

    Real* afA0 = new Real[iQuantity];
    Real* afA1 = new Real[iQuantity];
    Real* afA2 = new Real[iQuantity];

    for (int i = 0; i < iQuantity; i++)
    {
        Vector3<Real> kDiff = akPoint[i] - rkC;
        Vector3<Real> kProd = rkR*kDiff;

        afA0[i] = kProd.X()*kProd.X();
        afA1[i] = kProd.Y()*kProd.Y();
        afA2[i] = kProd.Z()*kProd.Z();
    }

    MaxProduct(iQuantity,afA0,afA1,afA2,afD[0],afD[1],afD[2]);

    delete[] afA2;
    delete[] afA1;
    delete[] afA0;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM void MinEllipsoidCR3<float> (int,
    const Vector3<float>*, const Vector3<float>&, const Matrix3<float>&,
    float[3]);

template WML_ITEM void MinEllipsoidCR3<double> (int,
    const Vector3<double>*, const Vector3<double>&, const Matrix3<double>&,
    double[3]);
}
//----------------------------------------------------------------------------
