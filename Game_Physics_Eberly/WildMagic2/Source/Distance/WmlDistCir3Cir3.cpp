// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlDistCir3Cir3.h"
#include "WmlPolynomial1.h"
#include "WmlPolynomialRoots.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Real Wml::SqrDistance (const Circle3<Real>& rkCircle0,
    const Circle3<Real>& rkCircle1, Vector3<Real>* pkClosest0,
    Vector3<Real>* pkClosest1)
{
    Vector3<Real> kDiff = rkCircle1.Center() - rkCircle0.Center();
    Real fU0U1 = rkCircle0.U().Dot(rkCircle1.U());
    Real fU0V1 = rkCircle0.U().Dot(rkCircle1.V());
    Real fV0U1 = rkCircle0.V().Dot(rkCircle1.U());
    Real fV0V1 = rkCircle0.V().Dot(rkCircle1.V());

    Real fA0 = -kDiff.Dot(rkCircle0.U());
    Real fA1 = -rkCircle1.Radius()*fU0U1;
    Real fA2 = -rkCircle1.Radius()*fU0V1;
    Real fA3 = kDiff.Dot(rkCircle0.V());
    Real fA4 = rkCircle1.Radius()*fV0U1;
    Real fA5 = rkCircle1.Radius()*fV0V1;

    Real fB0 = -kDiff.Dot(rkCircle1.U());
    Real fB1 = rkCircle0.Radius()*fU0U1;
    Real fB2 = rkCircle0.Radius()*fV0U1;
    Real fB3 = kDiff.Dot(rkCircle1.V());
    Real fB4 = -rkCircle0.Radius()*fU0V1;
    Real fB5 = -rkCircle0.Radius()*fV0V1;

    // compute polynomial p0 = p00+p01*z+p02*z^2
    Polynomial1<Real> kP0(2);
    kP0[0] = fA2*fB1-fA5*fB2;
    kP0[1] = fA0*fB4-fA3*fB5;
    kP0[2] = fA5*fB2-fA2*fB1+fA1*fB4-fA4*fB5;

    // compute polynomial p1 = p10+p11*z
    Polynomial1<Real> kP1(1);
    kP1[0] = fA0*fB1-fA3*fB2;
    kP1[1] = fA1*fB1-fA5*fB5+fA2*fB4-fA4*fB2;

    // compute polynomial q0 = q00+q01*z+q02*z^2
    Polynomial1<Real> kQ0(2);
    kQ0[0] = fA0*fA0+fA2*fA2+fA3*fA3+fA5*fA5;
    kQ0[1] = ((Real)2.0)*(fA0*fA1+fA3*fA4);
    kQ0[2] = fA1*fA1-fA2*fA2+fA4*fA4-fA5*fA5;

    // compute polynomial q1 = q10+q11*z
    Polynomial1<Real> kQ1(1);
    kQ1[0] = ((Real)2.0)*(fA0*fA2+fA3*fA5);
    kQ1[1] = ((Real)2.0)*(fA1*fA2+fA4*fA5);

    // compute coefficients of r0 = r00+r02*z^2
    Polynomial1<Real> kR0(2);
    kR0[0] = fB0*fB0;
    kR0[1] = (Real)0.0;
    kR0[2] = fB3*fB3-fB0*fB0;

    // compute polynomial r1 = r11*z;
    Polynomial1<Real> kR1(1);
    kR1[0] = (Real)0.0;
    kR1[1] = ((Real)2.0)*fB0*fB3;

    // compute polynomial g0 = g00+g01*z+g02*z^2+g03*z^3+g04*z^4
    Polynomial1<Real> kG0(4);
    kG0[0] = kP0[0]*kP0[0] + kP1[0]*kP1[0] - kQ0[0]*kR0[0];
    kG0[1] = ((Real)2.0)*(kP0[0]*kP0[1] + kP1[0]*kP1[1]) - kQ0[1]*kR0[0] -
        kQ1[0]*kR1[1];
    kG0[2] = kP0[1]*kP0[1] + ((Real)2.0)*kP0[0]*kP0[2] - kP1[0]*kP1[0] +
        kP1[1]*kP1[1] - kQ0[2]*kR0[0] - kQ0[0]*kR0[2] - kQ1[1]*kR1[1];
    kG0[3] = ((Real)2.0)*(kP0[1]*kP0[2] - kP1[0]*kP1[1]) - kQ0[1]*kR0[2] +
        kQ1[0]*kR1[1];
    kG0[4] = kP0[2]*kP0[2] - kP1[1]*kP1[1] - kQ0[2]*kR0[2] + kQ1[1]*kR1[1];

    // compute polynomial g1 = g10+g11*z+g12*z^2+g13*z^3
    Polynomial1<Real> kG1(3);
    kG1[0] = ((Real)2.0)*kP0[0]*kP1[0] - kQ1[0]*kR0[0];
    kG1[1] = ((Real)2.0)*(kP0[1]*kP1[0] + kP0[0]*kP1[1]) - kQ1[1]*kR0[0] -
        kQ0[0]*kR1[1];
    kG1[2] = ((Real)2.0)*(kP0[2]*kP1[0] + kP0[1]*kP1[1]) - kQ1[0]*kR0[2] -
        kQ0[1]*kR1[1];
    kG1[3] = ((Real)2.0)*kP0[2]*kP1[1] - kQ1[1]*kR0[2] - kQ0[2]*kR1[1];

    // compute polynomial h = sum_{i=0}^8 h_i z^i
    Polynomial1<Real> kH(8);
    kH[0] = kG0[0]*kG0[0] - kG1[0]*kG1[0];
    kH[1] = ((Real)2.0)*(kG0[0]*kG0[1] - kG1[0]*kG1[1]);
    kH[2] = kG0[1]*kG0[1] + kG1[0]*kG1[0] - kG1[1]*kG1[1] +
        ((Real)2.0)*(kG0[0]*kG0[2] - kG1[0]*kG1[2]);
    kH[3] = ((Real)2.0)*(kG0[1]*kG0[2] + kG0[0]*kG0[3] + kG1[0]*kG1[1] -
        kG1[1]*kG1[2] - kG1[0]*kG1[3]);
    kH[4] = kG0[2]*kG0[2] + kG1[1]*kG1[1] - kG1[2]*kG1[2] +
        ((Real)2.0)*(kG0[1]*kG0[3] + kG0[0]*kG0[4] + kG1[0]*kG1[2] -
        kG1[1]*kG1[3]);
    kH[5] = ((Real)2.0)*(kG0[2]*kG0[3] + kG0[1]*kG0[4] + kG1[1]*kG1[2] +
        kG1[0]*kG1[3] - kG1[2]*kG1[3]);
    kH[6] = kG0[3]*kG0[3] + kG1[2]*kG1[2] - kG1[3]*kG1[3] +
        ((Real)2.0)*(kG0[2]*kG0[4] + kG1[1]*kG1[3]);
    kH[7] = ((Real)2.0)*(kG0[3]*kG0[4] + kG1[2]*kG1[3]);
    kH[8] = kG0[4]*kG0[4] + kG1[3]*kG1[3];

    PolynomialRoots<Real> kPR(Math<Real>::EPSILON);
    kPR.FindB(kH,-(Real)1.01,(Real)1.01,6);
    int iCount = kPR.GetCount();
    const Real* afRoot = kPR.GetRoots();

    Real fMinSqrDist = Math<Real>::MAX_REAL;
    Real fCs0, fSn0, fCs1, fSn1;

    for (int i = 0; i < iCount; i++)
    {
        fCs1 = afRoot[i];
        if ( fCs1 < -(Real)1.0 )
            fCs1 = -(Real)1.0;
        else if ( fCs1 > (Real)1.0 )
            fCs1 = (Real)1.0;

        // You can also try sn1 = -g0(cs1)/g1(cs1) to avoid the sqrt call,
        // but beware when g1 is nearly zero.  For now I use g0 and g1 to
        // determine the sign of sn1.
        fSn1 = Math<Real>::Sqrt(Math<Real>::FAbs((Real)1.0-fCs1*fCs1));

        Real fG0 = kG0(fCs1), fG1 = kG1(fCs1), fProd = fG0*fG1;
        if ( fProd > (Real)0.0 )
        {
            fSn1 = -fSn1;
        }
        else if ( fProd < (Real)0.0 )
        {
            // fSn1 already has correct sign
        }
        else if ( fG1 != (Real)0.0 )
        {
            // g0 == 0.0
            // assert( fSn1 == 0.0 );
        }
        else // g1 == 0.0
        {
            // TO DO:  When g1 = 0, there is no constraint on fSn1.
            // What should be done here?  In this case, fCs1 is a afRoot
            // to the quartic equation g0(fCs1) = 0.  Is there some
            // geometric significance?
            assert( false );
        }

        Real fM00 = fA0 + fA1*fCs1 + fA2*fSn1;
        Real fM01 = fA3 + fA4*fCs1 + fA5*fSn1;
        Real fM10 = fB2*fSn1 + fB5*fCs1;
        Real fM11 = fB1*fSn1 + fB4*fCs1;
        Real fDet = fM00*fM11 - fM01*fM10;
        if ( Math<Real>::FAbs(fDet) >= Math<Real>::EPSILON )
        {
            Real fInvDet = ((Real)1.0)/fDet;
            Real fLambda = -(fB0*fSn1 + fB3*fCs1);
            fCs0 = fLambda*fM00*fInvDet;
            fSn0 = -fLambda*fM01*fInvDet;

            // Unitize in case of numerical error.  Remove if you feel
            // confidant of the accuracy for fCs0 and fSn0.
            Real fTmp = Math<Real>::InvSqrt(fCs0*fCs0+fSn0*fSn0);
            fCs0 *= fTmp;
            fSn0 *= fTmp;

            Vector3<Real> kClosest0 = rkCircle0.Center() +
                rkCircle0.Radius()*(fCs0*rkCircle0.U() + fSn0*rkCircle0.V());
            Vector3<Real> kClosest1 = rkCircle1.Center() +
                rkCircle1.Radius()*(fCs1*rkCircle1.U() + fSn1*rkCircle1.V());
            kDiff = kClosest1 - kClosest0;

            Real fSqrDist = kDiff.SquaredLength();
            if ( fSqrDist < fMinSqrDist )
            {
                fMinSqrDist = fSqrDist;
                if ( pkClosest0 )
                    *pkClosest0 = kClosest0;
                if ( pkClosest1 )
                    *pkClosest1 = kClosest1;
            }
        }
        else
        {
            // TO DO:  Handle this case.  Is there some geometric
            // significance?
            assert( false );
        }
    }

    return fMinSqrDist;
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::Distance (const Circle3<Real>& rkCircle0,
    const Circle3<Real>& rkCircle1, Vector3<Real>* pkClosest0,
    Vector3<Real>* pkClosest1)
{
    return Math<Real>::Sqrt(SqrDistance(rkCircle0,rkCircle1,pkClosest0,
        pkClosest1));
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM float SqrDistance<float> (const Circle3<float>&,
    const Circle3<float>&, Vector3<float>*, Vector3<float>*);
template WML_ITEM float Distance<float> (const Circle3<float>&,
    const Circle3<float>&, Vector3<float>*, Vector3<float>*);

template WML_ITEM double SqrDistance<double> (const Circle3<double>&,
    const Circle3<double>&, Vector3<double>*, Vector3<double>*);
template WML_ITEM double Distance<double> (const Circle3<double>&,
    const Circle3<double>&, Vector3<double>*, Vector3<double>*);
}
//----------------------------------------------------------------------------
