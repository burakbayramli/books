// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlApprCircleFit2.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
bool Wml::CircleFit (int iQuantity, const Vector2<Real>* akPoint,
    int iMaxIterations, Vector2<Real>& rkCenter, Real& rfRadius)
{
    // compute the average of the data points
    Vector2<Real> kAverage = akPoint[0];
    int i0;
    for (i0 = 1; i0 < iQuantity; i0++)
        kAverage += akPoint[i0];
    Real fInvQuantity = ((Real)1.0)/(Real)iQuantity;
    kAverage *= fInvQuantity;

    // initial guess
    rkCenter = kAverage;

    int i1;
    for (i1 = 0; i1 < iMaxIterations; i1++)
    {
        // update the iterates
        Vector2<Real> kCurrent = rkCenter;

        // compute average L, dL/da, dL/db
        Real fLAverage = (Real)0.0;
        Vector2<Real> kDerLAverage = Vector2<Real>::ZERO;
        for (i0 = 0; i0 < iQuantity; i0++)
        {
            Vector2<Real> kDiff = akPoint[i0] - rkCenter;
            Real fLength = kDiff.Length();
            if ( fLength > Math<Real>::EPSILON )
            {
                fLAverage += fLength;
                Real fInvLength = ((Real)1.0)/fLength;
                kDerLAverage -= fInvLength*kDiff;
            }
        }
        fLAverage *= fInvQuantity;
        kDerLAverage *= fInvQuantity;

        rkCenter = kAverage + fLAverage*kDerLAverage;
        rfRadius = fLAverage;

        Vector2<Real> kDiff = rkCenter - kCurrent;
        if ( Math<Real>::FAbs(kDiff.X()) <= Math<Real>::EPSILON
        &&   Math<Real>::FAbs(kDiff.Y()) <= Math<Real>::EPSILON )
        {
            break;
        }
    }

    return i1 < iMaxIterations;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM bool CircleFit<float> (int, const Vector2<float>*,
    int, Vector2<float>&, float&);
template WML_ITEM bool CircleFit<double> (int, const Vector2<double>*,
    int, Vector2<double>&, double&);
}
//----------------------------------------------------------------------------
