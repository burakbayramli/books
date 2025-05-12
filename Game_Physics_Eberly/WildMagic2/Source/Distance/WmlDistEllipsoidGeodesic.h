// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLDISTCELLIPSOIDGEODESIC_H
#define WMLDISTCELLIPSOIDGEODESIC_H

#include "WmlVector3.h"

namespace Wml
{

// Ellipsoid is (x/a0)^2 + (y/a1)^2 + (z/a2)^2 = 1.
// Input:   axis[] = { a0, a1, a2 }
//          P, Q are points on the ellipsoid
// Output:  Geodesic distance between P and Q (distance of shortest
//          path on ellipsoid between P and Q)

template <class Real>
class WML_ITEM EllipsoidGeodesicDistance
{
public:
    EllipsoidGeodesicDistance (const Real afAxis[3], const Vector3<Real>& rkP,
        const Vector3<Real>& rkQ, Real& rfDistance);

private:
    // Callback for the numerical integration of the differential equations
    // that define the geodesic path.
    static Real Speed (Real fT, void* pvData);

    // parameters used by the numerical integration
    Real m_fPP, m_fPQ, m_fQQ, m_fPDQ;

    static const Real ms_fEpsilon;
};

typedef EllipsoidGeodesicDistance<float> EllipsoidGeodesicDistancef;
typedef EllipsoidGeodesicDistance<double> EllipsoidGeodesicDistanced;

}

#endif
