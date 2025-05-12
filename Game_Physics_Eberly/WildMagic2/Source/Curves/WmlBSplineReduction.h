// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLBSPLINEREDUCTION_H
#define WMLBSPLINEREDUCTION_H

#include "WmlIntegrate1.h"
#include "WmlIntrInterval.h"
#include "WmlLinearSystem.h"
#include "WmlVector.h"

namespace Wml
{

template <int N, class Real>
class WML_ITEM BSplineReduction
{
public:
    // The input quantity iQuantity must be 2 or larger.  The caller is
    // responsible for deleting the input array akCtrl and the output array
    // akCtrlOut.  The degree iDegree of the input curve must satisfy the
    // condition 1 <= iDegree <= iQuantity-1.  The degree of the output
    // curve is the same as that of the input curve.  The value fFraction
    // must be in [0,1].  If the fraction is 1, the output curve is identical
    // to the input curve.  If the fraction is too small to produce a valid
    // number of control points, the output control quantity is chosen to be
    // riQuantityOut = iDegree+1.

    BSplineReduction (int iQuantity, const Vector<N,Real>* akCtrl,
        int iDegree, Real fFraction, int& riQuantityOut,
        Vector<N,Real>*& rakCtrlOut);

    ~BSplineReduction ();

private:
    Real MinSupport (int iBasis, int i) const;
    Real MaxSupport (int iBasis, int i) const;
    Real F (int iBasis, int i, int j, Real fTime);
    static Real Integrand (Real fTime, void* pvThis);

    int m_iDegree;
    int m_aiQuantity[2];
    int m_aiNumKnots[2];  // N+D+2
    Real* m_aafKnot[2];

    // for the integration-based least-squares fitting
    int m_aiBasis[2], m_aiIndex[2];
};

typedef BSplineReduction<2,float> BSplineReduction2f;
typedef BSplineReduction<2,double> BSplineReduction2d;
typedef BSplineReduction<3,float> BSplineReduction3f;
typedef BSplineReduction<3,double> BSplineReduction3d;

}

#endif
