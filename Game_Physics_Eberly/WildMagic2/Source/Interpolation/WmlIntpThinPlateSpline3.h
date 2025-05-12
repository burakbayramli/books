// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTPTHINPLATESPLINE3_H
#define WMLINTPTHINPLATESPLINE3_H

// WARNING.  This code maps the inputs (x,y,z) to the unit cube.  The thin
// plate spline function evaluation maps the input to the unit cube and
// performs the interpolation in that space.  The idea is to keep the floating
// point numbers to order 1 for numerical stability of the algorithm. Some
// folks are not excited about this preprocessing step as it is not part of
// the classical thin plate spline algorithm. It is easy enough to remove the
// remappings from the code.

#include "WmlSystem.h"

namespace Wml
{

template <class Real>
class WML_ITEM IntpThinPlateSpline3
{
public:
    // Data points are of form (x,y,z,f(x,y,z)).  The smoothing parameter
    // must be nonnegative.  The caller is responsible for deleting the
    // arrays afX, afY, afZ, and afF.
    IntpThinPlateSpline3 (int iQuantity, Real* afX, Real* afY,
        Real* afZ, Real* afF, Real fSmooth = (Real)0.0);

    ~IntpThinPlateSpline3 ();

    // Check this after the constructor call to see if the thin plate spline
    // coefficients were successfully computed.  If so, then calls to
    // operator()(Real,Real,Real) will work properly.
    bool IsInitialized () const;

    // Evaluate the thin plate spline interpolator.
    Real operator() (Real fX, Real fY, Real fZ);

protected:
    static Real Kernel (Real fT);

    bool m_bInitialized;
    int m_iQuantity;

    // input data mapped to unit cube
    Real* m_afX;
    Real* m_afY;
    Real* m_afZ;

    // thin plate spline coefficients
    Real* m_afA;
    Real m_afB[4];

    // extent of input data
    Real m_fXMin, m_fXMax, m_fXInvRange;
    Real m_fYMin, m_fYMax, m_fYInvRange;
    Real m_fZMin, m_fZMax, m_fZInvRange;
};

typedef IntpThinPlateSpline3<float> IntpThinPlateSpline3f;
typedef IntpThinPlateSpline3<double> IntpThinPlateSpline3d;

}

#endif
