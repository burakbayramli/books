// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLAPPRPARABOLOIDFIT3_H
#define WMLAPPRPARABOLOIDFIT3_H

// Least-squares fit of a paraboloid to a set of point.  The paraboloid is
// of the form z = c0*x^2+c1*x*y+c2*y^2+c3*x+c4*y+c5.  Successful fit is
// indicated by return value of 'true'.  If return value is false, the
// internal linear system solver was unable to invert the system.

#include "WmlVector3.h"

namespace Wml
{

template <class Real>
WML_ITEM bool ParaboloidFit (int iQuantity, const Vector3<Real>* akPoint,
    Real afCoeff[6]);

}

#endif
