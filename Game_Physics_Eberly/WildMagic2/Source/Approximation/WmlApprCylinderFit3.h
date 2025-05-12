// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLAPPRCYLINDERFIT3_H
#define WMLAPPRCYLINDERFIT3_H

#include "WmlVector3.h"

namespace Wml
{

// Least-squares fit of a finite cylinder to (x,y,z) data.  The cylinder has
// center C, unit-length axis direction U, radius r, and height h.  The end
// disks of the cylinder are at C+(h/2)*U and C-(h/2)*U.  A point X is on the
// cylinder wall if (X-C)^T*(I-U*U^T)*(X-C) = r^2.  A point X is on the end
// disk at C+(h/2)*U if Dot(U,X-C) = h/2 and (X-C)^T*(I-U*U^T)*(X-C) <= r^2.
// A point X is on the end disk at C-(h/2)*U if Dot(U,X-C) = -h/2 and
// (X-C)^T*(I-U*U^T)*(X-C) <= r^2.
  
// The inputs are the quantity of points and the point array.  The outputs
// are the center C, unit-length axis direction U, radius R, and height H.
// You can supply initial guesses for C and U.  In this case you need to set
// bInputsAreInitialGuess to 'true'.  Otherwise set it to 'false' and the
// function will select C and U by first fitting the data with a least-squares
// line.  The return function value is the error for the least-squares fit,
// e >= 0.  If all the points lie exactly on a cylinder, then e = 0.
//
// You can examine the error e and iterate the calls yourself.  The outputs
// C, U, R, and H can be fed back into the function call as initial guesses.
//
// Real fError = CylinderFit(iQuantity,akPoint,kC,kU,fR,fH,false);
// for (i = 1; i <= iMax; i++)
// {
//     Real fCurrError = CylinderFit(iQuantity,akPoint,kC,kU,fR,fH,true);
//     if ( fCurrError not changed much from fError )
//         break;
// }

template <class Real>
WML_ITEM Real CylinderFit (int iQuantity, const Vector3<Real>* akPoint,
    Vector3<Real>& rkC, Vector3<Real>& rkU, Real& rfR, Real& rfH,
    bool bInputsAreInitialGuess);

}

#endif
