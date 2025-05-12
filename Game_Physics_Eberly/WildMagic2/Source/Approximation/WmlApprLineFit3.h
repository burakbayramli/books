// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLAPPRLINEFIT3_H
#define WMLAPPRLINEFIT3_H

#include "WmlVector3.h"

namespace Wml
{

// Least-squares fit of a line to (x,y,z) data by using distance measurements
// orthogonal to the proposed line.  The resulting line is represented by
// Offset + t*Direction where the returned direction is a unit-length vector.
template <class Real>
WML_ITEM void OrthogonalLineFit (int iQuantity, const Vector3<Real>* akPoint,
    Vector3<Real>& rkOffset, Vector3<Real>& rkDirection);


// This function allows for selection of vertices from a pool.  The return
// value is 'true' if and only if at least one vertex is valid.
template <class Real>
WML_ITEM bool OrthogonalLineFit (int iQuantity, const Vector3<Real>* akPoint,
    const bool* abValid, Vector3<Real>& rkOffset, Vector3<Real>& rkDirection);

}

#endif
