// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLCONTCYLINDER3_H
#define WMLCONTCYLINDER3_H

#include "WmlCylinder3.h"

namespace Wml
{

// Compute the cylinder axis segment using least-squares fit.  The radius is
// the maximum distance from points to the axis.  The height is determined by
// projection of points onto the axis and determining the containing interval.

template <class Real>
WML_ITEM Cylinder3<Real> ContCylinder (int iQuantity,
    const Vector3<Real>* akPoint);

}

#endif
