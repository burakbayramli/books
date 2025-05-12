// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLCONTSEPARATEPOINTS3_H
#define WMLCONTSEPARATEPOINTS3_H

// Separate two point sets, if possible, by computing a plane for which the
// point sets lie on opposite sides.  The algorithm computes the convex hull
// of the point sets, then uses the method of separating axes to determine if
// the two convex polyhedra are disjoint.  The convex hull calculation is
// O(n*log(n)).  There is a randomized linear approach that takes O(n), but
// I have not yet implemented it.
//
// The return value of the function is 'true' if and only if there is a
// separation.  If 'true', the returned plane is a separating plane.

#include "WmlPlane3.h"

namespace Wml
{

// Assumes that both sets have at least 4 noncoplanar points.
template <class Real>
WML_ITEM bool SeparatePoints3 (int iQuantity0,
    const Vector3<Real>* akVertex0, int iQuantity1,
    const Vector3<Real>* akVertex1, Plane3<Real>& rkSeprPlane);

}

#endif
