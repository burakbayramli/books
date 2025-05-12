// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#ifndef WMLPOLYHEDRALMASSPROPERTIES_H
#define WMLPOLYHEDRALMASSPROPERTIES_H

#include "WmlMatrix3.h"

namespace Wml
{

// The input triangle mesh must be a convex polyhedron.  The triangles are
// represented as triples of indices <V0,V1,V2> into the vertex array.
// The connectivity array has iTQuantity such triples.  The Boolean value
// bBodyCoords is 'true' if you want the inertia tensor to be relative to
// body coordinates, 'false' if you want it in world coordinates.

template <class Real>
void WML_ITEM ComputeMassProperties (const Vector3<Real>* akVertex,
    int iTQuantity, const int* aiConnect, bool bBodyCoords, Real& rfMass,
    Vector3<Real>& rkCenter, Matrix3<Real>& rkInertia);

}

#endif
