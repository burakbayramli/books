// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLTETRAHEDRON3_H
#define WMLTETRAHEDRON3_H

#include "WmlPlane3.h"

namespace Wml
{

template <class Real>
class WML_ITEM Tetrahedron3
{
public:
    Tetrahedron3 ();
    Tetrahedron3 (const Vector3<Real>& rkV0, const Vector3<Real>& rkV1,
        const Vector3<Real>& rkV2, const Vector3<Real>& rkV3);
    Tetrahedron3 (const Vector3<Real> akV[4]);

    Vector3<Real>& operator[] (int i);
    const Vector3<Real>& operator[] (int i) const;

    // Construct the planes of the faces.  The planes have outer pointing
    // normal vectors, not necessarily unit length.
    void GetPlanes (Plane3<Real> akPlane[4]) const;

protected:
    Vector3<Real> m_akVertex[4];
};

typedef Tetrahedron3<float> Tetrahedron3f;
typedef Tetrahedron3<double> Tetrahedron3d;

}

#endif
