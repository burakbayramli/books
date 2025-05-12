// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLBOUND_H
#define WMLBOUND_H

#include "WmlMatrix3.h"
#include "WmlPlane3.h"

namespace Wml
{

class WML_ITEM Bound
{
public:
    Bound ();

    Vector3f& Center ();
    float& Radius ();
    const Vector3f& Center () const;
    const float& Radius () const;

    // access bound B as B[0] = C.x, B[1] = C.y, B[2] = C.z, B[3] = r
    //
    // WARNING.  These member functions rely on
    // (1) Bound not having virtual functions
    // (2) the data packed in a 4*sizeof(float) memory block
    float& operator[] (int i) const;
    operator float* ();

    Bound& operator= (const Bound& rkBound);
    Bound& operator+= (const Bound& rkBound);

    void ComputeFromData (int iQuantity, const Vector3f* akData);

    Bound TransformBy (const Matrix3f& rkRotate, const Vector3f& rkTranslate,
        float fScale) const;

    Plane3f::Side WhichSide (const Plane3f& rkPlane) const;

    // Test for intersection of ray and bound (points of intersection not
    // computed).
    bool TestIntersection (const Vector3f& rkOrigin,
        const Vector3f& rkDirection) const;

protected:
    Vector3f m_kCenter;
    float m_fRadius;
};

#include "WmlBound.inl"

}

#endif


