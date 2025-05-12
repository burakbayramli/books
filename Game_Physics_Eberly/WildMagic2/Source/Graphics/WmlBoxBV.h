// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLBOXBV_H
#define WMLBOXBV_H

#include "WmlBoundingVolume.h"
#include "WmlBox3.h"

namespace Wml
{

class WML_ITEM BoxBV : public BoundingVolume
{
    WmlDeclareBV(BoxBV);

public:
    virtual Type GetType () const;

    virtual void CopyTo (BoundingVolume* pkTargetBV) const;

    virtual void TransformTo (BoundingVolume* pkTargetBV,
        const Matrix3f& rkRot, const Vector3f& rkTrn, float fScale) const;

    virtual bool IsValid () const;
    virtual void Invalidate ();

    virtual bool Contains (const Vector3f& rkPoint,
        float fEpsilon = 0.0f) const;

    virtual bool TestIntersection (const BoundingVolume* pkBV) const;

    Box3f m_kBox;
};

WmlRegisterBV(BoxBV);
#include "WmlBoxBV.inl"

}

#endif
