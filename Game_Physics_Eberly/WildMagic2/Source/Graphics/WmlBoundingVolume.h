// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLBOUNDINGVOLUME_H
#define WMLBOUNDINGVOLUME_H

#include "WmlMatrix3.h"
#include "WmlBoundingVolume.mcr"

namespace Wml
{

class WML_ITEM BoundingVolume
{
public:
    // abstract base class
    virtual ~BoundingVolume ();

    // run-time type information
    enum Type
    {
        BV_SPHERE,
        BV_CAPSULE,
        BV_LOZENGE,
        BV_BOX,
        BV_QUANTITY
    };

    virtual Type GetType () const = 0;

    virtual void CopyTo (BoundingVolume* pkTargetBV) const = 0;

    virtual void TransformTo (BoundingVolume* pkTargetBV,
        const Matrix3f& rkRot, const Vector3f& rkTrn, float fScale) const = 0;

    virtual bool IsValid () const = 0;
    virtual void Invalidate () = 0;

    virtual bool Contains (const Vector3f& rkPoint, float fEpsilon = 0.0f)
        const = 0;

    virtual bool TestIntersection (const BoundingVolume* pkBV) const = 0;

protected:
    // bounding volume factories
    friend class BoundingVolumeTree;

    typedef BoundingVolume* (*CreatorS)(void);
    static CreatorS ms_aoCreatorS[BV_QUANTITY];
    static BoundingVolume* Create (Type eType);

    typedef BoundingVolume* (*CreatorT)(int, const Vector3f*, const int*,
        int, int, int*, Vector3f&, Vector3f&);
    static CreatorT ms_aoCreatorT[BV_QUANTITY];
    static BoundingVolume* Create (Type eType, int iVertexCount,
        const Vector3f* akVertex, const int* aiConnect, int i0, int i1,
        int* aiISplit, Vector3f& rkOrigin, Vector3f& rkDirection);
};

}

#endif
