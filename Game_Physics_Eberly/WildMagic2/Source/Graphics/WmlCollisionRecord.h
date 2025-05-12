// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLCOLLISIONRECORD_H
#define WMLCOLLISIONRECORD_H

#include "WmlBoundingVolume.h"
#include "WmlTriMesh.h"

namespace Wml
{

class BoundingVolumeTree;

class WML_ITEM CollisionRecord
{
public:
    typedef void (*Callback) (CollisionRecord& rkRecord0, int iT0,
        CollisionRecord& rkRecord1, int iT1, void* pvIntersectionData);

    CollisionRecord (TriMesh* pkMesh, Vector3f* pkVelocity,
        void* pvCallbackData, Callback oCallback,
        BoundingVolume::Type eType, int iMaxTrisPerLeaf = 1,
        bool bStoreInteriorTris = false);

    ~CollisionRecord ();

    // member access
    TriMesh* GetMesh ();
    Vector3f* GetVelocity ();
    void* GetCallbackData ();

    // intersection queries
    void Initialize ();
    void TestIntersection (CollisionRecord& rkRecord);
    void FindIntersection (CollisionRecord& rkRecord);
    void TestIntersection (float fTMax, CollisionRecord& rkRecord);
    void FindIntersection (float fTMax ,CollisionRecord& rkRecord);

protected:
    TriMesh* m_pkMesh;
    Vector3f* m_pkVelocity;
    void* m_pvCallbackData;
    Callback m_oCallback;
    BoundingVolumeTree* m_pkTree;
};

#include "WmlCollisionRecord.inl"

}

#endif
