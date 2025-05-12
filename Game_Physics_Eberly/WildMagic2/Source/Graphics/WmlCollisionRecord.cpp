// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlBoundingVolumeTree.h"
#include "WmlCollisionRecord.h"
#include "WmlIntrTri3Tri3.h"
#include "WmlTriMesh.h"
using namespace Wml;

//----------------------------------------------------------------------------
CollisionRecord::CollisionRecord (TriMesh* pkMesh, Vector3f* pkVelocity,
    void* pvCallbackData, Callback oCallback, BoundingVolume::Type eType,
    int iMaxTrisPerLeaf, bool bStoreInteriorTris)
{
    assert( pkMesh );

    m_pkMesh = pkMesh;
    m_pkVelocity = pkVelocity;
    m_pvCallbackData = pvCallbackData;
    m_oCallback = oCallback;

    m_pkTree = new BoundingVolumeTree(eType,pkMesh->GetVertexQuantity(),
        pkMesh->Vertices(),pkMesh->GetTriangleQuantity(),
        pkMesh->Connectivity(),iMaxTrisPerLeaf,bStoreInteriorTris);
}
//----------------------------------------------------------------------------
CollisionRecord::~CollisionRecord ()
{
    delete m_pkTree;
}
//----------------------------------------------------------------------------
void CollisionRecord::Initialize ()
{
    m_pkTree->InvalidateWorldBounds();
}
//----------------------------------------------------------------------------
void CollisionRecord::TestIntersection (CollisionRecord& rkRecord)
{
    // for convenience/clarity/readability
    BoundingVolumeTree* pkTree0 = m_pkTree;
    BoundingVolumeTree* pkTree1 = rkRecord.m_pkTree;
    TriMesh* pkMesh0 = m_pkMesh;
    TriMesh* pkMesh1 = rkRecord.m_pkMesh;

    BoundingVolume* pkWorldBV0 = pkTree0->GetWorldBound();
    if ( !pkWorldBV0->IsValid() )
    {
        pkTree0->GetModelBound()->TransformTo(pkWorldBV0,
            pkMesh0->WorldRotate(),
            pkMesh0->WorldTranslate(),
            pkMesh0->WorldScale());
    }

    BoundingVolume* pkWorldBV1 = pkTree1->GetWorldBound();
    if ( !pkWorldBV1->IsValid() )
    {
        pkTree1->GetModelBound()->TransformTo(pkWorldBV1,
            pkMesh1->WorldRotate(),
            pkMesh1->WorldTranslate(),
            pkMesh1->WorldScale());
    }

    if ( pkWorldBV0->TestIntersection(pkWorldBV1) )
    {
        BoundingVolumeTree* pkRoot;

        if ( pkTree0->IsInteriorNode() )
        {
            pkRoot = m_pkTree;

            // compare Tree0.L to Tree1
            m_pkTree = pkRoot->GetLChild();
            TestIntersection(rkRecord);

            // compare Tree0.R to Tree1
            m_pkTree = pkRoot->GetRChild();
            TestIntersection(rkRecord);

            m_pkTree = pkRoot;
        }
        else if ( pkTree1->IsInteriorNode() )
        {
            pkRoot = rkRecord.m_pkTree;

            // compare Tree0 to Tree1.L
            rkRecord.m_pkTree = pkRoot->GetLChild();
            TestIntersection(rkRecord);

            // compare Tree0 to Tree1.R
            rkRecord.m_pkTree = pkRoot->GetRChild();
            TestIntersection(rkRecord);

            rkRecord.m_pkTree = pkRoot;
        }
        else
        {
            int i;

            // at a leaf in each tree
            int iMax0 = pkTree0->GetTriangleQuantity();
            for (int i0 = 0; i0 < iMax0; i0++)
            {
                int iT0 = pkTree0->GetTriangle(i0);

                // get model space triangle
                Vector3f akV0[3];
                pkMesh0->GetTriangle(iT0,akV0[0],akV0[1],akV0[2]);

                // compute world space triangle
                for (i = 0; i < 3; i++)
                {
                    akV0[i] = pkMesh0->WorldScale()*(
                        pkMesh0->WorldRotate()*akV0[i]) +
                        pkMesh0->WorldTranslate();
                }

                int iMax1 = pkTree1->GetTriangleQuantity();
                for (int i1 = 0; i1 < iMax1; i1++)
                {
                    int iT1 = pkTree1->GetTriangle(i1);

                    // get model space triangle
                    Vector3f akV1[3];
                    pkMesh1->GetTriangle(iT1,akV1[0],akV1[1],akV1[2]);

                    // compute world space triangle
                    for (i = 0; i < 3; i++)
                    {
                        akV1[i] = pkMesh1->WorldScale()*(
                            pkMesh1->WorldRotate()*akV1[i]) +
                            pkMesh1->WorldTranslate();
                    }

                    Triangle3f kTri0, kTri1;
                    kTri0.Origin() = akV0[0];
                    kTri0.Edge0() = akV0[1] - akV0[0];
                    kTri0.Edge1() = akV0[2] - akV0[0];
                    kTri1.Origin() = akV1[0];
                    kTri1.Edge0() = akV1[1] - akV0[0];
                    kTri1.Edge1() = akV1[2] - akV0[0];
                    if ( Wml::TestIntersection(kTri0,kTri1) )
                    {
                        if ( m_oCallback )
                            m_oCallback(*this,iT0,rkRecord,iT1,NULL);

                        if ( rkRecord.m_oCallback )
                            rkRecord.m_oCallback(rkRecord,iT1,*this,iT0,NULL);
                    }
                }
            }
        }
    }
}
//----------------------------------------------------------------------------
void CollisionRecord::FindIntersection (CollisionRecord&)
{
    // TO DO.  implement
}
//----------------------------------------------------------------------------
void CollisionRecord::TestIntersection (float, CollisionRecord&)
{
    // TO DO.  implement
}
//----------------------------------------------------------------------------
void CollisionRecord::FindIntersection (float, CollisionRecord&)
{
    // TO DO.  implement
}
//----------------------------------------------------------------------------


