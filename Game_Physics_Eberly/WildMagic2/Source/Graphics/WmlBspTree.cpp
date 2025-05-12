// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlBspTree.h"
using namespace Wml;

WmlImplementRTTI(BspTree,BspNode);
WmlImplementStream(BspTree);

//----------------------------------------------------------------------------
BspTree::BspTree (TriangleList& rkList)
{
    assert( rkList.size() > 0 );
    CreateTree(rkList);
}
//----------------------------------------------------------------------------
BspTree::BspTree ()
{
}
//----------------------------------------------------------------------------
BspTree::Triangle::Triangle ()
{
    memset(m_apkNormal,0,3*sizeof(Vector3f*));
    memset(m_apkColor,0,3*sizeof(ColorRGB*));
    memset(m_apkTexture,0,3*sizeof(Vector2f*));
}
//----------------------------------------------------------------------------
void BspTree::CreateTree (TriangleList& rkList)
{
    // construct plane of first triangle in list
    Triangle* pkTri = rkList.front();
    rkList.pop_front();
    m_kModelPlane = Plane3f(pkTri->m_akVertex[0],pkTri->m_akVertex[1],
        pkTri->m_akVertex[2]);

    // place first triangle on node's coincident list
    TriangleList* pkCoincident = new TriangleList;
    pkCoincident->push_front(pkTri);
    m_pvData = pkCoincident;

    if ( rkList.size() > 0 )
    {
        // split remaining triangles against plane
        TriangleList kPositive, kNegative;
        while ( rkList.size() > 0 )
        {
            pkTri = rkList.front();
            rkList.pop_front();
            SplitTriangle(pkTri,kPositive,kNegative,*pkCoincident);
        }

        // recurse on left child
        if ( kPositive.size() > 0 )
        {
            BspTree* pkLeftChild = new BspTree(kPositive);
            AttachLeftChild(pkLeftChild);
        }

        // recurse on right child
        if ( kNegative.size() > 0 )
        {
            BspTree* pkRightChild = new BspTree(kNegative);
            AttachRightChild(pkRightChild);
        }
    }
    else
    {
        // TO DO.  This node has no children.  Create a TriMesh from the
        // coincident triangles?
    }
}
//----------------------------------------------------------------------------
void BspTree::SplitTriangle (Triangle* pkTri, TriangleList& rkPositive,
    TriangleList& rkNegative, TriangleList& rkCoincident)
{
    // compute signed pseudodistances
    float afDistance[3];
    int i, iPosCount = 0, iNegCount = 0;
    for (i = 0; i < 3; i++)
    {
        afDistance[i] = m_kModelPlane.DistanceTo(pkTri->m_akVertex[i]);
        if ( afDistance[i] > Mathf::EPSILON )
            iPosCount++;
        else if ( afDistance[i] < -Mathf::EPSILON )
            iNegCount--;
        else
            afDistance[i] = 0.0f;
    }

    if ( iPosCount > 0 )
    {
        if ( iNegCount == 0 )
        {
            // triangle is on positive side of plane
            rkPositive.push_front(pkTri);
            return;
        }
    }
    else
    {
        if ( iNegCount > 0 )
        {
            // triangle is on negative side of plane
            rkNegative.push_front(pkTri);
        }
        else
        {
            // triangle is contained in plane
            rkCoincident.push_front(pkTri);
        }
        return;
    }

    // triangle straddles plane and must be split
    if ( afDistance[0]*afDistance[1] < 0.0f )
    {
        ClipTriangle(0,1,2,pkTri,afDistance,rkPositive,rkNegative);
        return;
    }

    if ( afDistance[2]*afDistance[0] < 0.0f )
    {
        ClipTriangle(2,0,1,pkTri,afDistance,rkPositive,rkNegative);
        return;
    }

    if ( afDistance[1]*afDistance[2] < 0.0f )
    {
        ClipTriangle(1,2,0,pkTri,afDistance,rkPositive,rkNegative);
        return;
    }

    // Distances are all nonnegative or all nonpositive.  This should have
    // been caught by ((iPos > 0 && iNeg == 0) || (iPos == 0 && iNeg > 0)),
    // but just in case of numerical round-off errors screwing up the
    // theory...
    assert( false );
}
//----------------------------------------------------------------------------
void BspTree::ClipTriangle (int i0, int i1, int i2, Triangle* pkTri,
    float afDistance[3], TriangleList& rkPositive, TriangleList& rkNegative)
{
    // Warning.  The normal interpolation is a convex combination of the
    // normals that is unitized.  You might want to use SLERP to get a more
    // standard interpolation.

    float fD0 = afDistance[i0];
    float fD1 = afDistance[i1];
    float fD2 = afDistance[i2];
    Vector3f& rkV0 = pkTri->m_akVertex[i0];
    Vector3f& rkV1 = pkTri->m_akVertex[i1];
    Vector3f& rkV2 = pkTri->m_akVertex[i2];

    bool bHasNormals = (pkTri->m_apkNormal[0] != NULL);
    Vector3f* apkN[3];
    if ( bHasNormals )
    {
        apkN[0] = pkTri->m_apkNormal[0];
        apkN[1] = pkTri->m_apkNormal[1];
        apkN[2] = pkTri->m_apkNormal[2];
    }

    bool bHasColors = (pkTri->m_apkColor[0] != NULL);
    ColorRGB* apkC[3];
    if ( bHasColors )
    {
        apkC[0] = pkTri->m_apkColor[0];
        apkC[1] = pkTri->m_apkColor[1];
        apkC[2] = pkTri->m_apkColor[2];
    }

    bool bHasTextures = (pkTri->m_apkTexture[0] != NULL);
    Vector2f* apkT[3];
    if ( bHasTextures )
    {
        apkT[0] = pkTri->m_apkTexture[0];
        apkT[1] = pkTri->m_apkTexture[1];
        apkT[2] = pkTri->m_apkTexture[2];
    }

    // clip edge <I0,I1>
    float fT = fD0/(fD0 - fD1);
    Vector3f kV01 = rkV0 + fT*(rkV1 - rkV0);

    Vector3f kN01;
    if ( bHasNormals )
    {
        // TO DO.  Use slerp instead of convex combination?
        kN01 = *apkN[i0] + fT*(*apkN[i1] - *apkN[i0]);
        kN01.Normalize();
    }

    ColorRGB kC01;
    if ( bHasColors )
        kC01 = *apkC[i0] + fT*(*apkC[i1] - *apkC[i0]);

    Vector2f kT01;
    if ( bHasTextures )
        kT01 = *apkT[i0] + fT*(*apkT[i1] - *apkT[i0]);

    Vector3f kV12, kN12;
    ColorRGB kC12;
    Vector2f kT12;

    if ( fD0 > 0.0f )
    {
        if ( fD2 > 0.0f )
        {
            // clip edge <I1,I2>
            fT = fD1/(fD1 - fD2);
            kV12 = rkV1 + fT*(rkV2 - rkV1);

            if ( bHasNormals )
            {
                kN12 = *apkN[i1] + fT*(*apkN[i2] - *apkN[i1]);
                kN12.Normalize();
            }

            if ( bHasColors )
                kC12 = *apkC[i1] + fT*(*apkC[i2] - *apkC[i1]);

            if ( bHasTextures )
                kT12 = *apkT[i1] + fT*(*apkT[i2] - *apkT[i1]);

            AddTriangle(rkPositive,kV01,kV12,rkV0,bHasNormals,&kN01,&kN12,
                apkN[i0],bHasColors,&kC01,&kC12,apkC[i0],bHasTextures,&kT01,
                &kT12,apkT[i0]);

            AddTriangle(rkPositive,kV12,rkV2,rkV0,bHasNormals,&kN12,apkN[i2],
                apkN[i0],bHasColors,&kC12,apkC[i2],apkC[i0],bHasTextures,
                &kT12,apkT[i2],apkT[i0]);

            AddTriangle(rkNegative,kV01,rkV1,kV12,bHasNormals,&kV01,apkN[i1],
                &kN12,bHasColors,&kC01,apkC[i1],&kC12,bHasTextures,&kT01,
                apkT[i1],&kT12);
        }
        else if ( fD2 < 0.0f )
        {
            // clip edge <I0,I2>
            fT = fD2/(fD2 - fD0);
            kV12 = rkV2 + fT*(rkV0 - rkV2);

            if ( bHasNormals )
            {
                kN12 = *apkN[i2] + fT*(*apkN[i0] - *apkN[i2]);
                kN12.Normalize();
            }

            if ( bHasColors )
                kC12 = *apkC[i2] + fT*(*apkC[i0] - *apkC[i2]);

            if ( bHasTextures )
                kT12 = *apkT[i2] + fT*(*apkT[i0] - *apkT[i2]);

            AddTriangle(rkPositive,kV01,kV12,rkV0,bHasNormals,&kN01,&kN12,
                apkN[i0],bHasColors,&kC01,&kC12,apkC[i0],bHasTextures,&kT01,
                &kT12,apkT[i0]);

            AddTriangle(rkNegative,kV01,rkV1,rkV2,bHasNormals,&kN01,apkN[i1],
                apkN[i2],bHasColors,&kC01,apkC[i1],apkC[i2],bHasTextures,
                &kT01,apkT[i1],apkT[i2]);

            AddTriangle(rkNegative,kV01,rkV2,kV12,bHasNormals,&kN01,apkN[i2],
                &kN12,bHasColors,&kC01,apkC[i2],&kC12,bHasTextures,&kT01,
                apkT[i2],&kT12);
        }
        else
        {
            AddTriangle(rkPositive,kV01,rkV2,rkV0,bHasNormals,&kN01,apkN[i2],
                apkN[i0],bHasColors,&kC01,apkC[i2],apkC[i0],bHasTextures,
                &kT01,apkT[i2],apkT[i0]);

            AddTriangle(rkNegative,kV01,rkV1,rkV2,bHasNormals,&kN01,apkN[i1],
                apkN[i2],bHasColors,&kC01,apkC[i1],apkC[i2],bHasTextures,
                &kT01,apkT[i1],apkT[i2]);
        }
    }
    else
    {
        if ( fD2 > 0.0f )
        {
            // clip edge <I0,I2>
            fT = fD0/(fD0 - fD2);
            kV12 = rkV0 + fT*(rkV2 - rkV0);

            if ( bHasNormals )
            {
                kN12 = *apkN[i0] + fT*(*apkN[i2] - *apkN[i0]);
                kN12.Normalize();
            }

            if ( bHasColors )
                kC12 = *apkC[i0] + fT*(*apkC[i2] - *apkC[i0]);

            if ( bHasTextures )
                kT12 = *apkT[i0] + fT*(*apkT[i2] - *apkT[i0]);

            AddTriangle(rkPositive,kV01,rkV1,rkV2,bHasNormals,&kN01,apkN[i1],
                apkN[i2],bHasColors,&kC01,apkC[i1],apkC[i2],bHasTextures,
                &kT01,apkT[i1],apkT[i2]);

            AddTriangle(rkPositive,kV01,rkV2,kV12,bHasNormals,&kN01,apkN[i2],
                &kN12,bHasColors,&kC01,apkC[i2],&kC12,bHasTextures,&kT01,
                apkT[i2],&kT12);

            AddTriangle(rkNegative,kV01,kV12,rkV0,bHasNormals,&kN01,&kN12,
                apkN[i0],bHasColors,&kC01,&kC12,apkC[i0],bHasTextures,&kT01,
                &kT12,apkT[i0]);
        }
        else if ( fD2 < 0.0f )
        {
            // clip edge <I1,I2>
            fT = fD2/(fD2 - fD1);
            kV12 = rkV2 + fT*(rkV1 - rkV2);

            if ( bHasNormals )
            {
                kN12 = *apkN[i2] + fT*(*apkN[i1] - *apkN[i2]);
                kN12.Normalize();
            }

            if ( bHasColors )
                kC12 = *apkC[i2] + fT*(*apkC[i1] - *apkC[i2]);

            if ( bHasTextures )
                kT12 = *apkT[i2] + fT*(*apkT[i1] - *apkT[i2]);

            AddTriangle(rkPositive,kV01,rkV1,kV12,bHasNormals,&kN01,apkN[i1],
                &kN12,bHasColors,&kC01,apkC[i1],&kC12,bHasTextures,&kT01,
                apkT[i1],&kT12);

            AddTriangle(rkNegative,rkV0,kV01,kV12,bHasNormals,apkN[i0],&kN01,
                &kN12,bHasColors,apkC[i0],&kC01,&kC12,bHasTextures,apkT[i0],
                &kT01,&kT12);

            AddTriangle(rkNegative,rkV0,kV12,rkV2,bHasNormals,apkN[i0],&kN01,
                apkN[i2],bHasColors,apkC[i0],&kC12,apkC[i2],bHasTextures,
                apkT[i0],&kT12,apkT[i2]);
        }
        else
        {
            AddTriangle(rkPositive,kV01,rkV1,rkV2,bHasNormals,&kN01,apkN[i1],
                apkN[i2],bHasColors,&kC01,apkC[i1],apkC[i2],bHasTextures,
                &kT01,apkT[i1],apkT[i2]);

            AddTriangle(rkNegative,kV01,rkV2,rkV0,bHasNormals,&kN01,apkN[i2],
                apkN[i0],bHasColors,&kC01,apkC[i2],apkC[i0],bHasTextures,
                &kT01,apkT[i2],apkT[i0]);
        }
    }
}
//----------------------------------------------------------------------------
void BspTree::AddTriangle (TriangleList& rkList, const Vector3f& rkV0,
    const Vector3f& rkV1, const Vector3f& rkV2, bool bHasNormals,
    const Vector3f* pkN0, const Vector3f* pkN1, const Vector3f* pkN2,
    bool bHasColors, const ColorRGB* pkC0, const ColorRGB* pkC1,
    const ColorRGB* pkC2, bool bHasTextures, const Vector2f* pkT0,
    const Vector2f* pkT1, const Vector2f* pkT2)
{
    Triangle* pkClipTri = new Triangle;
    rkList.push_front(pkClipTri);

    pkClipTri->m_akVertex[0] = rkV0;
    pkClipTri->m_akVertex[1] = rkV1;
    pkClipTri->m_akVertex[2] = rkV2;

    if ( bHasNormals )
    {
        pkClipTri->m_apkNormal[0] = new Vector3f;
        pkClipTri->m_apkNormal[1] = new Vector3f;
        pkClipTri->m_apkNormal[2] = new Vector3f;
        *pkClipTri->m_apkNormal[0] = *pkN0;
        *pkClipTri->m_apkNormal[1] = *pkN1;
        *pkClipTri->m_apkNormal[2] = *pkN2;
    }

    if ( bHasColors )
    {
        pkClipTri->m_apkColor[0] = new ColorRGB;
        pkClipTri->m_apkColor[1] = new ColorRGB;
        pkClipTri->m_apkColor[2] = new ColorRGB;
        *pkClipTri->m_apkColor[0] = *pkC0;
        *pkClipTri->m_apkColor[1] = *pkC1;
        *pkClipTri->m_apkColor[2] = *pkC2;
    }

    if ( bHasTextures )
    {
        pkClipTri->m_apkTexture[0] = new Vector2f;
        pkClipTri->m_apkTexture[1] = new Vector2f;
        pkClipTri->m_apkTexture[2] = new Vector2f;
        *pkClipTri->m_apkTexture[0] = *pkT0;
        *pkClipTri->m_apkTexture[1] = *pkT1;
        *pkClipTri->m_apkTexture[2] = *pkT2;
    }
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* BspTree::Factory (Stream& rkStream)
{
    BspTree* pkObject = new BspTree;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void BspTree::Load (Stream& rkStream, Stream::Link* pkLink)
{
    BspNode::Load(rkStream,pkLink);
}
//----------------------------------------------------------------------------
void BspTree::Link (Stream& rkStream, Stream::Link* pkLink)
{
    BspNode::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool BspTree::Register (Stream& rkStream)
{
    return BspNode::Register(rkStream);
}
//----------------------------------------------------------------------------
void BspTree::Save (Stream& rkStream)
{
    BspNode::Save(rkStream);
}
//----------------------------------------------------------------------------
StringTree* BspTree::SaveStrings ()
{
    // TO DO.  Finish implementation.
    StringTree* pkTree = new StringTree(1,0,1,0);
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetChild(0,BspNode::SaveStrings());
    return pkTree;
}
//----------------------------------------------------------------------------
int BspTree::GetMemoryUsed () const
{
    int iBaseSize = sizeof(BspTree) - sizeof(BspNode);
    int iTotalSize = iBaseSize + BspNode::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int BspTree::GetDiskUsed () const
{
    return BspNode::GetDiskUsed();
}
//----------------------------------------------------------------------------
