// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlConvexHull3.h"
using namespace Wml;

#include <algorithm>
using namespace std;

//----------------------------------------------------------------------------
template <class Real>
ConvexHull3<Real>::ConvexHull3 (int iVQuantity, const Vector3<Real>* akVertex)
    :
    m_kHullS(NULL,CreateTriangle)
{
    // uniformly scale and translate to [-1,1]^3 for numerical preconditioning
    Real fMin = akVertex[0].X(), fMax = fMin;
    int i;
    for (i = 0; i < iVQuantity; i++)
    {
        if ( akVertex[i].X() < fMin )
            fMin = akVertex[i].X();
        else if ( akVertex[i].X() > fMax )
            fMax = akVertex[i].X();

        if ( akVertex[i].Y() < fMin )
            fMin = akVertex[i].Y();
        else if ( akVertex[i].Y() > fMax )
            fMax = akVertex[i].Y();

        if ( akVertex[i].Z() < fMin )
            fMin = akVertex[i].Z();
        else if ( akVertex[i].Z() > fMax )
            fMax = akVertex[i].Z();
    }
    Real fHalfRange = ((Real)0.5)*(fMax - fMin);
    Real fInvHalfRange = ((Real)1.0)/fHalfRange;

    m_iVQuantity = iVQuantity;
    m_akVertex = new Vector3<Real>[m_iVQuantity];
    for (i = 0; i < m_iVQuantity; i++)
    {
        m_akVertex[i].X() = -(Real)1.0+fInvHalfRange*(akVertex[i].X()-fMin);
        m_akVertex[i].Y() = -(Real)1.0+fInvHalfRange*(akVertex[i].Y()-fMin);
        m_akVertex[i].Z() = -(Real)1.0+fInvHalfRange*(akVertex[i].Z()-fMin);
    }

    // Compute convex hull incrementally.  The first and second vertices in
    // the hull are managed separately until at least one triangle is formed.
    // At that point, a triangle mesh is used to store the hull.
    m_iHullType = HULL_POINT;
    m_kHullP.push_back(0);
    for (i = 1; i < m_iVQuantity; i++)
    {
        switch ( m_iHullType )
        {
        case HULL_POINT:
            MergePoint(i);
            break;
        case HULL_LINEAR:
            MergeLinear(i);
            break;
        case HULL_PLANAR:
            MergePlanar(i);
            break;
        case HULL_SPATIAL:
            MergeSpatial(i);
            break;
        }
    }

    // map convex hull indices back to original ordering
    if ( m_iHullType == HULL_SPATIAL )
    {
        const ETManifoldMesh::TMap& rkTMap = m_kHullS.GetTriangles();
        m_kHull.resize(3*rkTMap.size());
        i = 0;
        ETManifoldMesh::TMap::const_iterator pkTIter;
        for (pkTIter = rkTMap.begin(); pkTIter != rkTMap.end(); pkTIter++)
        {
            const Triangle* pkTri = (const Triangle*)pkTIter->second;
            for (int j = 0; j < 3; j++)
                m_kHull[i++] = pkTri->V[j];
        }
    }
    else
    {
        int iHQuantity = (int)m_kHullP.size();
        m_kHull.resize(iHQuantity);

        for (i = 0; i < iHQuantity; i++)
            m_kHull[i] = m_kHullP[i];
    }

    m_kHullP.clear();
    delete[] m_akVertex;
    m_akVertex = NULL;
}
//----------------------------------------------------------------------------
template <class Real>
ConvexHull3<Real>::~ConvexHull3 ()
{
    assert( m_akVertex == NULL );
}
//----------------------------------------------------------------------------
template <class Real>
int ConvexHull3<Real>::GetType () const
{
    return m_iHullType;
}
//----------------------------------------------------------------------------
template <class Real>
const vector<int>& ConvexHull3<Real>::GetConnectivity () const
{
    return m_kHull;
}
//----------------------------------------------------------------------------
template <class Real>
void ConvexHull3<Real>::MergePoint (int iP)
{
    // hull is {Q0}
    Vector3<Real> kDiff = m_akVertex[m_kHullP[0]] - m_akVertex[iP];
    if ( kDiff.Length() > Math<Real>::EPSILON )
    {
        m_iHullType = HULL_LINEAR;
        m_kHullP.push_back(iP);
    }
}
//----------------------------------------------------------------------------
template <class Real>
void ConvexHull3<Real>::MergeLinear (int iP)
{
    // hull is {Q0,Q1}
    const Vector3<Real>& rkQ0 = m_akVertex[m_kHullP[0]];
    const Vector3<Real>& rkQ1 = m_akVertex[m_kHullP[1]];
    const Vector3<Real>& rkP = m_akVertex[iP];

    // assert:  Q0 and Q1 are not collocated
    Vector3<Real> kEdge1 = rkQ1 - rkQ0;
    Vector3<Real> kEdge2 = rkP - rkQ0;
    m_kNormal = kEdge1.Cross(kEdge2);
    Real fLength = m_kNormal.Normalize();

    if ( fLength > Math<Real>::EPSILON )
    {
        // <Q0,Q1,P> is a triangle, updated hull is {Q0,Q1,P}
        m_iHullType = HULL_PLANAR;
        m_kHullP.push_back(iP);
        m_kOrigin = rkQ0;
        return;
    }

    // P is on line of <Q0,Q1>
    Real fE1dE2 = kEdge1.Dot(kEdge2);
    if ( fE1dE2 < 0.0f )
    {
        // order is <P,Q0,Q1>, updated hull is {P,Q1}
        m_kHullP[0] = iP;
        return;
    }

    Real fE1dE1 = kEdge1.SquaredLength();
    if ( fE1dE2 > fE1dE1 )
    {
        // order is <Q0,Q1,P>, updated hull is {Q0,P}
        m_kHullP[1] = iP;
        return;
    }

    // order is <Q0,P,Q1>, hull does not change
}
//----------------------------------------------------------------------------
template <class Real>
void ConvexHull3<Real>::MergePlanar (int iP)
{
    // hull is convex polygon in plane m_kNormal.Dot(X - m_kOrigin) = 0
    const Vector3<Real>& rkP = m_akVertex[iP];
    Real fOrder = m_kNormal.Dot(rkP - m_kOrigin);
    int i;
    if ( Math<Real>::FAbs(fOrder) <= Math<Real>::EPSILON )
    {
        // order essentially zero
        Vector3<Real>* pkQ0;
        Vector3<Real>* pkQ1;
        Vector3<Real> kEdge1, kEdge2;

        // find an edge visible to P
        int iSize = (int)m_kHullP.size(), iSizeM1 = iSize-1;
        int iL, iU;
        for (iL = iSizeM1, iU = 0; iU < iSize; iL = iU++)
        {
            pkQ0 = &m_akVertex[m_kHullP[iL]];
            pkQ1 = &m_akVertex[m_kHullP[iU]];
            kEdge1 = *pkQ1 - *pkQ0;
            kEdge2 = rkP - *pkQ0;
            fOrder = m_kNormal.Dot(kEdge1.UnitCross(kEdge2));
            if ( fOrder < (Real)0.0 )
                break;
        }
        if ( iU == iSize )
        {
            // P is inside the current hull
            return;
        }

        // Edge <L,U> is visible to P.  Perform a breadth-first search to
        // locate all remaining visible edges.

        // find the upper index for visible edges
        for (i = iU+1; i < iSize; iU = i++)
        {
            pkQ0 = &m_akVertex[m_kHullP[iU]];
            pkQ1 = &m_akVertex[m_kHullP[i]];
            kEdge1 = *pkQ1 - *pkQ0;
            kEdge2 = rkP - *pkQ0;
            fOrder = m_kNormal.Dot(kEdge1.UnitCross(kEdge2));
            if ( fOrder >= (Real)0.0 )
                break;
        }

        // If edge <size-1,0> was not visible, we had to search for the first
        // visible edge in which case the lower index L is already correct.
        // Thus, we only need to handle the case when L = size-1 and search
        // for smaller indices to find the lower index.
        if ( iL == iSizeM1 )
        {
            for (i = iL-1; i >= 0; iL = i--)
            {
                pkQ0 = &m_akVertex[m_kHullP[i]];
                pkQ1 = &m_akVertex[m_kHullP[iL]];
                kEdge1 = *pkQ1 - *pkQ0;
                kEdge2 = rkP - *pkQ0;
                fOrder = m_kNormal.Dot(kEdge1.UnitCross(kEdge2));
                if ( fOrder >= (Real)0.0 )
                    break;
            }
        }

        // assert( iU != iL );  the theory
        if ( iU == iL )
        {
            // Numerical round-off error causes you to get here.  P is
            // inside the hull, so just return.
            return;
        }

        // construct the counterclockwise-ordered merged-hull vertices
        vector<int> kTmpHull;
        kTmpHull.push_back(iP);
        while ( true )
        {
            kTmpHull.push_back(m_kHullP[iU]);
            if ( iU == iL )
                break;

            if ( ++iU == iSize )
                iU = 0;
        }
        assert( kTmpHull.size() > 2 );

        m_kHullP = kTmpHull;
        return;
    }

    // Hull is about to become spatial.  The current planar hull is a convex
    // polygon that must be tri-fanned to initialize the triangle mesh that
    // represents the final hull.  The triangle order is required to be
    // counterclockwise when viewed from outside the hull.  If the current
    // planar hull has N vertices, the triangle fan has N-2 triangles and
    // P forms N triangles with the planar hull edges, a total of 2*N-2
    // triangles in the initial spatial hull.
    m_iHullType = HULL_SPATIAL;

    int iSize = (int)m_kHullP.size();
    int iQ0 = m_kHullP[0];
    int iQ1 = m_kHullP[1];
    int iQ2;

    if ( fOrder > Math<Real>::EPSILON )
    {
        // insert <P,Q[0],Q[1]>
        InsertTriangle(iP,iQ0,iQ1);

        for (i = 2; i < iSize; iQ1 = iQ2, i++)
        {
            // insert <Q[0],Q[i+1],Q[i]> and <P,Q[i],Q[i+1]>
            iQ2 = m_kHullP[i];
            InsertTriangle(iQ0,iQ2,iQ1);
            InsertTriangle(iP,iQ1,iQ2);
        }

        // insert <P,Q[n-1],Q[0]>
        InsertTriangle(iP,iQ1,iQ0);
    }
    else  // fOrder < -ms_fEpsilon
    {
        // insert <P,Q[1],Q[0]>
        InsertTriangle(iP,iQ1,iQ0);

        for (i = 2; i < iSize; iQ1 = iQ2, i++)
        {
            // insert <Q[0],Q[i],Q[i+1]> and <P,Q[i+1],Q[i]>
            iQ2 = m_kHullP[i];
            InsertTriangle(iQ0,iQ1,iQ2);
            InsertTriangle(iP,iQ2,iQ1);
        }

        // insert <P,Q[0],Q[n-1]>
        InsertTriangle(iP,iQ0,iQ1);
    }
}
//----------------------------------------------------------------------------
template <class Real>
void ConvexHull3<Real>::MergeSpatial (int iP)
{
    // Find the triangle that is "most visible" to P.  Just finding the first
    // visible triangle is not sufficient when floating point round-off errors
    // are present.  The most visible triangle is used to start a
    // breadth-first for the connected mesh of triangles that are visible
    // to P.
    Vector3<Real> kDiff;
    Real fOrder, fMaxOrder = (Real)0.0;
    const Triangle* pkTri;
    const Triangle* pkMaxTri = NULL;
    const ETManifoldMesh::TMap& rkTMap = m_kHullS.GetTriangles();
    ETManifoldMesh::TMap::const_iterator pkTIter;
    for (pkTIter = rkTMap.begin(); pkTIter != rkTMap.end(); pkTIter++)
    {
        pkTri = (const Triangle*)pkTIter->second;
        kDiff = m_akVertex[iP] - m_akVertex[pkTri->V[0]];
        kDiff.Normalize();
        fOrder = pkTri->Normal.Dot(kDiff);
        if ( fOrder > fMaxOrder )
        {
            // triangle is visible to P
            fMaxOrder = fOrder;
            pkMaxTri = pkTri;
        }
    }

    if ( !pkMaxTri || fMaxOrder < Math<Real>::EPSILON )
    {
        // P is inside the current hull
        return;
    }

    // Triangle pkMinTri is visible to P.  Perform a breadth-first search to
    // locate all remaining visible triangles.
    set<const Triangle*> kInterior, kBoundary;
    kInterior.insert(pkMaxTri);
    const Triangle* pkAdj;
    int i;
    for (i = 0; i < 3; i++)
    {
        pkAdj = (const Triangle*)pkMaxTri->T[i];
        assert( pkAdj );
        kBoundary.insert(pkAdj);
    }

    typename set<const Triangle*>::iterator pkIter;
    while ( kBoundary.size() > 0 )
    {
        set<const Triangle*> kExterior;

        // process boundary triangles
        for (pkIter = kBoundary.begin(); pkIter != kBoundary.end(); pkIter++)
        {
            // current boundary triangle to process
            pkTri = (const Triangle*)*pkIter;
            kDiff = m_akVertex[iP] - m_akVertex[pkTri->V[0]];
            kDiff.Normalize();
            fOrder = pkTri->Normal.Dot(kDiff);
            if ( fOrder > (Real)0.0 )
            {
                // triangle is visible to P
                kInterior.insert(pkTri);

                // locate adjacent, exterior triangles for later processing
                for (i = 0; i < 3; i++)
                {
                    pkAdj = (const Triangle*)pkTri->T[i];
                    assert( pkAdj );
                    if ( kInterior.find(pkAdj) == kInterior.end()
                    &&   kBoundary.find(pkAdj) == kBoundary.end() )
                    {
                        kExterior.insert(pkAdj);
                    }
                }
            }
        }

        // exterior triangles are next in line to be processed
        kBoundary = kExterior;
    }

    // locate the terminator edges
    map<int,int> kTerminator;
    int iV0, iV1;
    for (pkIter = kInterior.begin(); pkIter != kInterior.end(); pkIter++)
    {
        // get a visible triangle
        pkTri = (const Triangle*)*pkIter;

        // If an adjacent triangle is not visible, then the shared edge is a
        // terminator edge.
        for (i = 0; i < 3; i++)
        {
            pkAdj = (const Triangle*)pkTri->T[i];
            assert( pkAdj );
            if ( kInterior.find(pkAdj) == kInterior.end() )
            {
                // adjacent triangle is not visible
                iV0 = pkTri->V[i];
                iV1 = pkTri->V[(i+1)%3];
                kTerminator[iV0] = iV1;
            }
        }
    }

    // remove the visible triangles
    for (pkIter = kInterior.begin(); pkIter != kInterior.end(); pkIter++)
    {
        pkTri = (const Triangle*)*pkIter;
        bool bRemoved = m_kHullS.RemoveTriangle(pkTri->V[0],pkTri->V[1],
            pkTri->V[2]);
        assert( bRemoved );
    }

    // add new triangles formed by P and terminator edges
    int iSize = (int)kTerminator.size();
    assert( iSize >= 3 );
    map<int,int>::iterator pkE = kTerminator.begin();
    int iVStart = pkE->first;
    iV0 = iVStart;
    for (i = 0; i < iSize; i++)
    {
        iV1 = pkE->second;

        // insert <P,V0,V1>
        InsertTriangle(iP,iV0,iV1);

        pkE = kTerminator.find(iV1);
        assert( pkE != kTerminator.end() );
        iV0 = iV1;
    }
    assert( iV0 == iVStart );
}
//----------------------------------------------------------------------------
template <class Real>
void ConvexHull3<Real>::InsertTriangle (int iV0, int iV1, int iV2)
{
    Triangle* pkTri = (Triangle*) m_kHullS.InsertTriangle(iV0,iV1,iV2);
    assert( pkTri );

    const Vector3<Real>& rkV0 = m_akVertex[iV0];
    const Vector3<Real>& rkV1 = m_akVertex[iV1];
    const Vector3<Real>& rkV2 = m_akVertex[iV2];
    Vector3<Real> kE1 = rkV1 - rkV0, kE2 = rkV2 - rkV0;
    pkTri->Normal = kE1.UnitCross(kE2);
}
//----------------------------------------------------------------------------
template <class Real>
ETManifoldMesh::Triangle* ConvexHull3<Real>::CreateTriangle (int iV0, int iV1,
    int iV2)
{
    return new Triangle(iV0,iV1,iV2);
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM ConvexHull3<float>;
template class WML_ITEM ConvexHull3<double>;
}
//----------------------------------------------------------------------------
