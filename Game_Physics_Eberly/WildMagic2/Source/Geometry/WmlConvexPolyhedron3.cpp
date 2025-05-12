// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlConvexPolyhedron3.h"
#include "WmlConvexClipper.h"
#include <fstream>
#include <map>
#include <set>
using namespace Wml;
using namespace std;

//----------------------------------------------------------------------------
template <class Real>
ConvexPolyhedron3<Real>::ConvexPolyhedron3 ()
{
}
//----------------------------------------------------------------------------
template <class Real>
ConvexPolyhedron3<Real>::ConvexPolyhedron3 (const V3Array& rakPoint,
    const IArray& raiConnect)
{
    Create(rakPoint,raiConnect);
}
//----------------------------------------------------------------------------
template <class Real>
ConvexPolyhedron3<Real>::ConvexPolyhedron3 (const V3Array& rakPoint,
    const IArray& raiConnect, const PArray& rakPlane)
{
    Create(rakPoint,raiConnect,rakPlane);
}
//----------------------------------------------------------------------------
template <class Real>
ConvexPolyhedron3<Real>::ConvexPolyhedron3 (const ConvexPolyhedron3& rkPoly)
    :
    MTMesh(rkPoly),
    m_akPoint(rkPoly.m_akPoint),
    m_akPlane(rkPoly.m_akPlane)
{
}
//----------------------------------------------------------------------------
template <class Real>
void ConvexPolyhedron3<Real>::Create (const V3Array& rakPoint,
    const IArray& raiConnect)
{
    assert( rakPoint.size() >= 4 && raiConnect.size() >= 4 );

    int iVQuantity = (int)rakPoint.size();
    int iTQuantity = (int)raiConnect.size()/3;
    int iEQuantity = iVQuantity + iTQuantity - 2;
    Reset(iVQuantity,iEQuantity,iTQuantity);
    m_akPoint = rakPoint;

    // Copy polyhedron points into vertex array.  Compute centroid for use in
    // making sure the triangles are counterclockwise oriented when viewed
    // from the outside.
    ComputeCentroid();

    // get polyhedron edge and triangle information
    for (int iT = 0, iIndex = 0; iT < iTQuantity; iT++)
    {
        // get vertex indices for triangle
        int iV0 = raiConnect[iIndex++];
        int iV1 = raiConnect[iIndex++];
        int iV2 = raiConnect[iIndex++];

        // make sure triangle is counterclockwise
        Vector3<Real>& rkV0 = m_akPoint[iV0];
        Vector3<Real>& rkV1 = m_akPoint[iV1];
        Vector3<Real>& rkV2 = m_akPoint[iV2];

        Vector3<Real> kDiff = m_kCentroid - rkV0;
        Vector3<Real> kE1 = rkV1 - rkV0;
        Vector3<Real> kE2 = rkV2 - rkV0;
        Vector3<Real> kNormal = kE1.Cross(kE2);
        Real fLength = kNormal.Length();
        if ( fLength > Math<Real>::EPSILON )
        {
            kNormal /= fLength;
        }
        else
        {
            kNormal = kDiff;
            kNormal.Normalize();
        }

        Real fDistance = kNormal.Dot(kDiff);

        if ( fDistance < (Real)0.0 )
        {
            // triangle is counterclockwise
            Insert(iV0,iV1,iV2);
        }
        else
        {
            // triangle is clockwise
            Insert(iV0,iV2,iV1);
        }
    }

    UpdatePlanes();
}
//----------------------------------------------------------------------------
template <class Real>
void ConvexPolyhedron3<Real>::Create (const V3Array& rakPoint,
    const IArray& raiConnect, const PArray& rakPlane)
{
    assert( rakPoint.size() >= 4 && raiConnect.size() >= 4 );

    int iVQuantity = (int)rakPoint.size();
    int iTQuantity = (int)raiConnect.size()/3;
    int iEQuantity = iVQuantity + iTQuantity - 2;
    Reset(iVQuantity,iEQuantity,iTQuantity);
    m_akPoint = rakPoint;
    m_akPlane = rakPlane;

    // Copy polyhedron points into vertex array.  Compute centroid for use in
    // making sure the triangles are counterclockwise oriented when viewed
    // from the outside.
    ComputeCentroid();

    // get polyhedron edge and triangle information
    for (int iT = 0, iIndex = 0; iT < iTQuantity; iT++)
    {
        // get vertex indices for triangle
        int iV0 = raiConnect[iIndex++];
        int iV1 = raiConnect[iIndex++];
        int iV2 = raiConnect[iIndex++];

        Real fDistance = m_akPlane[iT].DistanceTo(m_kCentroid);
        if ( fDistance > (Real)0.0 )
        {
            // triangle is counterclockwise
            Insert(iV0,iV1,iV2);
        }
        else
        {
            // triangle is clockwise
            Insert(iV0,iV2,iV1);
        }
    }
}
//----------------------------------------------------------------------------
template <class Real>
const typename ConvexPolyhedron3<Real>::V3Array&
ConvexPolyhedron3<Real>::GetPoints () const
{
    return m_akPoint;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector3<Real>& ConvexPolyhedron3<Real>::GetPoint (int iV) const
{
    assert( 0 <= iV && iV < (int)m_akPoint.size() );
    return m_akPoint[iV];
}
//----------------------------------------------------------------------------
template <class Real>
int ConvexPolyhedron3<Real>::AddPoint (const Vector3<Real>& rkPoint)
{
    int iLocation = (int)m_akPoint.size();
    m_akPoint.push_back(rkPoint);
    return iLocation;
}
//----------------------------------------------------------------------------
template <class Real>
typename ConvexPolyhedron3<Real>::V3Array& ConvexPolyhedron3<Real>::Points ()
{
    return m_akPoint;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real>& ConvexPolyhedron3<Real>::Point (int iV)
{
    assert( 0 <= iV && iV < (int)m_akPoint.size() );
    return m_akPoint[iV];
}
//----------------------------------------------------------------------------
template <class Real>
const typename ConvexPolyhedron3<Real>::PArray&
ConvexPolyhedron3<Real>::GetPlanes () const
{
    return m_akPlane;
}
//----------------------------------------------------------------------------
template <class Real>
const Plane3<Real>& ConvexPolyhedron3<Real>::GetPlane (int iT) const
{
    assert( 0 <= iT && iT < (int)m_akPlane.size() );
    return m_akPlane[iT];
}
//----------------------------------------------------------------------------
template <class Real>
const Vector3<Real>& ConvexPolyhedron3<Real>::GetCentroid () const
{
    return m_kCentroid;
}
//----------------------------------------------------------------------------
template <class Real>
ConvexPolyhedron3<Real>& ConvexPolyhedron3<Real>::operator= (
    const ConvexPolyhedron3& rkPoly)
{
    MTMesh::operator=(rkPoly);
    m_akPoint = rkPoly.m_akPoint;
    m_akPlane = rkPoly.m_akPlane;
    return *this;
}
//----------------------------------------------------------------------------
template <class Real>
void ConvexPolyhedron3<Real>::UpdatePlanes ()
{
    // The planes are constructed to have *inner pointing* normals.  This
    // supports my Wild Magic software clipping code that was based on a view
    // frustum having inner pointing normals.
    ComputeCentroid();
    int iTQuantity = m_akTriangle.GetQuantity();
    m_akPlane.resize(iTQuantity);
    for (int iT = 0; iT < iTQuantity; iT++)
    {
        MTTriangle& rkT = m_akTriangle[iT];
        int iV0 = GetVLabel(rkT.Vertex(0));
        int iV1 = GetVLabel(rkT.Vertex(1));
        int iV2 = GetVLabel(rkT.Vertex(2));
        Vector3<Real>& rkV0 = m_akPoint[iV0];
        Vector3<Real>& rkV1 = m_akPoint[iV1];
        Vector3<Real>& rkV2 = m_akPoint[iV2];

        Vector3<Real> kDiff = m_kCentroid - rkV0;
        Vector3<Real> kE1 = rkV1 - rkV0;
        Vector3<Real> kE2 = rkV2 - rkV0;
        Vector3<Real> kNormal = kE2.Cross(kE1);
        Real fLength = kNormal.Length();
        if ( fLength > Math<Real>::EPSILON )
        {
            kNormal /= fLength;
            Real fDot = kNormal.Dot(kDiff);
            if ( fDot < (Real)0.0 )
                kNormal = -kNormal;
        }
        else
        {
            // triangle is degenerate, use "normal" that points towards
            // centroid
            kNormal = kDiff;
            kNormal.Normalize();
        }

        // inner pointing normal
        m_akPlane[iT] = Plane3<Real>(kNormal,kNormal.Dot(rkV0));
    }
}
//----------------------------------------------------------------------------
template <class Real>
bool ConvexPolyhedron3<Real>::ValidateHalfSpaceProperty (Real fThreshold)
    const
{
    Real fMax = -Math<Real>::MAX_REAL, fMin = Math<Real>::MAX_REAL;
    for (int iT = 0; iT < m_akTriangle.GetQuantity(); iT++)
    {
        const Plane3<Real>& rkPlane = m_akPlane[iT];
        for (int i = 0; i < (int)m_akPoint.size(); i++)
        {
            Real fDistance = rkPlane.DistanceTo(m_akPoint[i]);
            if ( fDistance < fMin )
                fMin = fDistance;
            if ( fDistance > fMax )
                fMax = fDistance;
            if ( fDistance < fThreshold )
                return false;
        }
    }

    return true;
}
//----------------------------------------------------------------------------
template <class Real>
void ConvexPolyhedron3<Real>::ComputeCentroid ()
{
    m_kCentroid = Vector3<Real>::ZERO;
    for (int iV = 0; iV < (int)m_akPoint.size(); iV++)
        m_kCentroid += m_akPoint[iV];
    m_kCentroid /= (Real)m_akPoint.size();
}
//----------------------------------------------------------------------------
template <class Real>
bool ConvexPolyhedron3<Real>::Clip (const Plane3<Real>& rkPlane,
    ConvexPolyhedron3& rkIntr) const
{
    ConvexClipper<Real> kClipper(*this);
    int iSide = kClipper.Clip(rkPlane);

    if ( iSide == Plane3<Real>::POSITIVE_SIDE )
    {
        rkIntr = *this;
        return true;
    }

    if ( iSide == Plane3<Real>::NEGATIVE_SIDE )
        return false;

    kClipper.Convert(rkIntr);
    return true;
}
//----------------------------------------------------------------------------
template <class Real>
bool ConvexPolyhedron3<Real>::FindIntersection (
    const ConvexPolyhedron3& rkPoly, ConvexPolyhedron3& rkIntr) const
{
    ConvexClipper<Real> kClipper(*this);

    const PArray& rakPlane = rkPoly.GetPlanes();
    for (int i = 0; i < (int)rakPlane.size(); i++)
    {
        if ( kClipper.Clip(rakPlane[i]) == Plane3<Real>::NEGATIVE_SIDE )
            return false;
    }

    kClipper.Convert(rkIntr);
    return true;
}
//----------------------------------------------------------------------------
template <class Real>
void ConvexPolyhedron3<Real>::FindAllIntersections (int iQuantity,
    ConvexPolyhedron3* akPoly, int& riCombos, ConvexPolyhedron3**& rapkIntr)
{
    // Only 2^16 possible combinations for intersections are currently
    // supported.  If you need more, then GetHighBit(int) must be modified
    // to handle more than 16-bit inputs.
    if ( iQuantity <= 0 || iQuantity > 16 )
    {
        riCombos = 0;
        rapkIntr = NULL;
        return;
    }

    riCombos = (1 << iQuantity);
    bool* abNeedsTesting = new bool[riCombos];
    rapkIntr = new ConvexPolyhedron3*[riCombos];
    int i;
    for (i = 0; i < riCombos; i++)
    {
        abNeedsTesting[i] = true;
        rapkIntr[i] = NULL;
    }

    // trivial cases, zero or one polyhedron--already the intersection
    abNeedsTesting[0] = false;
    for (i = 0; i < iQuantity; i++)
    {
        int j = (1 << i);
        abNeedsTesting[j] = false;
        rapkIntr[j] = new ConvexPolyhedron3(akPoly[i]);
    }
    
    for (i = 3; i < riCombos; i++)
    {
        if ( abNeedsTesting[i] )
        {
            // In binary, i = b[m]...b[0] where b[m] is not zero (the
            // high-order bit.  Also, i1 = b[m-1]...b[0] is not zero
            // since, if it were, we would have ruled out the combination
            // by the j-loop below.  Therefore, i0 = b[m]0...0 and
            // i1 correspond to already existing polyhedra.  The
            // intersection finding just needs to look at the intersection
            // of the two polyhedra.
            int i0 = GetHighBit(i);
            int i1 = i & ~i0;
            rapkIntr[i] = FindSolidIntersection(*rapkIntr[i0],
                *rapkIntr[i1]);
            if ( !rapkIntr[i] )
            {
                // No intersection for this combination.  No need to test
                // other combinations that include this one.
                for (int j = 0; j < riCombos; j++)
                {
                    if ( (i & j) == i )
                        abNeedsTesting[j] = false;
                }
            }
#ifdef _DEBUG
            else  // test if well-formed convex polyhedron
            {
                Vector3<Real> kCentroid = rapkIntr[i]->GetCentroid();
                bool bContains = rapkIntr[i]->ContainsPoint(kCentroid);
                assert( bContains );
            }
#endif
        }
    }

    delete[] abNeedsTesting;
}
//----------------------------------------------------------------------------
template <class Real>
ConvexPolyhedron3<Real>* ConvexPolyhedron3<Real>::FindSolidIntersection (
    const ConvexPolyhedron3& rkPoly0, const ConvexPolyhedron3& rkPoly1)
{
    ConvexPolyhedron3* pkIntr = new ConvexPolyhedron3;
    if ( rkPoly0.FindIntersection(rkPoly1,*pkIntr) )
        return pkIntr;
    
    // As surfaces, the polyhedra do not intersect.  However, as solids,
    // one polyhedron might be fully contained in the other.
    if ( rkPoly0.ContainsPoint(rkPoly1.GetCentroid()) )
    {
        *pkIntr = rkPoly1;
        return pkIntr;
    }
        
    if ( rkPoly1.ContainsPoint(rkPoly0.GetCentroid()) )
    {
        *pkIntr = rkPoly0;
        return pkIntr;
    }

    delete pkIntr;
    return NULL;
}
//----------------------------------------------------------------------------
template <class Real>
int ConvexPolyhedron3<Real>::GetHighBit (int i)
{
    // assert: i in [1,2^16].  That is, (i>0) && (0xFFFF0000&i)==0.
    // This is a binary search for the high-order bit of i.
    if ( (i & 0xFF00) != 0 )
    {
        if ( (i & 0xF000) != 0 )
        {
            if ( (i & 0xC000) != 0 )
            {
                if ( (i & 0x8000) != 0 )
                    return 0x8000;
                else // (i & 0x4000) != 0
                    return 0x4000;
            }
            else  // (i & 0x3000) != 0
            {
                if ( (i & 0x2000) != 0 )
                    return 0x2000;
                else  // (i & 0x1000) != 0
                    return 0x1000;
            }
        }
        else  // (i & 0x0F00) != 0
        {
            if ( (i & 0x0C00) != 0 )
            {
                if ( (i & 0x0800) != 0 )
                    return 0x0800;
                else  // (i & 0x0400) != 0
                    return 0x0400;
            }
            else  // (i & 0x0300) != 0
            {
                if ( (i & 0x0200) != 0 )
                    return 0x0200;
                else  // (i & 0x0100) != 0
                    return 0x0100;
            }
        }
    }
    else  // (i & 0x00FF)
    {
        if ( (i & 0x00F0) != 0 )
        {
            if ( (i & 0x00C0) != 0 )
            {
                if ( (i & 0x0080) != 0 )
                    return 0x0080;
                else  // (i & 0x0040) != 0
                    return 0x0040;
            }
            else  // (i & 0x0030) != 0
            {
                if ( (i & 0x0020) != 0 )
                    return 0x0020;
                else  // (i & 0x0010) != 0
                    return 0x0010;
            }
        }
        else  // (i & 0x000F) != 0
        {
            if ( (i & 0x000C) != 0 )
            {
                if ( (i & 0x0008) != 0 )
                    return 0x0008;
                else  // (i & 0x0004) != 0
                    return 0x0004;
            }
            else  // (i & 0x0003) != 0
            {
                if ( (i & 0x0002) != 0 )
                    return 0x0002;
                else  // (i & 0x0001) != 0
                    return 0x0001;
            }
        }
    }
}
//----------------------------------------------------------------------------
template <class Real>
Real ConvexPolyhedron3<Real>::GetTriangleArea (const Vector3<Real>& rkN,
    const Vector3<Real>& rkV0, const Vector3<Real>& rkV1,
    const Vector3<Real>& rkV2) const
{
    // compute maximum absolute component of normal vector
    int iMax = 0;
    Real fMax = Math<Real>::FAbs(rkN.X());

    Real fAbs = Math<Real>::FAbs(rkN.Y());
    if ( fAbs > fMax )
    {
        iMax = 1;
        fMax = fAbs;
    }

    fAbs = Math<Real>::FAbs(rkN.Z());
    if ( fAbs > fMax )
    {
        iMax = 2;
        fMax = fAbs;
    }

    // catch degenerate triangles
    if ( fMax == (Real)0.0 )
        return (Real)0.0;

    // compute area of projected triangle
    Real fD0, fD1, fD2, fArea;
    if ( iMax == 0 )
    {
        fD0 = rkV1.Z() - rkV2.Z();
        fD1 = rkV2.Z() - rkV0.Z();
        fD2 = rkV0.Z() - rkV1.Z();
        fArea = Math<Real>::FAbs(rkV0.Y()*fD0 + rkV1.Y()*fD1 + rkV2.Y()*fD2);
    }
    else if ( iMax == 1 )
    {
        fD0 = rkV1.X() - rkV2.X();
        fD1 = rkV2.X() - rkV0.X();
        fD2 = rkV0.X() - rkV1.X();
        fArea = Math<Real>::FAbs(rkV0.Z()*fD0 + rkV1.Z()*fD1 + rkV2.Z()*fD2);
    }
    else
    {
        fD0 = rkV1.Y() - rkV2.Y();
        fD1 = rkV2.Y() - rkV0.Y();
        fD2 = rkV0.Y() - rkV1.Y();
        fArea = Math<Real>::FAbs(rkV0.X()*fD0 + rkV1.X()*fD1 + rkV2.X()*fD2);
    }

    fArea *= ((Real)0.5)/fMax;
    return fArea;
}
//----------------------------------------------------------------------------
template <class Real>
Real ConvexPolyhedron3<Real>::GetSurfaceArea () const
{
    Real fSurfaceArea = (Real)0.0;

    for (int iT = 0; iT < m_akTriangle.GetQuantity(); iT++)
    {
        const MTTriangle& rkT = m_akTriangle.Get(iT);
        int iV0 = GetVLabel(rkT.GetVertex(0));
        int iV1 = GetVLabel(rkT.GetVertex(1));
        int iV2 = GetVLabel(rkT.GetVertex(2));
        const Vector3<Real>& rkV0 = m_akPoint[iV0];
        const Vector3<Real>& rkV1 = m_akPoint[iV1];
        const Vector3<Real>& rkV2 = m_akPoint[iV2];
        const Vector3<Real>& rkN = m_akPlane[iT].GetNormal();

        fSurfaceArea += GetTriangleArea(rkN,rkV0,rkV1,rkV2);
    }

    return fSurfaceArea;
}
//----------------------------------------------------------------------------
template <class Real>
Real ConvexPolyhedron3<Real>::GetVolume () const
{
    Real fVolume = (Real)0.0;

    for (int iT = 0; iT < m_akTriangle.GetQuantity(); iT++)
    {
        const MTTriangle& rkT = m_akTriangle.Get(iT);
        int iV0 = GetVLabel(rkT.GetVertex(0));
        int iV1 = GetVLabel(rkT.GetVertex(1));
        int iV2 = GetVLabel(rkT.GetVertex(2));
        const Vector3<Real>& rkV0 = m_akPoint[iV0];
        const Vector3<Real>& rkV1 = m_akPoint[iV1];
        const Vector3<Real>& rkV2 = m_akPoint[iV2];
        fVolume += rkV0.Dot(rkV1.Cross(rkV2));
    }

    fVolume /= (Real)6.0;
    return fVolume;
}
//----------------------------------------------------------------------------
template <class Real>
bool ConvexPolyhedron3<Real>::ContainsPoint (const Vector3<Real>& rkP) const
{
    for (int iT = 0; iT < m_akTriangle.GetQuantity(); iT++)
    {
        Real fDistance = m_akPlane[iT].DistanceTo(rkP);
        if ( fDistance < (Real)0.0 )
            return false;
    }

    return true;
}
//----------------------------------------------------------------------------
template <class Real>
Real ConvexPolyhedron3<Real>::GetDistance (const Vector3<Real>& rkEye, int iT,
    vector<Real>& rafDistance) const
{
    // Signed distance from eye to plane of triangle.  When distance is
    // positive, triangle is visible from eye (front-facing).  When distance
    // is negative, triangle is not visible from eye (back-facing).  When
    // distance is zero, triangle is visible "on-edge" from eye.

    if ( rafDistance[iT] == Math<Real>::MAX_REAL )
    {
        rafDistance[iT] = -m_akPlane[iT].DistanceTo(rkEye);

        if ( Math<Real>::FAbs(rafDistance[iT]) < Math<Real>::EPSILON )
            rafDistance[iT] = (Real)0.0;
    }

    return rafDistance[iT];
}
//----------------------------------------------------------------------------
template <class Real>
bool ConvexPolyhedron3<Real>::IsNegativeProduct (Real fDist0, Real fDist1)
{
    return (fDist0 != (Real)0.0 ? (fDist0*fDist1 <= (Real)0.0) :
        (fDist1 != (Real)0.0));
}
//----------------------------------------------------------------------------
template <class Real>
void ConvexPolyhedron3<Real>::ComputeTerminator (const Vector3<Real>& rkEye,
    V3Array& rkTerminator)
{
    // temporary storage for signed distances from eye to triangles
    int iTQuantity = m_akTriangle.GetQuantity();
    vector<Real> afDistance(iTQuantity);
    int i, j;
    for (i = 0; i < iTQuantity; i++)
        afDistance[i] = Math<Real>::MAX_REAL;

    // Start a search for a front-facing triangle that has an adjacent
    // back-facing triangle or for a back-facing triangle that has an
    // adjacent front-facing triangle.
    int iTCurrent = 0;
    MTTriangle* pkTCurrent = &m_akTriangle[iTCurrent];
    Real fTriDist = GetDistance(rkEye,iTCurrent,afDistance);
    int iEFirst = -1;
    for (i = 0; i < iTQuantity; i++)
    {
        // Check adjacent neighbors for edge of terminator.  Such an
        // edge occurs if the signed distance changes sign.
        int iMinIndex = -1;
        Real fMinAbsDist = Math<Real>::MAX_REAL;
        Real afAdjDist[3];
        for (j = 0; j < 3; j++)
        {
            afAdjDist[j] = GetDistance(rkEye,pkTCurrent->Adjacent(j),
                afDistance);
            if ( IsNegativeProduct(fTriDist,afAdjDist[j]) )
            {
                iEFirst = pkTCurrent->Edge(j);
                break;
            }

            Real fAbsDist = Math<Real>::FAbs(afAdjDist[j]);
            if ( fAbsDist < fMinAbsDist )
            {
                fMinAbsDist = fAbsDist;
                iMinIndex = j;
            }
        }
        if ( j < 3 )
            break;

        // First edge not found during this iteration.  Move to adjacent
        // triangle whose distance is smallest of all adjacent triangles.
        iTCurrent = pkTCurrent->Adjacent(iMinIndex);
        pkTCurrent = &m_akTriangle[iTCurrent];
        fTriDist = afAdjDist[iMinIndex];
    }
    assert( i < iTQuantity );

    MTEdge& rkEFirst = m_akEdge[iEFirst];
    rkTerminator.push_back(m_akPoint[GetVLabel(rkEFirst.Vertex(0))]);
    rkTerminator.push_back(m_akPoint[GetVLabel(rkEFirst.Vertex(1))]);

    // walk along the terminator
    int iVFirst = rkEFirst.Vertex(0);
    int iV = rkEFirst.Vertex(1);
    int iE = iEFirst;
    int iEQuantity = m_akEdge.GetQuantity();
    for (i = 0; i < iEQuantity; i++)
    {
        // search all edges sharing the vertex for another terminator edge
        int j, jMax = m_akVertex[iV].GetEdgeQuantity();
        for (j = 0; j < m_akVertex[iV].GetEdgeQuantity(); j++)
        {
            int iENext = m_akVertex[iV].GetEdge(j);
            if ( iENext == iE )
                continue;

            Real fDist0 = GetDistance(rkEye,m_akEdge[iENext].GetTriangle(0),
                afDistance);
            Real fDist1 = GetDistance(rkEye,m_akEdge[iENext].GetTriangle(1),
                afDistance);
            if ( IsNegativeProduct(fDist0,fDist1) )
            {
                if ( m_akEdge[iENext].GetVertex(0) == iV )
                {
                    iV = m_akEdge[iENext].GetVertex(1);
                    rkTerminator.push_back(m_akPoint[GetVLabel(iV)]);
                    if ( iV == iVFirst )
                        return;
                }
                else
                {
                    iV = m_akEdge[iENext].GetVertex(0);
                    rkTerminator.push_back(m_akPoint[GetVLabel(iV)]);
                    if ( iV == iVFirst )
                        return;
                }

                iE = iENext;
                break;
            }
        }
        assert( j < jMax );
    }
    assert( i < iEQuantity );
}
//----------------------------------------------------------------------------
template <class Real>
bool ConvexPolyhedron3<Real>::ComputeSilhouette (const Vector3<Real>& rkEye,
    const Plane3<Real>& rkPlane, const Vector3<Real>& rkU,
    const Vector3<Real>& rkV, V2Array& rkSilhouette)
{
    V3Array kTerminator;
    ComputeTerminator(rkEye,kTerminator);
    return ComputeSilhouette(kTerminator,rkEye,rkPlane,rkU,rkV,rkSilhouette);
}
//----------------------------------------------------------------------------
template <class Real>
bool ConvexPolyhedron3<Real>::ComputeSilhouette (V3Array& rkTerminator,
    const Vector3<Real>& rkEye, const Plane3<Real>& rkPlane,
    const Vector3<Real>& rkU, const Vector3<Real>& rkV,
    V2Array& rkSilhouette)
{
    Real fEDist = rkPlane.DistanceTo(rkEye);  // assert:  fEDist > 0

    // closest planar point to E is K = E-dist*N
    Vector3<Real> kClosest = rkEye - fEDist*rkPlane.GetNormal();

    // project polyhedron points onto plane
    for (int i = 0; i < (int)rkTerminator.size(); i++)
    {
        Vector3<Real>& rkPoint = rkTerminator[i];

        Real fVDist = rkPlane.DistanceTo(rkPoint);
        if ( fVDist >= fEDist )
        {
            // cannot project vertex onto plane
            return false;
        }

        // compute projected point Q
        Real fRatio = fEDist/(fEDist-fVDist);
        Vector3<Real> kProjected = rkEye + fRatio*(rkPoint - rkEye);

        // compute (x,y) so that Q = K+x*U+y*V+z*N
        Vector3<Real> kDiff = kProjected - kClosest;
        rkSilhouette.push_back(Vector2<Real>(rkU.Dot(kDiff),rkV.Dot(kDiff)));
    }

    return true;
}
//----------------------------------------------------------------------------
template <class Real>
void ConvexPolyhedron3<Real>::CreateEggShape (const Vector3<Real>& rkCenter,
    Real fX0, Real fX1, Real fY0, Real fY1, Real fZ0, Real fZ1,
    int iMaxSteps, ConvexPolyhedron3& rkEgg)
{
    assert( fX0 > (Real)0.0 && fX1 > (Real)0.0 );
    assert( fY0 > (Real)0.0 && fY1 > (Real)0.0 );
    assert( fZ0 > (Real)0.0 && fZ1 > (Real)0.0 );
    assert( iMaxSteps >= 0 );

    // Start with an octahedron whose 6 vertices are (-x0,0,0), (x1,0,0),
    // (0,-y0,0), (0,y1,0), (0,0,-z0), (0,0,z1).  The center point will be
    // added later.
    V3Array akPoint(6);
    akPoint[0] = Vector3<Real>(-fX0,(Real)0.0,(Real)0.0);
    akPoint[1] = Vector3<Real>(fX1,(Real)0.0,(Real)0.0);
    akPoint[2] = Vector3<Real>((Real)0.0,-fY0,(Real)0.0);
    akPoint[3] = Vector3<Real>((Real)0.0,fY1,(Real)0.0);
    akPoint[4] = Vector3<Real>((Real)0.0,(Real)0.0,-fZ0);
    akPoint[5] = Vector3<Real>((Real)0.0,(Real)0.0,fZ1);

    IArray aiConnect(24);
    aiConnect[ 0] = 1;  aiConnect[ 1] = 3;  aiConnect[ 2] = 5;
    aiConnect[ 3] = 3;  aiConnect[ 4] = 0;  aiConnect[ 5] = 5;
    aiConnect[ 6] = 0;  aiConnect[ 7] = 2;  aiConnect[ 8] = 5;
    aiConnect[ 9] = 2;  aiConnect[10] = 1;  aiConnect[11] = 5;
    aiConnect[12] = 3;  aiConnect[13] = 1;  aiConnect[14] = 4;
    aiConnect[15] = 0;  aiConnect[16] = 3;  aiConnect[17] = 4;
    aiConnect[18] = 2;  aiConnect[19] = 0;  aiConnect[20] = 4;
    aiConnect[21] = 1;  aiConnect[22] = 2;  aiConnect[23] = 4;

    rkEgg.InitialELabel() = 0;
    rkEgg.Create(akPoint,aiConnect);

    // Subdivide the triangles.  The midpoints of the edges are computed.
    // The triangle is replaced by four sub triangles using the original 3
    // vertices and the 3 new edge midpoints.

    int i;
    for (int iStep = 1; iStep <= iMaxSteps; iStep++)
    {
        int iVQuantity = rkEgg.GetVQuantity();
        int iEQuantity = rkEgg.GetEQuantity();
        int iTQuantity = rkEgg.GetTQuantity();

        // compute lifted edge midpoints
        for (i = 0; i < iEQuantity; i++)
        {
            // get edge
            const MTEdge& rkE = rkEgg.GetEdge(i);
            int iV0 = rkEgg.GetVLabel(rkE.GetVertex(0));
            int iV1 = rkEgg.GetVLabel(rkE.GetVertex(1));

            // compute "lifted" centroid to points
            Vector3<Real> kCen = rkEgg.Point(iV0)+rkEgg.Point(iV1);
            Real fXR = (kCen.X() > (Real)0.0 ? kCen.X()/fX1 : kCen.X()/fX0);
            Real fYR = (kCen.Y() > (Real)0.0 ? kCen.Y()/fY1 : kCen.Y()/fY0);
            Real fZR = (kCen.Z() > (Real)0.0 ? kCen.Z()/fZ1 : kCen.Z()/fZ0);
            kCen *= Math<Real>::InvSqrt(fXR*fXR+fYR*fYR+fZR*fZR);

            // Add the point to the array.  Store the point index in the edge
            // label for support in adding new triangles.
            rkEgg.ELabel(i) = iVQuantity++;
            rkEgg.AddPoint(kCen);
        }

        // Add the new triangles and remove the old triangle.  The removal
        // in slot i will cause the last added triangle to be moved to that
        // slot.  This side effect will not interfere with the iteration
        // and removal of the triangles.
        for (i = 0; i < iTQuantity; i++)
        {
            const MTTriangle& rkT = rkEgg.GetTriangle(i);
            int iV0 = rkEgg.GetVLabel(rkT.GetVertex(0));
            int iV1 = rkEgg.GetVLabel(rkT.GetVertex(1));
            int iV2 = rkEgg.GetVLabel(rkT.GetVertex(2));
            int iV01 = rkEgg.GetELabel(rkT.GetEdge(0));
            int iV12 = rkEgg.GetELabel(rkT.GetEdge(1));
            int iV20 = rkEgg.GetELabel(rkT.GetEdge(2));
            rkEgg.Insert(iV0,iV01,iV20);
            rkEgg.Insert(iV01,iV1,iV12);
            rkEgg.Insert(iV20,iV12,iV2);
            rkEgg.Insert(iV01,iV12,iV20);
            rkEgg.Remove(iV0,iV1,iV2);
        }
    }

    // add center
    for (i = 0; i < (int)rkEgg.m_akPoint.size(); i++)
        rkEgg.m_akPoint[i] += rkCenter;

    rkEgg.UpdatePlanes();
}
//----------------------------------------------------------------------------
template <class Real>
void ConvexPolyhedron3<Real>::Print (ofstream& rkOStr) const
{
    MTMesh::Print(rkOStr);

    int i;
    char acMsg[512];

    rkOStr << "points:" << endl;
    for (i = 0; i < (int)m_akPoint.size(); i++)
    {
        const Vector3<Real>& rkV = m_akPoint[i];
        sprintf(acMsg,"point<%d> = (%+8.4f,%+8.4f,%+8.4f)",i,rkV.X(),rkV.Y(),
            rkV.Z());
        rkOStr << acMsg << endl;
    }
    rkOStr << endl;

    rkOStr << "planes:" << endl;
    for (i = 0; i < (int)m_akPlane.size(); i++)
    {
        const Plane3<Real>& rkP = m_akPlane[i];
        sprintf(acMsg,"plane<%d> = (%+8.6f,%+8.6f,%+8.6f;%+8.4f)",i,
            rkP.GetNormal().X(),rkP.GetNormal().Y(),rkP.GetNormal().Z(),
            rkP.GetConstant());
        rkOStr << acMsg << endl;
    }
    rkOStr << endl;
}
//----------------------------------------------------------------------------
template <class Real>
bool ConvexPolyhedron3<Real>::Print (const char* acFilename) const
{
    ofstream kOStr(acFilename);
    if ( !kOStr )
        return false;

    Print(kOStr);
    return true;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM ConvexPolyhedron3<float>;
template class WML_ITEM ConvexPolyhedron3<double>;
}
//----------------------------------------------------------------------------
