// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlConvexHull2.h"
using namespace Wml;

#include <algorithm>
#include <set>
using namespace std;

//----------------------------------------------------------------------------
template <class Real>
ConvexHull2<Real>::ConvexHull2 (int iVQuantity, const Vector2<Real>* akVertex,
    bool bIncremental)
{
    m_iVQuantity = iVQuantity;
    m_akVertex = akVertex;

    if ( bIncremental )
        ByIncremental();
    else
        ByDivideAndConquer();

    if ( m_iHQuantity >= 3 )
        RemoveCollinear();
}
//----------------------------------------------------------------------------
template <class Real>
ConvexHull2<Real>::~ConvexHull2 ()
{
    delete[] m_aiHIndex;
}
//----------------------------------------------------------------------------
template <class Real>
int ConvexHull2<Real>::GetQuantity () const
{
    return m_iHQuantity;
}
//----------------------------------------------------------------------------
template <class Real>
const int* ConvexHull2<Real>::GetIndices () const
{
    return m_aiHIndex;
}
//----------------------------------------------------------------------------
template <class Real>
bool ConvexHull2<Real>::ContainsPoint (const Vector2<Real>& rkP) const
{
    for (int i1 = 0, i0 = m_iHQuantity-1; i1 < m_iHQuantity; i0 = i1++)
    {
        const Vector2<Real>& rkV0 = m_akVertex[m_aiHIndex[i0]];
        const Vector2<Real>& rkV1 = m_akVertex[m_aiHIndex[i1]];
        Vector2<Real> kDir = rkV1 - rkV0;
        Vector2<Real> kNormal = kDir.Perp();  // outer normal
        if ( kNormal.Dot(rkP-rkV0) > (Real)0.0 )
            return false;
    }

    return true;
}
//----------------------------------------------------------------------------
template <class Real>
int ConvexHull2<Real>::CollinearTest (const Vector2<Real>& rkP,
    const Vector2<Real>& rkQ0, const Vector2<Real>& rkQ1) const
{
    Vector2<Real> kD = rkQ1 - rkQ0;
    Vector2<Real> kA = rkP - rkQ0;
    Real fDdD = kD.Dot(kD);
    Real fAdA = kA.Dot(kA);
    Real fDet = kD.Kross(kA);
    Real fRelative = fDet*fDet - COLLINEAR_EPSILON*fDdD*fAdA;

    if ( fRelative > (Real)0.0 )
    {
        if ( fDet > (Real)0.0 )
        {
            // points form counterclockwise triangle <P,Q0,Q1>
            return ORDER_POSITIVE;
        }
        else if ( fDet < (Real)0.0 )
        {
            // points form clockwise triangle <P,Q1,Q0>
            return ORDER_NEGATIVE;
        }
    }

    // P is on line of <Q0,Q1>
    Real fDdA = kD.Dot(kA);
    if ( fDdA < (Real)0.0 )
    {
        // order is <P,Q0,Q1>
        return ORDER_COLLINEAR_LEFT;
    }

    if ( fDdA > fDdD )
    {
        // order is <Q0,Q1,P>
        return ORDER_COLLINEAR_RIGHT;
    }

    // order is <Q0,P,Q1>
    return ORDER_COLLINEAR_CONTAIN;
}
//----------------------------------------------------------------------------
template <class Real>
void ConvexHull2<Real>::RemoveCollinear ()
{
    if ( m_iHQuantity <= 2 )
        return;

    vector<int> kHull;
    kHull.reserve(m_iHQuantity);
    for (int i0 = m_iHQuantity-1, i1 = 0, i2 = 1; i1 < m_iHQuantity; /**/)
    {
        int iCT = CollinearTest(m_akVertex[m_aiHIndex[i0]],
            m_akVertex[m_aiHIndex[i1]],m_akVertex[m_aiHIndex[i2]]);

        if ( iCT == ORDER_POSITIVE || iCT == ORDER_NEGATIVE )
        {
            // points are not collinear
            kHull.push_back(m_aiHIndex[i1]);
        }

        i0 = i1++;
        if ( ++i2 == m_iHQuantity )
            i2 = 0;
    }

    // construct index array for ordered vertices of convex hull
    m_iHQuantity = (int)kHull.size();
    delete[] m_aiHIndex;
    m_aiHIndex = new int[m_iHQuantity];
    memcpy(m_aiHIndex,&kHull.front(),m_iHQuantity*sizeof(int));
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// divide-and-conquer hull
//----------------------------------------------------------------------------
template <class Real>
void ConvexHull2<Real>::ByDivideAndConquer ()
{
    // Sort by x-component and store in contiguous array.  The sort is
    // O(N log N).
    SVArray kSVArray(m_iVQuantity);
    int i;
    for (i = 0; i < m_iVQuantity; i++)
    {
        kSVArray[i].m_kV = m_akVertex[i];
        kSVArray[i].m_iIndex = i;
    }
    sort(kSVArray.begin(),kSVArray.end());

    // remove duplicate points
    typename SVArray::iterator pkEnd =
        unique(kSVArray.begin(),kSVArray.end());
    kSVArray.erase(pkEnd,kSVArray.end());

    // compute convex hull using divide-and-conquer
    SVArray kHull;
    kHull.reserve(kSVArray.size());
    m_iHullType = HULL_POINT;
    GetHull(0,(int)kSVArray.size()-1,kSVArray,kHull);

    // construct index array for ordered vertices of convex hull
    m_iHQuantity = (int)kHull.size();
    m_aiHIndex = new int[m_iHQuantity];
    for (i = 0; i < m_iHQuantity; i++)
        m_aiHIndex[i] = kHull[i].m_iIndex;
}
//----------------------------------------------------------------------------
template <class Real>
void ConvexHull2<Real>::GetHull (int i0, int i1, const SVArray& rkSVArray,
    SVArray& rkHull)
{
    int iQuantity = i1 - i0 + 1;
    if ( iQuantity > 1 )
    {
        // middle index of input range
        int iMid = (i0+i1)/2;

        // find hull of subsets (iMid-i0+1 >= i1-iMid)
        SVArray kLHull, kRHull;
        kLHull.reserve(iMid-i0+1);
        kRHull.reserve(i1-iMid);

        GetHull(i0,iMid,rkSVArray,kLHull);
        GetHull(iMid+1,i1,rkSVArray,kRHull);

        // merge the convex hulls into a single convex hull
        Merge(kLHull,kRHull,rkHull);
    }
    else
    {
        // convex hull is a single point
        rkHull.push_back(rkSVArray[i0]);
    }
}
//----------------------------------------------------------------------------
template <class Real>
void ConvexHull2<Real>::Merge (SVArray& rkLHull, SVArray& rkRHull,
    SVArray& rkHull)
{
    // merge small sets, handle cases when sets are collinear
    int iLSize = (int)rkLHull.size(), iRSize = (int)rkRHull.size();
    if ( iLSize == 1 )
    {
        if ( iRSize == 1 )
        {
            rkHull.push_back(rkLHull[0]);
            rkHull.push_back(rkRHull[0]);
            if ( m_iHullType != HULL_PLANAR )
                m_iHullType = HULL_LINEAR;
            return;
        }
        else if ( iRSize == 2 )
        {
            MergeLinear(rkLHull[0],rkRHull);
            rkHull = rkRHull;
            return;
        }
    }
    else if ( iLSize == 2 )
    {
        if ( iRSize == 1 )
        {
            MergeLinear(rkRHull[0],rkLHull);
            rkHull = rkLHull;
            return;
        }
        else if ( iRSize == 2 )
        {
            MergeLinear(rkRHull[1],rkLHull);

            if ( rkLHull.size() == 2 )
            {
                MergeLinear(rkRHull[0],rkLHull);
                rkHull = rkLHull;
                return;
            }

            // fall-through, LHull is a triangle, RHull is a point
            iLSize = 3;
            iRSize = 1;
            rkRHull.pop_back();
        }
    }

    // find rightmost point of left hull
    Real fMax = rkLHull[0].m_kV.X();
    int i, iLMax = 0;
    for (i = 1; i < iLSize; i++)
    {
        if ( rkLHull[i].m_kV.X() > fMax )
        {
            fMax = rkLHull[i].m_kV.X();
            iLMax = i;
        }
    }

    // find leftmost point of right hull
    Real fMin = rkRHull[0].m_kV.X();
    int iRMin = 0;
    for (i = 1; i < iRSize; i++)
    {
        if ( rkRHull[i].m_kV.X() < fMin )
        {
            fMin = rkRHull[i].m_kV.X();
            iRMin = i;
        }
    }

    // get lower tangent to hulls (LL = lower left, LR = lower right)
    int iLLIndex = iLMax, iLRIndex = iRMin;
    GetTangent(rkLHull,rkRHull,iLLIndex,iLRIndex);

    // get upper tangent to hulls (UL = upper left, UR = upper right)
    int iULIndex = iLMax, iURIndex = iRMin;
    GetTangent(rkRHull,rkLHull,iURIndex,iULIndex);

    // Construct the counterclockwise-ordered merged-hull vertices.
    // TO DO.  If the hull is stored using linked lists, this block can be
    // improved by an O(1) detach, attach, and delete of the appropriate
    // sublists.
    i = iLRIndex;
    while ( true )
    {
        rkHull.push_back(rkRHull[i]);
        if ( i == iURIndex )
            break;

        if ( ++i == iRSize )
            i = 0;
    }

    i = iULIndex;
    while ( true )
    {
        rkHull.push_back(rkLHull[i]);
        if ( i == iLLIndex )
            break;

        if ( ++i == iLSize )
            i = 0;
    }
}
//----------------------------------------------------------------------------
template <class Real>
void ConvexHull2<Real>::MergeLinear (const SortedVertex& rkP, SVArray& rkHull)
{
    // hull = <Q0,Q1>

    switch ( CollinearTest(rkP.m_kV,rkHull[0].m_kV,rkHull[1].m_kV) )
    {
    case ORDER_POSITIVE:
        // merged hull is triangle <Q0,Q1,P>
        m_iHullType = HULL_PLANAR;
        rkHull.push_back(rkP);
        break;
    case ORDER_NEGATIVE:
    {
        // merged hull is triangle <Q1,Q0,P>
        m_iHullType = HULL_PLANAR;
        SortedVertex kSave = rkHull[0];
        rkHull[0] = rkHull[1];
        rkHull[1] = kSave;
        rkHull.push_back(rkP);
        break;
    }
    case ORDER_COLLINEAR_LEFT:
        // collinear order is <P,Q0,Q1>, merged hull is <P,Q1>
        rkHull[0] = rkP;
        break;
    case ORDER_COLLINEAR_RIGHT:
        // collinear order is <Q0,Q1,P>, merged hull is <Q0,P>
        rkHull[1] = rkP;
        break;
    //case ORDER_COLLINEAR_CONTAIN:
        // collinear order is <Q0,P,Q1>, merged hull is <Q0,Q1> (no change)
    }
}
//----------------------------------------------------------------------------
template <class Real>
void ConvexHull2<Real>::GetTangent (const SVArray& rkLHull,
    const SVArray& rkRHull, int& riL, int& riR)
{
    // In theory the loop terminates in a finite number of steps, but the
    // upper bound for the loop variable is used to trap problems caused by
    // floating point round-off errors that might lead to an infinite loop.

    int iLSize = (int)rkLHull.size(), iRSize = (int)rkRHull.size();
    int iLSizeM1 = iLSize-1, iRSizeM1 = iRSize-1;
    int i;
    for (i = 0; i < iLSize+iRSize; i++)
    {
        // end points of potential tangent
        const Vector2<Real>& rkL1 = rkLHull[riL].m_kV;
        const Vector2<Real>& rkR0 = rkRHull[riR].m_kV;

        // walk along left hull to find tangency
        int iLm1 = ( riL > 0 ? riL-1 : iLSizeM1 );
        const Vector2<Real>& rkL0 = rkLHull[iLm1].m_kV;
        int iCT = CollinearTest(rkR0,rkL0,rkL1);
        if ( iCT == ORDER_NEGATIVE || iCT == ORDER_COLLINEAR_LEFT )
        {
            riL = iLm1;
            continue;
        }

        // walk along right hull to find tangency
        int iRp1 = ( riR < iRSizeM1 ? riR+1 : 0 );
        const Vector2<Real>& rkR1 = rkRHull[iRp1].m_kV;
        iCT = CollinearTest(rkL1,rkR0,rkR1);
        if ( iCT == ORDER_NEGATIVE || iCT == ORDER_COLLINEAR_RIGHT )
        {
            riR = iRp1;
            continue;
        }

        // tangent segment has been found
        break;
    }

    // detect "infinite loop" caused by floating point round-off errors
    assert( i < iLSize+iRSize );
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// incremental hull
//----------------------------------------------------------------------------
template <class Real>
void ConvexHull2<Real>::ByIncremental ()
{
    // Sort by x-component and store in contiguous array.  The sort is
    // O(N log N).
    SVArray kSVArray(m_iVQuantity);
    int i;
    for (i = 0; i < m_iVQuantity; i++)
    {
        kSVArray[i].m_kV = m_akVertex[i];
        kSVArray[i].m_iIndex = i;
    }
    sort(kSVArray.begin(),kSVArray.end());

    // remove duplicate points
    typename SVArray::iterator pkEnd =
        unique(kSVArray.begin(),kSVArray.end());
    kSVArray.erase(pkEnd,kSVArray.end());

    // Compute convex hull incrementally.  The first and second vertices in
    // the hull are managed separately until at least one triangle is formed.
    // At that time an array is used to store the hull in counterclockwise
    // order.
    m_iHullType = HULL_POINT;
    m_kHull.push_back(kSVArray[0]);
    for (i = 1; i < (int)kSVArray.size(); i++)
    {
        switch ( m_iHullType )
        {
        case HULL_POINT:
            m_iHullType = HULL_LINEAR;
            m_kHull.push_back(kSVArray[i]);
            break;
        case HULL_LINEAR:
            MergeLinear(kSVArray[i]);
            break;
        case HULL_PLANAR:
            MergePlanar(kSVArray[i]);
            break;
        }
    }

    // construct index array for ordered vertices of convex hull
    m_iHQuantity = (int)m_kHull.size();
    m_aiHIndex = new int[m_iHQuantity];
    for (i = 0; i < m_iHQuantity; i++)
        m_aiHIndex[i] = m_kHull[i].m_iIndex;
}
//----------------------------------------------------------------------------
template <class Real>
void ConvexHull2<Real>::MergeLinear (const SortedVertex& rkP)
{
    switch ( CollinearTest(rkP.m_kV,m_kHull[0].m_kV,m_kHull[1].m_kV) )
    {
    case ORDER_POSITIVE:
    {
        // merged hull is <P,Q0,Q1>
        m_iHullType = HULL_PLANAR;
        SortedVertex kCopy = m_kHull[1];
        m_kHull.push_back(kCopy);
        m_kHull[1] = m_kHull[0];
        m_kHull[0] = rkP;
        break;
    }
    case ORDER_NEGATIVE:
    {
        // merged hull is <P,Q1,Q0>
        m_iHullType = HULL_PLANAR;
        SortedVertex kCopy = m_kHull[0];
        m_kHull.push_back(kCopy);
        m_kHull[0] = rkP;
        break;
    }
    case ORDER_COLLINEAR_LEFT:
        // linear order is <P,Q0,Q1>, merged hull is <P,Q1>
        m_kHull[0] = rkP;
        break;
    case ORDER_COLLINEAR_RIGHT:
        // linear order is <Q0,Q1,P>, merged hull is <Q0,P>
        m_kHull[1] = rkP;
        break;
    // case ORDER_COLLINEAR_CONTAIN:  linear order is <Q0,P,Q1>, no change
    }
}
//----------------------------------------------------------------------------
template <class Real>
void ConvexHull2<Real>::MergePlanar (const SortedVertex& rkP)
{
    int iSize = (int)m_kHull.size();
    int i, iU, iL, iCT;

    // search counterclockwise for last visible vertex
    for (iU = 0, i = 1; iU < iSize; iU = i++)
    {
        if ( i == iSize )
            i = 0;

        iCT = CollinearTest(rkP.m_kV,m_kHull[iU].m_kV,m_kHull[i].m_kV);
        if ( iCT == ORDER_NEGATIVE )
            continue;
        if ( iCT == ORDER_POSITIVE || iCT == ORDER_COLLINEAR_LEFT )
            break;

        // iCT == ORDER_COLLINEAR_CONTAIN || iCT == ORDER_COLLINEAR_RIGHT
        return;
    }
    assert( iU < iSize );

    // search clockwise for last visible vertex
    for (iL = 0, i = iSize-1; i >= 0; iL = i--)
    {
        iCT = CollinearTest(rkP.m_kV,m_kHull[i].m_kV,m_kHull[iL].m_kV);
        if ( iCT == ORDER_NEGATIVE )
            continue;
        if ( iCT == ORDER_POSITIVE || iCT == ORDER_COLLINEAR_RIGHT )
            break;

        // iCT == ORDER_COLLINEAR_CONTAIN || iCT == ORDER_COLLINEAR_LEFT
        return;
    }
    assert( i >= 0 );

    if ( iU == iL )
    {
        // This probably occurs when CollinearTest should report collinearity,
        // but does not.  If it does occur, and you care about this code
        // block not occurring, try increasing the size of the collinear
        // epsilon.  When this block does occur, the conclusion is that the
        // input point is collinear with an edge of the hull, so just return.
        return;
    }

    // construct the counterclockwise-ordered merged-hull vertices
    SVArray kTmpHull;
    kTmpHull.push_back(rkP);
    while ( true )
    {
        kTmpHull.push_back(m_kHull[iU]);
        if ( iU == iL )
            break;

        if ( ++iU == iSize )
            iU = 0;
    }
    assert( kTmpHull.size() > 2 );

    m_kHull = kTmpHull;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM ConvexHull2<float>;
float ConvexHull2f::COLLINEAR_EPSILON = 1e-06f;

template class WML_ITEM ConvexHull2<double>;
double ConvexHull2d::COLLINEAR_EPSILON = 1e-06;
}
//----------------------------------------------------------------------------
