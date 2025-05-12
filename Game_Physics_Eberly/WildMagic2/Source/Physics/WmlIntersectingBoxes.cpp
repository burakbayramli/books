// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#include "WmlIntersectingBoxes.h"
using namespace Wml;

#include <algorithm>
using namespace std;

//----------------------------------------------------------------------------
template <class Real>
IntersectingBoxes<Real>::IntersectingBoxes (
    vector<AxisAlignedBox3<Real> >& rkBoxes)
    :
    m_rkBoxes(rkBoxes)
{
    Initialize();
}
//----------------------------------------------------------------------------
template <class Real>
IntersectingBoxes<Real>::~IntersectingBoxes ()
{
}
//----------------------------------------------------------------------------
template <class Real>
void IntersectingBoxes<Real>::Initialize ()
{
    // get the rectangle end points
    int iISize = (int)m_rkBoxes.size(), iESize = 2*iISize;
    m_kXEndPoint.resize(iESize);
    m_kYEndPoint.resize(iESize);
    m_kZEndPoint.resize(iESize);
    int i, j;
    for (i = 0, j = 0; i < iISize; i++)
    {
        m_kXEndPoint[j].Type = 0;
        m_kXEndPoint[j].Value = m_rkBoxes[i].GetXMin();
        m_kXEndPoint[j].Index = i;
        m_kYEndPoint[j].Type = 0;
        m_kYEndPoint[j].Value = m_rkBoxes[i].GetYMin();
        m_kYEndPoint[j].Index = i;
        m_kZEndPoint[j].Type = 0;
        m_kZEndPoint[j].Value = m_rkBoxes[i].GetZMin();
        m_kZEndPoint[j].Index = i;
        j++;

        m_kXEndPoint[j].Type = 1;
        m_kXEndPoint[j].Value = m_rkBoxes[i].GetXMax();
        m_kXEndPoint[j].Index = i;
        m_kYEndPoint[j].Type = 1;
        m_kYEndPoint[j].Value = m_rkBoxes[i].GetYMax();
        m_kYEndPoint[j].Index = i;
        m_kZEndPoint[j].Type = 1;
        m_kZEndPoint[j].Value = m_rkBoxes[i].GetZMax();
        m_kZEndPoint[j].Index = i;
        j++;
    }

    // sort the rectangle end points
    sort(m_kXEndPoint.begin(),m_kXEndPoint.end());
    sort(m_kYEndPoint.begin(),m_kYEndPoint.end());
    sort(m_kZEndPoint.begin(),m_kZEndPoint.end());

    // create the interval-to-endpoint lookup tables
    m_kXLookup.resize(iESize);
    m_kYLookup.resize(iESize);
    m_kZLookup.resize(iESize);
    for (j = 0; j < iESize; j++)
    {
        m_kXLookup[2*m_kXEndPoint[j].Index + m_kXEndPoint[j].Type] = j;
        m_kYLookup[2*m_kYEndPoint[j].Index + m_kYEndPoint[j].Type] = j;
        m_kZLookup[2*m_kZEndPoint[j].Index + m_kZEndPoint[j].Type] = j;
    }

    // active set of rectangles (stored by index in array)
    set<int> kActive;

    // set of overlapping rectangles (stored by pairs of indices in array)
    m_kOverlap.clear();

    // sweep through the end points to determine overlapping x-intervals
    for (i = 0; i < iESize; i++)
    {
        EndPoint& rkEnd = m_kXEndPoint[i];
        int iIndex = rkEnd.Index;
        if ( rkEnd.Type == 0 )  // an interval 'begin' value
        {
            // In the 1D problem, the current interval overlaps with all the
            // active intervals.  In 3D this we also need to check for
            // y-overlap and z-overlap.
            set<int>::iterator pkIter = kActive.begin();
            for (/**/; pkIter != kActive.end(); pkIter++)
            {
                // Rectangles iAIndex and iIndex overlap in the x-dimension.
                // Test for overlap in the y-dimension and z-dimension.
                int iAIndex = *pkIter;
                const AxisAlignedBox3<Real>& rkB0 = m_rkBoxes[iAIndex];
                const AxisAlignedBox3<Real>& rkB1 = m_rkBoxes[iIndex];
                if ( rkB0.HasYOverlap(rkB1) && rkB0.HasZOverlap(rkB1) )
                {
                    if ( iAIndex < iIndex )
                        m_kOverlap.insert(make_pair(iAIndex,iIndex));
                    else
                        m_kOverlap.insert(make_pair(iIndex,iAIndex));
                }
            }
            kActive.insert(iIndex);
        }
        else  // an interval 'end' value
        {
            kActive.erase(iIndex);
        }
    }
}
//----------------------------------------------------------------------------
template <class Real>
void IntersectingBoxes<Real>::SetBox (int i,
    const AxisAlignedBox3<Real>& rkBox)
{
    assert( 0 <= i && i < (int)m_rkBoxes.size() );
    m_rkBoxes[i] = rkBox;
    m_kXEndPoint[m_kXLookup[2*i]].Value = rkBox.GetXMin();
    m_kXEndPoint[m_kXLookup[2*i+1]].Value = rkBox.GetXMax();
    m_kYEndPoint[m_kYLookup[2*i]].Value = rkBox.GetYMin();
    m_kYEndPoint[m_kYLookup[2*i+1]].Value = rkBox.GetYMax();
    m_kZEndPoint[m_kZLookup[2*i]].Value = rkBox.GetZMin();
    m_kZEndPoint[m_kZLookup[2*i+1]].Value = rkBox.GetZMax();
}
//----------------------------------------------------------------------------
template <class Real>
void IntersectingBoxes<Real>::GetBox (int i, AxisAlignedBox3<Real>& rkBox)
    const
{
    assert( 0 <= i && i < (int)m_rkBoxes.size() );
    rkBox = m_rkBoxes[i];
}
//----------------------------------------------------------------------------
template <class Real>
void IntersectingBoxes<Real>::InsertionSort (vector<EndPoint>& rkEndPoint,
    vector<int>& rkLookup)
{
    // Apply an insertion sort.  Under the assumption that the rectangles
    // have not changed much since the last call, the end points are nearly
    // sorted.  The insertion sort should be very fast in this case.

    int iESize = (int)rkEndPoint.size();
    for (int j = 1; j < iESize; j++)
    {
        EndPoint kKey = rkEndPoint[j];
        int i = j - 1;
        while ( i >= 0 && kKey < rkEndPoint[i] )
        {
            EndPoint kE0 = rkEndPoint[i];
            EndPoint kE1 = rkEndPoint[i+1];

            // update the overlap status
            if ( kE0.Type == 0 )
            {
                if ( kE1.Type == 1 )
                {
                    // The 'b' of interval E0.Index was smaller than the 'e'
                    // of interval E1.Index, and the intervals *might have
                    // been* overlapping.  Now 'b' and 'e' are swapped, and
                    // the intervals cannot overlap.  Remove the pair from
                    // the overlap set.  The removal operation needs to find
                    // the pair and erase it if it exists.  Finding the pair
                    // is the expensive part of the operation, so there is no
                    // real time savings in testing for existence first, then
                    // deleting if it does.
                    if ( kE0.Index < kE1.Index )
                        m_kOverlap.erase(make_pair(kE0.Index,kE1.Index));
                    else
                        m_kOverlap.erase(make_pair(kE1.Index,kE0.Index));
                }
            }
            else
            {
                if ( kE1.Type == 0 )
                {
                    // The 'b' of interval E0.index was larger than the 'e'
                    // of interval E1.index, and the intervals were not
                    // overlapping.  Now 'b' and 'e' are swapped, and the
                    // intervals *might be* overlapping.  We need to determine
                    // if this is so and insert only if they do overlap.
                    const AxisAlignedBox3<Real>& rkB0 = m_rkBoxes[kE0.Index];
                    const AxisAlignedBox3<Real>& rkB1 = m_rkBoxes[kE1.Index];
                    if ( rkB0.TestIntersection(rkB1) )
                    {
                        if ( kE0.Index < kE1.Index )
                            m_kOverlap.insert(make_pair(kE0.Index,kE1.Index));
                        else
                            m_kOverlap.insert(make_pair(kE1.Index,kE0.Index));
                    }
                }
            }

            // reorder the items to maintain the sorted list
            rkEndPoint[i] = kE1;
            rkEndPoint[i+1] = kE0;
            rkLookup[2*kE1.Index + kE1.Type] = i;
            rkLookup[2*kE0.Index + kE0.Type] = i+1;
            i--;
        }
        rkEndPoint[i+1] = kKey;
        rkLookup[2*kKey.Index + kKey.Type] = i+1;
    }
}
//----------------------------------------------------------------------------
template <class Real>
void IntersectingBoxes<Real>::Update ()
{
    InsertionSort(m_kXEndPoint,m_kXLookup);
    InsertionSort(m_kYEndPoint,m_kYLookup);
    InsertionSort(m_kZEndPoint,m_kZLookup);
}
//----------------------------------------------------------------------------
template <class Real>
#ifdef WML_USING_VC6
// VC6 on the PC generates an error if typename is included.
const set<IntersectingBoxes<Real>::BoxPair>&
#else
// g++ 3.x on the Macintosh generates a warning if typename is not included.
const set<typename IntersectingBoxes<Real>::BoxPair>&
#endif
IntersectingBoxes<Real>::GetOverlap () const
{
    return m_kOverlap;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM IntersectingBoxes<float>;
template class WML_ITEM IntersectingBoxes<double>;
}
//----------------------------------------------------------------------------
