// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#include "WmlIntersectingIntervals.h"
using namespace Wml;

#include <iostream>
using namespace std;

//----------------------------------------------------------------------------
void BookExample ()
{
    // The initial intervals that lead to an ordering as shown in Figure 5.29
    // of the Game Physics book.
    vector<pair<float,float> > kInterval(5);
    kInterval[0] = make_pair(2.0f,7.0f);
    kInterval[1] = make_pair(3.0f,13.0f);
    kInterval[2] = make_pair(1.0f,5.0f);
    kInterval[3] = make_pair(10.0f,14.0f);
    kInterval[4] = make_pair(8.0f,11.0f);

    // Compute all pairs of overlapping intervals.  In the implementation,
    // the intervals are labeled using zero-based indexing.  The cout
    // commands add one to each index to obtain the one-based indexing that
    // is used in the book.
    //
    // S = {(1,2), (1,3), (2,3), (2,4), (2,5), (4,5) }

    IntersectingIntervals<float> kIntr(kInterval);
    const set<pair<int,int> >& rkOverlap = kIntr.GetOverlap();
    set<pair<int,int> >::const_iterator pkIter;
    for (pkIter = rkOverlap.begin(); pkIter != rkOverlap.end(); pkIter++)
    {
        cout << "interval " << 1+pkIter->first << " overlaps with interval "
             << 1+pkIter->second << endl;
    }
    cout << endl;

    // Move intervals 1 and 4 (using the one-based book indexing) and
    // update the set of overlapping intervals.  The new ordering is now
    // of the form shown in Figure 5.30 of the Game Physics book.
    //
    // S = {(1,2), (1,3), (1,5), (2,3), (2,4), (2,5) }

    kIntr.SetInterval(0,3.5f,8.5f);
    kIntr.SetInterval(4,6.0f,9.0f);
    kIntr.Update();
    for (pkIter = rkOverlap.begin(); pkIter != rkOverlap.end(); pkIter++)
    {
        cout << "interval " << 1+pkIter->first << " overlaps with interval "
             << 1+pkIter->second << endl;
    }
    cout << endl;
}
//----------------------------------------------------------------------------
int main ()
{
    BookExample();
    return 0;
}
//----------------------------------------------------------------------------
