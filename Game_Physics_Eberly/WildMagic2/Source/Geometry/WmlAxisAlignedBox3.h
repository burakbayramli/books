// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLAXISALIGNEDBOX3_H
#define WMLAXISALIGNEDBOX3_H

#include "WmlSystem.h"

namespace Wml
{

template <class Real>
class WML_ITEM AxisAlignedBox3
{
public:
    // Construction.  The default constructor assigns 0 to all members.  In
    // the other constructor, the user must ensure that fXMin <= fXMax,
    // fYMin <= fYMax, and fZMin <= fZMax.  The class will not check for
    // validity of the input.
    AxisAlignedBox3 ();
    AxisAlignedBox3 (Real fXMin, Real fXMax, Real fYMin, Real fYMax,
        Real fZMin, Real fZMax);

    // member access
    Real GetXMin () const;
    Real& XMin ();
    Real GetXMax () const;
    Real& XMax ();
    Real GetYMin () const;
    Real& YMin ();
    Real GetYMax () const;
    Real& YMax ();
    Real GetZMin () const;
    Real& ZMin ();
    Real GetZMax () const;
    Real& ZMax ();

    // Overlap testing is in the strict sense.  If the two boxes are just
    // touching along a common edge, the boxes are reported as overlapping.
    bool HasXOverlap (const AxisAlignedBox3& rkBox) const;
    bool HasYOverlap (const AxisAlignedBox3& rkBox) const;
    bool HasZOverlap (const AxisAlignedBox3& rkBox) const;
    bool TestIntersection (const AxisAlignedBox3& rkBox) const;

    // The return value is 'true' if there is overlap.  In this case the
    // intersection is stored in rkIntr.  If the return value is 'false',
    // if there is no overlap.  In this case rkIntr is undefined.
    bool FindIntersection (const AxisAlignedBox3& rkBox,
        AxisAlignedBox3& rkIntr) const;

protected:
    Real m_afMin[3], m_afMax[3];
};

typedef AxisAlignedBox3<float> AxisAlignedBox3f;
typedef AxisAlignedBox3<double> AxisAlignedBox3d;

}

#endif
