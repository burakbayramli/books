// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLAXISALIGNEDBOX2_H
#define WMLAXISALIGNEDBOX2_H

#include "WmlSystem.h"

namespace Wml
{

template <class Real>
class WML_ITEM AxisAlignedBox2
{
public:
    // Construction.  The default constructor assigns 0 to all members.  In
    // the other constructor, the user must ensure that fXMin <= fXMax and
    // fYMin <= fYMax.  The class will not check for validity of the input.
    AxisAlignedBox2 ();
    AxisAlignedBox2 (Real fXMin, Real fXMax, Real fYMin, Real fYMax);

    // member access
    Real GetXMin () const;
    Real& XMin ();
    Real GetXMax () const;
    Real& XMax ();
    Real GetYMin () const;
    Real& YMin ();
    Real GetYMax () const;
    Real& YMax ();

    // Overlap testing is in the strict sense.  If the two boxes are just
    // touching along a common edge, the boxes are reported as overlapping.
    bool HasXOverlap (const AxisAlignedBox2& rkBox) const;
    bool HasYOverlap (const AxisAlignedBox2& rkBox) const;
    bool TestIntersection (const AxisAlignedBox2& rkBox) const;

    // The return value is 'true' if there is overlap.  In this case the
    // intersection is stored in rkIntr.  If the return value is 'false',
    // if there is no overlap.  In this case rkIntr is undefined.
    bool FindIntersection (const AxisAlignedBox2& rkBox,
        AxisAlignedBox2& rkIntr) const;

protected:
    Real m_afMin[2], m_afMax[2];
};

typedef AxisAlignedBox2<float> AxisAlignedBox2f;
typedef AxisAlignedBox2<double> AxisAlignedBox2d;

}

#endif
