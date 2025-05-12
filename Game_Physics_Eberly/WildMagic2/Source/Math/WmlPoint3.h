// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLPOINT3_H
#define WMLPOINT3_H

#include "WmlPoint.h"

namespace Wml
{

template <class Real>
class WML_ITEM Point3 : public Point<3,Real>
{
public:
    // construction
    Point3 ();
    Point3 (Real fX, Real fY, Real fZ);
    Point3 (const Point3& rkP);
    Point3 (const Point<3,Real>& rkP);

    // member access
    Real X () const;
    Real& X ();
    Real Y () const;
    Real& Y ();
    Real Z () const;
    Real& Z ();

    // special point
    static const Point3 ZERO;
};

typedef Point3<float> Point3f;
typedef Point3<double> Point3d;

}

#endif
