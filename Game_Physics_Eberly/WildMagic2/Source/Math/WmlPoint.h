// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLPOINT_H
#define WMLPOINT_H

#include "WmlVector.h"

namespace Wml
{

template <int N, class Real>
class WML_ITEM Point
{
public:
    // construction
    Point ();
    Point (const Real* afTuple);
    Point (const Point& rkP);

    // coordinate access
    operator const Real* () const;
    operator Real* ();
    Real operator[] (int i) const;
    Real& operator[] (int i);

    // assignment
    Point& operator= (const Point& rkP);

    // comparison
    bool operator== (const Point& rkP) const;
    bool operator!= (const Point& rkP) const;
    bool operator<  (const Point& rkP) const;
    bool operator<= (const Point& rkP) const;
    bool operator>  (const Point& rkP) const;
    bool operator>= (const Point& rkP) const;

    // arithmetic operations
    Point operator+ (const Vector<N,Real>& rkV) const;
    Vector<N,Real> operator- (const Point& rkP) const;

    // result = (1-t)*this + t*P
    Point AffineSum (Real fT, const Point& rkP) const;

protected:
    // support for comparisons
    int CompareArrays (const Point& rkP) const;

    Real m_afTuple[N];
};

}

#endif
