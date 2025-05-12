// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlArc2.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Arc2<Real>::Arc2 ()
    :
    m_kEnd0(Vector2<Real>::ZERO),
    m_kEnd1(Vector2<Real>::ZERO)
{
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real>& Arc2<Real>::End0 ()
{
    return m_kEnd0;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector2<Real>& Arc2<Real>::End0 () const
{
    return m_kEnd0;
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real>& Arc2<Real>::End1 ()
{
    return m_kEnd1;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector2<Real>& Arc2<Real>::End1 () const
{
    return m_kEnd1;
}
//----------------------------------------------------------------------------
template <class Real>
bool Arc2<Real>::Contains (const Vector2<Real>& rkP) const
{
    // Assert: |P-C| = R where P is the input point, C is the circle center,
    // and R is the circle radius.  For P to be on the arc from A to B, it
    // must be on the side of the plane containing A with normal N = Perp(B-A)
    // where Perp(u,v) = (v,-u).

    Vector2<Real> kPmE0 = rkP - m_kEnd0;
    Vector2<Real> kE1mE0 = m_kEnd1 - m_kEnd0;
    Real fKross = kPmE0.Kross(kE1mE0);
    return fKross >= (Real)0.0;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM Arc2<float>;
template class WML_ITEM Arc2<double>;
}
//----------------------------------------------------------------------------
