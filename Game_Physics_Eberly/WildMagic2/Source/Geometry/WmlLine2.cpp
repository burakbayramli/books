// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlLine2.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Line2<Real>::Line2 ()
    :
    m_kOrigin(Vector2<Real>::ZERO),
    m_kDirection(Vector2<Real>::ZERO),
    m_kNormal(Vector2<Real>::ZERO)
{
    m_fConstant = (Real)0.0;
}
//----------------------------------------------------------------------------
template <class Real>
Line2<Real>::Line2 (const Vector2<Real>& rkV0, const Vector2<Real>& rkV1,
    bool bParametricForm)
{
    Vector2<Real> kDiff = rkV1 - rkV0;

    if ( bParametricForm )
    {
        m_kOrigin = rkV0;
        m_kDirection = kDiff;
    }
    else
    {
        m_kNormal = kDiff.Perp();
        m_fConstant = m_kNormal.Dot(rkV0);
    }
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real>& Line2<Real>::Origin ()
{
    return m_kOrigin;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector2<Real>& Line2<Real>::Origin () const
{
    return m_kOrigin;
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real>& Line2<Real>::Direction ()
{
    return m_kDirection;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector2<Real>& Line2<Real>::Direction () const
{
    return m_kDirection;
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real>& Line2<Real>::Normal ()
{
    return m_kNormal;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector2<Real>& Line2<Real>::Normal () const
{
    return m_kNormal;
}
//----------------------------------------------------------------------------
template <class Real>
Real& Line2<Real>::Constant ()
{
    return m_fConstant;
}
//----------------------------------------------------------------------------
template <class Real>
const Real& Line2<Real>::Constant () const
{
    return m_fConstant;
}
//----------------------------------------------------------------------------
template <class Real>
Real Line2<Real>::GetPseudodistance (const Vector2<Real>& rkV) const
{
    return m_kNormal.Dot(rkV) - m_fConstant;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM Line2<float>;
template class WML_ITEM Line2<double>;
}
//----------------------------------------------------------------------------
