// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlSegment2.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Segment2<Real>::Segment2 ()
    :
    m_kOrigin(Vector2<Real>::ZERO),
    m_kDirection(Vector2<Real>::ZERO)
{
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real>& Segment2<Real>::Origin ()
{
    return m_kOrigin;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector2<Real>& Segment2<Real>::Origin () const
{
    return m_kOrigin;
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real>& Segment2<Real>::Direction ()
{
    return m_kDirection;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector2<Real>& Segment2<Real>::Direction () const
{
    return m_kDirection;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM Segment2<float>;
template class WML_ITEM Segment2<double>;
}
//----------------------------------------------------------------------------
