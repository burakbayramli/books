// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlCircle2.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Circle2<Real>::Circle2 ()
    :
    m_kCenter(Vector2<Real>::ZERO)
{
    m_fRadius = (Real)0.0;
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real>& Circle2<Real>::Center ()
{
    return m_kCenter;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector2<Real>& Circle2<Real>::Center () const
{
    return m_kCenter;
}
//----------------------------------------------------------------------------
template <class Real>
Real& Circle2<Real>::Radius ()
{
    return m_fRadius;
}
//----------------------------------------------------------------------------
template <class Real>
const Real& Circle2<Real>::Radius () const
{
    return m_fRadius;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM Circle2<float>;
template class WML_ITEM Circle2<double>;
}
//----------------------------------------------------------------------------
