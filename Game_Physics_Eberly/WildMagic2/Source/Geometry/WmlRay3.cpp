// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlRay3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Ray3<Real>::Ray3 ()
    :
    m_kOrigin(Vector3<Real>::ZERO),
    m_kDirection(Vector3<Real>::ZERO)
{
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real>& Ray3<Real>::Origin ()
{
    return m_kOrigin;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector3<Real>& Ray3<Real>::Origin () const
{
    return m_kOrigin;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real>& Ray3<Real>::Direction ()
{
    return m_kDirection;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector3<Real>& Ray3<Real>::Direction () const
{
    return m_kDirection;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM Ray3<float>;
template class WML_ITEM Ray3<double>;
}
//----------------------------------------------------------------------------
