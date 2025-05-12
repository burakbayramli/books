// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlLozenge3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Lozenge3<Real>::Lozenge3 ()
{
    m_fRadius = (Real)0.0;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real>& Lozenge3<Real>::Origin ()
{
    return m_kRectangle.Origin();
}
//----------------------------------------------------------------------------
template <class Real>
const Vector3<Real>& Lozenge3<Real>::Origin () const
{
    return m_kRectangle.Origin();
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real>& Lozenge3<Real>::Edge0 ()
{
    return m_kRectangle.Edge0();
}
//----------------------------------------------------------------------------
template <class Real>
const Vector3<Real>& Lozenge3<Real>::Edge0 () const
{
    return m_kRectangle.Edge0();
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real>& Lozenge3<Real>::Edge1 ()
{
    return m_kRectangle.Edge1();
}
//----------------------------------------------------------------------------
template <class Real>
const Vector3<Real>& Lozenge3<Real>::Edge1 () const
{
    return m_kRectangle.Edge1();
}
//----------------------------------------------------------------------------
template <class Real>
Real& Lozenge3<Real>::Radius ()
{
    return m_fRadius;
}
//----------------------------------------------------------------------------
template <class Real>
const Real& Lozenge3<Real>::Radius () const
{
    return m_fRadius;
}
//----------------------------------------------------------------------------
template <class Real>
Rectangle3<Real>& Lozenge3<Real>::Rectangle ()
{
    return m_kRectangle;
}
//----------------------------------------------------------------------------
template <class Real>
const Rectangle3<Real>& Lozenge3<Real>::Rectangle () const
{
    return m_kRectangle;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM Lozenge3<float>;
template class WML_ITEM Lozenge3<double>;
}
//----------------------------------------------------------------------------
