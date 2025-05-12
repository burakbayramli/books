// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlRectangle3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Rectangle3<Real>::Rectangle3 ()
    :
    m_kOrigin(Vector3<Real>::ZERO),
    m_kEdge0(Vector3<Real>::ZERO),
    m_kEdge1(Vector3<Real>::ZERO)
{
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real>& Rectangle3<Real>::Origin ()
{
    return m_kOrigin;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector3<Real>& Rectangle3<Real>::Origin () const
{
    return m_kOrigin;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real>& Rectangle3<Real>::Edge0 ()
{
    return m_kEdge0;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector3<Real>& Rectangle3<Real>::Edge0 () const
{
    return m_kEdge0;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real>& Rectangle3<Real>::Edge1 ()
{
    return m_kEdge1;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector3<Real>& Rectangle3<Real>::Edge1 () const
{
    return m_kEdge1;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM Rectangle3<float>;
template class WML_ITEM Rectangle3<double>;
}
//----------------------------------------------------------------------------
