// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlTriangle2.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Triangle2<Real>::Triangle2 ()
{
    // no initialization for efficiency
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real>& Triangle2<Real>::Origin ()
{
    return m_kOrigin;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector2<Real>& Triangle2<Real>::Origin () const
{
    return m_kOrigin;
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real>& Triangle2<Real>::Edge0 ()
{
    return m_kEdge0;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector2<Real>& Triangle2<Real>::Edge0 () const
{
    return m_kEdge0;
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real>& Triangle2<Real>::Edge1 ()
{
    return m_kEdge1;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector2<Real>& Triangle2<Real>::Edge1 () const
{
    return m_kEdge1;
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real>& Triangle2<Real>::Vertex (int i)
{
    assert( 0 <= i && i < 3 );
    return m_akV[i];
}
//----------------------------------------------------------------------------
template <class Real>
const Vector2<Real>& Triangle2<Real>::Vertex (int i) const
{
    assert( 0 <= i && i < 3 );
    return m_akV[i];
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM Triangle2<float>;
template class WML_ITEM Triangle2<double>;
}
//----------------------------------------------------------------------------
