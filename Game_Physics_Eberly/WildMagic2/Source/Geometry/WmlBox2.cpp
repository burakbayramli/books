// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlBox2.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Box2<Real>::Box2 ()
{
    // no initialization for efficiency
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real>& Box2<Real>::Center ()
{
    return m_kCenter;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector2<Real>& Box2<Real>::Center () const
{
    return m_kCenter;
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real>& Box2<Real>::Axis (int i)
{
    assert( 0 <= i && i < 2 );
    return m_akAxis[i];
}
//----------------------------------------------------------------------------
template <class Real>
const Vector2<Real>& Box2<Real>::Axis (int i) const
{
    assert( 0 <= i && i < 2 );
    return m_akAxis[i];
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real>* Box2<Real>::Axes ()
{
    return m_akAxis;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector2<Real>* Box2<Real>::Axes () const
{
    return m_akAxis;
}
//----------------------------------------------------------------------------
template <class Real>
Real& Box2<Real>::Extent (int i)
{
    assert( 0 <= i && i < 2 );
    return m_afExtent[i];
}
//----------------------------------------------------------------------------
template <class Real>
const Real& Box2<Real>::Extent (int i) const
{
    assert( 0 <= i && i < 2 );
    return m_afExtent[i];
}
//----------------------------------------------------------------------------
template <class Real>
Real* Box2<Real>::Extents ()
{
    return m_afExtent;
}
//----------------------------------------------------------------------------
template <class Real>
const Real* Box2<Real>::Extents () const
{
    return m_afExtent;
}
//----------------------------------------------------------------------------
template <class Real>
void Box2<Real>::ComputeVertices (Vector2<Real> akVertex[4]) const
{
    Vector2<Real> akEAxis[2] =
    {
        m_afExtent[0]*m_akAxis[0],
        m_afExtent[1]*m_akAxis[1]
    };

    akVertex[0] = m_kCenter - akEAxis[0] - akEAxis[1];
    akVertex[1] = m_kCenter + akEAxis[0] - akEAxis[1];
    akVertex[2] = m_kCenter + akEAxis[0] + akEAxis[1];
    akVertex[3] = m_kCenter - akEAxis[0] + akEAxis[1];
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM class Box2<float>;
template WML_ITEM class Box2<double>;
}
//----------------------------------------------------------------------------
