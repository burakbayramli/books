// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlCylinder3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Cylinder3<Real>::Cylinder3 ()
    :
    m_kCenter(Vector3<Real>::ZERO),
    m_kDirection(Vector3<Real>::ZERO),
    m_kU(Vector3<Real>::ZERO),
    m_kV(Vector3<Real>::ZERO)
{
    m_fHeight = (Real)0.0;
    m_fRadius = (Real)0.0;
    m_bCapped = false;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real>& Cylinder3<Real>::Center ()
{
    return m_kCenter;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector3<Real>& Cylinder3<Real>::Center () const
{
    return m_kCenter;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real>& Cylinder3<Real>::Direction ()
{
    return m_kDirection;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector3<Real>& Cylinder3<Real>::Direction () const
{
    return m_kDirection;
}
//----------------------------------------------------------------------------
template <class Real>
Real& Cylinder3<Real>::Height ()
{
    return m_fHeight;
}
//----------------------------------------------------------------------------
template <class Real>
Real Cylinder3<Real>::Height () const
{
    return m_fHeight;
}
//----------------------------------------------------------------------------
template <class Real>
Real& Cylinder3<Real>::Radius ()
{
    return m_fRadius;
}
//----------------------------------------------------------------------------
template <class Real>
Real Cylinder3<Real>::Radius () const
{
    return m_fRadius;
}
//----------------------------------------------------------------------------
template <class Real>
bool& Cylinder3<Real>::Capped ()
{
    return m_bCapped;
}
//----------------------------------------------------------------------------
template <class Real>
bool Cylinder3<Real>::Capped () const
{
    return m_bCapped;
}
//----------------------------------------------------------------------------
template <class Real>
Segment3<Real> Cylinder3<Real>::GetSegment () const
{
    Segment3<Real> kSegment;
    kSegment.Direction() = m_fHeight*m_kDirection;
    kSegment.Origin() = m_kCenter - ((Real)0.5)*kSegment.Direction();
    return kSegment;
}
//----------------------------------------------------------------------------
template <class Real>
void Cylinder3<Real>::GenerateCoordinateSystem ()
{
    Vector3<Real>::GenerateOrthonormalBasis(m_kU,m_kV,m_kDirection,true);
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real>& Cylinder3<Real>::U ()
{
    return m_kU;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector3<Real>& Cylinder3<Real>::U () const
{
    return m_kU;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real>& Cylinder3<Real>::V ()
{
    return m_kV;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector3<Real>& Cylinder3<Real>::V () const
{
    return m_kV;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real>& Cylinder3<Real>::W ()
{
    return m_kDirection;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector3<Real>& Cylinder3<Real>::W () const
{
    return m_kDirection;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM Cylinder3<float>;
template class WML_ITEM Cylinder3<double>;
}
//----------------------------------------------------------------------------
