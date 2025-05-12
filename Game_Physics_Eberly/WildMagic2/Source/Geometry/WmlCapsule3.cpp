// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlCapsule3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Capsule3<Real>::Capsule3 ()
{
    m_fRadius = (Real)0.0;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real>& Capsule3<Real>::Origin ()
{
    return m_kSegment.Origin();
}
//----------------------------------------------------------------------------
template <class Real>
const Vector3<Real>& Capsule3<Real>::Origin () const
{
    return m_kSegment.Origin();
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real>& Capsule3<Real>::Direction ()
{
    return m_kSegment.Direction();
}
//----------------------------------------------------------------------------
template <class Real>
const Vector3<Real>& Capsule3<Real>::Direction () const
{
    return m_kSegment.Direction();
}
//----------------------------------------------------------------------------
template <class Real>
Real& Capsule3<Real>::Radius ()
{
    return m_fRadius;
}
//----------------------------------------------------------------------------
template <class Real>
const Real& Capsule3<Real>::Radius () const
{
    return m_fRadius;
}
//----------------------------------------------------------------------------
template <class Real>
Segment3<Real>& Capsule3<Real>::Segment ()
{
    return m_kSegment;
}
//----------------------------------------------------------------------------
template <class Real>
const Segment3<Real>& Capsule3<Real>::Segment () const
{
    return m_kSegment;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM class Capsule3<float>;
template WML_ITEM class Capsule3<double>;
}
//----------------------------------------------------------------------------
