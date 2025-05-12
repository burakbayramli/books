// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlEllipsoid3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
EllipsoidStandard3<Real>::EllipsoidStandard3 ()
{
    // no initialization for efficiency
}
//----------------------------------------------------------------------------
template <class Real>
Real& EllipsoidStandard3<Real>::Extent (int i)
{
    return m_afExtent[i];
}
//----------------------------------------------------------------------------
template <class Real>
const Real& EllipsoidStandard3<Real>::Extent (int i) const
{
    return m_afExtent[i];
}
//----------------------------------------------------------------------------
template <class Real>
Real* EllipsoidStandard3<Real>::Extents ()
{
    return m_afExtent;
}
//----------------------------------------------------------------------------
template <class Real>
const Real* EllipsoidStandard3<Real>::Extents () const
{
    return m_afExtent;
}
//----------------------------------------------------------------------------
template <class Real>
Ellipsoid3<Real>::Ellipsoid3 ()
{
    // no initialization for efficiency
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real>& Ellipsoid3<Real>::Center ()
{
    return m_kCenter;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector3<Real>& Ellipsoid3<Real>::Center () const
{
    return m_kCenter;
}
//----------------------------------------------------------------------------
template <class Real>
Matrix3<Real>& Ellipsoid3<Real>::A ()
{
    return m_kA;
}
//----------------------------------------------------------------------------
template <class Real>
const Matrix3<Real>& Ellipsoid3<Real>::A () const
{
    return m_kA;
}
//----------------------------------------------------------------------------
template <class Real>
Matrix3<Real>& Ellipsoid3<Real>::InverseA ()
{
    return m_kInverseA;
}
//----------------------------------------------------------------------------
template <class Real>
const Matrix3<Real>& Ellipsoid3<Real>::InverseA () const
{
    return m_kInverseA;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM class EllipsoidStandard3<float>;
template WML_ITEM class EllipsoidStandard3<double>;
template WML_ITEM class Ellipsoid3<float>;
template WML_ITEM class Ellipsoid3<double>;
template WML_ITEM class EllipsoidGeneral3<float>;
template WML_ITEM class EllipsoidGeneral3<double>;
}
//----------------------------------------------------------------------------
