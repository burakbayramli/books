// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlEllipse2.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
EllipseStandard2<Real>::EllipseStandard2 ()
{
    // no initialization for efficiency
}
//----------------------------------------------------------------------------
template <class Real>
Real& EllipseStandard2<Real>::Extent (int i)
{
    return m_afExtent[i];
}
//----------------------------------------------------------------------------
template <class Real>
const Real& EllipseStandard2<Real>::Extent (int i) const
{
    return m_afExtent[i];
}
//----------------------------------------------------------------------------
template <class Real>
Real* EllipseStandard2<Real>::Extents ()
{
    return m_afExtent;
}
//----------------------------------------------------------------------------
template <class Real>
const Real* EllipseStandard2<Real>::Extents () const
{
    return m_afExtent;
}
//----------------------------------------------------------------------------
template <class Real>
Ellipse2<Real>::Ellipse2 ()
{
    // no initialization for efficiency
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real>& Ellipse2<Real>::Center ()
{
    return m_kCenter;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector2<Real>& Ellipse2<Real>::Center () const
{
    return m_kCenter;
}
//----------------------------------------------------------------------------
template <class Real>
Matrix2<Real>& Ellipse2<Real>::A ()
{
    return m_kA;
}
//----------------------------------------------------------------------------
template <class Real>
const Matrix2<Real>& Ellipse2<Real>::A () const
{
    return m_kA;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM class EllipseStandard2<float>;
template WML_ITEM class EllipseStandard2<double>;
template WML_ITEM class Ellipse2<float>;
template WML_ITEM class Ellipse2<double>;
template WML_ITEM class EllipseGeneral2<float>;
template WML_ITEM class EllipseGeneral2<double>;
}
//----------------------------------------------------------------------------
