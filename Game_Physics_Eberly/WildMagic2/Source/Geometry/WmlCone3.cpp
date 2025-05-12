// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlCone3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Cone3<Real>::Cone3 ()
    :
    m_kVertex(Vector3<Real>::ZERO),
    m_kAxis(Vector3<Real>::ZERO)
{
    m_fCos = (Real)0.0;
    m_fSin = (Real)0.0;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real>& Cone3<Real>::Vertex ()
{
    return m_kVertex;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector3<Real>& Cone3<Real>::Vertex () const
{
    return m_kVertex;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real>& Cone3<Real>::Axis ()
{
    return m_kAxis;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector3<Real>& Cone3<Real>::Axis () const
{
    return m_kAxis;
}
//----------------------------------------------------------------------------
template <class Real>
Real& Cone3<Real>::CosAngle ()
{
    return m_fCos;
}
//----------------------------------------------------------------------------
template <class Real>
const Real& Cone3<Real>::CosAngle () const
{
    return m_fCos;
}
//----------------------------------------------------------------------------
template <class Real>
Real& Cone3<Real>::SinAngle ()
{
    return m_fSin;
}
//----------------------------------------------------------------------------
template <class Real>
const Real& Cone3<Real>::SinAngle () const
{
    return m_fSin;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM Cone3<float>;
template class WML_ITEM Cone3<double>;
}
//----------------------------------------------------------------------------
