// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlFrustum3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Frustum3<Real>::Frustum3 ()
    :
    m_kOrigin(Vector3<Real>::ZERO),
    m_kLVector(Vector3<Real>::UNIT_X),
    m_kUVector(Vector3<Real>::UNIT_Y),
    m_kDVector(Vector3<Real>::UNIT_Z)
{
    m_fLBound = (Real)1.0;
    m_fUBound = (Real)1.0;
    m_fDMin = (Real)1.0;
    m_fDMax = (Real)2.0;

    Update();
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real>& Frustum3<Real>::Origin ()
{
    return m_kOrigin;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector3<Real>& Frustum3<Real>::Origin () const
{
    return m_kOrigin;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real>& Frustum3<Real>::LVector ()
{
    return m_kLVector;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector3<Real>& Frustum3<Real>::LVector () const
{
    return m_kLVector;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real>& Frustum3<Real>::UVector ()
{
    return m_kUVector;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector3<Real>& Frustum3<Real>::UVector () const
{
    return m_kUVector;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real>& Frustum3<Real>::DVector ()
{
    return m_kDVector;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector3<Real>& Frustum3<Real>::DVector () const
{
    return m_kDVector;
}
//----------------------------------------------------------------------------
template <class Real>
Real& Frustum3<Real>::LBound ()
{
    return m_fLBound;
}
//----------------------------------------------------------------------------
template <class Real>
const Real& Frustum3<Real>::LBound () const
{
    return m_fLBound;
}
//----------------------------------------------------------------------------
template <class Real>
Real& Frustum3<Real>::UBound ()
{
    return m_fUBound;
}
//----------------------------------------------------------------------------
template <class Real>
const Real& Frustum3<Real>::UBound () const
{
    return m_fUBound;
}
//----------------------------------------------------------------------------
template <class Real>
Real& Frustum3<Real>::DMin ()
{
    return m_fDMin;
}
//----------------------------------------------------------------------------
template <class Real>
const Real& Frustum3<Real>::DMin () const
{
    return m_fDMin;
}
//----------------------------------------------------------------------------
template <class Real>
Real& Frustum3<Real>::DMax ()
{
    return m_fDMax;
}
//----------------------------------------------------------------------------
template <class Real>
const Real& Frustum3<Real>::DMax () const
{
    return m_fDMax;
}
//----------------------------------------------------------------------------
template <class Real>
Real Frustum3<Real>::GetDRatio () const
{
    return m_fDRatio;
}
//----------------------------------------------------------------------------
template <class Real>
Real Frustum3<Real>::GetMTwoLF () const
{
    return ((Real)-2.0)*m_fLBound*m_fDMax;
}
//----------------------------------------------------------------------------
template <class Real>
Real Frustum3<Real>::GetMTwoUF () const
{
    return ((Real)-2.0)*m_fUBound*m_fDMax;
}
//----------------------------------------------------------------------------
template <class Real>
void Frustum3<Real>::ComputeVertices (Vector3<Real> akVertex[8]) const
{
    Vector3<Real> kDScaled = m_fDMin*m_kDVector;
    Vector3<Real> kLScaled = m_fLBound*m_kLVector;
    Vector3<Real> kUScaled = m_fUBound*m_kUVector;

    akVertex[0] = kDScaled - kLScaled - kUScaled;
    akVertex[1] = kDScaled - kLScaled + kUScaled;
    akVertex[2] = kDScaled + kLScaled + kUScaled;
    akVertex[3] = kDScaled + kLScaled - kUScaled;

    for (int i = 0, ip = 4; i < 4; i++, ip++)
    {
        akVertex[ip] = m_kOrigin + m_fDRatio*akVertex[i];
        akVertex[i] += m_kOrigin;
    }
}
//----------------------------------------------------------------------------
template <class Real>
void Frustum3<Real>::Update ()
{
    m_fDRatio = m_fDMax/m_fDMin;
    m_fMTwoLF = ((Real)-2.0)*m_fLBound*m_fDMax;
    m_fMTwoUF = ((Real)-2.0)*m_fUBound*m_fDMax;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM Frustum3<float>;
template class WML_ITEM Frustum3<double>;
}
//----------------------------------------------------------------------------
