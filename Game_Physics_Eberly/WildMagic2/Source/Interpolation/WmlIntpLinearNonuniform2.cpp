// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntpLinearNonuniform2.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
IntpLinearNonuniform2<Real>::IntpLinearNonuniform2 (int iVertexQuantity,
    Vector2<Real>* akVertex, Real* afF)
    :
    Delaunay2<Real>(iVertexQuantity,akVertex)
{
    assert( afF );
    m_afF = afF;
}
//----------------------------------------------------------------------------
template <class Real>
IntpLinearNonuniform2<Real>::IntpLinearNonuniform2 (Delaunay2<Real>& rkNet,
    Real* afF)
    :
    Delaunay2<Real>(rkNet)
{
    assert( afF );
    m_afF = afF;
}
//----------------------------------------------------------------------------
template <class Real>
IntpLinearNonuniform2<Real>::~IntpLinearNonuniform2 ()
{
    delete[] m_afF;
}
//----------------------------------------------------------------------------
template <class Real>
bool IntpLinearNonuniform2<Real>::Evaluate (const Vector2<Real>& rkPoint,
    Real& rfF)
{
    // determine which triangle contains the target point
    Vector2<Real> kV0, kV1, kV2;
    int i;
    for (i = 0; i < m_iTriangleQuantity; i++)
    {
        typename Delaunay2<Real>::Triangle& rkTri = m_akTriangle[i];
        kV0 = m_akVertex[rkTri.m_aiVertex[0]];
        kV1 = m_akVertex[rkTri.m_aiVertex[1]];
        kV2 = m_akVertex[rkTri.m_aiVertex[2]];
        if ( InTriangle(kV0,kV1,kV2,rkPoint) )
            break;
    }

    if ( i == m_iTriangleQuantity )
    {
        // point is outside interpolation region
        return false;
    }

    // the input point is in this triangle
    typename Delaunay2<Real>::Triangle& rkTri = m_akTriangle[i];

    // compute barycentric coordinates with respect to subtriangle
    Real afBary[3];
    ComputeBarycenter(kV0,kV1,kV2,rkPoint,afBary);

    // compute barycentric combination of function values at vertices
    rfF = afBary[0]*m_afF[rkTri.m_aiVertex[0]] +
        afBary[1]*m_afF[rkTri.m_aiVertex[1]] +
        afBary[2]*m_afF[rkTri.m_aiVertex[2]];

    return true;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM IntpLinearNonuniform2<float>;
template class WML_ITEM IntpLinearNonuniform2<double>;
}
//----------------------------------------------------------------------------
