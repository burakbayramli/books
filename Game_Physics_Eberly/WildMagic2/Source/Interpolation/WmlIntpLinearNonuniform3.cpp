// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntpLinearNonuniform3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
IntpLinearNonuniform3<Real>::IntpLinearNonuniform3 (int iVertexQuantity,
    Vector3<Real>* akVertex, Real* afF)
    :
    Delaunay3<Real>(iVertexQuantity,akVertex)
{
    assert( afF );
    m_afF = afF;
}
//----------------------------------------------------------------------------
template <class Real>
IntpLinearNonuniform3<Real>::IntpLinearNonuniform3 (Delaunay3<Real>& rkNet,
    Real* afF)
    :
    Delaunay3<Real>(rkNet)
{
    assert( afF );
    m_afF = afF;
}
//----------------------------------------------------------------------------
template <class Real>
IntpLinearNonuniform3<Real>::~IntpLinearNonuniform3 ()
{
    delete[] m_afF;
}
//----------------------------------------------------------------------------
template <class Real>
bool IntpLinearNonuniform3<Real>::Evaluate (const Vector3<Real>& rkPoint,
    Real& rfF)
{
    // Determine which triangle contains the target point.
    //
    // TO DO.  This is an easy-to-implement, but slow search.  Better is to
    // start with a single tetrahedron.  If the point is inside, evaluate and
    // return.  If the point is outside, the next tetrahedron to visit is one
    // adjacent to the first.  The adjacent one you select is the one that is
    // intersected by the line segment whose end points are the centroid of
    // the current tetrahedron and the target point.  You effectively follow
    // a linear path through the tetrahedra to get to the target point.
    Vector3<Real> kV0, kV1, kV2, kV3;
    typename Delaunay3<Real>::Tetrahedron* pkTetra;
    Real afNumer[4], fDenom;
    int i;
    for (i = 0; i < m_iTetrahedronQuantity; i++)
    {
        pkTetra = &m_akTetrahedron[i];
        kV0 = m_akVertex[pkTetra->m_aiVertex[0]];
        kV1 = m_akVertex[pkTetra->m_aiVertex[1]];
        kV2 = m_akVertex[pkTetra->m_aiVertex[2]];
        kV3 = m_akVertex[pkTetra->m_aiVertex[3]];
        ComputeBarycenter(kV0,kV1,kV2,kV3,rkPoint,afNumer,fDenom);
        if ( InTetrahedron(afNumer,fDenom) )
            break;
    }

    if ( i == m_iTetrahedronQuantity )
    {
        // point is outside interpolation region
        return false;
    }

    // compute barycentric combination of function values at vertices
    rfF = (afNumer[0]*m_afF[pkTetra->m_aiVertex[0]] +
        afNumer[1]*m_afF[pkTetra->m_aiVertex[1]] +
        afNumer[2]*m_afF[pkTetra->m_aiVertex[2]] +
        afNumer[3]*m_afF[pkTetra->m_aiVertex[3]])/fDenom;

    return true;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM IntpLinearNonuniform3<float>;
template class WML_ITEM IntpLinearNonuniform3<double>;
}
//----------------------------------------------------------------------------
