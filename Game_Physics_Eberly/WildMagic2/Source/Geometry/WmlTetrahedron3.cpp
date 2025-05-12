// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlTetrahedron3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Tetrahedron3<Real>::Tetrahedron3 ()
{
    // no initialization of vertices
}
//----------------------------------------------------------------------------
template <class Real>
Tetrahedron3<Real>::Tetrahedron3 (const Vector3<Real>& rkV0,
    const Vector3<Real>& rkV1, const Vector3<Real>& rkV2,
    const Vector3<Real>& rkV3)
{
    m_akVertex[0] = rkV0;
    m_akVertex[1] = rkV1;
    m_akVertex[2] = rkV2;
    m_akVertex[3] = rkV3;
}
//----------------------------------------------------------------------------
template <class Real>
Tetrahedron3<Real>::Tetrahedron3 (const Vector3<Real> akV[4])
{
    memcpy(m_akVertex,akV,4*sizeof(Vector3<Real>));
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real>& Tetrahedron3<Real>::operator[] (int i)
{
    assert( 0 <= i && i <= 3 );
    return m_akVertex[i];
}
//----------------------------------------------------------------------------
template <class Real>
const Vector3<Real>& Tetrahedron3<Real>::operator[] (int i) const
{
    assert( 0 <= i && i <= 3 );
    return m_akVertex[i];
}
//----------------------------------------------------------------------------
template <class Real>
void Tetrahedron3<Real>::GetPlanes (Plane3<Real> akPlane[4]) const
{
    Vector3<Real> kEdge10 = m_akVertex[1] - m_akVertex[0];
    Vector3<Real> kEdge20 = m_akVertex[2] - m_akVertex[0];
    Vector3<Real> kEdge30 = m_akVertex[3] - m_akVertex[0];
    Vector3<Real> kEdge21 = m_akVertex[2] - m_akVertex[1];
    Vector3<Real> kEdge31 = m_akVertex[3] - m_akVertex[1];

    akPlane[0].SetNormal(kEdge20.Cross(kEdge10));  // <v0,v2,v1>
    akPlane[1].SetNormal(kEdge10.Cross(kEdge30));  // <v0,v1,v3>
    akPlane[2].SetNormal(kEdge30.Cross(kEdge20));  // <v0,v3,v2>
    akPlane[3].SetNormal(kEdge21.Cross(kEdge31));  // <v1,v2,v3>

    Real fDet = kEdge10.Dot(akPlane[3].GetNormal());
    int i;
    if ( fDet < (Real)0.0 )
    {
        // normals are inner pointing, reverse their directions
        for (i = 0; i < 4; i++)
            akPlane[i].SetNormal(-akPlane[i].GetNormal());
    }

    for (i = 0; i < 4; i++)
        akPlane[i].SetConstant(m_akVertex[i].Dot(akPlane[i].GetNormal()));
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM Tetrahedron3<float>;
template class WML_ITEM Tetrahedron3<double>;
}
//----------------------------------------------------------------------------
