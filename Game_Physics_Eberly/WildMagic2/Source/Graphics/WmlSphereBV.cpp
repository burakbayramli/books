// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlApprLineFit3.h"
#include "WmlContSphere3.h"
#include "WmlIntrSph3Sph3.h"
#include "WmlMatrix3.h"
#include "WmlSphereBV.h"
using namespace Wml;

WmlImplementBV(SphereBV,BV_SPHERE);

//----------------------------------------------------------------------------
void SphereBV::CopyTo (BoundingVolume* pkTargetBV) const
{
    assert( GetType() == pkTargetBV->GetType() );
    ((SphereBV*)pkTargetBV)->m_kSphere = m_kSphere;
}
//----------------------------------------------------------------------------
void SphereBV::TransformTo (BoundingVolume* pkTargetBV,
    const Matrix3f& rkRot, const Vector3f& rkTrn, float fScale) const
{
    assert( GetType() == pkTargetBV->GetType() );
    Sphere3f& rkTarget = ((SphereBV*)pkTargetBV)->m_kSphere;

    rkTarget.Center() = fScale*(rkRot*m_kSphere.Center()) + rkTrn;
    rkTarget.Radius() = fScale*m_kSphere.Radius();
}
//----------------------------------------------------------------------------
bool SphereBV::Contains (const Vector3f& rkPoint, float fEpsilon) const
{
    return InSphere(rkPoint,m_kSphere,fEpsilon);
}
//----------------------------------------------------------------------------
bool SphereBV::TestIntersection (const BoundingVolume* pkBV) const
{
    assert( GetType() == pkBV->GetType() );
    return Wml::TestIntersection(m_kSphere,((SphereBV*)pkBV)->m_kSphere);
}
//----------------------------------------------------------------------------
BoundingVolume* SphereBV::Create ()
{
    return new SphereBV;
}
//----------------------------------------------------------------------------
BoundingVolume* SphereBV::Create (int iVertexCount, const Vector3f* akVertex,
    const int* aiConnect, int i0, int i1, int* aiISplit, Vector3f& rkOrigin,
    Vector3f& rkDirection)
{
    // tag vertices that are used in the submesh
    bool* abValid = new bool[iVertexCount];
    memset(abValid,0,iVertexCount*sizeof(bool));
    for (int i = i0; i <= i1; i++)
    {
        int iIndex = 3*aiISplit[i];
        abValid[aiConnect[iIndex++]] = true;
        abValid[aiConnect[iIndex++]] = true;
        abValid[aiConnect[iIndex++]] = true;
    }

    SphereBV* pkSphereBV = new SphereBV;
    ContSphereAverage(iVertexCount,akVertex,abValid,pkSphereBV->m_kSphere);

    OrthogonalLineFit(iVertexCount,akVertex,abValid,rkOrigin,rkDirection);
    delete[] abValid;

    return pkSphereBV;
}
//----------------------------------------------------------------------------
