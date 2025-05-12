// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlCapsuleBV.h"
#include "WmlContCapsule3.h"
#include "WmlIntrCap3Cap3.h"
#include "WmlMatrix3.h"
using namespace Wml;

WmlImplementBV(CapsuleBV,BV_CAPSULE);

//----------------------------------------------------------------------------
void CapsuleBV::CopyTo (BoundingVolume* pkTargetBV) const
{
    assert( GetType() == pkTargetBV->GetType() );
    ((CapsuleBV*)pkTargetBV)->m_kCapsule = m_kCapsule;
}
//----------------------------------------------------------------------------
void CapsuleBV::TransformTo (BoundingVolume* pkTargetBV,
    const Matrix3f& rkRot, const Vector3f& rkTrn, float fScale) const
{
    assert( GetType() == pkTargetBV->GetType() );
    Capsule3f& rkTarget = ((CapsuleBV*)pkTargetBV)->m_kCapsule;

    rkTarget.Origin() = fScale*(rkRot*m_kCapsule.Origin()) + rkTrn;
    rkTarget.Direction() = fScale*(rkRot*m_kCapsule.Direction());
    rkTarget.Radius() = fScale*m_kCapsule.Radius();
}
//----------------------------------------------------------------------------
bool CapsuleBV::Contains (const Vector3f& rkPoint, float fEpsilon) const
{
    return InCapsule(rkPoint,m_kCapsule,fEpsilon);
}
//----------------------------------------------------------------------------
bool CapsuleBV::TestIntersection (const BoundingVolume* pkBV) const
{
    assert( GetType() == pkBV->GetType() );
    return Wml::TestIntersection(m_kCapsule,((CapsuleBV*)pkBV)->m_kCapsule);
}
//----------------------------------------------------------------------------
BoundingVolume* CapsuleBV::Create ()
{
    return new CapsuleBV;
}
//----------------------------------------------------------------------------
BoundingVolume* CapsuleBV::Create (int iVertexCount, const Vector3f* akVertex,
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

    CapsuleBV* pkCapsuleBV = new CapsuleBV;
    ContCapsule(iVertexCount,akVertex,abValid,pkCapsuleBV->m_kCapsule);
    delete[] abValid;

    rkOrigin = pkCapsuleBV->m_kCapsule.Origin();
    rkDirection = pkCapsuleBV->m_kCapsule.Direction();
    return pkCapsuleBV;
}
//----------------------------------------------------------------------------
