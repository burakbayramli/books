// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlContLozenge3.h"
#include "WmlIntrLoz3Loz3.h"
#include "WmlLozengeBV.h"
#include "WmlMatrix3.h"
using namespace Wml;

WmlImplementBV(LozengeBV,BV_LOZENGE);

//----------------------------------------------------------------------------
void LozengeBV::CopyTo (BoundingVolume* pkTargetBV) const
{
    assert( GetType() == pkTargetBV->GetType() );
    ((LozengeBV*)pkTargetBV)->m_kLozenge = m_kLozenge;
}
//----------------------------------------------------------------------------
void LozengeBV::TransformTo (BoundingVolume* pkTargetBV,
    const Matrix3f& rkRot, const Vector3f& rkTrn, float fScale) const
{
    assert( GetType() == pkTargetBV->GetType() );
    Lozenge3f& rkTarget = ((LozengeBV*)pkTargetBV)->m_kLozenge;

    rkTarget.Origin() = fScale*(rkRot*m_kLozenge.Origin()) + rkTrn;
    rkTarget.Edge0() = fScale*(rkRot*m_kLozenge.Edge0());
    rkTarget.Edge1() = fScale*(rkRot*m_kLozenge.Edge1());
    rkTarget.Radius() = fScale*m_kLozenge.Radius();
}
//----------------------------------------------------------------------------
bool LozengeBV::Contains (const Vector3f& rkPoint, float fEpsilon) const
{
    return InLozenge(rkPoint,m_kLozenge,fEpsilon);
}
//----------------------------------------------------------------------------
bool LozengeBV::TestIntersection (const BoundingVolume* pkBV) const
{
    assert( GetType() == pkBV->GetType() );
    return Wml::TestIntersection(m_kLozenge,((LozengeBV*)pkBV)->m_kLozenge);
}
//----------------------------------------------------------------------------
BoundingVolume* LozengeBV::Create ()
{
    return new LozengeBV;
}
//----------------------------------------------------------------------------
BoundingVolume* LozengeBV::Create (int iVertexCount, const Vector3f* akVertex,
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

    LozengeBV* pkLozengeBV = new LozengeBV;
    ContLozenge(iVertexCount,akVertex,abValid,pkLozengeBV->m_kLozenge);
    delete[] abValid;

    rkOrigin = pkLozengeBV->m_kLozenge.Origin();
    rkDirection = pkLozengeBV->m_kLozenge.Edge0();
    return pkLozengeBV;
}
//----------------------------------------------------------------------------
