// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlBoxBV.h"
#include "WmlContBox3.h"
#include "WmlIntrBox3Box3.h"
#include "WmlMatrix3.h"
using namespace Wml;

WmlImplementBV(BoxBV,BV_BOX);

//----------------------------------------------------------------------------
void BoxBV::CopyTo (BoundingVolume* pkTargetBV) const
{
    assert( GetType() == pkTargetBV->GetType() );
    ((BoxBV*)pkTargetBV)->m_kBox = m_kBox;
}
//----------------------------------------------------------------------------
void BoxBV::TransformTo (BoundingVolume* pkTargetBV, const Matrix3f& rkRot,
    const Vector3f& rkTrn, float fScale) const
{
    assert( GetType() == pkTargetBV->GetType() );
    Box3f& rkTarget = ((BoxBV*)pkTargetBV)->m_kBox;

    rkTarget.Center() = fScale*(rkRot*m_kBox.Center()) + rkTrn;
    for (int i = 0; i < 3; i++)
    {
        rkTarget.Axis(i) = rkRot*m_kBox.Axis(i);
        rkTarget.Extent(i) = fScale*m_kBox.Extent(i);
    }
}
//----------------------------------------------------------------------------
bool BoxBV::Contains (const Vector3f& rkPoint, float fEpsilon) const
{
    return InBox(rkPoint,m_kBox,fEpsilon);
}
//----------------------------------------------------------------------------
bool BoxBV::TestIntersection (const BoundingVolume* pkBV) const
{
    assert( GetType() == pkBV->GetType() );
    return Wml::TestIntersection(m_kBox,((BoxBV*)pkBV)->m_kBox);
}
//----------------------------------------------------------------------------
BoundingVolume* BoxBV::Create ()
{
    return new BoxBV;
}
//----------------------------------------------------------------------------
BoundingVolume* BoxBV::Create (int iVertexCount, const Vector3f* akVertex,
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

    BoxBV* pkBoxBV = new BoxBV;
    ContOrientedBox(iVertexCount,akVertex,abValid,pkBoxBV->m_kBox);
    delete[] abValid;

    rkOrigin = pkBoxBV->m_kBox.Center();
    rkDirection = pkBoxBV->m_kBox.Axis(2);
    return pkBoxBV;
}
//----------------------------------------------------------------------------


