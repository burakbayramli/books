// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlBoundingVolume.h"
using namespace Wml;

BoundingVolume::CreatorS BoundingVolume::ms_aoCreatorS[
    BoundingVolume::BV_QUANTITY];

BoundingVolume::CreatorT BoundingVolume::ms_aoCreatorT[
    BoundingVolume::BV_QUANTITY];

//----------------------------------------------------------------------------
BoundingVolume::~BoundingVolume ()
{
}
//----------------------------------------------------------------------------
BoundingVolume* BoundingVolume::Create (Type eType)
{
    if ( eType != BV_QUANTITY )
        return ms_aoCreatorS[eType]();
    else
        return NULL;
}
//----------------------------------------------------------------------------
BoundingVolume* BoundingVolume::Create (Type eType, int iVertexCount,
    const Vector3f* akVertex, const int* aiConnect, int i0, int i1,
    int* aiISplit, Vector3f& rkOrigin, Vector3f& rkDirection)
{
    if ( eType != BV_QUANTITY )
    {
        return ms_aoCreatorT[eType](iVertexCount,akVertex,aiConnect,i0,i1,
            aiISplit,rkOrigin,rkDirection);
    }
    else
    {
        return NULL;
    }
}
//----------------------------------------------------------------------------
