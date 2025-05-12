// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLSTANDARDMESH_H
#define WMLSTANDARDMESH_H

#include "WmlTriMesh.h"

namespace Wml
{

// If the input mesh is null, a new mesh is created and returned.  If the
// input mesh is non-null, the mesh is reconstructed.

WML_ITEM void CreateRectangleMesh (TriMesh*& rpkMesh,
    const Vector3f& rkCenter, const Vector3f& rkU, const Vector3f& rkV,
    const Vector3f& rkAxis, float fUExtent, float fVExtent,
    bool bWantNormals, bool bWantColors, bool bWantUVs);

WML_ITEM void CreateDiskMesh (TriMesh*& rpkMesh, int iShellSamples,
    int iRadialSamples, const Vector3f& rkCenter, float fRadius,
    const Vector3f& rkU, const Vector3f& rkV, const Vector3f& rkAxis,
    bool bWantNormals, bool bWantColors, bool bWantUVs);

WML_ITEM void CreateBoxMesh (TriMesh*& rpkMesh, const Vector3f& rkCenter,
    const Vector3f& rkU, const Vector3f& rkV, const Vector3f& rkW,
    float fUExtent, float fVExtent, float fWExtent, bool bWantNormals,
    bool bWantColors, bool bWantUVs, bool bOutsideView);

WML_ITEM void CreateCylinderMesh (TriMesh*& rpkMesh, int iAxisSamples,
    int iRadialSamples, const Vector3f& rkCenter, const Vector3f& rkU,
    const Vector3f& rkV, const Vector3f& rkAxis, float fRadius, float fHeight,
    bool bWantNormals, bool bWantColors, bool bWantUVs, bool bOutsideView);

WML_ITEM void CreateSphereMesh (TriMesh*& rpkMesh, int iZSamples,
    int iRadialSamples, const Vector3f& rkCenter, float fRadius,
    const Vector3f& rkU, const Vector3f& rkV, const Vector3f& rkAxis,
    bool bWantNormals, bool bWantColors, bool bWantUVs, bool bOutsideView);

WML_ITEM void CreateTorusMesh (TriMesh*& rpkMesh, int iCircleSamples,
    int iRadialSamples, const Vector3f& rkCenter, const Vector3f& rkU,
    const Vector3f& rkV, const Vector3f& rkAxis, float fOuterRadius,
    float fInnerRadius, bool bWantNormals, bool bWantColors, bool bWantUVs,
    bool bOutsideView);


// Platonic solids

WML_ITEM void CreateTetrahedronMesh (TriMesh*& rpkMesh, bool bWantNormals,
    bool bWantColors, bool bWantUVs, bool bOutsideView);

WML_ITEM void CreateHexahedronMesh (TriMesh*& rpkMesh, bool bWantNormals,
    bool bWantColors, bool bWantUVs, bool bOutsideView);

WML_ITEM void CreateOctahedronMesh (TriMesh*& rpkMesh, bool bWantNormals,
    bool bWantColors, bool bWantUVs, bool bOutsideView);

WML_ITEM void CreateDodecahedronMesh (TriMesh*& rpkMesh, bool bWantNormals,
    bool bWantColors, bool bWantUVs, bool bOutsideView);

WML_ITEM void CreateIcosahedronMesh (TriMesh*& rpkMesh, bool bWantNormals,
    bool bWantColors, bool bWantUVs, bool bOutsideView);

}

#endif
