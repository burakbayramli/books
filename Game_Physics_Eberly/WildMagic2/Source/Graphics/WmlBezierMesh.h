// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLBEZIERMESH_H
#define WMLBEZIERMESH_H

#include "WmlGeometry.h"
#include "WmlBezierPatch.h"

namespace Wml
{

class WML_ITEM BezierMesh : public Geometry
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    // Construction and destruction.  BezierMesh accepts responsibility
    // for deleting the input arrays.
    BezierMesh (int iCtrlQuantity, Vector3f* akCtrlVertex, bool bUseNormals,
        ColorRGB* akCtrlColor, Vector2f* akCtrlTexture, int iPatchQuantity,
        BezierPatchPtr* aspkPatch);

    virtual ~BezierMesh ();

    // patch access
    int GetPatchQuantity () const;
    BezierPatchPtr* Patches ();
    const BezierPatchPtr* Patches () const;
    BezierPatchPtr Patch (int i);
    const BezierPatchPtr Patch (int i) const;

    // mesh access
    TriMeshPtr GetMesh ();

    // tessellation
    void Tessellate (int iLevel);
    int GetTessellationLevel () const;

    // updates
    virtual void UpdateModelNormals ();

    // support for searching by name
    virtual Object* GetObjectByName (const char* acName);
    virtual void GetAllObjectsByName (const char* acName,
        std::vector<Object*>& rkObjects);

    // Picking support.  The intersection between ray and surface mesh is
    // computed as ray-triangle intersections where the triangles used are
    // those of the current tessellation.  Intersections of ray and Bezier
    // surface patch are more complicated to calculate and potentially more
    // expensive to compute (unless the tessellation level is very large).
    // That said, if the level of tessellation is large, the pick operation
    // can also be expensive since it will iterate over a lot of triangles
    // that do not intersect the ray.  Eventually I will add a bounding
    // volume tree member to help localize the intersection calculations.
    //
    // The pick record stores the index of any triangle intersected by the
    // ray.  It stores the barycentric coordinates of the intersection point.
    // This allows the application to compute interpolated vertex attributes
    // as well as the actual point of intersection.  It also stores the ray
    // parameter corresponding to the point of intersection.  This is useful
    // for sorting the intersection points along the pick ray.
    class WML_ITEM PickRecord : public Geometry::PickRecord
    {
    public:
        PickRecord (BezierMesh* pkObject, float fRayT, int iPatch,
            int iTriangle, float fBary0, float fBary1, float fBary2);

        // Index of the patch in the array m_aspkPatch[]
        int m_iPatch;

        // Index of the triangle in the connectivity array of m_pkMesh that
        // is intersected by the ray.
        int m_iTriangle;

        // Barycentric coordinates of the point of intersection.  If b0, b1,
        // and b2 are the values, then all are in [0,1] and b0+b1+b2=1.
        float m_fBary0, m_fBary1, m_fBary2;
    };

    // The origin and direction of the ray must be in world coordinates.  The
    // application is responsible for deleting the pick records in the array.
    virtual void DoPick (const Vector3f& rkOrigin,
        const Vector3f& rkDirection, PickArray& rkResults);

protected:
    BezierMesh ();

    // geometric updates
    virtual void UpdateWorldData (float fAppTime);
    virtual void UpdateWorldBound ();

    // render state updates
    virtual void UpdateRenderState (RenderState::Stack* pkStack);

    // drawing
    virtual void Draw (Renderer& rkRenderer);

    // tessellation
    void GetMeshQuantities (int iLevel, int& riVertexQuantity,
        int& riTriangleQuantity);

    // patch information
    int m_iPatchQuantity;
    BezierPatchPtr* m_aspkPatch;

    // tessellation
    TriMeshPtr m_spkMesh;
    int m_iLevel;
};

WmlSmartPointer(BezierMesh);
WmlRegisterStream(BezierMesh);
#include "WmlBezierMesh.inl"

}

#endif
