// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLTRIMESH_H
#define WMLTRIMESH_H

#include "WmlGeometry.h"

namespace Wml
{

class WML_ITEM TriMesh : public Geometry
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    // Construction and destruction.  TriMesh accepts responsibility for
    // deleting the input arrays.
    TriMesh (int iVertexQuantity, Vector3f* akVertex, Vector3f* akNormal,
        ColorRGB* akColor, Vector2f* akTexture, int iTriangleQuantity,
        int* aiConnect, Vector2f* akTexture1 = NULL,
        Vector2f* akTexture2 = NULL, Vector2f* akTexture3 = NULL,
        Vector2f* akTextureBump = NULL, VertexShader* pkVertexShader = NULL,
        PixelShader* pkPixelShader = NULL);

    virtual ~TriMesh ();

    void Reconstruct (int iVertexQuantity, int iTriangleQuantity);

    // The reconstruction allows you to change only individual arrays.  You
    // do this by passing in the current array pointers for any arrays you
    // want to preserve.
    void Reconstruct (int iVertexQuantity, Vector3f* akVertex,
        Vector3f* akNormal, ColorRGB* akColor, Vector2f* akTexture,
        int iTriangleQuantity, int* aiConnect,  Vector2f* akTexture1 = NULL, 
        Vector2f* akTexture2 = NULL, Vector2f* akTexture3 = NULL,
        Vector2f* akTextureBump = NULL);

    // connectivity access
    int* Connectivity ();
    const int* Connectivity () const;

    // triangle access
    int GetTriangleQuantity () const;
    void GetTriangle (int i, int& riV0, int& riV1, int& riV2) const;
    void GetTriangle (int i, Vector3f& rkV0, Vector3f& rkV1,
        Vector3f& rkV2) const;

    // updates
    virtual void UpdateModelNormals ();

    // Picking support.  The pick record stores the index of any triangle
    // intersected by the ray.  It stores the barycentric coordinates of
    // the intersection point.  This allows the application to compute
    // interpolated vertex attributes as well as the actual point of
    // intersection.
    class WML_ITEM PickRecord : public Geometry::PickRecord
    {
    public:
        PickRecord (TriMesh* pkObject, float fRayT, int iTriangle,
            float fBary0, float fBary1, float fBary2);

        // Index of the triangle that is intersected by the ray.
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
    TriMesh ();

    // drawing
    friend class BezierMesh;
    friend class ScreenPolygon;
    virtual void Draw (Renderer& rkRenderer);

    // Picking support.  The ray is in model-space coordinates.  The function
    // returns 'true' if and only if the ray intersects the triangle.  When
    // 'true', the intersection point's barycentric coordinates with respect
    // to the triangle and ray parameter are returned.
    static bool GetRayTriangleIntersection (const Vector3f& rkModelOrigin,
        const Vector3f& rkModelDirection, const Vector3f& rkV0,
        const Vector3f& rkV1, const Vector3f& rkV2, float& rfBary0,
        float& rfBary1, float& rfBary2, float& rfRayT);

    // triangles
    int m_iTriangleQuantity;
    int* m_aiConnect;
};

WmlSmartPointer(TriMesh);
WmlRegisterStream(TriMesh);
#include "WmlTriMesh.inl"

}

#endif
