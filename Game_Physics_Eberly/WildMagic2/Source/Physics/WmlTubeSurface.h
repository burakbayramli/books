// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#ifndef WMLTUBESURFACE_H
#define WMLTUBESURFACE_H

#include "WmlCurve3.h"
#include "WmlTriMesh.h"

namespace Wml
{

class WML_ITEM TubeSurface : public TriMesh
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    typedef float (*RadialFunction)(float);

    // Construction and destruction.  TubeSurface accepts responsibility
    // for deleting the input curve.  If rkUpVector is not the zero vector,
    // it will be used as 'up' in the frame calculations.  If it is the zero
    // vector, the Frenet frame will be used.  If bWantColors is 'true',
    // the vertex colors are allocated and set to black.  The application
    // needs to assign colors as needed.  If either of pkTextureMin or
    // pkTextureMax is not null, both must be not null.  In this case,
    // texture coordinates are generated for the surface.
    TubeSurface (Curve3f* pkMedial, RadialFunction oRadial, bool bClosed,
        const Vector3f& rkUpVector, int iMedialSamples, int iSliceSamples,
        bool bWantNormals, bool bWantColors, bool bSampleByArcLength,
        bool bInsideView, const Vector2f* pkTextureMin,
        const Vector2f* pkTextureMax);

    virtual ~TubeSurface ();

    // member access
    Curve3f*& Medial ();
    const Curve3f* GetMedial () const;
    RadialFunction& Radial ();
    RadialFunction GetRadial () const;
    Vector3f& UpVector ();
    const Vector3f& GetUpVector () const;
    int GetSliceSamples () const;

    // Generate vertices for the end slices.  These are useful when you build
    // an open tube and want to attach meshes at the ends to close the tube.
    // The input array must have size (at least) S+1 where S is the number
    // returned by GetSliceSamples.  Function GetTMinSlice is used to access
    // the slice corresponding to the medial curve evaluated at its domain
    // minimum, tmin.  Function GetTMaxSlice accesses the slice for the
    // domain maximum, tmax.  If the curve is closed, the slices are the same.
    void GetTMinSlice (Vector3f* akSlice);
    void GetTMaxSlice (Vector3f* akSlice);

    // If the medial curve is modified, for example if it is control point
    // based and the control points are modified, then you should call this
    // update function to recompute the tube surface geometry.
    void UpdateSurface ();

protected:
    TubeSurface ();

    // tessellation support
    int Index (int iS, int iM);
    void ComputeSinCos ();
    void ComputeVertices (Vector3f* akVertex);
    void ComputeNormals (const Vector3f* akVertex, Vector3f* akNormal);
    void ComputeTextures (const Vector2f& rkTextureMin,
        const Vector2f& rkTextureMax, Vector2f* akTexture);
    void ComputeConnectivity (int& riTQuantity, int*& raiConnect,
        bool bInsideView);

    Curve3f* m_pkMedial;
    RadialFunction m_oRadial;
    int m_iMedialSamples, m_iSliceSamples;
    Vector3f m_kUpVector;
    float* m_afSin;
    float* m_afCos;
    bool m_bClosed, m_bSampleByArcLength;
};

WmlSmartPointer(TubeSurface);
WmlRegisterStream(TubeSurface);
#include "WmlTubeSurface.inl"

}

#endif
