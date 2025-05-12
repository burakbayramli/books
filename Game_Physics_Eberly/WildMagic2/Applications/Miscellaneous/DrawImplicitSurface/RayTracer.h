// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef RAYTRACER_H
#define RAYTRACER_H

#include "WmlVector3.h"
using namespace Wml;

class RayTracer
{
public:
    typedef float (*Function)(const Vector3f&);
    typedef Vector3f (*Gradient)(const Vector3f&);

    // Ray trace level surface F(x,y,z) = 0.  The surface normals are
    // computed from DF(x,y,z), the gradient of F.
    RayTracer (Function oF, Gradient oDF, int iWidth, int iHeight);
    ~RayTracer ();

    // camera and view frustum
    Vector3f& Location ();
    Vector3f& Direction ();
    Vector3f& Up ();
    Vector3f& Right ();
    float& Near ();
    float& Far ();
    float& HalfWidth ();
    float& HalfHeight ();

    // rendered image
    int GetWidth () const;
    int GetHeight () const;
    const float* GetImage () const;

    // Ray trace the view frustum region.  The tracing uses a single
    // directional light.  TO DO:  Allow more lights and different light
    // types.  The number of samples per ray for computing intersection of
    // rays with the surface is specified in iMaxSample.
    void DrawSurface (int iMaxSample, const Vector3f& rkLightDir, bool bBlur);

private:
    // find intersection of ray with surface
    void FindSurface (float fS0, float fF0, const Vector3f& rkP0, float fS1,
        float fF1, const Vector3f& rkP1, const Vector3f& kRayDir,
        Vector3f& rkPos, Vector3f& rkNormal);

    // blur the image for a cheap antialiasing
    void BlurImage ();

    // camera and view frustum
    Vector3f m_kLocation, m_kDirection, m_kUp, m_kRight;
    float m_fNear, m_fFar, m_fHalfWidth, m_fHalfHeight;

    // level surface F(x,y,z) = 0, surface normal DF(x,y,z)
    Function m_oF;
    Gradient m_oDF;

    // rendered image
    int m_iWidth, m_iHeight;
    float* m_afImage;
    float* m_afBlur;
};

#include "RayTracer.inl"

#endif
