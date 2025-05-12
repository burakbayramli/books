// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLCAMERA_H
#define WMLCAMERA_H

#include "WmlMatrix3.h"
#include "WmlObject.h"
#include "WmlPlane3.h"

namespace Wml
{

class Bound;


class WML_ITEM Camera : public Object
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    // construction
    Camera ();

    // view frustum
    void SetFrustum (float fNear, float fFar, float fLeft, float fRight,
        float fTop, float fBottom);
    void SetFrustumNear (float fNear);
    void SetFrustumFar (float fFar);
    void GetFrustum (float& rfNear, float& rfFar, float& rfLeft,
        float& rfRight, float& rfTop, float& rfBottom) const;
    float GetFrustumNear () const;
    float GetFrustumFar () const;
    float GetFrustumLeft () const;
    float GetFrustumRight () const;
    float GetFrustumTop () const;
    float GetFrustumBottom () const;
    float GetMaxCosSqrFrustumAngle () const;

    // Set orthogonal view frustum (L = -R, B = -T) using field of view in
    // the "up" direction and an aspect ratio "width/height".  This call is
    // the equivalent of gluPerspective in OpenGL.  As such, the field of
    // view in this function must be specified in degrees and be in the
    // interval (0,180).
    void SetFrustum (float fUpFovDegrees, float fAspectRatio, float fNear,
        float fFar);

    // viewport (contained in [0,1]^2)
    void SetViewPort (float fLeft, float fRight, float fTop, float fBottom);
    void GetViewPort (float& rfLeft, float& rfRight, float& rfTop,
        float& rfBottom);
    float GetViewPortLeft () const;
    float GetViewPortRight () const;
    float GetViewPortTop () const;
    float GetViewPortBottom () const;

    // camera frame (world coordinates)
    //   default location = (0,0,0)
    //   default left = (1,0,0)
    //   default up = (0,1,0)
    //   default direction = (0,0,1)
    void SetFrame (const Vector3f& rkLocation, const Vector3f& rkLeft,
        const Vector3f& rkUp, const Vector3f& rkDirection);
    void SetFrame (const Vector3f& rkLocation, const Matrix3f& rkAxes);
    void SetLocation (const Vector3f& rkLocation);
    void SetAxes (const Vector3f& rkLeft, const Vector3f& rkUp,
        const Vector3f& rkDirection);
    void SetAxes (const Matrix3f& rkAxes);
    const Vector3f& GetLocation () const;
    const Vector3f& GetLeft () const;
    const Vector3f& GetUp () const;
    const Vector3f& GetDirection () const;

    // update frustum, viewport, and frame
    void Update ();

    // access to stack of world culling planes
    int GetPlaneQuantity () const;
    const Plane3f* GetPlanes () const;
    void PushPlane (const Plane3f& rkPlane);
    void PopPlane ();

    // Mouse picking support.  The (x,y) input point is in left-handed screen
    // coordinates (what you usually read from the windowing system).  The
    // function returns 'true' if and only if the input point is located in
    // the current viewport.  When 'true', the origin and direction values
    // are valid and are in world coordinates.  The direction vector is unit
    // length.
    bool GetPickRay (int iX, int iY, int iWidth, int iHeight,
        Vector3f& rkOrigin, Vector3f& rkDirection) const;
    bool GetPickRayOrtho (int iX, int iY, int iWidth, int iHeight,
        Vector3f& rkOrigin, Vector3f& rkDirection) const;
        
    // Perspective (default) or orthogonal projection.
    void SetUsePerspective (bool bPerspective);
    bool GetUsePerspective () const;
    
    // Active state (camera attached to a Renderer).
    bool GetActive () const;

protected:
    // update callbacks
    friend class Renderer;
    virtual void OnResize (int iWidth, int iHeight);
    virtual void OnFrustumChange ();
    virtual void OnViewPortChange ();
    virtual void OnFrameChange ();

    // culling support in Spatial::OnDraw
    friend class Spatial;
    void SetPlaneState (unsigned int uiPlaneState);
    unsigned int GetPlaneState () const;
    bool Culled (const Bound& kWorldBound);

    // culling support in Portal::Draw
    friend class Portal;
    bool Culled (int iVertexQuantity, const Vector3f* akVertex,
        bool bIgnoreNearPlane);

    // view frustum
    float m_fFrustumN, m_fFrustumF;
    float m_fFrustumL, m_fFrustumR, m_fFrustumT, m_fFrustumB;

    // viewport
    float m_fPortL, m_fPortR, m_fPortT, m_fPortB;

    // camera frame (world location and coordinate axes)
    Vector3f m_kLocation, m_kLeft, m_kUp, m_kDirection;

    // Temporary values computed in OnFrustumChange that are needed if a
    // call is made to OnFrameChange.
    float m_afCoeffL[2], m_afCoeffR[2], m_afCoeffB[2], m_afCoeffT[2];

    // Bit flag to store whether or not a plane is active in the culling
    // system.  A bit of 1 means the plane is active, otherwise the plane is
    // inactive.  The first 6 planes are the view frustum planes.  Indices
    // are:  0 = near, 1 = far, 2 = left, 3 = right, 4 = bottom, 5 = top.
    // The system currently supports at most 32 culling planes.
    unsigned int m_uiPlaneState;

    // world planes
    enum
    {
        CAM_LEFT_PLANE = 0,
        CAM_RIGHT_PLANE = 1,
        CAM_BOTTOM_PLANE = 2,
        CAM_TOP_PLANE = 3,
        CAM_FAR_PLANE = 4,
        CAM_NEAR_PLANE = 5,
        CAM_FRUSTUM_PLANES = 6,
        CAM_MAX_WORLD_PLANES = 32
    };
    int m_iPlaneQuantity;
    Plane3f m_akWorldPlane[CAM_MAX_WORLD_PLANES];

    // Perspective projection and active rendering states.
    bool m_bUsePerspective;
    bool m_bActive;
};

WmlSmartPointer(Camera);
WmlRegisterStream(Camera);
#include "WmlCamera.inl"

}

#endif
