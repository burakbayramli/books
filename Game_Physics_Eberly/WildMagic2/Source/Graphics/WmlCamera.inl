// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

//----------------------------------------------------------------------------
inline float Camera::GetFrustumNear () const
{
    return m_fFrustumN;
}
//----------------------------------------------------------------------------
inline float Camera::GetFrustumFar () const
{
    return m_fFrustumF;
}
//----------------------------------------------------------------------------
inline float Camera::GetFrustumLeft () const
{
    return m_fFrustumL;
}
//----------------------------------------------------------------------------
inline float Camera::GetFrustumRight () const
{
    return m_fFrustumR;
}
//----------------------------------------------------------------------------
inline float Camera::GetFrustumTop () const
{
    return m_fFrustumT;
}
//----------------------------------------------------------------------------
inline float Camera::GetFrustumBottom () const
{
    return m_fFrustumB;
}
//----------------------------------------------------------------------------
inline float Camera::GetViewPortLeft () const
{
    return m_fPortL;
}
//----------------------------------------------------------------------------
inline float Camera::GetViewPortRight () const
{
    return m_fPortR;
}
//----------------------------------------------------------------------------
inline float Camera::GetViewPortTop () const
{
    return m_fPortT;
}
//----------------------------------------------------------------------------
inline float Camera::GetViewPortBottom () const
{
    return m_fPortB;
}
//----------------------------------------------------------------------------
inline const Vector3f& Camera::GetLocation () const
{
    return m_kLocation;
}
//----------------------------------------------------------------------------
inline const Vector3f& Camera::GetLeft () const
{
    return m_kLeft;
}
//----------------------------------------------------------------------------
inline const Vector3f& Camera::GetUp () const
{
    return m_kUp;
}
//----------------------------------------------------------------------------
inline const Vector3f& Camera::GetDirection () const
{
    return m_kDirection;
}
//----------------------------------------------------------------------------
inline void Camera::SetPlaneState (unsigned int uiPlaneState)
{
    m_uiPlaneState = uiPlaneState;
}
//----------------------------------------------------------------------------
inline unsigned int Camera::GetPlaneState () const
{
    return m_uiPlaneState;
}
//----------------------------------------------------------------------------
inline int Camera::GetPlaneQuantity () const
{
    return m_iPlaneQuantity;
}
//----------------------------------------------------------------------------
inline const Plane3f* Camera::GetPlanes () const
{
    return m_akWorldPlane;
}
//----------------------------------------------------------------------------
inline void Camera::SetUsePerspective (bool bPerspective)
{
    m_bUsePerspective = bPerspective;
}
//----------------------------------------------------------------------------
inline bool Camera::GetUsePerspective () const
{
    return m_bUsePerspective;
}
//----------------------------------------------------------------------------
inline bool Camera::GetActive () const
{
    return m_bActive;
}
//----------------------------------------------------------------------------


