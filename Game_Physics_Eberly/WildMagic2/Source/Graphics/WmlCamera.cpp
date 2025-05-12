// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlBound.h"
#include "WmlCamera.h"
using namespace Wml;

WmlImplementRTTI(Camera,Object);
WmlImplementStream(Camera);

//----------------------------------------------------------------------------
Camera::Camera ()
    :
    m_kLocation(Vector3f::ZERO),
    m_kLeft(Vector3f::UNIT_X),
    m_kUp(Vector3f::UNIT_Y),
    m_kDirection(Vector3f::UNIT_Z)
{
    m_fFrustumN = 1.0f;
    m_fFrustumF = 2.0f;
    m_fFrustumL = -0.5f;
    m_fFrustumR = 0.5f;
    m_fFrustumT = 0.5f;
    m_fFrustumB = -0.5f;
    m_fPortL = 0.0f;
    m_fPortR = 1.0f;
    m_fPortT = 1.0f;
    m_fPortB = 0.0f;
    m_uiPlaneState = (unsigned int)(~0);  // all planes initially active
    m_iPlaneQuantity = 6;  // frustum planes always processed for culling
    
    // Default is inactive camera in perspective mode.
    m_bUsePerspective = true;
    m_bActive = false;

    OnFrustumChange();
    OnViewPortChange();
    OnFrameChange();
}
//----------------------------------------------------------------------------
void Camera::SetFrustum (float fNear, float fFar, float fLeft, float fRight,
    float fTop, float fBottom)
{
    m_fFrustumN = fNear;
    m_fFrustumF = fFar;
    m_fFrustumL = fLeft;
    m_fFrustumR = fRight;
    m_fFrustumT = fTop;
    m_fFrustumB = fBottom;
    OnFrustumChange();
}
//----------------------------------------------------------------------------
void Camera::SetFrustumNear (float fNear)
{
    m_fFrustumN = fNear;
    OnFrustumChange();
}
//----------------------------------------------------------------------------
void Camera::SetFrustumFar (float fFar)
{
    m_fFrustumF = fFar;
    OnFrustumChange();
}
//----------------------------------------------------------------------------
void Camera::GetFrustum (float& rfNear, float& rfFar, float& rfLeft,
    float& rfRight, float& rfTop, float& rfBottom) const
{
    rfNear = m_fFrustumN;
    rfFar = m_fFrustumF;
    rfLeft = m_fFrustumL;
    rfRight = m_fFrustumR;
    rfTop = m_fFrustumT;
    rfBottom = m_fFrustumB;
}
//----------------------------------------------------------------------------
float Camera::GetMaxCosSqrFrustumAngle () const
{
    // Compute (cos(A))^2 where A is the largest angle between the frustum
    // axis direction D and the four edges of the frustum that lie along the
    // rays from the frustum origin.

    float fNSqr = m_fFrustumN*m_fFrustumN;

    float fDenom = fNSqr;
    if ( fabsf(m_fFrustumL) >= fabsf(m_fFrustumR) )
    {
        fDenom += m_fFrustumL*m_fFrustumL;
        if ( fabsf(m_fFrustumB) >= fabsf(m_fFrustumT) )
            fDenom += m_fFrustumB*m_fFrustumB;
        else
            fDenom += m_fFrustumT*m_fFrustumT;
    }
    else
    {
        fDenom += m_fFrustumR*m_fFrustumR;
        if ( fabsf(m_fFrustumB) >= fabsf(m_fFrustumT) )
            fDenom += m_fFrustumB*m_fFrustumB;
        else
            fDenom += m_fFrustumT*m_fFrustumT;
    }

    return fNSqr/fDenom;
}
//----------------------------------------------------------------------------
void Camera::SetFrustum (float fUpFovDegrees, float fAspectRatio, float fNear,
    float fFar)
{
    float fHalfAngleRadians = 0.5f*fUpFovDegrees*Mathf::DEG_TO_RAD;
    m_fFrustumT = fNear*Mathf::Tan(fHalfAngleRadians);
    m_fFrustumB = -m_fFrustumT;
    m_fFrustumR = fAspectRatio*m_fFrustumT;
    m_fFrustumL = -m_fFrustumR;
    m_fFrustumN = fNear;
    m_fFrustumF = fFar;
    OnFrustumChange();
}
//----------------------------------------------------------------------------
void Camera::SetViewPort (float fLeft, float fRight, float fTop,
    float fBottom)
{
    m_fPortL = fLeft;
    m_fPortR = fRight;
    m_fPortT = fTop;
    m_fPortB = fBottom;
    OnViewPortChange();
}
//----------------------------------------------------------------------------
void Camera::GetViewPort (float& rfLeft, float& rfRight, float& rfTop,
    float& rfBottom)
{
    rfLeft = m_fPortL;
    rfRight = m_fPortR;
    rfTop = m_fPortT;
    rfBottom = m_fPortB;
}
//----------------------------------------------------------------------------
void Camera::SetFrame (const Vector3f& rkLocation, const Vector3f& rkLeft,
    const Vector3f& rkUp, const Vector3f& rkDirection)
{
    m_kLocation = rkLocation;
    m_kLeft = rkLeft;
    m_kUp = rkUp;
    m_kDirection = rkDirection;
    OnFrameChange();
}
//----------------------------------------------------------------------------
void Camera::SetFrame (const Vector3f& rkLocation, const Matrix3f& rkAxes)
{
    m_kLocation = rkLocation;
    m_kLeft = rkAxes.GetColumn(0);
    m_kUp = rkAxes.GetColumn(1);
    m_kDirection = rkAxes.GetColumn(2);
    OnFrameChange();
}
//----------------------------------------------------------------------------
void Camera::SetLocation (const Vector3f& rkLocation)
{
    m_kLocation = rkLocation;
    OnFrameChange();
}
//----------------------------------------------------------------------------
void Camera::SetAxes (const Vector3f& rkLeft, const Vector3f& rkUp,
    const Vector3f& rkDirection)
{
    m_kLeft = rkLeft;
    m_kUp = rkUp;
    m_kDirection = rkDirection;
    OnFrameChange();
}
//----------------------------------------------------------------------------
void Camera::SetAxes (const Matrix3f& rkAxes)
{
    m_kLeft = rkAxes.GetColumn(0);
    m_kUp = rkAxes.GetColumn(1);
    m_kDirection = rkAxes.GetColumn(2);
    OnFrameChange();
}
//----------------------------------------------------------------------------
void Camera::OnResize (int, int)
{
}
//----------------------------------------------------------------------------
void Camera::OnFrustumChange ()
{
    float fNSqr = m_fFrustumN*m_fFrustumN;
    float fLSqr = m_fFrustumL*m_fFrustumL;
    float fRSqr = m_fFrustumR*m_fFrustumR;
    float fBSqr = m_fFrustumB*m_fFrustumB;
    float fTSqr = m_fFrustumT*m_fFrustumT;

    float fInvLength = 1.0f/Mathf::Sqrt(fNSqr + fLSqr);
    m_afCoeffL[0] = m_fFrustumN*fInvLength;
    m_afCoeffL[1] = -m_fFrustumL*fInvLength;

    fInvLength = 1.0f/Mathf::Sqrt(fNSqr + fRSqr);
    m_afCoeffR[0] = -m_fFrustumN*fInvLength;
    m_afCoeffR[1] = m_fFrustumR*fInvLength;

    fInvLength = 1.0f/Mathf::Sqrt(fNSqr + fBSqr);
    m_afCoeffB[0] = m_fFrustumN*fInvLength;
    m_afCoeffB[1] = -m_fFrustumB*fInvLength;

    fInvLength = 1.0f/Mathf::Sqrt(fNSqr + fTSqr);
    m_afCoeffT[0] = -m_fFrustumN*fInvLength;
    m_afCoeffT[1] = m_fFrustumT*fInvLength;
}
//----------------------------------------------------------------------------
void Camera::OnViewPortChange ()
{
}
//----------------------------------------------------------------------------
void Camera::OnFrameChange ()
{
    float fDdE = m_kDirection.Dot(m_kLocation);

    // left plane
    m_akWorldPlane[CAM_LEFT_PLANE].SetNormal(m_afCoeffL[0]*m_kLeft +
        m_afCoeffL[1]*m_kDirection);
    m_akWorldPlane[CAM_LEFT_PLANE].SetConstant(
        m_kLocation.Dot(m_akWorldPlane[CAM_LEFT_PLANE].GetNormal()));

    // right plane
    m_akWorldPlane[CAM_RIGHT_PLANE].SetNormal(m_afCoeffR[0]*m_kLeft +
        m_afCoeffR[1]*m_kDirection);
    m_akWorldPlane[CAM_RIGHT_PLANE].SetConstant( 
        m_kLocation.Dot(m_akWorldPlane[CAM_RIGHT_PLANE].GetNormal()));

    // bottom plane
    m_akWorldPlane[CAM_BOTTOM_PLANE].SetNormal(m_afCoeffB[0]*m_kUp +
        m_afCoeffB[1]*m_kDirection);
    m_akWorldPlane[CAM_BOTTOM_PLANE].SetConstant( 
        m_kLocation.Dot(m_akWorldPlane[CAM_BOTTOM_PLANE].GetNormal()));

    // top plane
    m_akWorldPlane[CAM_TOP_PLANE].SetNormal(m_afCoeffT[0]*m_kUp +
        m_afCoeffT[1]*m_kDirection);
    m_akWorldPlane[CAM_TOP_PLANE].SetConstant(
        m_kLocation.Dot(m_akWorldPlane[CAM_TOP_PLANE].GetNormal()));

    // far plane
    m_akWorldPlane[CAM_FAR_PLANE].SetNormal(-m_kDirection);
    m_akWorldPlane[CAM_FAR_PLANE].SetConstant(-(fDdE + m_fFrustumF));

    // near plane
    m_akWorldPlane[CAM_NEAR_PLANE].SetNormal(m_kDirection);
    m_akWorldPlane[CAM_NEAR_PLANE].SetConstant(fDdE + m_fFrustumN);
}
//----------------------------------------------------------------------------
void Camera::Update ()
{
    OnFrustumChange();
    OnViewPortChange();
    OnFrameChange();
}
//----------------------------------------------------------------------------
void Camera::PushPlane (const Plane3f& rkPlane)
{
    if ( m_iPlaneQuantity < CAM_MAX_WORLD_PLANES )
        m_akWorldPlane[m_iPlaneQuantity++] = rkPlane;
}
//----------------------------------------------------------------------------
void Camera::PopPlane ()
{
    if ( m_iPlaneQuantity > CAM_FRUSTUM_PLANES )
    {
        // frustum planes may not be removed from the stack
        m_iPlaneQuantity--;
    }
}
//----------------------------------------------------------------------------
bool Camera::Culled (const Bound& kWorldBound)
{
    // TO DO.  This code does plane-at-a-time culling.  It is possible that
    // the bounding sphere is outside the visibility region without being
    // totally outside one of the planes.  Change this to be an exact
    // intersection test between sphere and convex polyhedron.

    // start with last pushed plane (potentially the most restrictive plane)
    int iP = m_iPlaneQuantity - 1;
    unsigned int uiMask = 1 << iP;

    for (int i = 0; i < m_iPlaneQuantity; i++, iP--, uiMask >>= 1)
    {
        if ( m_uiPlaneState & uiMask )
        {
            Plane3f::Side eSide = kWorldBound.WhichSide(m_akWorldPlane[iP]);

            if ( eSide == Plane3f::NEGATIVE_SIDE )
            {
                // Object is on negative side.  Cull it.
                return true;
            }

            if ( eSide == Plane3f::POSITIVE_SIDE )
            {
                // Object is on positive side of plane.  There is no need to
                // compare subobjects against this plane, so mark it as
                // inactive.
                m_uiPlaneState &= ~uiMask;
            }
        }
    }

    return false;
}
//----------------------------------------------------------------------------
bool Camera::Culled (int iVertexQuantity, const Vector3f* akVertex,
    bool bIgnoreNearPlane)
{
    // TO DO.  This code does plane-at-a-time culling.  It is possible that
    // the convex polygon is outside the visibility region without being
    // totally outside one of the planes.  Change this to be an exact
    // intersection test between convex polygon and convex polyhedron.
    //
    // The Boolean variable bIgnoreNearPlane should be set to 'true' when
    // the test polygon is a portal.  This avoids the situation when the
    // portal is in the view pyramid (eye+left/right/top/bottom), but is
    // between the eye and near plane.  In such a situation you do not want
    // the portal system to cull the portal.  This situation typically occurs
    // when the camera moves through the portal from current region to
    // adjacent region.

    // start with last pushed plane (potentially the most restrictive plane)
    int iP = m_iPlaneQuantity - 1;
    for (int i = 0; i < m_iPlaneQuantity; i++, iP--)
    {
        Plane3f& rkPlane = m_akWorldPlane[iP];
        if ( bIgnoreNearPlane && iP == CAM_NEAR_PLANE )
            continue;

        int iV;
        for (iV = 0; iV < iVertexQuantity; iV++)
        {
            Plane3f::Side eSide = rkPlane.WhichSide(akVertex[iV]);
            if ( eSide != Plane3f::NEGATIVE_SIDE )
            {
                // polygon is not totally outside this plane
                break;
            }
        }

        if ( iV == iVertexQuantity )
        {
            // polygon is totally outside this plane
            return true;
        }
    }

    return false;
}
//----------------------------------------------------------------------------
bool Camera::GetPickRay (int iX, int iY, int iWidth, int iHeight,
    Vector3f& rkOrigin, Vector3f& rkDirection) const
{
    float fPortX = ((float)(iWidth-1-iX))/(float)iWidth;
    if ( fPortX < m_fPortL || fPortX > m_fPortR )
        return false;

    float fPortY = ((float)(iHeight-1-iY))/(float)iHeight;
    if ( fPortY < m_fPortB || fPortY > m_fPortT )
        return false;

    float fViewX = (1.0f-fPortX)*m_fFrustumL + fPortX*m_fFrustumR;
    float fViewY = (1.0f-fPortY)*m_fFrustumB + fPortY*m_fFrustumT;

    rkOrigin = m_kLocation;
    rkDirection = m_fFrustumN*m_kDirection+fViewX*m_kLeft+fViewY*m_kUp;
    rkDirection.Normalize();
    return true;
}
//----------------------------------------------------------------------------
bool Camera::GetPickRayOrtho (int iX, int iY, int iWidth, int iHeight,
    Vector3f& rkOrigin, Vector3f& rkDirection) const
{
    float fPortX = ((float)(iWidth-1-iX))/(float)iWidth;
    if ( fPortX < m_fPortL || fPortX > m_fPortR )
        return false;
    float fPortY = ((float)(iHeight-1-iY))/(float)iHeight;
    if ( fPortY < m_fPortB || fPortY > m_fPortT )
        return false;
    float fViewX = (1.0f-fPortX)*m_fFrustumL + fPortX*m_fFrustumR;
    float fViewY = (1.0f-fPortY)*m_fFrustumB + fPortY*m_fFrustumT;
    rkOrigin = m_kLocation + m_fFrustumN*m_kDirection +
        fViewX*m_kLeft + fViewY*m_kUp;
    rkDirection = m_kDirection;
    return true;
}
//---------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* Camera::Factory (Stream& rkStream)
{
    Camera* pkObject = new Camera;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void Camera::Load (Stream& rkStream, Stream::Link* pkLink)
{
    Object::Load(rkStream,pkLink);

    // native data
    StreamRead(rkStream,m_fFrustumN);
    StreamRead(rkStream,m_fFrustumF);
    StreamRead(rkStream,m_fFrustumL);
    StreamRead(rkStream,m_fFrustumR);
    StreamRead(rkStream,m_fFrustumT);
    StreamRead(rkStream,m_fFrustumB);
    StreamRead(rkStream,m_fPortL);
    StreamRead(rkStream,m_fPortR);
    StreamRead(rkStream,m_fPortT);
    StreamRead(rkStream,m_fPortB);
    StreamRead(rkStream,m_kLocation);
    StreamRead(rkStream,m_kLeft);
    StreamRead(rkStream,m_kUp);
    StreamRead(rkStream,m_kDirection);

    StreamRead(rkStream,m_iPlaneQuantity);
    for (int i = 0; i < m_iPlaneQuantity; i++)
        StreamRead(rkStream,m_akWorldPlane[i]);

    if ( rkStream.GetVersion() >= Version(1,7) )
    {
        StreamReadBool(rkStream,m_bUsePerspective);
        StreamReadBool(rkStream,m_bActive);
    }
    else
    {
        m_bUsePerspective = true;
        m_bActive = false;
    }
}
//----------------------------------------------------------------------------
void Camera::Link (Stream& rkStream, Stream::Link* pkLink)
{
    Object::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool Camera::Register (Stream& rkStream)
{
    return Object::Register(rkStream);
}
//----------------------------------------------------------------------------
void Camera::Save (Stream& rkStream)
{
    Object::Save(rkStream);

    // native data
    StreamWrite(rkStream,m_fFrustumN);
    StreamWrite(rkStream,m_fFrustumF);
    StreamWrite(rkStream,m_fFrustumL);
    StreamWrite(rkStream,m_fFrustumR);
    StreamWrite(rkStream,m_fFrustumT);
    StreamWrite(rkStream,m_fFrustumB);
    StreamWrite(rkStream,m_fPortL);
    StreamWrite(rkStream,m_fPortR);
    StreamWrite(rkStream,m_fPortT);
    StreamWrite(rkStream,m_fPortB);
    StreamWrite(rkStream,m_kLocation);
    StreamWrite(rkStream,m_kLeft);
    StreamWrite(rkStream,m_kUp);
    StreamWrite(rkStream,m_kDirection);

    StreamWrite(rkStream,m_iPlaneQuantity);
    for (int i = 0; i < m_iPlaneQuantity; i++)
        StreamWrite(rkStream,m_akWorldPlane[i]);

    StreamWriteBool(rkStream,m_bUsePerspective);
    StreamWriteBool(rkStream,m_bActive);
}
//----------------------------------------------------------------------------
StringTree* Camera::SaveStrings ()
{
    StringTree* pkTree = new StringTree(9,0,1,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));

    char acDummy[512];
    sprintf(acDummy,"n: %f, f:%f, l:%f, r:%f, t:%f, b:%f",m_fFrustumN,
        m_fFrustumF,m_fFrustumL,m_fFrustumR,m_fFrustumT,m_fFrustumB);
    pkTree->SetString(1,MakeString("frustum =",acDummy));

    sprintf(acDummy,"l: %f, r:%f, t:%f, b:%f",m_fPortL,m_fPortR,m_fPortT,
        m_fPortB);
    pkTree->SetString(2,MakeString("viewport =",acDummy));

    pkTree->SetString(3,MakeString("location =",m_kLocation));
    pkTree->SetString(4,MakeString("left vec =",m_kLeft));
    pkTree->SetString(5,MakeString("up vec =",m_kUp));
    pkTree->SetString(6,MakeString("direction vec =",m_kDirection));
    pkTree->SetString(7,MakeString("use perspective =",m_bUsePerspective));
    pkTree->SetString(8,MakeString("active =",m_bActive));

    // children
    pkTree->SetChild(0,Object::SaveStrings());

    return pkTree;
}
//----------------------------------------------------------------------------
int Camera::GetMemoryUsed () const
{
    int iBaseSize = sizeof(Camera) - sizeof(Object);
    int iTotalSize = iBaseSize + Object::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int Camera::GetDiskUsed () const
{
    return Object::GetDiskUsed() +
        sizeof(m_fFrustumN) +
        sizeof(m_fFrustumF) +
        sizeof(m_fFrustumL) +
        sizeof(m_fFrustumR) +
        sizeof(m_fFrustumT) +
        sizeof(m_fFrustumB) +
        sizeof(m_fPortL) +
        sizeof(m_fPortR) +
        sizeof(m_fPortT) +
        sizeof(m_fPortB) +
        sizeof(m_kLocation) +
        sizeof(m_kLeft) +
        sizeof(m_kUp) +
        sizeof(m_kDirection) +
        sizeof(m_iPlaneQuantity) +
        m_iPlaneQuantity*sizeof(m_akWorldPlane[0]) +
        StreamBytesBool(m_bUsePerspective) +
        StreamBytesBool(m_bActive);
}
//----------------------------------------------------------------------------


