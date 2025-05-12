// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "RayTracer.h"

//----------------------------------------------------------------------------
RayTracer::RayTracer (Function oF, Gradient oDF, int iWidth, int iHeight)
    :
    m_kLocation(0.0f,0.0f,0.0f),
    m_kDirection(0.0f,0.0f,1.0f),
    m_kUp(0.0f,1.0f,0.0f),
    m_kRight(-1.0f,0.0f,0.0f)
{
    m_fNear = 1.0f;
    m_fFar = 10.0f;
    m_fHalfWidth = 0.5f;
    m_fHalfHeight = 0.5f;

    m_oF = oF;
    m_oDF = oDF;

    m_iWidth = iWidth;
    m_iHeight = iHeight;
    m_afImage = new float[m_iWidth*m_iHeight];
    m_afBlur = new float[m_iWidth*m_iHeight];
    memset(m_afImage,0,m_iWidth*m_iHeight*sizeof(float));
    memset(m_afBlur,0,m_iWidth*m_iHeight*sizeof(float));
}
//----------------------------------------------------------------------------
RayTracer::~RayTracer ()
{
    delete[] m_afImage;
    delete[] m_afBlur;
}
//----------------------------------------------------------------------------
void RayTracer::DrawSurface (int iMaxSample, const Vector3f& rkLightDir,
    bool bBlur)
{
    float fXMult = 2.0f/(m_iWidth-1.0f);
    float fYMult = 2.0f/(m_iHeight-1.0f);
    float fDS = (m_fFar - m_fNear)/(m_fNear*(iMaxSample - 1.0f));

    // generate a ray per pixel of the image
    for (int iY = 0; iY < m_iHeight; iY++)
    {
        float fY = m_fHalfHeight*(-1.0f + fYMult*iY);
        for (int iX = 0; iX < m_iWidth; iX++)
        {
            float fX = m_fHalfWidth*(-1.0f + fXMult*iX);

            // background is zero
            int iIndex = iX + m_iWidth*iY;
            m_afImage[iIndex] = 0.0f;

            // compute ray direction
            Vector3f kRayDir = m_fNear*m_kDirection + fX*m_kRight + fY*m_kUp;

            // start point of ray
            float fS0 = 1.0f;
            Vector3f kP0 = m_kLocation;
            float fF0 = m_oF(kP0);

            // sample the ray
            for (int i = 0; i < iMaxSample; i++)
            {
                // trace ray E+s*V where 1 <= s <= far/near
                float fS1 = 1.0f + fDS*i;
                Vector3f kP1 = m_kLocation + fS1*kRayDir;
                float fF1 = m_oF(kP1);
                if ( fF0*fF1 <= 0.0f )
                {
                    // Bisect [s0,s1] to find s for which F(E+s*V) = 0.  This
                    // approach is not accurate near the contour of an object
                    // since there will be multiple roots that are close
                    // together and the uniform ray sampling can miss these
                    // roots.  Either Newton's method should be used (or a
                    // hybrid of bisection and Newton's).
                    Vector3f kPos, kNormal;
                    FindSurface(fS0,fF0,kP0,fS1,fF1,kP1,kRayDir,kPos,kNormal);

                    // compute light intensity
                    float fDot = rkLightDir.Dot(kNormal);
                    m_afImage[iIndex] = ( fDot < 0.0f ? -fDot : 0.0f );
                    break;
                }

                // proceed to next sample interval
                fS0 = fS1;
                kP0 = kP1;
                fF0 = fF1;
            }
        }
    }

    if ( bBlur )
        BlurImage();
}
//----------------------------------------------------------------------------
void RayTracer::FindSurface (float fS0, float fF0, const Vector3f& rkP0,
    float fS1, float fF1, const Vector3f& rkP1, const Vector3f& rkRayDir,
    Vector3f& rkPos, Vector3f& rkNormal)
{
    // check for endpoint zeros
    const float fEpsilon = 1.0e-4f;  // TO DO:  allow user to set this
    if ( Mathf::FAbs(fF0) <= fEpsilon )
    {
        rkPos = rkP0;
        rkNormal = m_oDF(rkP0);
        rkNormal.Normalize();
        return;
    }
    if ( Mathf::FAbs(fF1) <= fEpsilon )
    {
        rkPos = rkP1;
        rkNormal = m_oDF(rkP1);
        rkNormal.Normalize();
        return;
    }

    // bisect the interval [s0,s1]
    const int iMax = 8;  // TO DO:  allow user to set this
    int i;
    for (i = 0; i < iMax; i++)
    {
        float fS = 0.5f*(fS0 + fS1);
        rkPos = m_kLocation + fS*rkRayDir;
        float fF = m_oF(rkPos);
        if ( Mathf::FAbs(fF) <= fEpsilon )
            break;
        
        if ( fF*fF0 < 0.0f )
        {
            fS1 = fS;
            fF1 = fF;
        }
        else
        {
            fS0 = fS;
            fF0 = fF;
        }
    }

    rkNormal = m_oDF(rkPos);
    rkNormal.Normalize();
}
//----------------------------------------------------------------------------
void RayTracer::BlurImage ()
{
    memcpy(m_afBlur,m_afImage,m_iWidth*m_iHeight*sizeof(float));

    for (int iY = 1; iY < m_iHeight-1; iY++)
    {
        for (int iX = 1; iX < m_iWidth-1; iX++)
        {
            float fValue = 0.0f;
            for (int iDY = -1; iDY <= 1; iDY++)
            {
                for (int iDX = -1; iDX <= 1; iDX++)
                    fValue += m_afImage[(iX+iDX)+m_iWidth*(iY+iDY)];
            }
            m_afBlur[iX+m_iWidth*iY] = fValue/9.0f;
        }
    }

    memcpy(m_afImage,m_afBlur,m_iWidth*m_iHeight*sizeof(float));
}
//----------------------------------------------------------------------------
