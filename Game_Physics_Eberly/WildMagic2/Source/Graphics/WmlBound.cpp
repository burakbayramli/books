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
using namespace Wml;

//----------------------------------------------------------------------------
Bound& Bound::operator+= (const Bound& rkBound)
{
    static const float s_fTolerance = 1e-06f;

    // difference of current sphere center and input sphere center
    Vector3f kDiff = m_kCenter - rkBound.m_kCenter;
    
    float fLengthSqr = kDiff.SquaredLength();
    float fDeltaRad = rkBound.m_fRadius - m_fRadius;
    float fDeltaRadSqr = fDeltaRad*fDeltaRad;
    float fLength, fAlpha;
    
    if ( fDeltaRad >= 0.0f )
    {
        if ( fDeltaRadSqr >= fLengthSqr )
        {
            // rkBound's sphere encloses 'this' sphere
            m_kCenter = rkBound.m_kCenter;
            m_fRadius = rkBound.m_fRadius;
        }
        else
        {
            // this' sphere does not enclose rkBound sphere
            fLength = Mathf::Sqrt(fLengthSqr);
            if ( fLength > s_fTolerance )
            {
                fAlpha = (fLength - fDeltaRad)/(2.0f*fLength);
                m_kCenter = rkBound.m_kCenter + fAlpha*kDiff;
            }
            m_fRadius = 0.5f*(rkBound.m_fRadius+fLength+m_fRadius);
        }
    }
    else if ( fDeltaRadSqr < fLengthSqr )
    {
        // this' sphere does not enclose rkBound sphere
        fLength = Mathf::Sqrt(fLengthSqr);
        if ( fLength > s_fTolerance )
        {
            fAlpha = (fLength - fDeltaRad)/(2.0f*fLength);
            m_kCenter = rkBound.m_kCenter + fAlpha*kDiff;
        }
        m_fRadius = 0.5f*(rkBound.m_fRadius+fLength+m_fRadius);
    }
    // else 'this' sphere encloses rkBound sphere

    return *this;
}
//----------------------------------------------------------------------------
Bound Bound::TransformBy (const Matrix3f& rkRotate,
    const Vector3f& rkTranslate, float fScale) const
{
    Bound bound;
    bound.m_kCenter = fScale*(rkRotate*m_kCenter) + rkTranslate;
    bound.m_fRadius = fScale*m_fRadius;
    return bound;
}
//----------------------------------------------------------------------------
void Bound::ComputeFromData (int iQuantity, const Vector3f* akData)
{
    if ( iQuantity <= 0 )
    {
        m_kCenter = Vector3f::ZERO;
        m_fRadius = 0.0f;
        return;
    }

    // compute the axis-aligned box containing the data
    Vector3f kMin = akData[0], kMax = kMin;
    int i;
    for (i = 1; i < iQuantity; i++) 
    {
        if ( kMin[0] > akData[i][0] )
            kMin[0] = akData[i][0];
        if ( kMin[1] > akData[i][1] )
            kMin[1] = akData[i][1];
        if ( kMin[2] > akData[i][2] )
            kMin[2] = akData[i][2];
        if ( kMax[0] < akData[i][0] )
            kMax[0] = akData[i][0];
        if ( kMax[1] < akData[i][1] )
            kMax[1] = akData[i][1];
        if ( kMax[2] < akData[i][2] )
            kMax[2] = akData[i][2];
    }

    // sphere center is the axis-aligned box center
    m_kCenter = 0.5f*(kMin+kMax);

    // compute the radius
    float fRadiusSqr = 0.0f;
    for (i = 0; i < iQuantity; i++) 
    {
        Vector3f kDiff = akData[i] - m_kCenter;
        float fLengthSqr = kDiff.SquaredLength();
        if ( fLengthSqr > fRadiusSqr )
            fRadiusSqr = fLengthSqr;
    }
    m_fRadius = Mathf::Sqrt(fRadiusSqr);
}
//----------------------------------------------------------------------------
Plane3f::Side Bound::WhichSide (const Plane3f& rkPlane) const
{
    float fPseudoDistance = rkPlane.DistanceTo(m_kCenter);

    if ( fPseudoDistance <= -m_fRadius)
    {
        return Plane3f::NEGATIVE_SIDE;
    }
    else if ( fPseudoDistance >= m_fRadius)
    {
        return Plane3f::POSITIVE_SIDE;
    }
    else
    {
        return Plane3f::NO_SIDE;
    }
}
//----------------------------------------------------------------------------
bool Bound::TestIntersection (const Vector3f& rkOrigin,
    const Vector3f& rkDirection) const
{
    // The ray is P+t*D for t >= 0 (P = rkOrigin, D = rkDirection).  The
    // sphere has center C and radius R.  The quadratic for the intersection
    // is t^2 + 2*a1*t + a0 = 0 where a0 = |P-C|^2 - R^2 and a1 = Dot(D,P-C).

    Vector3f kDiff = rkOrigin - m_kCenter;
    float fA0 = kDiff.Dot(kDiff) - m_fRadius*m_fRadius;
    if ( fA0 <= 0.0f )
    {
        // P is inside the sphere
        return true;
    }
    // else: P is outside the sphere

    float fA1 = rkDirection.Dot(kDiff);
    if ( fA1 >= 0.0f )
    {
        // acute angle between P-C and D, C is "behind" the ray
        return false;
    }

    // quadratic has a real root if discriminant is negative
    return fA1*fA1 >= fA0;
}
//----------------------------------------------------------------------------
