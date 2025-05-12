// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef POSSPLINE_H
#define POSSPLINE_H

// Kochanek-Bartels tension-continuity-bias spline interpolation for
// positional data.

#include "PosKey.h"

class PosSpline
{
public:
    // The keys should be sorted by increasing time.
    PosSpline (int iNumKeys, PosKey* akKey);
    ~PosSpline ();

    // The interpolators clamp the time to the range of times in the N input
    // keys, [key[0].time,key[N-1].time].
    Vector3f Position (float fTime);      // X(t)
    Vector3f Velocity (float fTime);      // X'(t)
    Vector3f Acceleration (float fTime);  // X"(t)

    // length of the spline
    float Length (float fTime);
    float TotalLength ();

    // Evaluate position and derivatives by specifying arc length s along the
    // spline.  If L is the total length of the curve, then 0 <= s <= L is
    // required.
    Vector3f PositionAL (float fS);
    Vector3f VelocityAL (float fS);
    Vector3f AccelerationAL (float fS);

private:
    class Poly
    {
    public:
        Vector3f Position (float fU);      // P(u)
        Vector3f Velocity (float fU);      // P'(u)
        Vector3f Acceleration (float fU);  // P"(u)
        float Speed (float fU);
        float Length (float fU);

        // Time interval on which polynomial is valid, tmin <= t <= tmax.
        // The normalized time is u = (t - tmin)/(tmax - tmin).  The inverse
        // range 1/(tmax-tmin) is computed once and stored to avoid having to
        // use divisions during interpolation.
        float m_fTMin, m_fTMax, m_fTInvRange;

        // P(u) = C0 + u*C1 + u^2*C2 + u^3*C3,  0 <= u <= 1
        Vector3f m_akC[4];

        // Legendre polynomial degree 5 for numerical integration
        static float ms_afModRoot[5];
        static float ms_afModCoeff[5];
    };

    void DoPolyLookup (float fTime, int& riI, float& rfU);

    int m_iNumPolys;
    Poly* m_akPoly;

    // support for arc length parameterization of spline
    void InvertIntegral (float fS, int& riI, float& rfU);
    float* m_afLength;
    float m_fTotalLength;
};

#endif
