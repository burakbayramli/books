// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef ROTSPLINE_H
#define ROTSPLINE_H

// Kochanek-Bartels tension-continuity-bias spline interpolation adapted
// to quaternion interpolation.

#include "RotKey.h"

class RotSpline
{
public:
    RotSpline (int iNumKeys, RotKey* akKey);
    ~RotSpline ();

    Quaternionf Q (float fTime);

private:
    class SquadPoly
    {
    public:
        Quaternionf Q (float fU);

        // Time interval on which polynomial is valid, tmin <= t <= tmax.
        // The normalized time is u = (t - tmin)/(tmax - tmin).  The inverse
        // range 1/(tmax-tmin) is computed once and stored to avoid having to
        // use divisions during interpolation.
        float m_fTMin, m_fTMax, m_fTInvRange;

        // Q(u) = Squad(2u(1-u),Slerp(u,p,q),Slerp(u,a,b))
        Quaternionf m_kP, m_kA, m_kB, m_kQ;
    };

    int m_iNumPolys;
    SquadPoly* m_akPoly;
};

#endif
