// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntrTri3Con3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Triangle3<Real>& rkTri,
    const Cone3<Real>& rkCone)
{
    // NOTE.  The following quantities computed in this function can be
    // precomputed and stored in the cone and triangle classes as an
    // optimization.
    //   1. The cone squared cosine.
    //   2. The triangle squared edge lengths |E0|^2 and |E1|^2.
    //   3. The third triangle edge E2 = E1 - E0 and squared length |E2|^2.
    //   4. The triangle normal N = Cross(E0,E1) or unitized normal
    //      N = Cross(E0,E1)/Length(Cross(E0,E1)).

    // triangle is <P0,P1,P2>, edges are E0 = P1-P0, E1=P2-P0
    int iOnConeSide = 0;
    Real fP0Test, fP1Test, fP2Test, fAdE, fEdE, fEdD, fC1, fC2;

    Real fCosSqr = rkCone.CosAngle()*rkCone.CosAngle();

    // test vertex P0
    Vector3<Real> kDiff0 = rkTri.Origin() - rkCone.Vertex();
    Real fAdD0 = rkCone.Axis().Dot(kDiff0);
    if ( fAdD0 >= (Real)0.0 )
    {
        // P0 is on cone side of plane
        fP0Test = fAdD0*fAdD0 - fCosSqr*(kDiff0.Dot(kDiff0));
        if ( fP0Test >= (Real)0.0 )
        {
            // P0 is inside the cone
            return true;
        }
        else
        {
            // P0 is outside the cone, but on cone side of plane
            iOnConeSide |= 1;
        }
    }
    // else P0 is not on cone side of plane

    // test vertex P1
    Vector3<Real> kDiff1 = kDiff0 + rkTri.Edge0();
    Real fAdD1 = rkCone.Axis().Dot(kDiff1);
    if ( fAdD1 >= (Real)0.0 )
    {
        // P1 is on cone side of plane
        fP1Test = fAdD1*fAdD1 - fCosSqr*(kDiff1.Dot(kDiff1));
        if ( fP1Test >= (Real)0.0 )
        {
            // P1 is inside the cone
            return true;
        }
        else
        {
            // P1 is outside the cone, but on cone side of plane
            iOnConeSide |= 2;
        }
    }
    // else P1 is not on cone side of plane

    // test vertex P2
    Vector3<Real> kDiff2 = kDiff0 + rkTri.Edge1();
    Real fAdD2 = rkCone.Axis().Dot(kDiff2);
    if ( fAdD2 >= (Real)0.0 )
    {
        // P2 is on cone side of plane
        fP2Test = fAdD2*fAdD2 - fCosSqr*(kDiff2.Dot(kDiff2));
        if ( fP2Test >= (Real)0.0 )
        {
            // P2 is inside the cone
            return true;
        }
        else
        {
            // P2 is outside the cone, but on cone side of plane
            iOnConeSide |= 4;
        }
    }
    // else P2 is not on cone side of plane

    // test edge <P0,P1> = E0
    if ( iOnConeSide & 3 )
    {
        fAdE = fAdD1 - fAdD0;
        fEdE = rkTri.Edge0().Dot(rkTri.Edge0());
        fC2 = fAdE*fAdE - fCosSqr*fEdE;
        if ( fC2 < (Real)0.0 )
        {
            fEdD = rkTri.Edge0().Dot(kDiff0);
            fC1 = fAdE*fAdD0 - fCosSqr*fEdD;
            if ( iOnConeSide & 1 )
            {
                if ( iOnConeSide & 2 )
                {
                    // <P0,P1> fully on cone side of plane, fC0 = fP0Test
                    if ( (Real)0.0 <= fC1 && fC1 <= -fC2
                    &&   fC1*fC1 >= fP0Test*fC2 )
                    {
                        return true;
                    }
                }
                else
                {
                    // P0 on cone side (Dot(A,P0-V) >= 0),
                    // P1 on opposite side (Dot(A,P1-V) <= 0)
                    // (Dot(A,E0) <= 0), fC0 = fP0Test
                    if ( (Real)0.0 <= fC1 && fC2*fAdD0 <= fC1*fAdE
                    &&   fC1*fC1 >= fP0Test*fC2 )
                    {
                        return true;
                    }
                }
            }
            else
            {
                // P1 on cone side (Dot(A,P1-V) >= 0),
                // P0 on opposite side (Dot(A,P0-V) <= 0)
                // (Dot(A,E0) >= 0), fC0 = fP0Test (needs calculating)
                if ( fC1 <= -fC2 && fC2*fAdD0 <= fC1*fAdE )
                {
                    fP0Test = fAdD0*fAdD0 - fCosSqr*(kDiff0.Dot(kDiff0));
                    if ( fC1*fC1 >= fP0Test*fC2 )
                        return true;
                }
            }
        }
    }
    // else <P0,P1> does not intersect cone half space

    // test edge <P0,P2> = E1
    if ( iOnConeSide & 5 )
    {
        fAdE = fAdD2 - fAdD0;
        fEdE = rkTri.Edge1().Dot(rkTri.Edge1());
        fC2 = fAdE*fAdE - fCosSqr*fEdE;
        if ( fC2 < (Real)0.0 )
        {
            fEdD = rkTri.Edge1().Dot(kDiff0);
            fC1 = fAdE*fAdD0 - fCosSqr*fEdD;
            if ( iOnConeSide & 1 )
            {
                if ( iOnConeSide & 4 )
                {
                    // <P0,P2> fully on cone side of plane, fC0 = fP0Test
                    if ( (Real)0.0 <= fC1 && fC1 <= -fC2
                    &&   fC1*fC1 >= fP0Test*fC2 )
                    {
                        return true;
                    }
                }
                else
                {
                    // P0 on cone side (Dot(A,P0-V) >= 0),
                    // P2 on opposite side (Dot(A,P2-V) <= 0)
                    // (Dot(A,E1) <= 0), fC0 = fP0Test
                    if ( (Real)0.0 <= fC1 && fC2*fAdD0 <= fC1*fAdE
                    &&   fC1*fC1 >= fP0Test*fC2 )
                    {
                        return true;
                    }
                }
            }
            else
            {
                // P2 on cone side (Dot(A,P2-V) >= 0),
                // P0 on opposite side (Dot(A,P0-V) <= 0)
                // (Dot(A,E1) >= 0), fC0 = fP0Test (needs calculating)
                if ( fC1 <= -fC2 && fC2*fAdD0 <= fC1*fAdE )
                {
                    fP0Test = fAdD0*fAdD0 - fCosSqr*(kDiff0.Dot(kDiff0));
                    if ( fC1*fC1 >= fP0Test*fC2 )
                        return true;
                }
            }
        }
    }
    // else <P0,P2> does not intersect cone half space

    // test edge <P1,P2> = E1-E0 = E2
    if ( iOnConeSide & 6 )
    {
        Vector3<Real> kE2 = rkTri.Edge1() - rkTri.Edge0();
        fAdE = fAdD2 - fAdD1;
        fEdE = kE2.Dot(kE2);
        fC2 = fAdE*fAdE - fCosSqr*fEdE;
        if ( fC2 < (Real)0.0 )
        {
            fEdD = kE2.Dot(kDiff1);
            fC1 = fAdE*fAdD1 - fCosSqr*fEdD;
            if ( iOnConeSide & 2 )
            {
                if ( iOnConeSide & 4 )
                {
                    // <P1,P2> fully on cone side of plane, fC0 = fP1Test
                    if ( (Real)0.0 <= fC1 && fC1 <= -fC2
                    &&   fC1*fC1 >= fP1Test*fC2 )
                    {
                        return true;
                    }
                }
                else
                {
                    // P1 on cone side (Dot(A,P1-V) >= 0),
                    // P2 on opposite side (Dot(A,P2-V) <= 0)
                    // (Dot(A,E2) <= 0), fC0 = fP1Test
                    if ( (Real)0.0 <= fC1 && fC2*fAdD1 <= fC1*fAdE
                    &&   fC1*fC1 >= fP1Test*fC2 )
                    {
                        return true;
                    }
                }
            }
            else
            {
                // P2 on cone side (Dot(A,P2-V) >= 0),
                // P1 on opposite side (Dot(A,P1-V) <= 0)
                // (Dot(A,E2) >= 0), fC0 = fP1Test (needs calculating)
                if ( fC1 <= -fC2 && fC2*fAdD1 <= fC1*fAdE )
                {
                    fP1Test = fAdD1*fAdD1 - fCosSqr*(kDiff1.Dot(kDiff1));
                    if ( fC1*fC1 >= fP1Test*fC2 )
                        return true;
                }
            }
        }
    }
    // else <P1,P2> does not intersect cone half space

    // Test triangle <P0,P1,P2>.  It is enough to handle only the case when
    // at least one Pi is on the cone side of the plane.  In this case and
    // after the previous testing, if the triangle intersects the cone, the
    // set of intersection must contain the point of intersection between
    // the cone axis and the triangle.
    if ( iOnConeSide > 0 )
    {
        Vector3<Real> kN = rkTri.Edge0().Cross(rkTri.Edge1());
        Real fNdA = kN.Dot(rkCone.Axis());
        Real fNdD = kN.Dot(kDiff0);
        Vector3<Real> kU = fNdD*rkCone.Axis() - fNdA*kDiff0;
        Vector3<Real> kNcU = kN.Cross(kU);

        Real fNcUdE0 = kNcU.Dot(rkTri.Edge0()), fNcUdE1, fNcUdE2, fNdN;
        if ( fNdA >= (Real)0.0 )
        {
            if ( fNcUdE0 <= (Real)0.0 )
            {
                fNcUdE1 = kNcU.Dot(rkTri.Edge1());
                if ( fNcUdE1 >= (Real)0.0 )
                {
                    fNcUdE2 = fNcUdE1 - fNcUdE0;
                    fNdN = kN.SquaredLength();
                    if ( fNcUdE2 <= fNdA*fNdN )
                        return true;
                }
            }
        }
        else
        {
            if ( fNcUdE0 >= (Real)0.0 )
            {
                fNcUdE1 = kNcU.Dot(rkTri.Edge1());
                if ( fNcUdE1 <= (Real)0.0 )
                {
                    fNcUdE2 = fNcUdE1 - fNcUdE0;
                    fNdN = kN.SquaredLength();
                    if ( fNcUdE2 >= fNdA*fNdN )
                        return true;
                }
            }
        }
    }

    return false;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM bool TestIntersection<float> (
    const Triangle3<float>&, const Cone3<float>&);

template WML_ITEM bool TestIntersection<double> (
    const Triangle3<double>&, const Cone3<double>&);
}
//----------------------------------------------------------------------------
