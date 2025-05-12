// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntrLin3Con3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (const Line3<Real>& rkLine,
    const Cone3<Real>& rkCone, int& riQuantity, Vector3<Real> akPoint[2])
{
    // set up quadratic Q(t) = c2*t^2 + 2*c1*t + c0
    Real fAdD = rkCone.Axis().Dot(rkLine.Direction());
    Real fDdD = rkLine.Direction().Dot(rkLine.Direction());
    Real fCosSqr = rkCone.CosAngle()*rkCone.CosAngle();
    Vector3<Real> kE = rkLine.Origin() - rkCone.Vertex();
    Real fAdE = rkCone.Axis().Dot(kE);
    Real fDdE = rkLine.Direction().Dot(kE);
    Real fEdE = kE.Dot(kE);
    Real fC2 = fAdD*fAdD - fCosSqr*fDdD;
    Real fC1 = fAdD*fAdE - fCosSqr*fDdE;
    Real fC0 = fAdE*fAdE - fCosSqr*fEdE;

    // Solve the quadratic.  Keep only those X for which Dot(A,X-V) > 0.
    if ( Math<Real>::FAbs(fC2) >= Math<Real>::EPSILON )
    {
        // c2 != 0
        Real fDiscr = fC1*fC1 - fC0*fC2;
        if ( fDiscr < (Real)0.0 )
        {
            // no real roots
            riQuantity = 0;
            return false;
        }
        else if ( fDiscr > (Real)0.0 )
        {
            // two distinct real roots
            Real fRoot = Math<Real>::Sqrt(fDiscr);
            Real fInvC2 = ((Real)1.0)/fC2;
            riQuantity = 0;

            Real fT = (-fC1 - fRoot)*fInvC2;
            akPoint[riQuantity] = rkLine.Origin() + fT*rkLine.Direction();
            kE = akPoint[riQuantity] - rkCone.Vertex();
            if ( kE.Dot(rkCone.Axis()) > (Real)0.0 )
                riQuantity++;

            fT = (-fC1 + fRoot)*fInvC2;
            akPoint[riQuantity] = rkLine.Origin() + fT*rkLine.Direction();
            kE = akPoint[riQuantity] - rkCone.Vertex();
            if ( kE.Dot(rkCone.Axis()) > (Real)0.0 )
                riQuantity++;

            return true;
        }
        else
        {
            // one repeated real root
            akPoint[0] = rkLine.Origin() - (fC1/fC2)*rkLine.Direction();
            kE = akPoint[0] - rkCone.Vertex();
            if ( kE.Dot(rkCone.Axis()) > (Real)0.0 )
                riQuantity = 1;
            else
                riQuantity = 0;

            return true;
        }
    }
    else if ( Math<Real>::FAbs(fC1) >= Math<Real>::EPSILON )
    {
        // c2 = 0, c1 != 0
        akPoint[0] = rkLine.Origin() - 
            (((Real)0.5)*fC0/fC1)*rkLine.Direction();
        kE = akPoint[0] - rkCone.Vertex();
        if ( kE.Dot(rkCone.Axis()) > (Real)0.0 )
            riQuantity = 1;
        else
            riQuantity = 0;

        return true;
    }
    else if ( Math<Real>::FAbs(fC0) >= Math<Real>::EPSILON )
    {
        // c2 = c1 = 0, c0 != 0
        return false;
    }
    else
    {
        // c2 = c1 = c0 = 0, cone contains ray V+t*D where V is cone vertex
        // and D is the line direction.
        riQuantity = -1;
        return true;
    }
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM bool FindIntersection<float> (const Line3<float>&,
    const Cone3<float>&, int&, Vector3<float>[2]);

template WML_ITEM bool FindIntersection<double> (const Line3<double>&,
    const Cone3<double>&, int&, Vector3<double>[2]);
}
//----------------------------------------------------------------------------
