// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlQuadToQuadTransforms.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
HmQuadToSqr<Real>::HmQuadToSqr (const Vector2<Real>& rkP00,
    const Vector2<Real>& rkP10, const Vector2<Real>& rkP11,
    const Vector2<Real>& rkP01)
{
    // translate to origin
    m_kT = rkP00;
    Vector2<Real> kQ10 = rkP10 - rkP00;
    Vector2<Real> kQ11 = rkP11 - rkP00;
    Vector2<Real> kQ01 = rkP01 - rkP00;

    Matrix2<Real> kInvM(kQ10.X(),kQ01.X(),kQ10.Y(),kQ01.Y());
    m_kM = kInvM.Inverse();

    // compute where p11 is mapped to
    Vector2<Real> kCorner = m_kM*kQ11;  // = (a,b)

    // Compute homogeneous transform of quadrilateral
    // {(0,0),(1,0),(a,b),(0,1)} to square {(0,0),(1,0),(1,1),(0,1)}
    m_kG.X() = (kCorner.Y() - (Real)1.0)/kCorner.X();
    m_kG.Y() = (kCorner.X() - (Real)1.0)/kCorner.Y();
    m_kD.X() = (Real)1.0 + m_kG.X();
    m_kD.Y() = (Real)1.0 + m_kG.Y();
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real> HmQuadToSqr<Real>::Transform (const Vector2<Real>& rkP)
{
    Vector2<Real> kProd = m_kM*(rkP - m_kT);
    Real fInvDenom = ((Real)1.0)/((Real)1.0 + m_kG.Dot(kProd));
    Vector2<Real> kResult = fInvDenom*kProd;
    kResult.X() *= m_kD.X();
    kResult.Y() *= m_kD.Y();
    return kResult;
}
//----------------------------------------------------------------------------
template <class Real>
HmSqrToQuad<Real>::HmSqrToQuad (const Vector2<Real>& rkP00,
    const Vector2<Real>& rkP10, const Vector2<Real>& rkP11,
    const Vector2<Real>& rkP01)
{
    // translate to origin
    m_kT = rkP00;
    m_kM[0][0] = rkP10.X() - rkP00.X();
    m_kM[0][1] = rkP01.X() - rkP00.X();
    m_kM[1][0] = rkP10.Y() - rkP00.Y();
    m_kM[1][1] = rkP01.Y() - rkP00.Y();

    Matrix2<Real> kInvM = m_kM.Inverse();

    // find point which is mapped to p11
    Vector2<Real> kCorner = kInvM*(rkP11-rkP00);  // = (a,b)

    // compute homogeneous transform of square {(0,0),(1,0),(1,1),(0,1)} to
    // quadrilateral {(0,0),(1,0),(a,b),(0,1)}
    Real fInvDenom = ((Real)1.0)/(kCorner.X() + kCorner.Y() - (Real)1.0);
    m_kG.X() = fInvDenom*((Real)1.0 - kCorner.Y());
    m_kG.Y() = fInvDenom*((Real)1.0 - kCorner.X());
    m_kD.X() = fInvDenom*kCorner.X();
    m_kD.Y() = fInvDenom*kCorner.Y();
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real> HmSqrToQuad<Real>::Transform (const Vector2<Real>& rkP)
{
    Real fInvDenom = ((Real)1.0)/((Real)1.0 + m_kG.Dot(rkP));
    Vector2<Real> kResult(m_kD.X()*rkP.X(),m_kD.Y()*rkP.Y());
    Vector2<Real> kProd = m_kM*kResult;
    kResult.X() = fInvDenom*kProd.X() + m_kT.X();
    kResult.Y() = fInvDenom*kProd.Y() + m_kT.Y();
    return kResult;
}
//----------------------------------------------------------------------------
template <class Real>
BiQuadToSqr<Real>::BiQuadToSqr (const Vector2<Real>& rkP00,
    const Vector2<Real>& rkP10, const Vector2<Real>& rkP11,
    const Vector2<Real>& rkP01)
{
    m_kA = rkP00;
    m_kB = rkP10 - rkP00;
    m_kC = rkP01 - rkP00;
    m_kD = rkP11 + rkP00 - rkP10 - rkP01;
    m_fBCdet = m_kB.X()*m_kC.Y() - m_kB.Y()*m_kC.X();
    assert( Math<Real>::FAbs(m_fBCdet) >= Math<Real>::EPSILON );
        // else quad degenerate
    m_fCDdet = m_kC.Y()*m_kD.X() - m_kC.X()*m_kD.Y();
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real> BiQuadToSqr<Real>::Transform (const Vector2<Real>& rkP)
{
    Vector2<Real> kDiff = m_kA - rkP;
    Real fABdet = kDiff.Y()*m_kB.X()-kDiff.X()*m_kB.Y();
    Real fADdet = kDiff.Y()*m_kD.X()-kDiff.X()*m_kD.Y();

    Real fA = m_fCDdet;
    Real fB = fADdet + m_fBCdet;
    Real fC = fABdet;

    Vector2<Real> kResult;

    if ( Math<Real>::FAbs(fA) >= Math<Real>::EPSILON )
    {
        // t-equation is quadratic
        Real fDiscr = Math<Real>::Sqrt(Math<Real>::FAbs(fB*fB -
            ((Real)4.0)*fA*fC));
        kResult.Y() = (-fB + fDiscr)/(((Real)2.0)*fA);
        if ( kResult.Y() < (Real)0.0 || kResult.Y() > (Real)1.0 )
        {
            kResult.Y() = (-fB - fDiscr)/(((Real)2.0)*fA);
            if ( kResult.Y() < (Real)0.0 || kResult.Y() > (Real)1.0 )
            {
                // point p not inside quadrilateral, return invalid result
                return Vector2<Real>(-(Real)1.0,-(Real)1.0);
            }
        }
    }
    else
    {
        // t-equation is linear
        kResult.Y() = -fC/fB;
    }

    kResult.X() = -(kDiff.X() + kResult.Y()*m_kC.X())/(m_kB.X() +
        kResult.Y()*m_kD.X());

    return kResult;
}
//----------------------------------------------------------------------------
template <class Real>
BiSqrToQuad<Real>::BiSqrToQuad (const Vector2<Real>& rkP00,
    const Vector2<Real>& rkP10, const Vector2<Real>& rkP11,
    const Vector2<Real>& rkP01)
{
    m_kS00 = rkP00;
    m_kS10 = rkP10;
    m_kS11 = rkP11;
    m_kS01 = rkP01;
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real> BiSqrToQuad<Real>::Transform (const Vector2<Real>& rkP)
{
    Vector2<Real> kOmP((Real)1.0-rkP.X(),(Real)1.0-rkP.Y());
    Vector2<Real> kResult;
    kResult.X() = kOmP.Y()*(kOmP.X()*m_kS00.X() + rkP.X()*m_kS10.X()) +
        rkP.Y()*(kOmP.X()*m_kS01.X() + rkP.X()*m_kS11.X());
    kResult.Y() = kOmP.Y()*(kOmP.X()*m_kS00.Y() + rkP.X()*m_kS10.Y()) +
        rkP.Y()*(kOmP.X()*m_kS01.Y() + rkP.X()*m_kS11.Y());
    return kResult;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM HmQuadToSqr<float>;
template class WML_ITEM HmQuadToSqr<double>;
template class WML_ITEM HmSqrToQuad<float>;
template class WML_ITEM HmSqrToQuad<double>;
template class WML_ITEM BiQuadToSqr<float>;
template class WML_ITEM BiQuadToSqr<double>;
template class WML_ITEM BiSqrToQuad<float>;
template class WML_ITEM BiSqrToQuad<double>;
}
//----------------------------------------------------------------------------
