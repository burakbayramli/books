// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlImplicitSurface.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
ImplicitSurface<Real>::ImplicitSurface (Function oF, Function aoDF[3],
    Function aoD2F[6])
{
    m_oF = oF;
    memcpy(m_aoDF,aoDF,3*sizeof(Function));
    memcpy(m_aoD2F,aoD2F,6*sizeof(Function));
}
//----------------------------------------------------------------------------
template <class Real>
bool ImplicitSurface<Real>::IsOnSurface (Real fX, Real fY, Real fZ,
    Real fTolerance) const
{
    return Math<Real>::FAbs(m_oF(fX,fY,fZ)) <= fTolerance;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real> ImplicitSurface<Real>::GetGradient (Real fX, Real fY, Real fZ)
    const
{
    Real fDx = m_aoDF[0](fX,fY,fZ);
    Real fDy = m_aoDF[1](fX,fY,fZ);
    Real fDz = m_aoDF[2](fX,fY,fZ);
    return Vector3<Real>(fDx,fDy,fDz);
}
//----------------------------------------------------------------------------
template <class Real>
Matrix3<Real> ImplicitSurface<Real>::GetHessian (Real fX, Real fY, Real fZ)
    const
{
    Real fDxx = m_aoD2F[0](fX,fY,fZ);
    Real fDxy = m_aoD2F[1](fX,fY,fZ);
    Real fDxz = m_aoD2F[2](fX,fY,fZ);
    Real fDyy = m_aoD2F[3](fX,fY,fZ);
    Real fDyz = m_aoD2F[4](fX,fY,fZ);
    Real fDzz = m_aoD2F[5](fX,fY,fZ);
    return Matrix3<Real>(fDxx,fDxy,fDxz,fDxy,fDyy,fDyz,fDxz,fDyz,fDzz);
}
//----------------------------------------------------------------------------
template <class Real>
void ImplicitSurface<Real>::GetFrame (Real fX, Real fY, Real fZ,
    Vector3<Real>& kTangent0, Vector3<Real>& kTangent1,
    Vector3<Real>& kNormal) const
{
    kNormal = GetGradient(fX,fY,fZ);

    if ( Math<Real>::FAbs(kNormal.X()) >= Math<Real>::FAbs(kNormal.Y())
    &&   Math<Real>::FAbs(kNormal.X()) >= Math<Real>::FAbs(kNormal.Z()) )
    {
        kTangent0.X() = -kNormal.Y();
        kTangent0.Y() = kNormal.X();
        kTangent0.Z() = (Real)0.0;
    }
    else
    {
        kTangent0.X() = (Real)0.0;
        kTangent0.Y() = kNormal.Z();
        kTangent0.Z() = -kNormal.Y();
    }

    kTangent0.Normalize();
    kTangent1 = kNormal.Cross(kTangent0);
}
//----------------------------------------------------------------------------
template <class Real>
bool ImplicitSurface<Real>::ComputePrincipalCurvatureInfo (Real fX, Real fY,
    Real fZ, Real& rfCurv0, Real& rfCurv1, Vector3<Real>& rkDir0,
    Vector3<Real>& rkDir1)
{
    // Principal curvatures and directions for implicitly defined surfaces
    // F(x,y,z) = 0.
    //
    // DF = (Fx,Fy,Fz), L = Length(DF)
    //
    // D^2 F = +-           -+
    //         | Fxx Fxy Fxz |
    //         | Fxy Fyy Fyz |
    //         | Fxz Fyz Fzz |
    //         +-           -+
    //
    // adj(D^2 F) = +-                                                 -+
    //              | Fyy*Fzz-Fyz*Fyz  Fyz*Fxz-Fxy*Fzz  Fxy*Fyz-Fxz*Fyy |
    //              | Fyz*Fxz-Fxy*Fzz  Fxx*Fzz-Fxz*Fxz  Fxy*Fxz-Fxx*Fyz |
    //              | Fxy*Fyz-Fxz*Fyy  Fxy*Fxz-Fxx*Fyz  Fxx*Fyy-Fxy*Fxy |
    //              +-                                                 -+
    //
    // Gaussian curvature = [DF^t adj(D^2 F) DF]/L^4
    // 
    // Mean curvature = 0.5*[trace(D^2 F)/L - (DF^t D^2 F DF)/L^3]

    // first derivatives
    Vector3<Real> kGradient = GetGradient(fX,fY,fZ);
    Real fL = kGradient.Length();
    const Real fTolerance = (Real)1e-08;
    if ( fL <= fTolerance )
        return false;

    Real fDxDx = kGradient.X()*kGradient.X();
    Real fDxDy = kGradient.X()*kGradient.Y();
    Real fDxDz = kGradient.X()*kGradient.Z();
    Real fDyDy = kGradient.Y()*kGradient.Y();
    Real fDyDz = kGradient.Y()*kGradient.Z();
    Real fDzDz = kGradient.Z()*kGradient.Z();

    Real fInvL = ((Real)1.0)/fL;
    Real fInvL2 = fInvL*fInvL;
    Real fInvL3 = fInvL*fInvL2;
    Real fInvL4 = fInvL2*fInvL2;

    // second derivatives
    Real fDxx = m_aoD2F[0](fX,fY,fZ);
    Real fDxy = m_aoD2F[1](fX,fY,fZ);
    Real fDxz = m_aoD2F[2](fX,fY,fZ);
    Real fDyy = m_aoD2F[3](fX,fY,fZ);
    Real fDyz = m_aoD2F[4](fX,fY,fZ);
    Real fDzz = m_aoD2F[5](fX,fY,fZ);

    // mean curvature
    Real fMCurv = ((Real)0.5)*fInvL3*(fDxx*(fDyDy+fDzDz) + fDyy*(fDxDx+fDzDz)
        + fDzz*(fDxDx+fDyDy)
        - ((Real)2.0)*(fDxy*fDxDy+fDxz*fDxDz+fDyz*fDyDz));

    // Gaussian curvature
    Real fGCurv = fInvL4*(fDxDx*(fDyy*fDzz-fDyz*fDyz)
        + fDyDy*(fDxx*fDzz-fDxz*fDxz) + fDzDz*(fDxx*fDyy-fDxy*fDxy)
        + ((Real)2.0)*(fDxDy*(fDxz*fDyz-fDxy*fDzz)
        + fDxDz*(fDxy*fDyz-fDxz*fDyy)
        + fDyDz*(fDxy*fDxz-fDxx*fDyz)));

    // solve for principal curvatures
    Real fDiscr = Math<Real>::Sqrt(Math<Real>::FAbs(fMCurv*fMCurv-fGCurv));
    rfCurv0 = fMCurv - fDiscr;
    rfCurv1 = fMCurv + fDiscr;

    Real fM00 = ((-(Real)1.0 + fDxDx*fInvL2)*fDxx)*fInvL + (fDxDy*fDxy)*fInvL3
        + (fDxDz*fDxz)*fInvL3;
    Real fM01 = ((-(Real)1.0 + fDxDx*fInvL2)*fDxy)*fInvL + (fDxDy*fDyy)*fInvL3
        + (fDxDz*fDyz)*fInvL3;
    Real fM02 = ((-(Real)1.0 + fDxDx*fInvL2)*fDxz)*fInvL + (fDxDy*fDyz)*fInvL3
        + (fDxDz*fDzz)*fInvL3;
    Real fM10 = (fDxDy*fDxx)*fInvL3 + ((-(Real)1.0 + fDyDy*fInvL2)*fDxy)*fInvL
        + (fDyDz*fDxz)*fInvL3;
    Real fM11 = (fDxDy*fDxy)*fInvL3 + ((-(Real)1.0 + fDyDy*fInvL2)*fDyy)*fInvL
        + (fDyDz*fDyz)*fInvL3;
    Real fM12 = (fDxDy*fDxz)*fInvL3 + ((-(Real)1.0 + fDyDy*fInvL2)*fDyz)*fInvL
        + (fDyDz*fDzz)*fInvL3;
    Real fM20 = (fDxDz*fDxx)*fInvL3 + (fDyDz*fDxy)*fInvL3 + ((-(Real)1.0
        + fDzDz*fInvL2)*fDxz)*fInvL;
    Real fM21 = (fDxDz*fDxy)*fInvL3 + (fDyDz*fDyy)*fInvL3 + ((-(Real)1.0
        + fDzDz*fInvL2)*fDyz)*fInvL;
    Real fM22 = (fDxDz*fDxz)*fInvL3 + (fDyDz*fDyz)*fInvL3 + ((-(Real)1.0
        + fDzDz*fInvL2)*fDzz)*fInvL;

    // solve for principal directions
    Real fTmp1 = fM00 + rfCurv0;
    Real fTmp2 = fM11 + rfCurv0;
    Real fTmp3 = fM22 + rfCurv0;

    Vector3<Real> akU[3];
    Real afLength[3];

    akU[0].X() = fM01*fM12-fM02*fTmp2;
    akU[0].Y() = fM02*fM10-fM12*fTmp1;
    akU[0].Z() = fTmp1*fTmp2-fM01*fM10;
    afLength[0] = akU[0].Length();

    akU[1].X() = fM01*fTmp3-fM02*fM21;
    akU[1].Y() = fM02*fM20-fTmp1*fTmp3;
    akU[1].Z() = fTmp1*fM21-fM01*fM20;
    afLength[1] = akU[1].Length();

    akU[2].X() = fTmp2*fTmp3-fM12*fM21;
    akU[2].Y() = fM12*fM20-fM10*fTmp3;
    akU[2].Z() = fM10*fM21-fM20*fTmp2;
    afLength[2] = akU[2].Length();

    int iMaxIndex = 0;
    Real fMax = afLength[0];
    if ( afLength[1] > fMax )
    {
        iMaxIndex = 1;
        fMax = afLength[1];
    }
    if ( afLength[2] > fMax )
        iMaxIndex = 2;

    Real fInvLength = ((Real)1.0)/afLength[iMaxIndex];
    akU[iMaxIndex] *= fInvLength;

    rkDir1 = akU[iMaxIndex];
    rkDir0 = rkDir1.UnitCross(kGradient);

    return true;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM ImplicitSurface<float>;
template class WML_ITEM ImplicitSurface<double>;
}
//----------------------------------------------------------------------------
