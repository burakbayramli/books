// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlMatrix3.h"
#include "WmlParametricSurface.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
ParametricSurface<Real>::ParametricSurface (Real fUMin, Real fUMax,
   Real fVMin, Real fVMax, bool bRectangular)
{
    assert( fUMin < fUMax && fVMin < fVMax );

    m_fUMin = fUMin;
    m_fUMax = fUMax;
    m_fVMin = fVMin;
    m_fVMax = fVMax;
    m_bRectangular = bRectangular;
}
//----------------------------------------------------------------------------
template <class Real>
Real ParametricSurface<Real>::GetUMin () const
{
    return m_fUMin;
}
//----------------------------------------------------------------------------
template <class Real>
Real ParametricSurface<Real>::GetUMax () const
{
    return m_fUMax;
}
//----------------------------------------------------------------------------
template <class Real>
Real ParametricSurface<Real>::GetVMin () const
{
    return m_fVMin;
}
//----------------------------------------------------------------------------
template <class Real>
Real ParametricSurface<Real>::GetVMax () const
{
    return m_fVMax;
}
//----------------------------------------------------------------------------
template <class Real>
bool ParametricSurface<Real>::IsRectangular () const
{
    return m_bRectangular;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real> ParametricSurface<Real>::GetTangent0 (Real fU, Real fV) const
{
    Vector3<Real> kTangent0 = GetDerivativeU(fU,fV);
    kTangent0.Normalize();
    return kTangent0;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real> ParametricSurface<Real>::GetTangent1 (Real fU, Real fV) const
{
    Vector3<Real> kTangent1 = GetDerivativeV(fU,fV);
    kTangent1.Normalize();
    return kTangent1;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real> ParametricSurface<Real>::GetNormal (Real fU, Real fV) const
{
    Vector3<Real> kTangent0 = GetDerivativeU(fU,fV);
    Vector3<Real> kTangent1 = GetDerivativeV(fU,fV);
    Vector3<Real> kNormal = kTangent0.UnitCross(kTangent1);
    return kNormal;
}
//----------------------------------------------------------------------------
template <class Real>
void ParametricSurface<Real>::GetFrame (Real fU, Real fV,
    Vector3<Real>& kPosition, Vector3<Real>& kTangent0,
    Vector3<Real>& kTangent1, Vector3<Real>& kNormal) const
{
    kPosition = GetPosition(fU,fV);
    kTangent0 = GetDerivativeU(fU,fV);
    kTangent1 = GetDerivativeV(fU,fV);
    kTangent0.Normalize();
    kTangent1.Normalize();
    kNormal = kTangent0.UnitCross(kTangent1);
}
//----------------------------------------------------------------------------
template <class Real>
void ParametricSurface<Real>::ComputePrincipalCurvatureInfo (Real fU, Real fV,
    Real& rfCurv0, Real& rfCurv1, Vector3<Real>& rkDir0,
    Vector3<Real>& rkDir1)
{
    // Tangents:  U = (x_s,y_s,z_s), V = (x_t,y_t,z_t)
    // Normal:    N = Cross(U,V)/Length(Cross(U,V))
    // Metric Tensor:    G = +-                  -+
    //                       | Dot(U,U)  Dot(U,V) |
    //                       | Dot(V,U)  Dot(V,V) |
    //                       +-                  -+
    //
    // Curvature Tensor:  B = +-                        -+
    //                        | -Dot(N,U_s)  -Dot(N,U_t) |
    //                        | -Dot(N,V_s)  -Dot(N,V_t) |
    //                        +-                        -+
    //
    // Principal curvatures k are the generalized eigenvalues of
    //
    //     Bw = kGw
    //
    // If k is a curvature and w=(a,b) is the corresponding solution to
    // Bw=kGw, then the principal direction as a 3D vector is d = a*U+b*V.
    //
    // Let k1 and k2 be the principal curvatures.  The mean curvature
    // is (k1+k2)/2 and the Gaussian curvature is k1*k2.

    // derivatives
    Vector3<Real> kDerU = GetDerivativeU(fU,fV);
    Vector3<Real> kDerV = GetDerivativeV(fU,fV);
    Vector3<Real> kDerUU = GetDerivativeUU(fU,fV);
    Vector3<Real> kDerUV = GetDerivativeUV(fU,fV);
    Vector3<Real> kDerVV = GetDerivativeVV(fU,fV);

    // metric tensor
    Matrix3<Real> kMetricTensor;
    kMetricTensor[0][0] = kDerU.Dot(kDerU);
    kMetricTensor[0][1] = kDerU.Dot(kDerV);
    kMetricTensor[1][0] = kMetricTensor[1][0];
    kMetricTensor[1][1] = kDerV.Dot(kDerV);

    // curvature tensor
    Vector3<Real> kNormal = kDerU.UnitCross(kDerV);
    Matrix3<Real> kCurvatureTensor;
    kCurvatureTensor[0][0] = -kNormal.Dot(kDerUU);
    kCurvatureTensor[0][1] = -kNormal.Dot(kDerUV);
    kCurvatureTensor[1][0] = kCurvatureTensor[0][1];
    kCurvatureTensor[1][1] = -kNormal.Dot(kDerVV);

    // characteristic polynomial is 0 = det(B-kG) = c2*k^2+c1*k+c0
    Real fC0 = kCurvatureTensor.Determinant();
    Real fC1 = ((Real)2.0)*kCurvatureTensor[0][1]* kMetricTensor[0][1] -
        kCurvatureTensor[0][0]*kMetricTensor[1][1] -
        kCurvatureTensor[1][1]*kMetricTensor[0][0];
    Real fC2 = kMetricTensor.Determinant();

    // principal curvatures are roots of characteristic polynomial
    Real fTemp = Math<Real>::Sqrt(Math<Real>::FAbs(fC1*fC1 -
        ((Real)4.0)*fC0*fC2));
    rfCurv0 = -((Real)0.5)*(fC1+fTemp);
    rfCurv1 = ((Real)0.5)*(-fC1+fTemp);

    // principal directions are solutions to (B-kG)w = 0
    // w1 = (b12-k1*g12,-(b11-k1*g11)) OR (b22-k1*g22,-(b12-k1*g12))
    Real fA0 = kCurvatureTensor[0][1] - rfCurv0*kMetricTensor[0][1];
    Real fA1 = rfCurv0*kMetricTensor[0][0] - kCurvatureTensor[0][0];
    Real fLength = Math<Real>::Sqrt(fA0*fA0+fA1*fA1);
    if ( fLength >= Math<Real>::EPSILON )
    {
        rkDir0 = fA0*kDerU + fA1*kDerV;
    }
    else
    {
        fA0 = kCurvatureTensor[1][1] - rfCurv0*kMetricTensor[1][1];
        fA1 = rfCurv0*kMetricTensor[0][1] - kCurvatureTensor[0][1];
        fLength = Math<Real>::Sqrt(fA0*fA0+fA1*fA1);
        if ( fLength >= Math<Real>::EPSILON )
        {
            rkDir0 = fA0*kDerU + fA1*kDerV;
        }
        else
        {
            // umbilic (surface is locally sphere, any direction principal)
            rkDir0 = kDerU;
        }
    }
    rkDir0.Normalize();

    // second tangent is cross product of first tangent and normal
    rkDir1 = rkDir0.Cross(kNormal);
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM ParametricSurface<float>;
template class WML_ITEM ParametricSurface<double>;
}
//----------------------------------------------------------------------------
