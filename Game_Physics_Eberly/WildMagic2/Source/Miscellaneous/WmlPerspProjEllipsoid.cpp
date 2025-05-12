// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlPerspProjEllipsoid.h"
#include "WmlEigen.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
void Wml::PerspProjEllipsoid (const EllipsoidGeneral3<Real>& rkEllipsoid,
    const Vector3<Real>& rkEye, const Plane3<Real>& rkPlane,
    const Vector3<Real>& rkU, const Vector3<Real>& rkV, Vector3<Real>& rkP,
    EllipseGeneral2<Real>& rkEllipse)
{
    // compute matrix M
    Vector3<Real> kAE = rkEllipsoid.m_kA*rkEye;
    Real fEAE = rkEye.Dot(kAE);
    Real fBE = rkEllipsoid.m_kB.Dot(rkEye);
    Real fTmp = ((Real)4.0)*(fEAE + fBE + rkEllipsoid.m_fC);
    Vector3<Real> kTmp = rkEllipsoid.m_kB + ((Real)2.0)*kAE;

    Matrix3<Real> kMat;
    kMat[0][0] = kTmp.X()*kTmp.X() - fTmp*rkEllipsoid.m_kA[0][0];
    kMat[0][1] = kTmp.X()*kTmp.Y() - fTmp*rkEllipsoid.m_kA[0][1];
    kMat[0][2] = kTmp.X()*kTmp.Z() - fTmp*rkEllipsoid.m_kA[0][2];
    kMat[1][1] = kTmp.Y()*kTmp.Y() - fTmp*rkEllipsoid.m_kA[1][1];
    kMat[1][2] = kTmp.Y()*kTmp.Z() - fTmp*rkEllipsoid.m_kA[1][2];
    kMat[2][2] = kTmp.Z()*kTmp.Z() - fTmp*rkEllipsoid.m_kA[2][2];
    kMat[1][0] = kMat[0][1];
    kMat[2][0] = kMat[0][2];
    kMat[2][1] = kMat[1][2];

    // compute coefficients for projected ellipse
    Vector3<Real> kN = rkPlane.GetNormal();
    Vector3<Real> kMU = kMat*rkU, kMV = kMat*rkV, kMN = kMat*kN;
    Real fDmNE = rkPlane.GetConstant() - kN.Dot(rkEye);
    rkP = rkEye + fDmNE*kN;

    rkEllipse.m_kA[0][0] = rkU.Dot(kMU);
    rkEllipse.m_kA[0][1] = rkU.Dot(kMV);
    rkEllipse.m_kA[1][1] = rkV.Dot(kMV);
    rkEllipse.m_kA[1][0] = rkEllipse.m_kA[0][1];
    rkEllipse.m_kB.X() = ((Real)2.0)*fDmNE*(rkU.Dot(kMN));
    rkEllipse.m_kB.Y() = ((Real)2.0)*fDmNE*(rkV.Dot(kMN));
    rkEllipse.m_fC = fDmNE*fDmNE*(kN.Dot(kMN));
}
//----------------------------------------------------------------------------
template <class Real>
void Wml::ConvertEllipse (EllipseGeneral2<Real>& rkEllipse,
    Vector2<Real>& rkCenter, Vector2<Real>& rkAxis0, Vector2<Real>& rkAxis1,
    Real& rfHalfLength0, Real& rfHalfLength1)
{
    // factor A = R D R^T
    Eigen<Real> kES(2);
    kES(0,0) = rkEllipse.m_kA[0][0];
    kES(0,1) = rkEllipse.m_kA[0][1];
    kES(1,0) = rkEllipse.m_kA[1][0];
    kES(1,1) = rkEllipse.m_kA[1][1];
    kES.EigenStuff2();

    // matrix R (columes are eigenvectors of A)
    kES.GetEigenvector(0,rkAxis0);
    kES.GetEigenvector(1,rkAxis1);

    // matrix D (eigenvalues of A)
    Real fD0 = kES.GetEigenvalue(0);
    Real fD1 = kES.GetEigenvalue(1);

    // compute the ellipse center
    Matrix2<Real> kInvA = rkEllipse.m_kA.Inverse();
    rkCenter = -((Real)0.5)*(kInvA*rkEllipse.m_kB);

    // compute the ellipse axis half lengths
    Real fQForm = rkCenter.Dot(rkEllipse.m_kA*rkCenter) - rkEllipse.m_fC;
    rfHalfLength0 = Math<Real>::Sqrt(Math<Real>::FAbs(fQForm/fD0));
    rfHalfLength1 = Math<Real>::Sqrt(Math<Real>::FAbs(fQForm/fD1));
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM void Wml::PerspProjEllipsoid<float> (
    const EllipsoidGeneral3<float>&, const Vector3<float>&,
    const Plane3<float>&, const Vector3<float>&, const Vector3<float>&,
    Vector3<float>&, EllipseGeneral2<float>&);
template WML_ITEM void Wml::ConvertEllipse<float> (
    EllipseGeneral2<float>&, Vector2<float>&, Vector2<float>&,
    Vector2<float>&, float&, float&);

template WML_ITEM void PerspProjEllipsoid<double> (
    const EllipsoidGeneral3<double>&, const Vector3<double>&,
    const Plane3<double>&, const Vector3<double>&, const Vector3<double>&,
    Vector3<double>&, EllipseGeneral2<double>&);
template WML_ITEM void ConvertEllipse<double> (
    EllipseGeneral2<double>&, Vector2<double>&, Vector2<double>&,
    Vector2<double>&, double&, double&);
}
//----------------------------------------------------------------------------
