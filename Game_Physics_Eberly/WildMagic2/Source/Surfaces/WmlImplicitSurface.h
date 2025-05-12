// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WLMIMPLICITSURFACE_H
#define WLMIMPLICITSURFACE_H

#include "WmlMatrix3.h"
#include "WmlSurface.h"

namespace Wml
{

template <class Real>
class WML_ITEM ImplicitSurface : public Surface<Real>
{
public:
    // Surface is defined by F(x,y,z) = 0.  In all member functions it is
    // the application's responsibility to ensure that (x,y,z) is a solution
    // to F = 0.

    typedef Real (*Function)(Real,Real,Real);

    ImplicitSurface (
        Function oF,       // F(x,y,z) = 0 is the surface
        Function aoDF[3],  // (Fx,Fy,Fz)
        Function aoD2F[6]  // (Fxx,Fxy,Fxz,Fyy,Fyz,Fzz)
    );

    // verify point is on surface
    bool IsOnSurface (Real fX, Real fY, Real fZ, Real fTolerance) const;

    // derivatives up to second order
    Vector3<Real> GetGradient (Real fX, Real fY, Real fZ) const;
    Matrix3<Real> GetHessian (Real fX, Real fY, Real fZ) const;

    // coordinate frame values
    void GetFrame (Real fX, Real fY, Real fZ, Vector3<Real>& kTangent0,
        Vector3<Real>& kTangent1, Vector3<Real>& kNormal) const;

    // differential geometric quantities
    bool ComputePrincipalCurvatureInfo (Real fX, Real fY, Real fZ,
        Real& rfCurv0, Real& rfCurv1, Vector3<Real>& rkDir0,
        Vector3<Real>& rkDir1);

protected:
	Function m_oF;
	Function m_aoDF[3];
	Function m_aoD2F[6];
};

typedef ImplicitSurface<float> ImplicitSurfacef;
typedef ImplicitSurface<double> ImplicitSurfaced;

}

#endif
