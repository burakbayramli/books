// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLPARAMETRICSURFACE_H
#define WMLPARAMETRICSURFACE_H

#include "WmlSurface.h"
#include "WmlVector3.h"

namespace Wml
{

template <class Real>
class WML_ITEM ParametricSurface : public Surface<Real>
{
public:
    // The parametric domain is either rectangular or triangular.  Specify
    // which by the bRectangular value ('true' for rectangular, 'false' for
    // triangular).  If the domain is rectangular, valid (u,v) values satisfy
    //   umin <= u <= umax,  vmin <= v <= vmax
    // Valid (u,v) values for a triangular domain satisfy
    //   umin <= u <= umax,  vmin <= v <= vmax,
    //   (vmax-vmin)*(u-umin)+(umax-umin)*(v-vmax) <= 0

    ParametricSurface (Real fUMin, Real fUMax, Real fVMin, Real fVMax,
        bool bRectangular);

    Real GetUMin () const;
    Real GetUMax () const;
    Real GetVMin () const;
    Real GetVMax () const;
    bool IsRectangular () const;

    // position and derivatives up to second order
    virtual Vector3<Real> GetPosition (Real fU, Real fV) const = 0;
    virtual Vector3<Real> GetDerivativeU (Real fU, Real fV) const = 0;
    virtual Vector3<Real> GetDerivativeV (Real fU, Real fV) const = 0;
    virtual Vector3<Real> GetDerivativeUU (Real fU, Real fV) const = 0;
    virtual Vector3<Real> GetDerivativeUV (Real fU, Real fV) const = 0;
    virtual Vector3<Real> GetDerivativeVV (Real fU, Real fV) const = 0;

    // coordinate frame values
    Vector3<Real> GetTangent0 (Real fU, Real fV) const;
    Vector3<Real> GetTangent1 (Real fU, Real fV) const;
    Vector3<Real> GetNormal (Real fU, Real fV) const;
    void GetFrame (Real fU, Real fV, Vector3<Real>& kPosition,
        Vector3<Real>& kTangent0, Vector3<Real>& kTangent1,
        Vector3<Real>& kNormal) const;

    // differential geometric quantities
    void ComputePrincipalCurvatureInfo (Real fU, Real fV, Real& rfCurv0,
        Real& rfCurv1, Vector3<Real>& rkDir0, Vector3<Real>& rkDir1);

protected:
    Real m_fUMin, m_fUMax, m_fVMin, m_fVMax;
    bool m_bRectangular;
};

typedef ParametricSurface<float> ParametricSurfacef;
typedef ParametricSurface<double> ParametricSurfaced;

}

#endif
