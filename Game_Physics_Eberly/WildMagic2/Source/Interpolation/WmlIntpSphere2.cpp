// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntpSphere2.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
IntpSphere2<Real>::IntpSphere2 (int iVertexQuantity, Real* afTheta,
    Real* afPhi, Real* afF)
{
    // Copy the input data.  The larger arrays are used to support wrap-around
    // in the Delaunay triangulation for the interpolator.  The Vector2<Real>
    // object V corresponds to (V.X(),V.Y()) = (theta,phi).
    int iThreeQuantity = 3*iVertexQuantity;
    Vector2<Real>* akWrapAngles = new Vector2<Real>[iThreeQuantity];
    Real* afWrapF = new Real[iThreeQuantity];
    for (int i = 0; i < iVertexQuantity; i++)
    {
        akWrapAngles[i].X() = afTheta[i];
        akWrapAngles[i].Y() = afPhi[i];
        afWrapF[i] = afF[i];
    }
    delete[] afTheta;
    delete[] afPhi;
    delete[] afF;

    // use periodicity to get wrap-around in the Delaunay triangulation
    int iI0 = 0, iI1 = iVertexQuantity, iI2 = 2*iVertexQuantity;
    for (/**/; iI0 < iVertexQuantity; iI0++, iI1++, iI2++)
    {
        akWrapAngles[iI1].X() = akWrapAngles[iI0].X() + Math<Real>::TWO_PI;
        akWrapAngles[iI2].X() = akWrapAngles[iI0].X() - Math<Real>::TWO_PI;
        akWrapAngles[iI1].Y() = akWrapAngles[iI0].Y();
        akWrapAngles[iI2].Y() = akWrapAngles[iI0].Y();
        afWrapF[iI1] = afWrapF[iI0];
        afWrapF[iI2] = afWrapF[iI0];
    }

    m_pkInterp = new IntpQdrNonuniform2<Real>(iThreeQuantity,akWrapAngles,
        afWrapF);
}
//----------------------------------------------------------------------------
template <class Real>
IntpSphere2<Real>::~IntpSphere2 ()
{
    delete m_pkInterp;
}
//----------------------------------------------------------------------------
template <class Real>
void IntpSphere2<Real>::GetSphericalCoords (Real fX, Real fY, Real fZ,
    Real& rfTheta, Real& rfPhi)
{
    // Assumes (x,y,z) is unit length.  Returns -PI <= theta <= PI and
    // 0 <= phiAngle <= PI.

    if ( fZ < (Real)1.0 )
    {
        if ( fZ > -(Real)1.0 )
        {
            rfTheta = Math<Real>::ATan2(fY,fX);
            rfPhi = Math<Real>::ACos(fZ);
        }
        else
        {
            rfTheta = -Math<Real>::PI;
            rfPhi = Math<Real>::PI;
        }
    }
    else
    {
        rfTheta = -Math<Real>::PI;
        rfPhi = (Real)0.0;
    }
}
//----------------------------------------------------------------------------
template <class Real>
bool IntpSphere2<Real>::Evaluate (Real fTheta, Real fPhi, Real& rfF)
{
    Vector2<Real> kAngles(fTheta,fPhi);
    Real fThetaDeriv, fPhiDeriv;
    return m_pkInterp->Evaluate(kAngles,rfF,fThetaDeriv,fPhiDeriv);
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM IntpSphere2<float>;
template class WML_ITEM IntpSphere2<double>;
}
//----------------------------------------------------------------------------
