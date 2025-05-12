// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTPAKIMAUNIFORM3_H
#define WMLINTPAKIMAUNIFORM3_H

#include "WmlSystem.h"

namespace Wml
{

template <class Real>
class WML_ITEM IntpAkimaUniform3
{
public:
    // Construction and destruction.  Interp3DAkimaUniform does not accept
    // responsibility for deleting the input array.  The application must do
    // so.  The interpolator is for uniformly spaced (x,y,z)-values.  The
    // function values are assumed to be organized as f(x,y,z) = F[z][y][x].
    IntpAkimaUniform3 (int iXBound, int iYBound, int iZBound, Real fXMin,
        Real fXSpacing, Real fYMin, Real fYSpacing, Real fZMin,
        Real fZSpacing, Real*** aaafF);

    virtual ~IntpAkimaUniform3 ();

    class WML_ITEM Polynomial
    {
    public:
        Polynomial ()
        {
            memset(&m_aaafA[0][0][0],0,64*sizeof(Real));
        }

        // P(x,y,z) = sum_{i=0}^3 sum_{j=0}^3 sum_{k=0}^3 a_{ijk} x^i y^j z^k
        Real& A (int iX, int iY, int iZ)
        {
            return m_aaafA[iX][iY][iZ];
        }

        Real operator() (Real fX, Real fY, Real fZ) const
        {
            Real afXPow[4] = { (Real)1.0, fX, fX*fX, fX*fX*fX };
            Real afYPow[4] = { (Real)1.0, fY, fY*fY, fY*fY*fY };
            Real afZPow[4] = { (Real)1.0, fZ, fZ*fZ, fZ*fZ*fZ };

            Real fP = (Real)0.0;
            for (int iZ = 0; iZ <= 3; iZ++)
            {
                for (int iY = 0; iY <= 3; iY++)
                {
                    for (int iX = 0; iX <= 3; iX++)
                    {
                        fP += m_aaafA[iX][iY][iZ]*afXPow[iX]*afYPow[iY] *
                            afZPow[iZ];
                    }
                }
            }

            return fP;
        }

        Real operator() (int iXOrder, int iYOrder, int iZOrder, Real fX,
            Real fY, Real fZ) const
        {
            Real afXPow[4];
            switch ( iXOrder )
            {
            case 0:
                afXPow[0] = (Real)1.0;
                afXPow[1] = fX;
                afXPow[2] = fX*fX;
                afXPow[3] = fX*fX*fX;
                break;
            case 1:
                afXPow[0] = (Real)0.0;
                afXPow[1] = (Real)1.0;
                afXPow[2] = ((Real)2.0)*fX;
                afXPow[3] = ((Real)3.0)*fX*fX;
                break;
            case 2:
                afXPow[0] = (Real)0.0;
                afXPow[1] = (Real)0.0;
                afXPow[2] = (Real)2.0;
                afXPow[3] = ((Real)6.0)*fX;
                break;
            case 3:
                afXPow[0] = (Real)0.0;
                afXPow[1] = (Real)0.0;
                afXPow[2] = (Real)0.0;
                afXPow[3] = (Real)6.0;
                break;
            default:
                return (Real)0.0;
            }

            Real afYPow[4];
            switch ( iYOrder )
            {
            case 0:
                afYPow[0] = (Real)1.0;
                afYPow[1] = fY;
                afYPow[2] = fY*fY;
                afYPow[3] = fY*fY*fY;
                break;
            case 1:
                afYPow[0] = (Real)0.0;
                afYPow[1] = (Real)1.0;
                afYPow[2] = ((Real)2.0)*fY;
                afYPow[3] = ((Real)3.0)*fY*fY;
                break;
            case 2:
                afYPow[0] = (Real)0.0;
                afYPow[1] = (Real)0.0;
                afYPow[2] = (Real)2.0;
                afYPow[3] = ((Real)6.0)*fY;
                break;
            case 3:
                afYPow[0] = (Real)0.0;
                afYPow[1] = (Real)0.0;
                afYPow[2] = (Real)0.0;
                afYPow[3] = (Real)6.0;
                break;
            default:
                return (Real)0.0;
            }

            Real afZPow[4];
            switch ( iZOrder )
            {
            case 0:
                afZPow[0] = (Real)1.0;
                afZPow[1] = fZ;
                afZPow[2] = fZ*fZ;
                afZPow[3] = fZ*fZ*fZ;
                break;
            case 1:
                afZPow[0] = (Real)0.0;
                afZPow[1] = (Real)1.0;
                afZPow[2] = ((Real)2.0)*fZ;
                afZPow[3] = ((Real)3.0)*fZ*fZ;
                break;
            case 2:
                afZPow[0] = (Real)0.0;
                afZPow[1] = (Real)0.0;
                afZPow[2] = (Real)2.0;
                afZPow[3] = ((Real)6.0)*fZ;
                break;
            case 3:
                afZPow[0] = (Real)0.0;
                afZPow[1] = (Real)0.0;
                afZPow[2] = (Real)0.0;
                afZPow[3] = (Real)6.0;
                break;
            default:
                return (Real)0.0;
            }

            Real fP = (Real)0.0;

            for (int iZ = 0; iZ <= 3; iZ++)
            {
                for (int iY = 0; iY <= 3; iY++)
                {
                    for (int iX = 0; iX <= 3; iX++)
                    {
                        fP += m_aaafA[iX][iY][iZ]*afXPow[iX]*afYPow[iY] *
                            afZPow[iZ];
                    }
                }
            }

            return fP;
        }

    protected:
        Real m_aaafA[4][4][4];
    };

    int GetXBound () const;
    int GetYBound () const;
    int GetZBound () const;
    int GetQuantity () const;
    Real*** GetF () const;
    Polynomial*** GetPolynomials () const;
    const Polynomial& GetPolynomial (int iX, int iY, int iZ) const;

    Real GetXMin () const;
    Real GetXMax () const;
    Real GetXSpacing () const;
    Real GetYMin () const;
    Real GetYMax () const;
    Real GetYSpacing () const;
    Real GetZMin () const;
    Real GetZMax () const;
    Real GetZSpacing () const;

    // Evaluate the function and its derivatives.  The application is
    // responsible for ensuring that xmin <= x <= xmax, ymin <= y <= ymax,
    // and zmin <= z <= zmax.  If (x,y,z) is outside the extremes, the
    // function returns MAXREAL.  The first operator is for function
    // evaluation.  The second operator is for function or derivative
    // evaluations.  The uiXOrder argument is the order of the x-derivative,
    // the uiYOrder argument is the order of the y-derivative, and the
    // uiZOrder argument is the order of the z-derivative.  All orders are
    // zero to get the function value itself.
    Real operator() (Real fX, Real fY, Real fZ) const;
    Real operator() (int iXOrder, int iYOrder, int iZOrder, Real fX,
        Real fY, Real fZ) const;

protected:
    Real ComputeDerivative (Real* afSlope) const;
    void Construct (Polynomial& rkPoly, Real aaafF[2][2][2],
        Real aaafFX[2][2][2], Real aaafFY[2][2][2],
        Real aaafFZ[2][2][2], Real aaafFXY[2][2][2],
        Real aaafFXZ[2][2][2], Real aaafFYZ[2][2][2],
        Real aaafFXYZ[2][2][2]);

    bool XLookup (Real fX, int& riXIndex, Real& rfDX) const;
    bool YLookup (Real fY, int& riYIndex, Real& rfDY) const;
    bool ZLookup (Real fZ, int& riZIndex, Real& rfDZ) const;

    int m_iXBound, m_iYBound, m_iZBound, m_iQuantity;
    Real*** m_aaafF;
    Polynomial*** m_aaakPoly;
    Real m_fXMin, m_fXMax, m_fXSpacing;
    Real m_fYMin, m_fYMax, m_fYSpacing;
    Real m_fZMin, m_fZMax, m_fZSpacing;
};

typedef IntpAkimaUniform3<float> IntpAkimaUniform3f;
typedef IntpAkimaUniform3<double> IntpAkimaUniform3d;

}

#endif
