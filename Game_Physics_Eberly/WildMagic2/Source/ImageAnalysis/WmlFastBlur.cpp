// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlFastBlur.h"
#include "WmlMath.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class PixelType>
void Wml::FastBlur1 (int iXBound, PixelType* akImage, PixelType* akTemp,
    double dScale, double dLogBase)
{
    int iX;
    for (iX = 0; iX < iXBound; iX++) 
    {
        double dRXP = iX + dScale;
        double dRXM = iX - dScale;
        int iXP = (int)Mathd::Floor(dRXP);
        int iXM = (int)Mathd::Ceil(dRXM);

        double dXSum = -2.0*akImage[iX];

        if ( iXP >= iXBound-1 )  // use boundary value
        {
            dXSum += akImage[iXBound-1];
        }
        else  // linearly interpolate
        {
            dXSum += akImage[iXP]+(dRXP-iXP)*(akImage[iXP+1]-akImage[iXP]);
        }

        if ( iXM <= 0 )  // use boundary value
        {
            dXSum += akImage[0];
        }
        else  // linearly interpolate
        {
            dXSum += akImage[iXM]+(dRXM-iXM)*(akImage[iXM]-akImage[iXM-1]);
        }

        akTemp[iX] = (PixelType)(akImage[iX] + dLogBase*dXSum);
    }

    for (iX = 0; iX < iXBound; iX++)
        akImage[iX] = akTemp[iX];
}
//----------------------------------------------------------------------------
template <class PixelType>
void Wml::FastBlur2 (int iXBound, int iYBound, PixelType** aakImage,
    PixelType** aakTemp, double dScale, double dLogBase)
{
    int iX, iY;
    for (iY = 0; iY < iYBound; iY++) 
    {
        double dRYP = iY + dScale;
        double dRYM = iY - dScale;
        int iYP = (int)Mathd::Floor(dRYP);
        int iYM = (int)Mathd::Ceil(dRYM);

        for (iX = 0; iX < iXBound; iX++) 
        {
            double dRXP = iX + dScale;
            double dRXM = iX - dScale;
            int iXP = (int)Mathd::Floor(dRXP);
            int iXM = (int)Mathd::Ceil(dRXM);

            // x portion of second central difference
            double dXSum = -2.0*aakImage[iY][iX];
            if ( iXP >= iXBound-1 )  // use boundary value
            {
                dXSum += aakImage[iY][iXBound-1];
            }
            else  // linearly interpolate
            {
                dXSum += aakImage[iY][iXP]+(dRXP-iXP)*(aakImage[iY][iXP+1]-
                    aakImage[iY][iXP]);
            }

            if ( iXM <= 0 )  // use boundary value
            {
                dXSum += aakImage[iY][0];
            }
            else  // linearly interpolate
            {
                dXSum += aakImage[iY][iXM]+(dRXM-iXM)*(aakImage[iY][iXM]-
                    aakImage[iY][iXM-1]);
            }

            // y portion of second central difference
            double dYSum = -2.0*aakImage[iY][iX];
            if ( iYP >= iYBound-1 )  // use boundary value
            {
                dYSum += aakImage[iYBound-1][iX];
            }
            else  // linearly interpolate
            {
                dYSum += aakImage[iYP][iX]+(dRYP-iYP)*(aakImage[iYP+1][iX]-
                    aakImage[iYP][iX]);
            }

            if ( iYM <= 0 )  // use boundary value
            {
                dYSum += aakImage[0][iX];
            }
            else  // linearly interpolate
            {
                dYSum += aakImage[iYM][iX]+(dRYM-iYM)*(aakImage[iYM][iX]-
                    aakImage[iYM-1][iX]);
            }

            aakTemp[iY][iX] = (PixelType)(aakImage[iY][iX] +
                dLogBase*(dXSum+dYSum));
        }
    }

    for (iY = 0; iY < iYBound; iY++)
    {
        for (iX = 0; iX < iXBound; iX++)
            aakImage[iY][iX] = aakTemp[iY][iX];
    }
}
//----------------------------------------------------------------------------
template <class PixelType>
void Wml::FastBlur3 (int iXBound, int iYBound, int iZBound,
    PixelType*** aaakImage, PixelType*** aaakTemp, double dScale,
    double dLogBase)
{
    int iX, iY, iZ;
    for (iZ = 0; iZ < iZBound; iZ++)
    {
        double dRZP = iZ + dScale;
        double dRZM = iZ - dScale;
        int iZP = (int)Mathd::Floor(dRZP);
        int iZM = (int)Mathd::Ceil(dRZM);

        for (iY = 0; iY < iYBound; iY++) 
        {
            double dRYP = iY + dScale;
            double dRYM = iY - dScale;
            int iYP = (int)Mathd::Floor(dRYP);
            int iYM = (int)Mathd::Ceil(dRYM);

            for (iX = 0; iX < iXBound; iX++) 
            {
                double dRXP = iX + dScale;
                double dRXM = iX - dScale;
                int iXP = (int)Mathd::Floor(dRXP);
                int iXM = (int)Mathd::Ceil(dRXM);

                // iX portion of second central difference
                double dXSum = -2.0*aaakImage[iZ][iY][iX];
                if ( iXP >= iXBound-1 )  // use boundary value
                {
                    dXSum += aaakImage[iZ][iY][iXBound-1];
                }
                else  // linearly interpolate
                {
                    dXSum += aaakImage[iZ][iY][iXP]+(dRXP-iXP)*(
                        aaakImage[iZ][iY][iXP+1]-aaakImage[iZ][iY][iXP]);
                }

                if ( iXM <= 0 )  // use boundary value
                {
                    dXSum += aaakImage[iZ][iY][0];
                }
                else  // linearly interpolate
                {
                    dXSum += aaakImage[iZ][iY][iXM]+(dRXM-iXM)*(
                        aaakImage[iZ][iY][iXM]-aaakImage[iZ][iY][iXM-1]);
                }

                // iY portion of second central difference
                double dYSum = -2.0*aaakImage[iZ][iY][iX];
                if ( iYP >= iYBound-1 )  // use boundary value
                {
                    dYSum += aaakImage[iZ][iYBound-1][iX];
                }
                else  // linearly interpolate
                {
                    dYSum += aaakImage[iZ][iYP][iX]+(dRYP-iYP)*(
                        aaakImage[iZ][iYP+1][iX]-aaakImage[iZ][iYP][iX]);
                }

                if ( iYM <= 0 )  // use boundary value
                {
                    dYSum += aaakImage[iZ][0][iX];
                }
                else  // linearly interpolate
                {
                    dYSum += aaakImage[iZ][iYM][iX]+(dRYM-iYM)*(
                        aaakImage[iZ][iYM][iX]-aaakImage[iZ][iYM-1][iX]);
                }

                // iZ portion of second central difference
                double dZSum = -2.0*aaakImage[iZ][iY][iX];
                if ( iZP >= iZBound-1 )  // use boundary value
                {
                    dZSum += aaakImage[iZBound-1][iY][iX];
                }
                else  // linearly interpolate
                {
                    dZSum += aaakImage[iZP][iY][iX]+(dRZP-iZP)*(
                        aaakImage[iZP+1][iY][iX]-aaakImage[iZP][iY][iX]);
                }

                if ( iZM <= 0 )  // use boundary value
                {
                    dZSum += aaakImage[0][iY][iX];
                }
                else  // linearly interpolate
                {
                    dZSum += aaakImage[iZM][iY][iX]+(dRZM-iZM)*(
                        aaakImage[iZM][iY][iX]-aaakImage[iZM-1][iY][iX]);
                }

                aaakTemp[iZ][iY][iX] = (PixelType)(aaakImage[iZ][iY][iX] +
                    dLogBase*(dXSum+dYSum+dZSum));
            }
        }
    }

    for (iZ = 0; iZ < iZBound; iZ++)
    {
        for (iY = 0; iY < iYBound; iY++)
        {
            for (iX = 0; iX < iXBound; iX++)
                aaakImage[iZ][iY][iX] = aaakTemp[iZ][iY][iX];
        }
    }
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM void FastBlur1<short> (
    int, short*, short*, double, double);
template WML_ITEM void FastBlur1<int> (
    int, int*, int*, double, double);
template WML_ITEM void FastBlur1<float> (
    int, float*, float*, double, double);
template WML_ITEM void FastBlur1<double> (
    int, double*, double*, double, double);

template WML_ITEM void FastBlur2<short> (
    int, int, short**, short**, double, double);
template WML_ITEM void FastBlur2<int> (
    int, int, int**, int**, double, double);
template WML_ITEM void FastBlur2<float> (
    int, int, float**, float**, double, double);
template WML_ITEM void FastBlur2<double> (
    int, int, double**, double**, double, double);

template WML_ITEM void FastBlur3<short> (
    int, int, int, short***, short***, double, double);
template WML_ITEM void FastBlur3<int> (
    int, int, int, int***, int***, double, double);
template WML_ITEM void FastBlur3<float> (
    int, int, int, float***, float***, double, double);
template WML_ITEM void FastBlur3<double> (
    int, int, int, double***, double***, double, double);
}
//----------------------------------------------------------------------------
