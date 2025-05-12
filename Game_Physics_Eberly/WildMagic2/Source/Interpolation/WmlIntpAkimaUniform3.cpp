// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntpAkimaUniform3.h"
#include "WmlMath.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
IntpAkimaUniform3<Real>::IntpAkimaUniform3 (int iXBound, int iYBound,
    int iZBound, Real fXMin, Real fXSpacing, Real fYMin, Real fYSpacing,
    Real fZMin, Real fZSpacing, Real*** aaafF)
{
    // At least a 3x3x3 block of data points are needed to construct the
    // estimates of the boundary derivatives.
    assert( iXBound >= 3 && iYBound >= 3 && iZBound >= 3 && aaafF );
    assert( fXSpacing > (Real)0.0 && fYSpacing > (Real)0.0
        &&  fZSpacing > (Real)0.0 );

    m_iXBound = iXBound;
    m_iYBound = iYBound;
    m_iZBound = iZBound;
    m_fXMin = fXMin;
    m_fXSpacing = fXSpacing;
    m_fYMin = fYMin;
    m_fYSpacing = fYSpacing;
    m_fZMin = fZMin;
    m_fZSpacing = fZSpacing;
    m_aaafF = aaafF;

    int iXBoundM1 = iXBound - 1;
    int iYBoundM1 = iYBound - 1;
    int iZBoundM1 = iZBound - 1;
    m_iQuantity = iXBound*iYBound*iZBound;
    m_fXMax = fXMin + fXSpacing*iXBoundM1;
    m_fYMax = fYMin + fYSpacing*iYBoundM1;
    m_fZMax = fZMin + fZSpacing*iZBoundM1;

    // compute slopes
    Real fInvDX = ((Real)1.0)/fXSpacing;
    Real fInvDY = ((Real)1.0)/fYSpacing;
    Real fInvDZ = ((Real)1.0)/fZSpacing;
    Real fInvDXDY = fInvDX*fInvDY;
    Real fInvDXDZ = fInvDX*fInvDZ;
    Real fInvDYDZ = fInvDY*fInvDZ;
    Real fInvDXDYDZ = fInvDX*fInvDYDZ;

    // xslope[z][y][x]
    Real*** aaafXSlope;
    Allocate3D(iXBound+3,iYBound,iZBound,aaafXSlope);

    // yslope[z][x][y]
    Real*** aaafYSlope;
    Allocate3D(iYBound+3,iXBound,iZBound,aaafYSlope);

    // zslope[y][x][z]
    Real*** aaafZSlope;
    Allocate3D(iZBound+3,iXBound,iYBound,aaafZSlope);

    int iX, iY, iZ;
    for (iZ = 0; iZ < iZBound; iZ++)
    {
        for (iY = 0; iY < iYBound; iY++)
        {
            for (iX = 0; iX < iXBoundM1; iX++)
            {
                aaafXSlope[iZ][iY][iX+2] = (aaafF[iZ][iY][iX+1] - 
                    aaafF[iZ][iY][iX])*fInvDX;
            }

            aaafXSlope[iZ][iY][1] = ((Real)2.0)*aaafXSlope[iZ][iY][2] -
                aaafXSlope[iZ][iY][3];
            aaafXSlope[iZ][iY][0] = ((Real)2.0)*aaafXSlope[iZ][iY][1] -
                aaafXSlope[iZ][iY][2];
            aaafXSlope[iZ][iY][iXBound+1] =
                ((Real)2.0)*aaafXSlope[iZ][iY][iXBound] -
                aaafXSlope[iZ][iY][iXBound-1];
            aaafXSlope[iZ][iY][iXBound+2] =
                ((Real)2.0)*aaafXSlope[iZ][iY][iXBound+1] -
                aaafXSlope[iZ][iY][iXBound];
        }
    }

    for (iZ = 0; iZ < iZBound; iZ++)
    {
        for (iX = 0; iX < iXBound; iX++)
        {
            for (iY = 0; iY < iYBoundM1; iY++)
            {
                aaafYSlope[iZ][iX][iY+2] = (aaafF[iZ][iY+1][iX] -
                    aaafF[iZ][iY][iX])*fInvDY;
            }

            aaafYSlope[iZ][iX][1] = ((Real)2.0)*aaafYSlope[iZ][iX][2] -
                aaafYSlope[iZ][iX][3];
            aaafYSlope[iZ][iX][0] = ((Real)2.0)*aaafYSlope[iZ][iX][1] -
                aaafYSlope[iZ][iX][2];
            aaafYSlope[iZ][iX][iYBound+1] =
                ((Real)2.0)*aaafYSlope[iZ][iX][iYBound] -
                aaafYSlope[iZ][iX][iYBound-1];
            aaafYSlope[iZ][iX][iYBound+2] =
                ((Real)2.0)*aaafYSlope[iZ][iX][iYBound+1] -
                aaafYSlope[iZ][iX][iYBound];
        }
    }

    for (iY = 0; iY < iYBound; iY++)
    {
        for (iX = 0; iX < iXBound; iX++)
        {
            for (iZ = 0; iZ < iZBoundM1; iZ++)
            {
                aaafZSlope[iY][iX][iZ+2] = (aaafF[iZ+1][iY][iX] -
                    aaafF[iZ][iY][iX])*fInvDZ;
            }

            aaafZSlope[iY][iX][1] = ((Real)2.0)*aaafZSlope[iY][iX][2] -
                aaafZSlope[iY][iX][3];
            aaafZSlope[iY][iX][0] = ((Real)2.0)*aaafZSlope[iY][iX][1] -
                aaafZSlope[iY][iX][2];
            aaafZSlope[iY][iX][iZBound+1] =
                ((Real)2.0)*aaafZSlope[iY][iX][iZBound] -
                aaafZSlope[iY][iX][iZBound-1];
            aaafZSlope[iY][iX][iZBound+2] =
                ((Real)2.0)*aaafZSlope[iY][iX][iZBound+1] -
                aaafZSlope[iY][iX][iZBound];
        }
    }

    // construct first-order derivatives
    Real*** aaafFX;
    Allocate3D(iXBound,iYBound,iZBound,aaafFX);
    Real*** aaafFY;
    Allocate3D(iXBound,iYBound,iZBound,aaafFY);
    Real*** aaafFZ;
    Allocate3D(iXBound,iYBound,iZBound,aaafFZ);

    for (iZ = 0; iZ < iZBound; iZ++)
    {
        for (iY = 0; iY < iYBound; iY++)
        {
            for (iX = 0; iX < iXBound; iX++)
            {
                aaafFX[iZ][iY][iX] = ComputeDerivative(
                   aaafXSlope[iZ][iY]+iX);
            }
        }
    }

    for (iZ = 0; iZ < iZBound; iZ++)
    {
        for (iX = 0; iX < iXBound; iX++)
        {
            for (iY = 0; iY < iYBound; iY++)
            {
                aaafFY[iZ][iY][iX] = ComputeDerivative(
                    aaafYSlope[iZ][iX]+iY);
            }
        }
    }

    for (iY = 0; iY < iYBound; iY++)
    {
        for (iX = 0; iX < iXBound; iX++)
        {
            for (iZ = 0; iZ < iZBound; iZ++)
            {
                aaafFZ[iZ][iY][iX] = ComputeDerivative(
                    aaafZSlope[iY][iX]+iZ);
            }
        }
    }

    // construct second-order derivatives
    Real*** aaafFXY;
    Allocate3D(iXBound,iYBound,iZBound,aaafFXY);
    Real*** aaafFXZ;
    Allocate3D(iXBound,iYBound,iZBound,aaafFXZ);
    Real*** aaafFYZ;
    Allocate3D(iXBound,iYBound,iZBound,aaafFYZ);

    int iX0 = iXBoundM1, iX1 = iX0-1,  iX2 = iX1-1;
    int iY0 = iYBoundM1, iY1 = iY0-1,  iY2 = iY1-1;
    int iZ0 = iZBoundM1, iZ1 = iZ0-1,  iZ2 = iZ1-1;

    for (iZ = 0; iZ < iZBound; iZ++)
    {
        // corners of z-slice
        aaafFXY[iZ][0][0] = ((Real)0.25)*fInvDXDY*(
              ((Real)9.0)*aaafF[iZ][0][0]
            -((Real)12.0)*aaafF[iZ][0][1]
            + ((Real)3.0)*aaafF[iZ][0][2]
            -((Real)12.0)*aaafF[iZ][1][0]
            +((Real)16.0)*aaafF[iZ][1][1]
            - ((Real)4.0)*aaafF[iZ][1][2]
            + ((Real)3.0)*aaafF[iZ][2][0]
            - ((Real)4.0)*aaafF[iZ][2][1]
            +             aaafF[iZ][2][2]);

        aaafFXY[iZ][0][iXBoundM1] = ((Real)0.25)*fInvDXDY*(
              ((Real)9.0)*aaafF[iZ][0][iX0]
            -((Real)12.0)*aaafF[iZ][0][iX1]
            + ((Real)3.0)*aaafF[iZ][0][iX2]
            -((Real)12.0)*aaafF[iZ][1][iX0]
            +((Real)16.0)*aaafF[iZ][1][iX1]
            - ((Real)4.0)*aaafF[iZ][1][iX2]
            + ((Real)3.0)*aaafF[iZ][2][iX0]
            - ((Real)4.0)*aaafF[iZ][2][iX1]
            +             aaafF[iZ][2][iX2]);

        aaafFXY[iZ][iYBoundM1][0] = ((Real)0.25)*fInvDXDY*(
              ((Real)9.0)*aaafF[iZ][iY0][0]
            -((Real)12.0)*aaafF[iZ][iY0][1]
            + ((Real)3.0)*aaafF[iZ][iY0][2]
            -((Real)12.0)*aaafF[iZ][iY1][0]
            +((Real)16.0)*aaafF[iZ][iY1][1]
            - ((Real)4.0)*aaafF[iZ][iY1][2]
            + ((Real)3.0)*aaafF[iZ][iY2][0]
            - ((Real)4.0)*aaafF[iZ][iY2][1]
            +             aaafF[iZ][iY2][2]);

        aaafFXY[iZ][iYBoundM1][iXBoundM1] = ((Real)0.25)*fInvDXDY*(
            ((Real)9.0)*aaafF[iZ][iY0][iX0]
          -((Real)12.0)*aaafF[iZ][iY0][iX1]
          + ((Real)3.0)*aaafF[iZ][iY0][iX2]
          -((Real)12.0)*aaafF[iZ][iY1][iX0]
          +((Real)16.0)*aaafF[iZ][iY1][iX1]
          - ((Real)4.0)*aaafF[iZ][iY1][iX2]
          + ((Real)3.0)*aaafF[iZ][iY2][iX0]
          - ((Real)4.0)*aaafF[iZ][iY2][iX1]
          +             aaafF[iZ][iY2][iX2]);

        // x-edges of z-slice
        for (iX = 1; iX < iXBoundM1; iX++)
        {
            aaafFXY[iZ][0][iX] = ((Real)0.25)*fInvDXDY*(
                ((Real)3.0)*(aaafF[iZ][0][iX-1] - aaafF[iZ][0][iX+1]) -
                ((Real)4.0)*(aaafF[iZ][1][iX-1] - aaafF[iZ][1][iX+1]) +
                (aaafF[iZ][2][iX-1] - aaafF[iZ][2][iX+1]));

            aaafFXY[iZ][iYBoundM1][iX] = ((Real)0.25)*fInvDXDY*(
                ((Real)3.0)*(aaafF[iZ][iY0][iX-1] - aaafF[iZ][iY0][iX+1]) -
                ((Real)4.0)*(aaafF[iZ][iY1][iX-1] - aaafF[iZ][iY1][iX+1]) +
                (aaafF[iZ][iY2][iX-1] - aaafF[iZ][iY2][iX+1]));
        }

        // y-edges of z-slice
        for (iY = 1; iY < iYBoundM1; iY++)
        {
            aaafFXY[iZ][iY][0] = ((Real)0.25)*fInvDXDY*(
                ((Real)3.0)*(aaafF[iZ][iY-1][0] - aaafF[iZ][iY+1][0]) -
                ((Real)4.0)*(aaafF[iZ][iY-1][1] - aaafF[iZ][iY+1][1]) +
                (aaafF[iZ][iY-1][2] - aaafF[iZ][iY+1][2]));

            aaafFXY[iZ][iY][iXBoundM1] = ((Real)0.25)*fInvDXDY*(
                ((Real)3.0)*(aaafF[iZ][iY-1][iX0] - aaafF[iZ][iY+1][iX0]) -
                ((Real)4.0)*(aaafF[iZ][iY-1][iX1] - aaafF[iZ][iY+1][iX1]) +
                (aaafF[iZ][iY-1][iX2] - aaafF[iZ][iY+1][iX2]));
        }

        // interior of z-slice
        for (iY = 1; iY < iYBoundM1; iY++)
        {
            for (iX = 1; iX < iXBoundM1; iX++)
            {
                aaafFXY[iZ][iY][iX] = ((Real)0.25)*fInvDXDY*(
                    aaafF[iZ][iY-1][iX-1] - aaafF[iZ][iY-1][iX+1] -
                    aaafF[iZ][iY+1][iX-1] + aaafF[iZ][iY+1][iX+1]);
            }
        }
    }

    for (iY = 0; iY < iYBound; iY++)
    {
        // corners of z-slice
        aaafFXZ[0][iY][0] = ((Real)0.25)*fInvDXDZ*(
              ((Real)9.0)*aaafF[0][iY][0]
            -((Real)12.0)*aaafF[0][iY][1]
            + ((Real)3.0)*aaafF[0][iY][2]
            -((Real)12.0)*aaafF[1][iY][0]
            +((Real)16.0)*aaafF[1][iY][1]
            - ((Real)4.0)*aaafF[1][iY][2]
            + ((Real)3.0)*aaafF[2][iY][0]
            - ((Real)4.0)*aaafF[2][iY][1]
            +      aaafF[2][iY][2]);

        aaafFXZ[0][iY][iXBoundM1] = ((Real)0.25)*fInvDXDZ*(
              ((Real)9.0)*aaafF[0][iY][iX0]
            -((Real)12.0)*aaafF[0][iY][iX1]
            + ((Real)3.0)*aaafF[0][iY][iX2]
            -((Real)12.0)*aaafF[1][iY][iX0]
            +((Real)16.0)*aaafF[1][iY][iX1]
            - ((Real)4.0)*aaafF[1][iY][iX2]
            + ((Real)3.0)*aaafF[2][iY][iX0]
            - ((Real)4.0)*aaafF[2][iY][iX1]
            +      aaafF[2][iY][iX2]);

        aaafFXZ[iZBoundM1][iY][0] = ((Real)0.25)*fInvDXDZ*(
              ((Real)9.0)*aaafF[iZ0][iY][0]
            -((Real)12.0)*aaafF[iZ0][iY][1]
            + ((Real)3.0)*aaafF[iZ0][iY][2]
            -((Real)12.0)*aaafF[iZ1][iY][0]
            +((Real)16.0)*aaafF[iZ1][iY][1]
            - ((Real)4.0)*aaafF[iZ1][iY][2]
            + ((Real)3.0)*aaafF[iZ2][iY][0]
            - ((Real)4.0)*aaafF[iZ2][iY][1]
            +      aaafF[iZ2][iY][2]);

        aaafFXZ[iZBoundM1][iY][iXBoundM1] = ((Real)0.25)*fInvDXDZ*(
            ((Real)9.0)*aaafF[iZ0][iY][iX0]
          -((Real)12.0)*aaafF[iZ0][iY][iX1]
          + ((Real)3.0)*aaafF[iZ0][iY][iX2]
          -((Real)12.0)*aaafF[iZ1][iY][iX0]
          +((Real)16.0)*aaafF[iZ1][iY][iX1]
          - ((Real)4.0)*aaafF[iZ1][iY][iX2]
          + ((Real)3.0)*aaafF[iZ2][iY][iX0]
          - ((Real)4.0)*aaafF[iZ2][iY][iX1]
          +      aaafF[iZ2][iY][iX2]);

        // x-edges of y-slice
        for (iX = 1; iX < iXBoundM1; iX++)
        {
            aaafFXZ[0][iY][iX] = ((Real)0.25)*fInvDXDZ*(
                ((Real)3.0)*(aaafF[0][iY][iX-1] - aaafF[0][iY][iX+1]) -
                ((Real)4.0)*(aaafF[1][iY][iX-1] - aaafF[1][iY][iX+1]) +
                (aaafF[2][iY][iX-1] - aaafF[2][iY][iX+1]));

            aaafFXZ[iZBoundM1][iY][iX] = ((Real)0.25)*fInvDXDZ*(
                ((Real)3.0)*(aaafF[iZ0][iY][iX-1] - aaafF[iZ0][iY][iX+1]) -
                ((Real)4.0)*(aaafF[iZ1][iY][iX-1] - aaafF[iZ1][iY][iX+1]) +
                (aaafF[iZ2][iY][iX-1] - aaafF[iZ2][iY][iX+1]));
        }

        // z-edges of y-slice
        for (iZ = 1; iZ < iZBoundM1; iZ++)
        {
            aaafFXZ[iZ][iY][0] = ((Real)0.25)*fInvDXDZ*(
                ((Real)3.0)*(aaafF[iZ-1][iY][0] - aaafF[iZ+1][iY][0]) -
                ((Real)4.0)*(aaafF[iZ-1][iY][1] - aaafF[iZ+1][iY][1]) +
                (aaafF[iZ-1][iY][2] - aaafF[iZ+1][iY][2]));

            aaafFXZ[iZ][iY][iXBoundM1] = ((Real)0.25)*fInvDXDZ*(
                ((Real)3.0)*(aaafF[iZ-1][iY][iX0] - aaafF[iZ+1][iY][iX0]) -
                ((Real)4.0)*(aaafF[iZ-1][iY][iX1] - aaafF[iZ+1][iY][iX1]) +
                (aaafF[iZ-1][iY][iX2] - aaafF[iZ+1][iY][iX2]));
        }

        // interior of y-slice
        for (iZ = 1; iZ < iZBoundM1; iZ++)
        {
            for (iX = 1; iX < iXBoundM1; iX++)
            {
                aaafFXZ[iZ][iY][iX] = ((Real)0.25)*fInvDXDZ*(
                    aaafF[iZ-1][iY][iX-1] - aaafF[iZ-1][iY][iX+1] -
                    aaafF[iZ+1][iY][iX-1] + aaafF[iZ+1][iY][iX+1]);
            }
        }
    }

    for (iX = 0; iX < iXBound; iX++)
    {
        // corners of x-slice
        aaafFYZ[0][0][iX] = ((Real)0.25)*fInvDYDZ*(
              ((Real)9.0)*aaafF[0][0][iX]
            -((Real)12.0)*aaafF[0][1][iX]
            + ((Real)3.0)*aaafF[0][2][iX]
            -((Real)12.0)*aaafF[1][0][iX]
            +((Real)16.0)*aaafF[1][1][iX]
            - ((Real)4.0)*aaafF[1][2][iX]
            + ((Real)3.0)*aaafF[2][0][iX]
            - ((Real)4.0)*aaafF[2][1][iX]
            +             aaafF[2][2][iX]);

        aaafFYZ[0][iYBoundM1][iX] = ((Real)0.25)*fInvDYDZ*(
              ((Real)9.0)*aaafF[0][iY0][iX]
            -((Real)12.0)*aaafF[0][iY1][iX]
            + ((Real)3.0)*aaafF[0][iY2][iX]
            -((Real)12.0)*aaafF[1][iY0][iX]
            +((Real)16.0)*aaafF[1][iY1][iX]
            - ((Real)4.0)*aaafF[1][iY2][iX]
            + ((Real)3.0)*aaafF[2][iY0][iX]
            - ((Real)4.0)*aaafF[2][iY1][iX]
            +             aaafF[2][iY2][iX]);

        aaafFYZ[iZBoundM1][0][iX] = ((Real)0.25)*fInvDYDZ*(
              ((Real)9.0)*aaafF[iZ0][0][iX]
            -((Real)12.0)*aaafF[iZ0][1][iX]
            + ((Real)3.0)*aaafF[iZ0][2][iX]
            -((Real)12.0)*aaafF[iZ1][0][iX]
            +((Real)16.0)*aaafF[iZ1][1][iX]
            - ((Real)4.0)*aaafF[iZ1][2][iX]
            + ((Real)3.0)*aaafF[iZ2][0][iX]
            - ((Real)4.0)*aaafF[iZ2][1][iX]
            +             aaafF[iZ2][2][iX]);

        aaafFYZ[iZBoundM1][iYBoundM1][iX] = ((Real)0.25)*fInvDYDZ*(
            ((Real)9.0)*aaafF[iZ0][iY0][iX]
          -((Real)12.0)*aaafF[iZ0][iY1][iX]
          + ((Real)3.0)*aaafF[iZ0][iY2][iX]
          -((Real)12.0)*aaafF[iZ1][iY0][iX]
          +((Real)16.0)*aaafF[iZ1][iY1][iX]
          - ((Real)4.0)*aaafF[iZ1][iY2][iX]
          + ((Real)3.0)*aaafF[iZ2][iY0][iX]
          - ((Real)4.0)*aaafF[iZ2][iY1][iX]
          +             aaafF[iZ2][iY2][iX]);

        // y-edges of x-slice
        for (iY = 1; iY < iYBoundM1; iY++)
        {
            aaafFYZ[0][iY][iX] = ((Real)0.25)*fInvDYDZ*(
                ((Real)3.0)*(aaafF[0][iY-1][iX] - aaafF[0][iY+1][iX]) -
                ((Real)4.0)*(aaafF[1][iY-1][iX] - aaafF[1][iY+1][iX]) +
                (aaafF[2][iY-1][iX] - aaafF[2][iY+1][iX]));

            aaafFYZ[iZBoundM1][iY][iX] = ((Real)0.25)*fInvDYDZ*(
                ((Real)3.0)*(aaafF[iZ0][iY-1][iX] - aaafF[iZ0][iY+1][iX]) -
                ((Real)4.0)*(aaafF[iZ1][iY-1][iX] - aaafF[iZ1][iY+1][iX]) +
                (aaafF[iZ2][iY-1][iX] - aaafF[iZ2][iY+1][iX]));
        }

        // z-edges of x-slice
        for (iZ = 1; iZ < iZBoundM1; iZ++)
        {
            aaafFYZ[iZ][0][iX] = ((Real)0.25)*fInvDYDZ*(
                ((Real)3.0)*(aaafF[iZ-1][0][iX] - aaafF[iZ+1][0][iX]) -
                ((Real)4.0)*(aaafF[iZ-1][1][iX] - aaafF[iZ+1][1][iX]) +
                (aaafF[iZ-1][2][iX] - aaafF[iZ+1][2][iX]));

            aaafFYZ[iZ][iYBoundM1][iX] = ((Real)0.25)*fInvDYDZ*(
                ((Real)3.0)*(aaafF[iZ-1][iY0][iX] - aaafF[iZ+1][iY0][iX]) -
                ((Real)4.0)*(aaafF[iZ-1][iY1][iX] - aaafF[iZ+1][iY1][iX]) +
                (aaafF[iZ-1][iY2][iX] - aaafF[iZ+1][iY2][iX]));
        }

        // interior of x-slice
        for (iZ = 1; iZ < iZBoundM1; iZ++)
        {
            for (iY = 1; iY < iYBoundM1; iY++)
            {
                aaafFYZ[iZ][iY][iX] = ((Real)0.25)*fInvDYDZ*(
                    aaafF[iZ-1][iY-1][iX] - aaafF[iZ-1][iY+1][iX] -
                    aaafF[iZ+1][iY-1][iX] + aaafF[iZ+1][iY+1][iX]);
            }
        }
    }

    // construct third-order derivatives
    Real*** aaafFXYZ;
    Allocate3D(iXBound,iYBound,iZBound,aaafFXYZ);

    // convolution masks
    //   centered difference, O(h^2)
    Real afCDer[3] = { -(Real)0.5, (Real)0.0, (Real)0.5f };
    //   one-sided difference, O(h^2)
    Real afODer[3] = { -(Real)1.5, (Real)2.0, -(Real)0.5 };
    Real fMask;

    // corners
    aaafFXYZ[0][0][0] = (Real)0.0;
    aaafFXYZ[0][0][iXBoundM1] = (Real)0.0;
    aaafFXYZ[0][iYBoundM1][0] = (Real)0.0;
    aaafFXYZ[0][iYBoundM1][iXBoundM1] = (Real)0.0;
    aaafFXYZ[iZBoundM1][0][0] = (Real)0.0;
    aaafFXYZ[iZBoundM1][0][iXBoundM1] = (Real)0.0;
    aaafFXYZ[iZBoundM1][iYBoundM1][0] = (Real)0.0;
    aaafFXYZ[iZBoundM1][iYBoundM1][iXBoundM1] = (Real)0.0;
    for (iZ = 0; iZ <= 2; iZ++)
    {
        for (iY = 0; iY <= 2; iY++)
        {
            for (iX = 0; iX <= 2; iX++)
            {
                fMask = fInvDXDYDZ*afODer[iX]*afODer[iY]*afODer[iZ];

                aaafFXYZ[0][0][0] += fMask*
                    aaafF[iZ][iY][iX];

                aaafFXYZ[0][0][iXBoundM1] += fMask*
                    aaafF[iZ][iY][iXBoundM1-iX];

                aaafFXYZ[0][iYBoundM1][0] += fMask*
                    aaafF[iZ][iYBoundM1-iY][iX];

                aaafFXYZ[0][iYBoundM1][iXBoundM1] += fMask*
                    aaafF[iZ][iYBoundM1-iY][iXBoundM1-iX];

                aaafFXYZ[iZBoundM1][0][0] += fMask*
                    aaafF[iZBoundM1-iZ][iY][iX];

                aaafFXYZ[iZBoundM1][0][iXBoundM1] += fMask*
                    aaafF[iZBoundM1-iZ][iY][iXBoundM1-iX];

                aaafFXYZ[iZBoundM1][iYBoundM1][0] += fMask*
                    aaafF[iZBoundM1-iZ][iYBoundM1-iY][iX];

                aaafFXYZ[iZBoundM1][iYBoundM1][iXBoundM1] += fMask*
                    aaafF[iZBoundM1-iZ][iYBoundM1-iY][iXBoundM1-iX];
            }
        }
    }

    // x-edges
    for (iX0 = 1; iX0 < iXBoundM1; iX0++)
    {
        aaafFXYZ[0][0][iX0] = (Real)0.0;
        aaafFXYZ[0][iYBoundM1][iX0] = (Real)0.0;
        aaafFXYZ[iZBoundM1][0][iX0] = (Real)0.0;
        aaafFXYZ[iZBoundM1][iYBoundM1][iX0] = (Real)0.0;
        for (iZ = 0; iZ <= 2; iZ++)
        {
            for (iY = 0; iY <= 2; iY++)
            {
                for (iX = 0; iX <= 2; iX++)
                {
                    fMask = fInvDXDYDZ*afCDer[iX]*afODer[iY]*afODer[iZ];

                    aaafFXYZ[0][0][iX0] += fMask*
                        aaafF[iZ][iY][iX0+iX-1];

                    aaafFXYZ[0][iYBoundM1][iX0] += fMask*
                        aaafF[iZ][iYBoundM1-iY][iX0+iX-1];

                    aaafFXYZ[iZBoundM1][0][iX0] += fMask*
                        aaafF[iZBoundM1-iZ][iY][iX0+iX-1];

                    aaafFXYZ[iZBoundM1][iYBoundM1][iX0] += fMask*
                        aaafF[iZBoundM1-iZ][iYBoundM1-iY][iX0+iX-1];
                }
            }
        }
    }

    // y-edges
    for (iY0 = 1; iY0 < iYBoundM1; iY0++)
    {
        aaafFXYZ[0][iY0][0] = (Real)0.0;
        aaafFXYZ[0][iY0][iXBoundM1] = (Real)0.0;
        aaafFXYZ[iZBoundM1][iY0][0] = (Real)0.0;
        aaafFXYZ[iZBoundM1][iY0][iXBoundM1] = (Real)0.0;
        for (iZ = 0; iZ <= 2; iZ++)
        {
            for (iY = 0; iY <= 2; iY++)
            {
                for (iX = 0; iX <= 2; iX++)
                {
                    fMask = fInvDXDYDZ*afODer[iX]*afCDer[iY]*afODer[iZ];

                    aaafFXYZ[0][iY0][0] += fMask*
                        aaafF[iZ][iY0+iY-1][iX];

                    aaafFXYZ[0][iY0][iXBoundM1] += fMask*
                        aaafF[iZ][iY0+iY-1][iXBoundM1-iX];

                    aaafFXYZ[iZBoundM1][iY0][0] += fMask*
                        aaafF[iZBoundM1-iZ][iY0+iY-1][iX];

                    aaafFXYZ[iZBoundM1][iY0][iXBoundM1] += fMask*
                        aaafF[iZBoundM1-iZ][iY0+iY-1][iXBoundM1-iX];
                }
            }
        }
    }

    // z-edges
    for (iZ0 = 1; iZ0 < iZBoundM1; iZ0++)
    {
        aaafFXYZ[iZ0][0][0] = (Real)0.0;
        aaafFXYZ[iZ0][0][iXBoundM1] = (Real)0.0;
        aaafFXYZ[iZ0][iYBoundM1][0] = (Real)0.0;
        aaafFXYZ[iZ0][iYBoundM1][iXBoundM1] = (Real)0.0;
        for (iZ = 0; iZ <= 2; iZ++)
        {
            for (iY = 0; iY <= 2; iY++)
            {
                for (iX = 0; iX <= 2; iX++)
                {
                    fMask = fInvDXDYDZ*afODer[iX]*afODer[iY]*afCDer[iZ];

                    aaafFXYZ[iZ0][0][0] += fMask*
                        aaafF[iZ0+iZ-1][iY][iX];

                    aaafFXYZ[iZ0][0][iXBoundM1] += fMask*
                        aaafF[iZ0+iZ-1][iY][iXBoundM1-iX];

                    aaafFXYZ[iZ0][iYBoundM1][0] += fMask*
                        aaafF[iZ0+iZ-1][iYBoundM1-iY][iX];

                    aaafFXYZ[iZ0][iYBoundM1][iXBoundM1] += fMask*
                        aaafF[iZ0+iZ-1][iYBoundM1-iY][iXBoundM1-iX];
                }
            }
        }
    }

    // xy-faces
    for (iY0 = 1; iY0 < iYBoundM1; iY0++)
    {
        for (iX0 = 1; iX0 < iXBoundM1; iX0++)
        {
            aaafFXYZ[0][iY0][iX0] = (Real)0.0;
            aaafFXYZ[iZBoundM1][iY0][iX0] = (Real)0.0;
            for (iZ = 0; iZ <= 2; iZ++)
            {
                for (iY = 0; iY <= 2; iY++)
                {
                    for (iX = 0; iX <= 2; iX++)
                    {
                        fMask = fInvDXDYDZ*afCDer[iX]*afCDer[iY]*afODer[iZ];

                        aaafFXYZ[0][iY0][iX0] += fMask*
                            aaafF[iZ][iY0+iY-1][iX0+iX-1];

                        aaafFXYZ[iZBoundM1][iY0][iX0] += fMask*
                            aaafF[iZBoundM1-iZ][iY0+iY-1][iX0+iX-1];
                    }
                }
            }
        }
    }

    // xz-faces
    for (iZ0 = 1; iZ0 < iZBoundM1; iZ0++)
    {
        for (iX0 = 1; iX0 < iXBoundM1; iX0++)
        {
            aaafFXYZ[iZ0][0][iX0] = (Real)0.0;
            aaafFXYZ[iZ0][iYBoundM1][iX0] = (Real)0.0;
            for (iZ = 0; iZ <= 2; iZ++)
            {
                for (iY = 0; iY <= 2; iY++)
                {
                    for (iX = 0; iX <= 2; iX++)
                    {
                        fMask = fInvDXDYDZ*afCDer[iX]*afODer[iY]*afCDer[iZ];

                        aaafFXYZ[iZ0][0][iX0] += fMask*
                            aaafF[iZ0+iZ-1][iY][iX0+iX-1];

                        aaafFXYZ[iZ0][iYBoundM1][iX0] += fMask*
                            aaafF[iZ0+iZ-1][iYBoundM1-iY][iX0+iX-1];
                    }
                }
            }
        }
    }

    // yz-faces
    for (iZ0 = 1; iZ0 < iZBoundM1; iZ0++)
    {
        for (iY0 = 1; iY0 < iYBoundM1; iY0++)
        {
            aaafFXYZ[iZ0][iY0][0] = (Real)0.0;
            aaafFXYZ[iZ0][iY0][iXBoundM1] = (Real)0.0;
            for (iZ = 0; iZ <= 2; iZ++)
            {
                for (iY = 0; iY <= 2; iY++)
                {
                    for (iX = 0; iX <= 2; iX++)
                    {
                        fMask = fInvDXDYDZ*afODer[iX]*afCDer[iY]*afCDer[iZ];

                        aaafFXYZ[iZ0][iY0][0] += fMask*
                            aaafF[iZ0+iZ-1][iY0+iY-1][iX];

                        aaafFXYZ[iZ0][iY0][iXBoundM1] += fMask*
                            aaafF[iZ0+iZ-1][iY0+iY-1][iXBoundM1-iX];
                    }
                }
            }
        }
    }

    // interiors
    for (iZ0 = 1; iZ0 < iZBoundM1; iZ0++)
    {
        for (iY0 = 1; iY0 < iYBoundM1; iY0++)
        {
            for (iX0 = 1; iX0 < iXBoundM1; iX0++)
            {
                aaafFXYZ[iZ0][iY0][iX0] = (Real)0.0;

                for (iZ = 0; iZ <= 2; iZ++)
                {
                    for (iY = 0; iY <= 2; iY++)
                    {
                        for (iX = 0; iX <= 2; iX++)
                        {
                            fMask = fInvDXDYDZ*afCDer[iX]*afCDer[iY]*
                                afCDer[iZ];

                            aaafFXYZ[iZ0][iY0][iX0] += fMask*
                                aaafF[iZ0+iZ-1][iY0+iY-1][iX0+iX-1];
                        }
                    }
                }
            }
        }
    }

    // construct polynomials
    Allocate3D(iXBoundM1,iYBoundM1,iZBoundM1,m_aaakPoly);
    for (iZ = 0; iZ < iZBoundM1; iZ++)
    {
        for (iY = 0; iY < iYBoundM1; iY++)
        {
            for (iX = 0; iX < iXBoundM1; iX++)
            {
                // Note the 'transposing' of the 2x2x2 blocks (to match
                // notation used in the polynomial definition).
                Real aaafG[2][2][2] =
                {
                    {{aaafF[iZ][iY][iX],
                      aaafF[iZ+1][iY][iX]},
                     {aaafF[iZ][iY+1][iX],
                      aaafF[iZ+1][iY+1][iX]}},
                    {{aaafF[iZ][iY][iX+1],
                      aaafF[iZ+1][iY][iX+1]},
                     {aaafF[iZ][iY+1][iX+1],
                      aaafF[iZ+1][iY+1][iX+1]}}
                };

                Real aaafGX[2][2][2] =
                {
                    {{aaafFX[iZ][iY][iX],
                      aaafFX[iZ+1][iY][iX]},
                     {aaafFX[iZ][iY+1][iX],
                      aaafFX[iZ+1][iY+1][iX]}},
                    {{aaafFX[iZ][iY][iX+1],
                      aaafFX[iZ+1][iY][iX+1]},
                     {aaafFX[iZ][iY+1][iX+1],
                      aaafFX[iZ+1][iY+1][iX+1]}}
                };

                Real aaafGY[2][2][2] =
                {
                    {{aaafFY[iZ][iY][iX],
                      aaafFY[iZ+1][iY][iX]},
                     {aaafFY[iZ][iY+1][iX],
                      aaafFY[iZ+1][iY+1][iX]}},
                    {{aaafFY[iZ][iY][iX+1],
                      aaafFY[iZ+1][iY][iX+1]},
                     {aaafFY[iZ][iY+1][iX+1],
                      aaafFY[iZ+1][iY+1][iX+1]}}
                };

                Real aaafGZ[2][2][2] =
                {
                    {{aaafFZ[iZ][iY][iX],
                      aaafFZ[iZ+1][iY][iX]},
                     {aaafFZ[iZ][iY+1][iX],
                      aaafFZ[iZ+1][iY+1][iX]}},
                    {{aaafFZ[iZ][iY][iX+1],
                      aaafFZ[iZ+1][iY][iX+1]},
                     {aaafFZ[iZ][iY+1][iX+1],
                      aaafFZ[iZ+1][iY+1][iX+1]}}
                };

                Real aaafGXY[2][2][2] =
                {
                    {{aaafFXY[iZ][iY][iX],
                      aaafFXY[iZ+1][iY][iX]},
                     {aaafFXY[iZ][iY+1][iX],
                      aaafFXY[iZ+1][iY+1][iX]}},
                    {{aaafFXY[iZ][iY][iX+1],
                      aaafFXY[iZ+1][iY][iX+1]},
                     {aaafFXY[iZ][iY+1][iX+1],
                      aaafFXY[iZ+1][iY+1][iX+1]}}
                };

                Real aaafGXZ[2][2][2] =
                {
                    {{aaafFXZ[iZ][iY][iX],
                      aaafFXZ[iZ+1][iY][iX]},
                     {aaafFXZ[iZ][iY+1][iX],
                      aaafFXZ[iZ+1][iY+1][iX]}},
                    {{aaafFXZ[iZ][iY][iX+1],
                      aaafFXZ[iZ+1][iY][iX+1]},
                     {aaafFXZ[iZ][iY+1][iX+1],
                      aaafFXZ[iZ+1][iY+1][iX+1]}}
                };

                Real aaafGYZ[2][2][2] =
                {
                    {{aaafFYZ[iZ][iY][iX],
                      aaafFYZ[iZ+1][iY][iX]},
                     {aaafFYZ[iZ][iY+1][iX],
                      aaafFYZ[iZ+1][iY+1][iX]}},
                    {{aaafFYZ[iZ][iY][iX+1],
                      aaafFYZ[iZ+1][iY][iX+1]},
                     {aaafFYZ[iZ][iY+1][iX+1],
                      aaafFYZ[iZ+1][iY+1][iX+1]}}
                };

                Real aaafGXYZ[2][2][2] =
                {
                    {{aaafFXYZ[iZ][iY][iX],
                      aaafFXYZ[iZ+1][iY][iX]},
                     {aaafFXYZ[iZ][iY+1][iX],
                      aaafFXYZ[iZ+1][iY+1][iX]}},
                    {{aaafFXYZ[iZ][iY][iX+1],
                      aaafFXYZ[iZ+1][iY][iX+1]},
                     {aaafFXYZ[iZ][iY+1][iX+1],
                      aaafFXYZ[iZ+1][iY+1][iX+1]}}
                };

                Construct(m_aaakPoly[iZ][iY][iX],aaafG,aaafGX,aaafGY,
                    aaafGZ,aaafGXY,aaafGXZ,aaafGYZ,aaafGXYZ);
            }
        }
    }

    Deallocate3D(iYBound,iZBound,aaafXSlope);
    Deallocate3D(iXBound,iZBound,aaafYSlope);
    Deallocate3D(iXBound,iYBound,aaafZSlope);
    Deallocate3D(iYBound,iZBound,aaafFX);
    Deallocate3D(iYBound,iZBound,aaafFY);
    Deallocate3D(iYBound,iZBound,aaafFZ);
    Deallocate3D(iYBound,iZBound,aaafFXY);
    Deallocate3D(iYBound,iZBound,aaafFXZ);
    Deallocate3D(iYBound,iZBound,aaafFYZ);
    Deallocate3D(iYBound,iZBound,aaafFXYZ);
}
//----------------------------------------------------------------------------
template <class Real>
IntpAkimaUniform3<Real>::~IntpAkimaUniform3 ()
{
    Deallocate3D(m_iYBound-1,m_iZBound-1,m_aaakPoly);
}
//----------------------------------------------------------------------------
template <class Real>
int IntpAkimaUniform3<Real>::GetXBound () const
{
    return m_iXBound;
}
//----------------------------------------------------------------------------
template <class Real>
int IntpAkimaUniform3<Real>::GetYBound () const
{
    return m_iYBound;
}
//----------------------------------------------------------------------------
template <class Real>
int IntpAkimaUniform3<Real>::GetZBound () const
{
    return m_iZBound;
}
//----------------------------------------------------------------------------
template <class Real>
int IntpAkimaUniform3<Real>::GetQuantity () const
{
    return m_iQuantity;
}
//----------------------------------------------------------------------------
template <class Real>
Real*** IntpAkimaUniform3<Real>::GetF () const
{
    return m_aaafF;
}
//----------------------------------------------------------------------------
template <class Real>
typename IntpAkimaUniform3<Real>::Polynomial***
IntpAkimaUniform3<Real>::GetPolynomials () const
{
    return m_aaakPoly;
}
//----------------------------------------------------------------------------
template <class Real>
const typename IntpAkimaUniform3<Real>::Polynomial&
IntpAkimaUniform3<Real>::GetPolynomial (int iX, int iY, int iZ) const
{
    assert( iX < m_iXBound-1 && iY < m_iYBound-1 && iZ < m_iZBound-1 );
    return m_aaakPoly[iZ][iY][iX];
}
//----------------------------------------------------------------------------
template <class Real>
Real IntpAkimaUniform3<Real>::GetXMin () const
{
    return m_fXMin;
}
//----------------------------------------------------------------------------
template <class Real>
Real IntpAkimaUniform3<Real>::GetXMax () const
{
    return m_fXMax;
}
//----------------------------------------------------------------------------
template <class Real>
Real IntpAkimaUniform3<Real>::GetXSpacing () const
{
    return m_fXSpacing;
}
//----------------------------------------------------------------------------
template <class Real>
Real IntpAkimaUniform3<Real>::GetYMin () const
{
    return m_fYMin;
}
//----------------------------------------------------------------------------
template <class Real>
Real IntpAkimaUniform3<Real>::GetYMax () const
{
    return m_fYMax;
}
//----------------------------------------------------------------------------
template <class Real>
Real IntpAkimaUniform3<Real>::GetYSpacing () const
{
    return m_fYSpacing;
}
//----------------------------------------------------------------------------
template <class Real>
Real IntpAkimaUniform3<Real>::GetZMin () const
{
    return m_fZMin;
}
//----------------------------------------------------------------------------
template <class Real>
Real IntpAkimaUniform3<Real>::GetZMax () const
{
    return m_fZMax;
}
//----------------------------------------------------------------------------
template <class Real>
Real IntpAkimaUniform3<Real>::GetZSpacing () const
{
    return m_fZSpacing;
}
//----------------------------------------------------------------------------
template <class Real>
Real IntpAkimaUniform3<Real>::ComputeDerivative (Real* afSlope) const
{
    if ( afSlope[1] != afSlope[2] )
    {
        if ( afSlope[0] != afSlope[1] )
        {
            if ( afSlope[2] != afSlope[3] )
            {
                Real fAD0 = Math<Real>::FAbs(afSlope[3] - afSlope[2]);
                Real fAD1 = Math<Real>::FAbs(afSlope[0] - afSlope[1]);
                return (fAD0*afSlope[1]+fAD1*afSlope[2])/(fAD0+fAD1);
            }
            else
            {
                return afSlope[2];
            }
        }
        else
        {
            if ( afSlope[2] != afSlope[3] )
                return afSlope[1];
            else
                return ((Real)0.5)*(afSlope[1]+afSlope[2]);
        }
    }
    else
    {
        return afSlope[1];
    }
}
//----------------------------------------------------------------------------
template <class Real>
void IntpAkimaUniform3<Real>::Construct (Polynomial& rkPoly,
    Real aaafF[2][2][2], Real aaafFX[2][2][2], Real aaafFY[2][2][2],
    Real aaafFZ[2][2][2], Real aaafFXY[2][2][2], Real aaafFXZ[2][2][2],
    Real aaafFYZ[2][2][2], Real aaafFXYZ[2][2][2])
{
    Real fDX = m_fXSpacing, fDY = m_fYSpacing, fDZ = m_fZSpacing;
    Real fInvDX = ((Real)1.0)/fDX, fInvDX2 = fInvDX*fInvDX;
    Real fInvDY = ((Real)1.0)/fDY, fInvDY2 = fInvDY*fInvDY;
    Real fInvDZ = ((Real)1.0)/fDZ, fInvDZ2 = fInvDZ*fInvDZ;
    Real fB0, fB1, fB2, fB3, fB4, fB5, fB6, fB7;

    rkPoly.A(0,0,0) = aaafF[0][0][0];
    rkPoly.A(1,0,0) = aaafFX[0][0][0];
    rkPoly.A(0,1,0) = aaafFY[0][0][0];
    rkPoly.A(0,0,1) = aaafFZ[0][0][0];
    rkPoly.A(1,1,0) = aaafFXY[0][0][0];
    rkPoly.A(1,0,1) = aaafFXZ[0][0][0];
    rkPoly.A(0,1,1) = aaafFYZ[0][0][0];
    rkPoly.A(1,1,1) = aaafFXYZ[0][0][0];

    // solve for Aij0
    fB0 = (aaafF[1][0][0] - rkPoly(0,0,0,fDX,(Real)0.0,(Real)0.0))*fInvDX2;
    fB1 = (aaafFX[1][0][0] - rkPoly(1,0,0,fDX,(Real)0.0,(Real)0.0))*fInvDX;
    rkPoly.A(2,0,0) = ((Real)3.0)*fB0 - fB1;
    rkPoly.A(3,0,0) = (-((Real)2.0)*fB0 + fB1)*fInvDX;

    fB0 = (aaafF[0][1][0] - rkPoly(0,0,0,(Real)0.0,fDY,(Real)0.0))*fInvDY2;
    fB1 = (aaafFY[0][1][0] - rkPoly(0,1,0,(Real)0.0,fDY,(Real)0.0))*fInvDY;
    rkPoly.A(0,2,0) = ((Real)3.0)*fB0 - fB1;
    rkPoly.A(0,3,0) = (-((Real)2.0)*fB0 + fB1)*fInvDY;

    fB0 = (aaafFY[1][0][0] - rkPoly(0,1,0,fDX,(Real)0.0,(Real)0.0))*fInvDX2;
    fB1 = (aaafFXY[1][0][0] - rkPoly(1,1,0,fDX,(Real)0.0,(Real)0.0))*fInvDX;
    rkPoly.A(2,1,0) = ((Real)3.0)*fB0 - fB1;
    rkPoly.A(3,1,0) = (-((Real)2.0)*fB0 + fB1)*fInvDX;

    fB0 = (aaafFX[0][1][0] - rkPoly(1,0,0,(Real)0.0,fDY,(Real)0.0))*fInvDY2;
    fB1 = (aaafFXY[0][1][0] - rkPoly(1,1,0,(Real)0.0,fDY,(Real)0.0))*fInvDY;
    rkPoly.A(1,2,0) = ((Real)3.0)*fB0 - fB1;
    rkPoly.A(1,3,0) = (-((Real)2.0)*fB0 + fB1)*fInvDY;

    fB0 = (aaafF[1][1][0] - rkPoly(0,0,0,fDX,fDY,(Real)0.0))*fInvDX2*fInvDY2;
    fB1 = (aaafFX[1][1][0] - rkPoly(1,0,0,fDX,fDY,(Real)0.0))*fInvDX*fInvDY2;
    fB2 = (aaafFY[1][1][0] - rkPoly(0,1,0,fDX,fDY,(Real)0.0))*fInvDX2*fInvDY;
    fB3 = (aaafFXY[1][1][0] - rkPoly(1,1,0,fDX,fDY,(Real)0.0))*fInvDX*fInvDY;
    rkPoly.A(2,2,0) = ((Real)9.0)*fB0 - ((Real)3.0)*fB1 - ((Real)3.0)*fB2
        + fB3;
    rkPoly.A(3,2,0) = (-((Real)6.0)*fB0 + ((Real)3.0)*fB1 + ((Real)2.0)*fB2
        - fB3)*fInvDX;
    rkPoly.A(2,3,0) = (-((Real)6.0)*fB0 + ((Real)2.0)*fB1 + ((Real)3.0)*fB2
        - fB3)*fInvDY;
    rkPoly.A(3,3,0) = (((Real)4.0)*fB0 - ((Real)2.0)*fB1 - ((Real)2.0)*fB2
        + fB3)*fInvDX*fInvDY;

    // solve for Ai0k
    fB0 = (aaafF[0][0][1] - rkPoly(0,0,0,(Real)0.0,(Real)0.0,fDZ))*fInvDZ2;
    fB1 = (aaafFZ[0][0][1] - rkPoly(0,0,1,(Real)0.0,(Real)0.0,fDZ))*fInvDZ;
    rkPoly.A(0,0,2) = ((Real)3.0)*fB0 - fB1;
    rkPoly.A(0,0,3) = (-((Real)2.0)*fB0 + fB1)*fInvDZ;

    fB0 = (aaafFZ[1][0][0] - rkPoly(0,0,1,fDX,(Real)0.0,(Real)0.0))*fInvDX2;
    fB1 = (aaafFXZ[1][0][0] - rkPoly(1,0,1,fDX,(Real)0.0,(Real)0.0))*fInvDX;
    rkPoly.A(2,0,1) = ((Real)3.0)*fB0 - fB1;
    rkPoly.A(3,0,1) = (-((Real)2.0)*fB0 + fB1)*fInvDX;

    fB0 = (aaafFX[0][0][1] - rkPoly(1,0,0,(Real)0.0,(Real)0.0,fDZ))*fInvDZ2;
    fB1 = (aaafFXZ[0][0][1] - rkPoly(1,0,1,(Real)0.0,(Real)0.0,fDZ))*fInvDZ;
    rkPoly.A(1,0,2) = ((Real)3.0)*fB0 - fB1;
    rkPoly.A(1,0,3) = (-((Real)2.0)*fB0 + fB1)*fInvDZ;

    fB0 = (aaafF[1][0][1] - rkPoly(0,0,0,fDX,(Real)0.0,fDZ))*fInvDX2*fInvDZ2;
    fB1 = (aaafFX[1][0][1] - rkPoly(1,0,0,fDX,(Real)0.0,fDZ))*fInvDX*fInvDZ2;
    fB2 = (aaafFZ[1][0][1] - rkPoly(0,0,1,fDX,(Real)0.0,fDZ))*fInvDX2*fInvDZ;
    fB3 = (aaafFXZ[1][0][1] - rkPoly(1,0,1,fDX,(Real)0.0,fDZ))*fInvDX*fInvDZ;
    rkPoly.A(2,0,2) = ((Real)9.0)*fB0 - ((Real)3.0)*fB1 - ((Real)3.0)*fB2
        + fB3;
    rkPoly.A(3,0,2) = (-((Real)6.0)*fB0 + ((Real)3.0)*fB1 + ((Real)2.0)*fB2
        - fB3)*fInvDX;
    rkPoly.A(2,0,3) = (-((Real)6.0)*fB0 + ((Real)2.0)*fB1 + ((Real)3.0)*fB2
        - fB3)*fInvDZ;
    rkPoly.A(3,0,3) = (((Real)4.0)*fB0 - ((Real)2.0)*fB1 - ((Real)2.0)*fB2
        + fB3)*fInvDX*fInvDZ;

    // solve for A0jk
    fB0 = (aaafFZ[0][1][0] - rkPoly(0,0,1,(Real)0.0,fDY,(Real)0.0))*fInvDY2;
    fB1 = (aaafFYZ[0][1][0] - rkPoly(0,1,1,(Real)0.0,fDY,(Real)0.0))*fInvDY;
    rkPoly.A(0,2,1) = ((Real)3.0)*fB0 - fB1;
    rkPoly.A(0,3,1) = (-((Real)2.0)*fB0 + fB1)*fInvDY;

    fB0 = (aaafFY[0][0][1] - rkPoly(0,1,0,(Real)0.0,(Real)0.0,fDZ))*fInvDZ2;
    fB1 = (aaafFYZ[0][0][1] - rkPoly(0,1,1,(Real)0.0,(Real)0.0,fDZ))*fInvDZ;
    rkPoly.A(0,1,2) = ((Real)3.0)*fB0 - fB1;
    rkPoly.A(0,1,3) = (-((Real)2.0)*fB0 + fB1)*fInvDZ;

    fB0 = (aaafF[0][1][1] - rkPoly(0,0,0,(Real)0.0,fDY,fDZ))*fInvDY2*fInvDZ2;
    fB1 = (aaafFY[0][1][1] - rkPoly(0,1,0,(Real)0.0,fDY,fDZ))*fInvDY*fInvDZ2;
    fB2 = (aaafFZ[0][1][1] - rkPoly(0,0,1,(Real)0.0,fDY,fDZ))*fInvDY2*fInvDZ;
    fB3 = (aaafFYZ[0][1][1] - rkPoly(0,1,1,(Real)0.0,fDY,fDZ))*fInvDY*fInvDZ;
    rkPoly.A(0,2,2) = ((Real)9.0)*fB0 - ((Real)3.0)*fB1 - ((Real)3.0)*fB2
        + fB3;
    rkPoly.A(0,3,2) = (-((Real)6.0)*fB0 + ((Real)3.0)*fB1 + ((Real)2.0)*fB2
        - fB3)*fInvDY;
    rkPoly.A(0,2,3) = (-((Real)6.0)*fB0 + ((Real)2.0)*fB1 + ((Real)3.0)*fB2
        - fB3)*fInvDZ;
    rkPoly.A(0,3,3) = (((Real)4.0)*fB0 - ((Real)2.0)*fB1 - ((Real)2.0)*fB2
        + fB3)*fInvDY*fInvDZ;

    // solve for Aij1
    fB0 = (aaafFYZ[1][0][0] - rkPoly(0,1,1,fDX,(Real)0.0,(Real)0.0))*fInvDX2;
    fB1 = (aaafFXYZ[1][0][0] - rkPoly(1,1,1,fDX,(Real)0.0,(Real)0.0))*fInvDX;
    rkPoly.A(2,1,1) = ((Real)3.0)*fB0 - fB1;
    rkPoly.A(3,1,1) = (-((Real)2.0)*fB0 + fB1)*fInvDX;

    fB0 = (aaafFXZ[0][1][0] - rkPoly(1,0,1,(Real)0.0,fDY,(Real)0.0))*fInvDY2;
    fB1 = (aaafFXYZ[0][1][0] - rkPoly(1,1,1,(Real)0.0,fDY,(Real)0.0))*fInvDY;
    rkPoly.A(1,2,1) = ((Real)3.0)*fB0 - fB1;
    rkPoly.A(1,3,1) = (-((Real)2.0)*fB0 + fB1)*fInvDY;

    fB0 = (aaafFZ[1][1][0] - rkPoly(0,0,1,fDX,fDY,(Real)0.0))*fInvDX2*fInvDY2;
    fB1 = (aaafFXZ[1][1][0] - rkPoly(1,0,1,fDX,fDY,(Real)0.0))*fInvDX*fInvDY2;
    fB2 = (aaafFYZ[1][1][0] - rkPoly(0,1,1,fDX,fDY,(Real)0.0))*fInvDX2*fInvDY;
    fB3 = (aaafFXYZ[1][1][0] - rkPoly(1,1,1,fDX,fDY,(Real)0.0))*fInvDX*fInvDY;
    rkPoly.A(2,2,1) = ((Real)9.0)*fB0 - ((Real)3.0)*fB1 - ((Real)3.0)*fB2
        + fB3;
    rkPoly.A(3,2,1) = (-((Real)6.0)*fB0 + ((Real)3.0)*fB1 + ((Real)2.0)*fB2
        - fB3)*fInvDX;
    rkPoly.A(2,3,1) = (-((Real)6.0)*fB0 + ((Real)2.0)*fB1 + ((Real)3.0)*fB2
        - fB3)*fInvDY;
    rkPoly.A(3,3,1) = (((Real)4.0)*fB0 - ((Real)2.0)*fB1 - ((Real)2.0)*fB2
        + fB3)*fInvDX*fInvDY;

    // solve for Ai1k
    fB0 = (aaafFXY[0][0][1] - rkPoly(1,1,0,(Real)0.0,(Real)0.0,fDZ))*fInvDZ2;
    fB1 = (aaafFXYZ[0][0][1] - rkPoly(1,1,1,(Real)0.0,(Real)0.0,fDZ))*fInvDZ;
    rkPoly.A(1,1,2) = ((Real)3.0)*fB0 - fB1;
    rkPoly.A(1,1,3) = (-((Real)2.0)*fB0 + fB1)*fInvDZ;

    fB0 = (aaafFY[1][0][1] - rkPoly(0,1,0,fDX,(Real)0.0,fDZ))*fInvDX2*fInvDZ2;
    fB1 = (aaafFXY[1][0][1] - rkPoly(1,1,0,fDX,(Real)0.0,fDZ))*fInvDX*fInvDZ2;
    fB2 = (aaafFYZ[1][0][1] - rkPoly(0,1,1,fDX,(Real)0.0,fDZ))*fInvDX2*fInvDZ;
    fB3 = (aaafFXYZ[1][0][1] - rkPoly(1,1,1,fDX,(Real)0.0,fDZ))*fInvDX*fInvDZ;
    rkPoly.A(2,1,2) = ((Real)9.0)*fB0 - ((Real)3.0)*fB1 - ((Real)3.0)*fB2
        + fB3;
    rkPoly.A(3,1,2) = (-((Real)6.0)*fB0 + ((Real)3.0)*fB1 + ((Real)2.0)*fB2
        - fB3)*fInvDX;
    rkPoly.A(2,1,3) = (-((Real)6.0)*fB0 + ((Real)2.0)*fB1 + ((Real)3.0)*fB2
        - fB3)*fInvDZ;
    rkPoly.A(3,1,3) = (((Real)4.0)*fB0 - ((Real)2.0)*fB1 - ((Real)2.0)*fB2
        + fB3)*fInvDX*fInvDZ;

    // solve for A1jk
    fB0 = (aaafFX[0][1][1] - rkPoly(1,0,0,(Real)0.0,fDY,fDZ))*fInvDY2*fInvDZ2;
    fB1 = (aaafFXY[0][1][1] - rkPoly(1,1,0,(Real)0.0,fDY,fDZ))*fInvDY*fInvDZ2;
    fB2 = (aaafFXZ[0][1][1] - rkPoly(1,0,1,(Real)0.0,fDY,fDZ))*fInvDY2*fInvDZ;
    fB3 = (aaafFXYZ[0][1][1] - rkPoly(1,1,1,(Real)0.0,fDY,fDZ))*fInvDY*fInvDZ;
    rkPoly.A(1,2,2) = ((Real)9.0)*fB0 - ((Real)3.0)*fB1 - ((Real)3.0)*fB2
        + fB3;
    rkPoly.A(1,3,2) = (-((Real)6.0)*fB0 + ((Real)3.0)*fB1 + ((Real)2.0)*fB2
        - fB3)*fInvDY;
    rkPoly.A(1,2,3) = (-((Real)6.0)*fB0 + ((Real)2.0)*fB1 + ((Real)3.0)*fB2
        - fB3)*fInvDZ;
    rkPoly.A(1,3,3) = (((Real)4.0)*fB0 - ((Real)2.0)*fB1 - ((Real)2.0)*fB2
        + fB3)*fInvDY*fInvDZ;

    // solve for remaining Aijk with i >= 2, j >= 2, k >= 2
    fB0 = (aaafF[1][1][1]-rkPoly(0,0,0,fDX,fDY,fDZ))*fInvDX2*fInvDY2*fInvDZ2;
    fB1 = (aaafFX[1][1][1]-rkPoly(1,0,0,fDX,fDY,fDZ))*fInvDX*fInvDY2*fInvDZ2;
    fB2 = (aaafFY[1][1][1]-rkPoly(0,1,0,fDX,fDY,fDZ))*fInvDX2*fInvDY*fInvDZ2;
    fB3 = (aaafFZ[1][1][1]-rkPoly(0,0,1,fDX,fDY,fDZ))*fInvDX2*fInvDY2*fInvDZ;
    fB4 = (aaafFXY[1][1][1]-rkPoly(1,1,0,fDX,fDY,fDZ))*fInvDX*fInvDY*fInvDZ2;
    fB5 = (aaafFXZ[1][1][1]-rkPoly(1,0,1,fDX,fDY,fDZ))*fInvDX*fInvDY2*fInvDZ;
    fB6 = (aaafFYZ[1][1][1]-rkPoly(0,1,1,fDX,fDY,fDZ))*fInvDX2*fInvDY*fInvDZ;
    fB7 = (aaafFXYZ[1][1][1]-rkPoly(1,1,1,fDX,fDY,fDZ))*fInvDX*fInvDY*fInvDZ;
    rkPoly.A(2,2,2) = ((Real)27.0)*fB0 - ((Real)9.0)*fB1 - ((Real)9.0)*fB2 -
        ((Real)9.0)*fB3 + ((Real)3.0)*fB4 +
        ((Real)3.0)*fB5 + ((Real)3.0)*fB6 - fB7;
    rkPoly.A(3,2,2) = (-18.0f*fB0 + ((Real)9.0)*fB1 + ((Real)6.0)*fB2 +
        ((Real)6.0)*fB3 - ((Real)3.0)*fB4
        - ((Real)3.0)*fB5 - ((Real)2.0)*fB6 + fB7)*fInvDX;
    rkPoly.A(2,3,2) = (-18.0f*fB0 + ((Real)6.0)*fB1 + ((Real)9.0)*fB2 +
        ((Real)6.0)*fB3 - ((Real)3.0)*fB4
        - ((Real)2.0)*fB5 - ((Real)3.0)*fB6 + fB7)*fInvDY;
    rkPoly.A(2,2,3) = (-18.0f*fB0 + ((Real)6.0)*fB1 + ((Real)6.0)*fB2 +
        ((Real)9.0)*fB3 - ((Real)2.0)*fB4
        - ((Real)3.0)*fB5 - ((Real)3.0)*fB6 + fB7)*fInvDZ;
    rkPoly.A(3,3,2) = (12.0f*fB0 - ((Real)6.0)*fB1 - ((Real)6.0)*fB2 -
        ((Real)4.0)*fB3 + ((Real)3.0)*fB4
        + ((Real)2.0)*fB5 + ((Real)2.0)*fB6 - fB7)*fInvDX*fInvDY;
    rkPoly.A(3,2,3) = (12.0f*fB0 - ((Real)6.0)*fB1 - ((Real)4.0)*fB2 -
        ((Real)6.0)*fB3 + ((Real)2.0)*fB4
        + ((Real)3.0)*fB5 + ((Real)2.0)*fB6 - fB7)*fInvDX*fInvDZ;
    rkPoly.A(2,3,3) = (12.0f*fB0 - ((Real)4.0)*fB1 - ((Real)6.0)*fB2 -
        ((Real)6.0)*fB3 + ((Real)2.0)*fB4
        + ((Real)2.0)*fB5 + ((Real)3.0)*fB6 - fB7)*fInvDY*fInvDZ;
    rkPoly.A(3,3,3) = (-8.0f*fB0 + ((Real)4.0)*fB1 + ((Real)4.0)*fB2 +
        ((Real)4.0)*fB3 - ((Real)2.0)*fB4
        - ((Real)2.0)*fB5 - ((Real)2.0)*fB6 + fB7)*fInvDX*fInvDY*fInvDZ;
}
//----------------------------------------------------------------------------
template <class Real>
bool IntpAkimaUniform3<Real>::XLookup (Real fX, int& riXIndex, Real& rfDX)
    const
{
    if ( fX >= m_fXMin )
    {
        if ( fX <= m_fXMax )
        {
            for (riXIndex = 0; riXIndex+1 < m_iXBound; riXIndex++)
            {
                if ( fX < m_fXMin + m_fXSpacing*(riXIndex+1) )
                {
                    rfDX = fX - (m_fXMin + m_fXSpacing*riXIndex);
                    return true;
                }
            }

            riXIndex--;
            rfDX = fX - (m_fXMin + m_fXSpacing*riXIndex);
            return true;
        }
    }

    return false;
}
//----------------------------------------------------------------------------
template <class Real>
bool IntpAkimaUniform3<Real>::YLookup (Real fY, int& riYIndex, Real& rfDY)
    const
{
    if ( fY >= m_fYMin )
    {
        if ( fY <= m_fYMax )
        {
            for (riYIndex = 0; riYIndex+1 < m_iYBound; riYIndex++)
            {
                if ( fY < m_fYMin + m_fYSpacing*(riYIndex+1) )
                {
                    rfDY = fY - (m_fYMin + m_fYSpacing*riYIndex);
                    return true;
                }
            }

            riYIndex--;
            rfDY = fY - (m_fYMin + m_fYSpacing*riYIndex);
            return true;
        }
    }

    return false;
}
//----------------------------------------------------------------------------
template <class Real>
bool IntpAkimaUniform3<Real>::ZLookup (Real fZ, int& riZIndex, Real& rfDZ)
    const
{
    if ( fZ >= m_fZMin )
    {
        if ( fZ <= m_fZMax )
        {
            for (riZIndex = 0; riZIndex+1 < m_iZBound; riZIndex++)
            {
                if ( fZ < m_fZMin + m_fZSpacing*(riZIndex+1) )
                {
                    rfDZ = fZ - (m_fZMin + m_fZSpacing*riZIndex);
                    return true;
                }
            }

            riZIndex--;
            rfDZ = fZ - (m_fZMin + m_fZSpacing*riZIndex);
            return true;
        }
    }

    return false;
}
//----------------------------------------------------------------------------
template <class Real>
Real IntpAkimaUniform3<Real>::operator() (Real fX, Real fY, Real fZ) const
{
    int iX, iY, iZ;
    Real fDX, fDY, fDZ;

    if ( XLookup(fX,iX,fDX) && YLookup(fY,iY,fDY) && ZLookup(fZ,iZ,fDZ) )
        return m_aaakPoly[iZ][iY][iX](fDX,fDY,fDZ);
    else
        return Math<Real>::MAX_REAL;
}
//----------------------------------------------------------------------------
template <class Real>
Real IntpAkimaUniform3<Real>::operator() (int iXOrder, int iYOrder,
    int iZOrder, Real fX, Real fY, Real fZ) const
{
    int iX, iY, iZ;
    Real fDX, fDY, fDZ;

    if ( XLookup(fX,iX,fDX) && YLookup(fY,iY,fDY) && ZLookup(fZ,iZ,fDZ) )
        return m_aaakPoly[iZ][iY][iX](iXOrder,iYOrder,iZOrder,fDX,fDY,fDZ);
    else
        return Math<Real>::MAX_REAL;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM IntpAkimaUniform3<float>;
template class WML_ITEM IntpAkimaUniform3<double>;
}
//----------------------------------------------------------------------------
