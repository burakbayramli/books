// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlRasterDrawing.h"
#include "WmlMath.h"
#include "WmlTInteger.h"
using namespace Wml;

//----------------------------------------------------------------------------
void Wml::Line2D (int iX0, int iY0, int iX1, int iY1,
    void (*oCallback)(int,int))
{
    // starting point of line
    int iX = iX0, iY = iY0;

    // direction of line
    int iDx = iX1-iX0, iDy = iY1-iY0;

    // increment or decrement depending on direction of line
    int iSx = (iDx > 0 ? 1 : (iDx < 0 ? -1 : 0));
    int iSy = (iDy > 0 ? 1 : (iDy < 0 ? -1 : 0));

    // decision parameters for voxel selection
    if ( iDx < 0 ) iDx = -iDx;
    if ( iDy < 0 ) iDy = -iDy;
    int iAx = 2*iDx, iAy = 2*iDy;
    int iDecX, iDecY;

    // determine largest direction component, single-step related variable
    int iMax = iDx, iVar = 0;
    if ( iDy > iMax ) { iVar = 1; }

    // traverse Bresenham line
    switch ( iVar )
    {
    case 0:  // single-step in x-direction
        iDecY = iAy - iDx;
        for (/**/; /**/; iX += iSx, iDecY += iAy)
        {
            // process pixel
            oCallback(iX,iY);

            // take Bresenham step
            if ( iX == iX1 )  break;
            if ( iDecY >= 0 ) { iDecY -= iAx; iY += iSy; }
        }
        break;
    case 1:  // single-step in y-direction
        iDecX = iAx - iDy;
        for (/**/; /**/; iY += iSy, iDecX += iAx)
        {
            // process pixel
            oCallback(iX,iY);

            // take Bresenham step
            if ( iY == iY1 ) break;
            if ( iDecX >= 0 ) { iDecX -= iAy; iX += iSx; }
        }
        break;
    }
}
//----------------------------------------------------------------------------
void Wml::Line3D (int iX0, int iY0, int iZ0, int iX1, int iY1, int iZ1,
    void (*oCallback)(int,int,int))
{
    // starting point of line
    int iX = iX0, iY = iY0, iZ = iZ0;

    // direction of line
    int iDx = iX1-iX0, iDy = iY1-iY0, iDz = iZ1-iZ0;

    // increment or decrement depending on direction of line
    int iSx = (iDx > 0 ? 1 : (iDx < 0 ? -1 : 0));
    int iSy = (iDy > 0 ? 1 : (iDy < 0 ? -1 : 0));
    int iSz = (iDz > 0 ? 1 : (iDz < 0 ? -1 : 0));

    // decision parameters for voxel selection
    if ( iDx < 0 ) iDx = -iDx;
    if ( iDy < 0 ) iDy = -iDy;
    if ( iDz < 0 ) iDz = -iDz;
    int iAx = 2*iDx, iAy = 2*iDy, iAz = 2*iDz;
    int iDecX, iDecY, iDecZ;

    // determine largest direction component, single-step related variable
    int iMax = iDx, iVar = 0;
    if ( iDy > iMax ) { iMax = iDy; iVar = 1; }
    if ( iDz > iMax ) { iVar = 2; }

    // traverse Bresenham line
    switch ( iVar )
    {
    case 0:  // single-step in iX-direction
        iDecY = iAy - iDx;
        iDecZ = iAz - iDx;
        for (/**/; /**/; iX += iSx, iDecY += iAy, iDecZ += iAz)
        {
            // process voxel
            oCallback(iX,iY,iZ);

            // take Bresenham step
            if ( iX == iX1 ) break;
            if ( iDecY >= 0 ) { iDecY -= iAx; iY += iSy; }
            if ( iDecZ >= 0 ) { iDecZ -= iAx; iZ += iSz; }
        }
        break;
    case 1:  // single-step in iY-direction
        iDecX = iAx - iDy;
        iDecZ = iAz - iDy;
        for (/**/; /**/; iY += iSy, iDecX += iAx, iDecZ += iAz)
        {
            // process voxel
            oCallback(iX,iY,iZ);

            // take Bresenham step
            if ( iY == iY1 ) break;
            if ( iDecX >= 0 ) { iDecX -= iAy; iX += iSx; }
            if ( iDecZ >= 0 ) { iDecZ -= iAy; iZ += iSz; }
        }
        break;
    case 2:  // single-step in iZ-direction
        iDecX = iAx - iDz;
        iDecY = iAy - iDz;
        for (/**/; /**/; iZ += iSz, iDecX += iAx, iDecY += iAy)
        {
            // process voxel
            oCallback(iX,iY,iZ);

            // take Bresenham step
            if ( iZ == iZ1 ) break;
            if ( iDecX >= 0 ) { iDecX -= iAz; iX += iSx; }
            if ( iDecY >= 0 ) { iDecY -= iAz; iY += iSy; }
        }
        break;
    }
}
//----------------------------------------------------------------------------
void Wml::Line4D (int iX0, int iY0, int iZ0, int iW0, int iX1, int iY1,
    int iZ1, int iW1,  void (*oCallback)(int,int,int,int))
{
    // starting point of line
    int iX = iX0, iY = iY0, iZ = iZ0, iW = iW0;

    // direction of line
    int iDx = iX1-iX0, iDy = iY1-iY0, iDz = iZ1-iZ0, iDw = iW1-iW0;

    // increment or decrement depending on direction of line
    int iSx = (iDx > 0 ? 1 : (iDx < 0 ? -1 : 0));
    int iSy = (iDy > 0 ? 1 : (iDy < 0 ? -1 : 0));
    int iSz = (iDz > 0 ? 1 : (iDz < 0 ? -1 : 0));
    int iSw = (iDw > 0 ? 1 : (iDw < 0 ? -1 : 0));

    // decision parameters for voxel selection
    if ( iDx < 0 ) iDx = -iDx;
    if ( iDy < 0 ) iDy = -iDy;
    if ( iDz < 0 ) iDz = -iDz;
    if ( iDw < 0 ) iDw = -iDw;
    int iAx = 2*iDx, iAy = 2*iDy, iAz = 2*iDz, iAw = 2*iDw;
    int iDecX, iDecY, iDecZ, iDecW;

    // determine largest direction component, single-step related variable
    int iMax = iDx, iVar = 0;
    if ( iDy > iMax ) { iMax = iDy; iVar = 1; }
    if ( iDz > iMax ) { iMax = iDz; iVar = 2; }
    if ( iDw > iMax ) { iVar = 3; }

    // traverse Bresenham line
    switch ( iVar )
    {
    case 0:  // single-step in iX-direction
        iDecY = iAy - iDx;
        iDecZ = iAz - iDx;
        iDecW = iAw - iDx;
        for (/**/; /**/; iX += iSx, iDecY += iAy, iDecZ += iAz, iDecW += iAw)
        {
            // process hypervoxel
            oCallback(iX,iY,iZ,iW);

            // take Bresenham step
            if ( iX == iX1 ) break;
            if ( iDecY >= 0 ) { iDecY -= iAx; iY += iSy; }
            if ( iDecZ >= 0 ) { iDecZ -= iAx; iZ += iSz; }
            if ( iDecW >= 0 ) { iDecW -= iAx; iW += iSw; }
        }
        break;
    case 1:  // single-step in iY-direction
        iDecX = iAx - iDy;
        iDecZ = iAz - iDy;
        iDecW = iAw - iDy;
        for (/**/; /**/; iY += iSy, iDecX += iAx, iDecZ += iAz, iDecW += iAw)
        {
            // process hypervoxel
            oCallback(iX,iY,iZ,iW);

            // take Bresenham step
            if ( iY == iY1 ) break;
            if ( iDecX >= 0 ) { iDecX -= iAy; iX += iSx; }
            if ( iDecZ >= 0 ) { iDecZ -= iAy; iZ += iSz; }
            if ( iDecW >= 0 ) { iDecW -= iAy; iW += iSw; }
        }
        break;
    case 2:  // single-step in iZ-direction
        iDecX = iAx - iDz;
        iDecY = iAy - iDz;
        iDecW = iAw - iDz;
        for (/**/; /**/; iZ += iSz, iDecX += iAx, iDecY += iAy, iDecW += iAw)
        {
            // process hypervoxel
            oCallback(iX,iY,iZ,iW);

            // take Bresenham step
            if ( iZ == iZ1 ) break;
            if ( iDecX >= 0 ) { iDecX -= iAz; iX += iSx; }
            if ( iDecY >= 0 ) { iDecY -= iAz; iY += iSy; }
            if ( iDecW >= 0 ) { iDecW -= iAz; iW += iSw; }
        }
        break;
    case 3:  // single-step in iP-direction
        iDecX = iAx - iDw;
        iDecY = iAy - iDw;
        iDecZ = iAz - iDw;
        for (/**/; /**/; iW += iSw, iDecX += iAx, iDecY += iAy, iDecZ += iAz)
        {
            // process voxel
            oCallback(iX,iY,iZ,iW);

            // take Bresenham step
            if ( iW == iW1 ) break;
            if ( iDecX >= 0 ) { iDecX -= iAw; iX += iSx; }
            if ( iDecY >= 0 ) { iDecY -= iAw; iY += iSy; }
            if ( iDecZ >= 0 ) { iDecZ -= iAw; iZ += iSz; }
        }
        break;
    }
}
//----------------------------------------------------------------------------
void Wml::Circle2D (int iXC, int iYC, int iR, void (*oCallback)(int,int))
{
    for (int iX = 0, iY = iR, iDec = 3-2*iR; iX <= iY; iX++)
    {
        oCallback(iXC+iX,iYC+iY);
        oCallback(iXC+iX,iYC-iY);
        oCallback(iXC-iX,iYC+iY);
        oCallback(iXC-iX,iYC-iY);
        oCallback(iXC+iY,iYC+iX);
        oCallback(iXC+iY,iYC-iX);
        oCallback(iXC-iY,iYC+iX);
        oCallback(iXC-iY,iYC-iX);

        if ( iDec >= 0 )
            iDec += -4*(iY--)+4;
        iDec += 4*iX+6;
    }
}
//----------------------------------------------------------------------------
void Wml::Ellipse2D (int iXC, int iYC, int iA, int iB,
    void (*oCallback)(int,int))
{
    int iA2 = iA*iA, iB2 = iB*iB;
    int iX, iY, iDec;

    for (iX = 0, iY = iB, iDec = 2*iB2+iA2*(1-2*iB); iB2*iX <= iA2*iY; iX++)
    {
        oCallback(iXC+iX,iYC+iY);
        oCallback(iXC-iX,iYC+iY);
        oCallback(iXC+iX,iYC-iY);
        oCallback(iXC-iX,iYC-iY);

        if ( iDec >= 0 )
            iDec += 4*iA2*(1-(iY--));
        iDec += iB2*(4*iX+6);
    }

    for (iX = iA, iY = 0, iDec = 2*iA2+iB2*(1-2*iA); iA2*iY <= iB2*iX; iY++)
    {
        oCallback(iXC+iX,iYC+iY);
        oCallback(iXC-iX,iYC+iY);
        oCallback(iXC+iX,iYC-iY);
        oCallback(iXC-iX,iYC-iY);

        if ( iDec >= 0 )
            iDec += 4*iB2*(1-(iX--));
        iDec += iA2*(4*iY+6);
    }
}
//----------------------------------------------------------------------------
static void SelectEllipsePoint (int iA2, int iB2, float fX, float fY, int& iX,
    int& iY)
{
    int iXFloor = int(Mathf::Floor(fX)), iYFloor = int(Mathf::Floor(fY));
    int iXIncr = iB2*(2*iXFloor+1), iYIncr = iA2*(2*iYFloor+1);
    int iBase = iB2*iXFloor*iXFloor+iA2*iYFloor*iYFloor-iA2*iB2;
    int iA00 = abs(iBase);
    int iA10 = abs(iBase+iXIncr);
    int iA01 = abs(iBase+iYIncr);
    int iA11 = abs(iBase+iXIncr+iYIncr);

    int iMin = iA00;
    iX = iXFloor;
    iY = iYFloor;
    if ( iA10 < iMin )
    {
        iMin = iA10;
        iX = iXFloor+1;
        iY = iYFloor;
    }
    if ( iA01 < iMin )
    {
        iMin = iA01;
        iX = iXFloor;
        iY = iYFloor+1;
    }
    if ( iA11 < iMin )
    {
        iMin = iA11;
        iX = iXFloor+1;
        iY = iYFloor+1;
    }
}
//----------------------------------------------------------------------------
static int WhichArc (int iA2, int iB2, int iX, int iY)
{
    if ( iX > 0 )
    {
        if ( iY > 0 )
            return ( iB2*iX <  iA2*iY ? 0 : 1 );
        else if ( iY < 0 )
            return ( iB2*iX > -iA2*iY ? 2 : 3 );
        else
            return 2;
    }
    else if ( iX < 0 )
    {
        if ( iY < 0 )
            return ( iA2*iY <  iB2*iX ? 4 : 5 );
        else if ( iY > 0 )
            return ( iA2*iY < -iB2*iX ? 6 : 7 );
        else
            return 6;
    }
    else
    {
        return ( iY > 0 ? 0 : 4 );
    }
}
//----------------------------------------------------------------------------
void Wml::EllipseArc2D (int iXC, int iYC, int iA, int iB, float fX0,
    float fY0, float fX1, float fY1, void (*oCallback)(int,int))
{
    // Assert (within floating point roundoff errors):
    //   (x0-xc)^2/a^2 + (y0-yc)^2/b^2 = 1
    //   (x1-xc)^2/a^2 + (y1-yc)^2/b^2 = 1
    // Assume if (x0,y0) == (x1,y1), then entire ellipse should be drawn.
    //
    // Pixels on arc are guaranteed to be traversed clockwise.

    const int iA2 = iA*iA, iB2 = iB*iB;

    // get integer end points for iArc
    int iX0, iY0, iX1, iY1;
    SelectEllipsePoint(iA2,iB2,fX0-iXC,fY0-iYC,iX0,iY0);
    SelectEllipsePoint(iA2,iB2,fX1-iXC,fY1-iYC,iX1,iY1);

    int iDx = iX0 - iX1, iDy = iY0 - iY1, iSqrLen = iDx*iDx+iDy*iDy;
    if ( iSqrLen == 1 || ( iSqrLen == 2 && abs(iDx) == 1 ) )
    {
        oCallback(iXC+iX0,iYC+iY0);
        oCallback(iXC+iX1,iYC+iY1);
        return;
    }

    // determine initial case for arc drawing
    int iArc = WhichArc(iA2,iB2,iX0,iY0);
    while ( true )
    {
        // process the pixel
        oCallback(iXC+iX0,iYC+iY0);

        // Determine next pixel to process.  Notation <(x,y),dy/dx> indicates
        // point on ellipse and slope at that point.
        int iSigma;
        switch ( iArc )
        {
        case 0:  // <(0,b),0> to <(u0,v0),-1>
            iX0++;
            iDx++;
            iSigma = iB2*iX0*iX0+iA2*(iY0-1)*(iY0-1)-iA2*iB2;
            if ( iSigma >= 0 )
            {
                iY0--;
                iDy--;
            }
            if ( iB2*iX0 >= iA2*iY0 )
            {
                // Slope dy/dx is no longer between 0 and -1.  Switch to next
                // arc drawer.  For large a and b, you expect to go to
                // 'iArc = 1'.  But for small a or b, it is possible that the
                // next arc is so small (on the discrete raster) that it is
                // skipped.
                if ( iY0 > 0 )
                    iArc = 1;
                else
                    iArc = 2;
            }
            break;
        case 1:  // <(u0,v0),-1> to <(a,0),infinity>
            iY0--;
            iDy--;
            iSigma = iB2*iX0*iX0+iA2*iY0*iY0-iA2*iB2;
            if ( iSigma < 0 )
            {
                iX0++;
                iDx++;
            }
            if ( iY0 == 0 )
                iArc = 2;
            break;
        case 2:  // <(a,0),infinity> to <(u1,v1),+1>
            iY0--;
            iDy--;
            iSigma = iB2*(iX0-1)*(iX0-1)+iA2*iY0*iY0-iA2*iB2;
            if ( iSigma >= 0 )
            {
                iX0--;
                iDx--;
            }
            if ( iB2*iX0 <= -iA2*iY0 )
            {
                // Slope dy/dx is no longer between 0 and +1.  Switch to next
                // arc drawer.  For large a and b, you expect to go to
                // 'iArc = 3'.  But for small a or b, it is possible that the
                // next arc is so small (on the discrete raster) that it is
                // skipped.
                if ( iX0 > 0 )
                    iArc = 3;
                else
                    iArc = 4;
            }
            break;
        case 3:  // <(u1,v1),+1> to <(0,-b),0>
            iX0--;
            iDx--;
            iSigma = iB2*iX0*iX0+iA2*iY0*iY0-iA2*iB2;
            if ( iSigma < 0 )
            {
                iY0--;
                iDy--;
            }
            if ( iX0 == 0 )
                iArc = 4;
            break;
        case 4:  // <(0,-b),0> to <(u2,v2,-1)>
            iX0--;
            iDx--;
            iSigma = iB2*iX0*iX0+iA2*(iY0+1)*(iY0+1)-iA2*iB2;
            if ( iSigma >= 0 )
            {
                iY0++;
                iDy++;
            }
            if ( iA2*iY0 >= iB2*iX0 )
            {
                // Slope dy/dx is no longer between 0 and -1.  Switch to next
                // arc drawer.  For large a and b, you expect to go to
                // 'iArc = 5'.  But for small a or b, it is possible that the
                // next arc is so small (on the discrete raster) that it is
                // skipped.
                if ( iY0 < 0 )
                    iArc = 5;
                else
                    iArc = 6;
            }
            break;
        case 5:  // <(u2,v2,-1)> to <(-a,0),infinity>
            iY0++;
            iDy++;
            iSigma = iB2*iX0*iX0+iA2*iY0*iY0-iA2*iB2;
            if ( iSigma < 0 )
            {
                iX0--;
                iDx--;
            }
            if ( iY0 == 0 )
                iArc = 6;
            break;
        case 6:  // <(-a,0),infinity> to <(u3,v3),+1>
            iY0++;
            iDy++;
            iSigma = iB2*(iX0+1)*(iX0+1)+iA2*iY0*iY0-iA2*iB2;
            if ( iSigma >= 0 )
            {
                iX0++;
                iDx++;
            }
            if ( iA2*iY0 >= -iB2*iX0 )
            {
                // Slope dy/dx is no longer between 0 and +1.  Switch to next
                // arc drawer.  For large a and b, you expect to go to
                // 'iArc = 7'.  But for small a or b, it is possible that the
                // next arc is so small (on the discrete raster) that it is
                // skipped.
                if ( iX0 < 0 )
                    iArc = 7;
                else
                    iArc = 8;
            }
            break;
        case 7:  // <(u3,v3),+1> to <(0,b),0>
            iX0++;
            iDx++;
            iSigma = iB2*iX0*iX0+iA2*iY0*iY0-iA2*iB2;
            if ( iSigma < 0 )
            {
                iY0++;
                iDy++;
            }
            if ( iX0 == 0 )
                iArc = 0;
            break;
        }

        iSqrLen = iDx*iDx+iDy*iDy;
        if ( iSqrLen <= 1 )
            break;
    }
}
//----------------------------------------------------------------------------
void Wml::GeneralEllipse2D (int iXC, int iYC, int iXA, int iYA, int iXB,
    int iYB, void (*oCallback)(int,int))
{
    // assert:  xa > 0, ya >= 0, xb <= 0, yb > 0

    // Ellipse is a*(x-xc)^2+2*b*(x-xc)*(y-yc)+c*(y-yc)^2 = d where
    //
    //   a = xa^2*Lb^4 + xb^2*La^4
    //   b = xa*ya*Lb^4 + xb*yb*La^4
    //   c = ya^2*Lb^4 + yb^2*La^4
    //   d = La^4*Lb^4
    //   La^2 = xa^2+ya^2
    //   Lb^2 = xb^2+yb^2
    //
    // Pixel determination is performed relative to origin (0,0).  The
    // ellipse at origin is a*x^2+b*x*y+c*y^2=d.  Slope of curve is
    // dy/dx = -(a*x+b*y)/(b*x+c*y).  Slope at (xb,yb) is
    // dy/dx = -xb/yb >= 0 and slope at (xa,ya) is dy/dx = -xa/ya < 0.

    TInteger<4> iXA2 = iXA*iXA;
    TInteger<4> iYA2 = iYA*iYA;
    TInteger<4> iXB2 = iXB*iXB;
    TInteger<4> iYB2 = iYB*iYB;
    TInteger<4> iXAYA = iXA*iYA;
    TInteger<4> iXBYB = iXB*iYB;
    TInteger<4> iLa2 = iXA2+iYA2;
    TInteger<4> iLa4 = iLa2*iLa2;
    TInteger<4> iLb2 = iXB2+iYB2;
    TInteger<4> iLb4 = iLb2*iLb2;
    TInteger<4> iA = iXA2*iLb4+iXB2*iLa4;
    TInteger<4> iB = iXAYA*iLb4+iXBYB*iLa4;
    TInteger<4> iC = iYA2*iLb4+iYB2*iLa4;
    TInteger<4> iD = iLa4*iLb4;

    TInteger<4> iDx, iDy, iSigma;
    int iX, iY, iXp1, iYm1, iYp1;

    if ( iYA <= iXA )
    {
        // start at (-xA,-yA)
        iX = -iXA;
        iY = -iYA;
        iDx = -(iB*iXA+iC*iYA);
        iDy = iA*iXA+iB*iYA;

        // arc from (-xA,-yA) to point (x0,y0) where dx/dy = 0
        while ( iDx <= 0 )
        {
            oCallback(iXC+iX,iYC+iY);
            oCallback(iXC-iX,iYC-iY);
            iY++;
            iSigma = iA*iX*iX+2*iB*iX*iY+iC*iY*iY-iD;
            if ( iSigma < 0 )
            {
                iDx -= iB;
                iDy += iA;
                iX--;
            }
            iDx += iC;
            iDy -= iB;
        }

        // arc from (x0,y0) to point (x1,y1) where dy/dx = 1
        while ( iDx <= iDy )
        {
            oCallback(iXC+iX,iYC+iY);
            oCallback(iXC-iX,iYC-iY);
            iY++;
            iXp1 = iX+1;
            iSigma = iA*iXp1*iXp1+2*iB*iXp1*iY+iC*iY*iY-iD;
            if ( iSigma >= 0 )
            {
                iDx += iB;
                iDy -= iA;
                iX = iXp1;
            }
            iDx += iC;
            iDy -= iB;
        }

        // arc from (x1,y1) to point (x2,y2) where dy/dx = 0
        while ( iDy >= 0 )
        {
            oCallback(iXC+iX,iYC+iY);
            oCallback(iXC-iX,iYC-iY);
            iX++;
            iSigma = iA*iX*iX+2*iB*iX*iY+iC*iY*iY-iD;
            if ( iSigma < 0 )
            {
                iDx += iC;
                iDy -= iB;
                iY++;
            }
            iDx += iB;
            iDy -= iA;
        }

        // arc from (x2,y2) to point (x3,y3) where dy/dx = -1
        while ( iDy >= -iDx )
        {
            oCallback(iXC+iX,iYC+iY);
            oCallback(iXC-iX,iYC-iY);
            iX++;
            iYm1 = iY-1;
            iSigma = iA*iX*iX+2*iB*iX*iYm1+iC*iYm1*iYm1-iD;
            if ( iSigma >= 0 )
            {
                iDx -= iC;
                iDy += iB;
                iY = iYm1;
            }
            iDx += iB;
            iDy -= iA;
        }

        // arc from (x3,y3) to (xa,ya)
        while ( iY >= iYA )
        {
            oCallback(iXC+iX,iYC+iY);
            oCallback(iXC-iX,iYC-iY);
            iY--;
            iSigma = iA*iX*iX+2*iB*iX*iY+iC*iY*iY-iD;
            if ( iSigma < 0 )
            {
                iDx += iB;
                iDy -= iA;
                iX++;
            }
            iDx -= iC;
            iDy += iB;
        }
    }
    else
    {
        // start at (-xa,-ya)
        iX = -iXA;
        iY = -iYA;
        iDx = -(iB*iXA+iC*iYA);
        iDy = iA*iXA+iB*iYA;

        // arc from (-xa,-ya) to point (x0,y0) where dy/dx = -1
        while ( -iDx >= iDy )
        {
            oCallback(iXC+iX,iYC+iY);
            oCallback(iXC-iX,iYC-iY);
            iX--;
            iYp1 = iY+1;
            iSigma = iA*iX*iX+2*iB*iX*iYp1+iC*iYp1*iYp1-iD;
            if ( iSigma >= 0 )
            {
                iDx += iC;
                iDy -= iB;
                iY = iYp1;
            }
            iDx -= iB;
            iDy += iA;
        }

        // arc from (x0,y0) to point (x1,y1) where dx/dy = 0
        while ( iDx <= 0 )
        {
            oCallback(iXC+iX,iYC+iY);
            oCallback(iXC-iX,iYC-iY);
            iY++;
            iSigma = iA*iX*iX+2*iB*iX*iY+iC*iY*iY-iD;
            if ( iSigma < 0 )
            {
                iDx -= iB;
                iDy += iA;
                iX--;
            }
            iDx += iC;
            iDy -= iB;
        }

        // arc from (x1,y1) to point (x2,y2) where dy/dx = 1
        while ( iDx <= iDy )
        {
            oCallback(iXC+iX,iYC+iY);
            oCallback(iXC-iX,iYC-iY);
            iY++;
            iXp1 = iX+1;
            iSigma = iA*iXp1*iXp1+2*iB*iXp1*iY+iC*iY*iY-iD;
            if ( iSigma >= 0 )
            {
                iDx += iB;
                iDy -= iA;
                iX = iXp1;
            }
            iDx += iC;
            iDy -= iB;
        }

        // arc from (x2,y2) to point (x3,y3) where dy/dx = 0
        while ( iDy >= 0 )
        {
            oCallback(iXC+iX,iYC+iY);
            oCallback(iXC-iX,iYC-iY);
            iX++;
            iSigma = iA*iX*iX+2*iB*iX*iY+iC*iY*iY-iD;
            if ( iSigma < 0 )
            {
                iDx += iC;
                iDy -= iB;
                iY++;
            }
            iDx += iB;
            iDy -= iA;
        }

        // arc from (x3,y3) to (xa,ya)
        while ( iX <= iXA )
        {
            oCallback(iXC+iX,iYC+iY);
            oCallback(iXC-iX,iYC-iY);
            iX++;
            iYm1 = iY-1;
            iSigma = iA*iX*iX+2*iB*iX*iYm1+iC*iYm1*iYm1-iD;
            if ( iSigma >= 0 )
            {
                iDx -= iC;
                iDy += iB;
                iY = iYm1;
            }
            iDx += iB;
            iDy -= iA;
        }
    }
}
//----------------------------------------------------------------------------
void Wml::RecursiveFill (int iX, int iY, int iXMax, int iYMax, int** aaiImage,
    int iFColor, int iBColor)
{
    aaiImage[iY][iX] = iFColor;

    int iXp1 = iX+1;
    if ( iXp1 < iXMax && aaiImage[iY][iXp1] == iBColor )
        RecursiveFill(iXp1,iY,iXMax,iYMax,aaiImage,iFColor,iBColor);

    int iXm1 = iX-1;
    if ( 0 <= iXm1 && aaiImage[iY][iXm1] == iBColor )
        RecursiveFill(iXm1,iY,iXMax,iYMax,aaiImage,iFColor,iBColor);

    int iYp1 = iY+1;
    if ( iYp1 < iYMax && aaiImage[iYp1][iX] == iBColor )
        RecursiveFill(iX,iYp1,iXMax,iYMax,aaiImage,iFColor,iBColor);

    int iYm1 = iY-1;
    if ( 0 <= iYm1 && aaiImage[iYm1][iX] == iBColor )
        RecursiveFill(iX,iYm1,iXMax,iYMax,aaiImage,iFColor,iBColor);
}
//----------------------------------------------------------------------------
void Wml::NonrecursiveFill (int iX, int iY, int iXMax, int iYMax,
    int** aaiImage, int iFColor, int iBColor)
{
    // Allocate the maximum amount of space needed.  If you prefer less, you
    // need to modify this data structure to allow for dynamic reallocation
    // when it is needed.  An empty stack has iTop == -1.
    int iQuantity = iXMax*iYMax;
    int* aiXStack = new int[iQuantity];
    int* aiYStack = new int[iQuantity];

    // Push seed point onto stack if it has the background color.  All points
    // pushed onto stack have background color iBColor.
    int iTop = 0;
    aiXStack[iTop] = iX;
    aiYStack[iTop] = iY;

    while ( iTop >= 0 )  // stack is not empty
    {
        // Read top of stack.  Do not pop since we need to return to this
        // top value later to restart the fill in a different direction.
        iX = aiXStack[iTop];
        iY = aiYStack[iTop];

        // fill the pixel
        aaiImage[iY][iX] = iFColor;

        int iXp1 = iX+1;
        if ( iXp1 < iXMax && aaiImage[iY][iXp1] == iBColor )
        {
            // push pixel with background color
            iTop++;
            aiXStack[iTop] = iXp1;
            aiYStack[iTop] = iY;
            continue;
        }

        int iXm1 = iX-1;
        if ( 0 <= iXm1 && aaiImage[iY][iXm1] == iBColor )
        {
            // push pixel with background color
            iTop++;
            aiXStack[iTop] = iXm1;
            aiYStack[iTop] = iY;
            continue;
        }

        int iYp1 = iY+1;
        if ( iYp1 < iYMax && aaiImage[iYp1][iX] == iBColor )
        {
            // push pixel with background color
            iTop++;
            aiXStack[iTop] = iX;
            aiYStack[iTop] = iYp1;
            continue;
        }

        int iYm1 = iY-1;
        if ( 0 <= iYm1 && aaiImage[iYm1][iX] == iBColor )
        {
            // push pixel with background color
            iTop++;
            aiXStack[iTop] = iX;
            aiYStack[iTop] = iYm1;
            continue;
        }

        // done in all directions, pop and return to search other directions
        iTop--;
    }

    delete[] aiXStack;
    delete[] aiYStack;
}
//----------------------------------------------------------------------------
