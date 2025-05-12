// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLRASTERDRAWING_H
#define WMLRASTERDRAWING_H

#include "WmlSystem.h"

namespace Wml
{

// Process the pixels visited by Bresenham's algorithm for the line segment
// with end points (x0,y0) and (x1,y1).  The callback is executed for each
// visited pixel (x,y).  The arguments to the callback are x and y.
WML_ITEM void Line2D (int iX0, int iY0, int iX1, int iY1,
    void (*oCallback)(int,int));

// Process the voxels visited by Bresenham's algorithm for the line segment
// with end points (x0,y0,z0) and (x1,y1,z1).  The callback is executed for
// each visited voxel (x,y,z).  The arguments to the callback are x, y, and z.
WML_ITEM void Line3D (int iX0, int iY0, int iZ0, int iX1, int iY1, int iZ1,
     void (*oCallback)(int,int,int));

// Process the hypervoxels visited by Bresenham's algorithm for the line
// segment with end points (x0,y0,z0,w0) and (x1,y1,z1,w1).  The callback is
// executed for each visited hypervoxel (x,y,z,w).  The arguments to the
// callback are x, y, z, and w.
WML_ITEM void Line4D (int iX0, int iY0, int iZ0, int iW0, int iX1, int iY1,
     int iZ1, int iW1, void (*oCallback)(int,int,int,int));

// Process the pixels visited by Bresenham's algorithm for the circle
// (x-xc)^2 + (y-yc)^2 = r^2.  The callback is executed for each visited
// pixel (x,y).  The arguments to the callback are x and y.
WML_ITEM void Circle2D (int iXC, int iYC, int iR, void (*oCallback)(int,int));

// Process the pixels visited by Bresenham's algorithm for the axis-aligned
// ellipse (x-xc)^2/a^2 + (y-yc)^2/b^2 = 1.  The callback is executed for each
// visited pixel (x,y).  The arguments to the callback are x and y.
WML_ITEM void Ellipse2D (int iXC, int iYC, int iA, int iB,
    void (*oCallback)(int,int));

// Process the pixels visited by Bresenham's algorithm for an arc of the
// axis-aligned ellipse (x-xc)^2/a^2 + (y-yc)^2/b^2 = 1.  The arc has end
// points (x0,fy0) and (x1,fy1) where (xi-xc)^2/a^2 + (yi-yc)/b^2 = 1 for
// i = 0,1.  The arc is traversed in clockwise order.  The callback is
// executed for each visited pixel (x,y).  The arguments to the callback are
// x and y.
WML_ITEM void EllipseArc2D (int iXC, int iYC, int iA, int iB, float fX0,
    float fY0, float fX1, float fY1, void (*oCallback)(int,int));

// Process the pixels visited by Bresenham's algorithm for an oriented
// ellipse.  The ellipse has shape and orientation determined by the bounding
// box with center (xc,yc) and axes (xa,ya) and (xb,yb) where
// Dot((xa,ya),(xb,yb)) = 0.  On an integer lattice, the choice of axes
// satisfies n*(xb,yb) = m*(-ya,xa) for some positive integers n and m.
// Choose xa > 0, ya >= 0, xb <= 0, and yb > 0.  The callback is executed for
// each visited pixel (x,y).  The arguments to the callback are x and y.
WML_ITEM void GeneralEllipse2D (int iXC, int iYC, int iXA, int iYA, int iXB,
    int iYB, void (*oCallback)(int,int));

// The initial call must guarantee that the seed point (iX,iY) satisfies
// 0 <= iX < iXMax and 0 <= iY < iYMax and that aaiImage[iY][iX] is the
// background color (iBColor).  The fill color is specified by iFColor.
WML_ITEM void RecursiveFill (int iX, int iY, int iXMax, int iYMax,
    int** aaiImage, int iFColor, int iBColor);

WML_ITEM void NonrecursiveFill (int iX, int iY, int iXMax, int iYMax,
    int** aaiImage, int iFColor, int iBColor);

}

#endif
