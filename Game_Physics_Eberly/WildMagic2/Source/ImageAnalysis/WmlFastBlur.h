// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLFASTBLUR_H
#define WMLFASTBLUR_H


// The idea is to represent the blurred image as f(x,s) in terms of position
// x and scale s.  Gaussian blurring is accomplished by using the input image
// I(x,s0) as the initial image (of scale s0 > 0) for the partial differential
// equation
//   s*df/ds = s^2*Laplacian(f)
// where the Laplacian operator is
//   Laplacian = (d/dx)^2, dimension 1
//   Laplacian = (d/dx)^2+(d/dy)^2, dimension 2
//   Laplacian = (d/dx)^2+(d/dy)^2+(d/dz)^2, dimension 3
//
// The term s*df/ds is approximated by
//   s*df(x,s)/ds = (f(x,b*s)-f(x,s))/ln(b)
// for b > 1, but close to 1, where ln(b) is the natural logarithm of b.  If
// you take the limit of the right-hand side as b approaches 1, you get the
// left-hand side.
//
// The term s^2*((d/dx)^2)f is approximated by
//   s^2*((d/dx)^2)f = (f(x+h*s,s)-2*f(x,s)+f(x-h*s,s))/h^2
// for h > 0, but close to zero.
//
// Equating the approximations for the left-hand side and the right-hand side
// of the partial differential equation leads to the numerical method used in
// this code.
//
// For iterative application of these functions, the caller is responsible
// for constructing a geometric sequence of scales,
//   s0, s1 = s0*b, s2 = s1*b = s0*b^2, ...
// where the base b satisfies 1 < b < exp(0.5*d) where d is the dimension of
// the image.  The upper bound on b guarantees stability of the finite
// difference method used to approximate the partial differential equation.
// The method assumes a pixel size of h = 1.
//
//
// Sample usage in 2D:
//
// const int iXBound = 256, iYBound = 256;
// float** aakImage = new float*[iYBound];
// float** aakTemp = new float*[iYBound];
// int iX, iY;
// for (iY = 0; iY < iYBound; iY++)
// {
//     aakImage[y] = new float[iXBound];
//     aakTemp[y] = new float[iXBound];
// }
// <initialization of aakImage[iY][iX] goes here...>
// const int iMax = <number of passes to blur>;
// double dScale = 1.0, dLogBase = 0.125, dBase = exp(0.125);
// for (int i = 1; i <= iMax; i++, dScale *= dBase)
// {
//     FastBlur2(iXBound,iYBound,aakImage,aakTemp,dScale,dLogBase);
//     <use the blurred image aakImage here if desired>;
// }
// for (iY = 0; iY < iYBound; iY++)
// {
//     delete[] aakImage[iY];
//     delete[] aakTemp[iY];
// }
// delete[] aakImage;
// delete[] aakTemp;

#include "WmlSystem.h"

namespace Wml
{

// Explicit instantiation of the template code is done for PixelType of:
// short, int, float, or double.  The algorithm requires a temporary image
// of the same size as the original image.  The caller is responsible for
// providing this.

template <class PixelType>
WML_ITEM void FastBlur1 (int iXBound, PixelType* akImage, PixelType* akTemp,
    double dScale, double dLogBase);

template <class PixelType>
WML_ITEM void FastBlur2 (int iXBound, int iYBound, PixelType** aakImage,
    PixelType** aakTemp, double dScale, double dLogBase);

template <class PixelType>
WML_ITEM void FastBlur3 (int iXBound, int iYBound, int iZBound,
    PixelType*** aaakImage, PixelType*** aaakTemp, double dScale,
    double dLogBase);

}

#endif
