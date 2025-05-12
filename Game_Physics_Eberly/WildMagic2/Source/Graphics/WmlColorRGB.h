// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLCOLORRGB_H
#define WMLCOLORRGB_H

#include "WmlSystem.h"

namespace Wml
{

class WML_ITEM ColorRGB
{
public:
    // construction (components in range [0,1])
    ColorRGB ();
    ColorRGB (float fR, float fG, float fB);
    ColorRGB (const ColorRGB& rkColor);

    // color channels
    float r, g, b;

    // access color C as C[0] = C.r, C[1] = C.g, C[2] = C.r
    //
    // WARNING.  These member functions rely on
    // (1) ColorRGB not having virtual functions
    // (2) the data packed in a 3*sizeof(float) memory block
    float& operator[] (int i) const;
    operator float* ();

    void Clamp ();
    void ScaleByMax ();

    ColorRGB& operator= (const ColorRGB& rkColor);
    bool operator== (const ColorRGB& rkColor) const;
    bool operator!= (const ColorRGB& rkColor) const;

    ColorRGB operator+ (const ColorRGB& rkColor) const;
    ColorRGB operator- (const ColorRGB& rkColor) const;
    ColorRGB operator* (const ColorRGB& rkColor) const;
    ColorRGB operator/ (const ColorRGB& rkColor) const;
    ColorRGB operator- () const;
    WML_ITEM friend ColorRGB operator* (float fScalar,
        const ColorRGB& rkColor);
    ColorRGB& operator+= (const ColorRGB& rkColor);
    ColorRGB& operator-= (const ColorRGB& rkColor);
    ColorRGB& operator*= (const ColorRGB& rkColor);
    ColorRGB& operator/= (const ColorRGB& rkColor);

    static const ColorRGB BLACK; // = (0,0,0) 
    static const ColorRGB WHITE; // = (1,1,1)
};

#include "WmlColorRGB.inl"

}

#endif



