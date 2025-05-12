// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlColorRGB.h"
using namespace Wml;

const ColorRGB ColorRGB::BLACK(0.0f,0.0f,0.0f);
const ColorRGB ColorRGB::WHITE(1.0f,1.0f,1.0f);

//----------------------------------------------------------------------------
ColorRGB::ColorRGB (float fR, float fG, float fB)
{
    r = fR;
    g = fG;
    b = fB;
}
//----------------------------------------------------------------------------
ColorRGB::ColorRGB (const ColorRGB& rkColor)
{
    r = rkColor.r;
    g = rkColor.g;
    b = rkColor.b;
}
//----------------------------------------------------------------------------
ColorRGB& ColorRGB::operator= (const ColorRGB& rkColor)
{
    r = rkColor.r;
    g = rkColor.g;
    b = rkColor.b;
    return *this;
}
//----------------------------------------------------------------------------
bool ColorRGB::operator== (const ColorRGB& rkColor) const
{
    return r == rkColor.r
        && g == rkColor.g
        && b == rkColor.b;
}
//----------------------------------------------------------------------------
bool ColorRGB::operator!= (const ColorRGB& rkColor) const
{
    return r != rkColor.r
        || g != rkColor.g
        || b != rkColor.b;
}
//----------------------------------------------------------------------------
ColorRGB ColorRGB::operator+ (const ColorRGB& rkColor) const
{
    return ColorRGB(r+rkColor.r,g+rkColor.g,b+rkColor.b);
}
//----------------------------------------------------------------------------
ColorRGB ColorRGB::operator- (const ColorRGB& rkColor) const
{
    return ColorRGB(r-rkColor.r,g-rkColor.g,b-rkColor.b);
}
//----------------------------------------------------------------------------
ColorRGB ColorRGB::operator* (const ColorRGB& rkColor) const
{
    return ColorRGB(r*rkColor.r,g*rkColor.g,b*rkColor.b);
}
//----------------------------------------------------------------------------
ColorRGB ColorRGB::operator- () const
{
    return ColorRGB(-r,-g,-b);
}
//----------------------------------------------------------------------------
ColorRGB Wml::operator* (float fScalar, const ColorRGB& rkColor)
{
    return ColorRGB(fScalar*rkColor.r,fScalar*rkColor.g,fScalar*rkColor.b);
}
//----------------------------------------------------------------------------
ColorRGB& ColorRGB::operator+= (const ColorRGB& rkColor)
{
    r += rkColor.r;
    g += rkColor.g;
    b += rkColor.b;
    return *this;
}
//----------------------------------------------------------------------------
ColorRGB& ColorRGB::operator-= (const ColorRGB& rkColor)
{
    r -= rkColor.r;
    g -= rkColor.g;
    b -= rkColor.b;
    return *this;
}
//----------------------------------------------------------------------------
ColorRGB& ColorRGB::operator*= (const ColorRGB& rkColor)
{
    r *= rkColor.r;
    g *= rkColor.g;
    b *= rkColor.b;
    return *this;
}
//----------------------------------------------------------------------------
ColorRGB ColorRGB::operator/ (const ColorRGB& rkColor) const
{
    assert( rkColor.r > 0.0f && rkColor.g > 0.0f && rkColor.b > 0.0f );

    return ColorRGB(r/rkColor.r,g/rkColor.g,b/rkColor.b);
}
//----------------------------------------------------------------------------
ColorRGB& ColorRGB::operator/= (const ColorRGB& rkColor)
{
    assert( rkColor.r > 0.0f && rkColor.g > 0.0f && rkColor.b > 0.0f );

    r /= rkColor.r;
    g /= rkColor.g;
    b /= rkColor.b;
    return *this;
}
//----------------------------------------------------------------------------
void ColorRGB::Clamp ()
{
    if ( r < 0.0f )
        r = 0.0f;
    else if ( r > 1.0f )
        r = 1.0f;

    if ( g < 0.0f )
        g = 0.0f;
    else if ( g > 1.0f )
        g = 1.0f;

    if ( b < 0.0f )
        b = 0.0f;
    else if ( b > 1.0f )
        b = 1.0f;
}
//----------------------------------------------------------------------------
void ColorRGB::ScaleByMax ()
{
    float fMax = r;
    if ( g > fMax )
        fMax = g;
    if ( b > fMax )
        fMax = b;

    if ( fMax > 1.0f )
    {
        float fInvMax = 1.0f/fMax;
        r *= fInvMax;
        g *= fInvMax;
        b *= fInvMax;
    }
}
//----------------------------------------------------------------------------
