// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLRENDERERFONT_H
#define WMLRENDERERFONT_H

#include "WmlColorRGB.h"
#include <string>

namespace Wml
{

class WML_ITEM RendererFont
{
public:
    RendererFont ();

    RendererFont& operator= (const RendererFont& rkFont);

    std::string& Face ();
    const std::string& Face () const;
    int& Size ();
    int Size() const;
    bool& Bold ();
    bool Bold () const;
    bool& Italic ();
    bool Italic () const;
    ColorRGB& Color ();
    const ColorRGB& Color () const;

    int GetID () const;

protected:
    std::string m_kStrFace;
    int m_iSize;
    bool m_bBold;
    bool m_bItalic;
    ColorRGB m_kColor;

    int m_iID;
    static int ms_iNextID;
};

}

#endif
