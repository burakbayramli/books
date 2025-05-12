// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlRendererFont.h"
using namespace Wml;
using namespace std;

int RendererFont::ms_iNextID = 0;

//----------------------------------------------------------------------------
RendererFont::RendererFont ()
{
    m_kStrFace = "Arial";
    m_iSize = 18;
    m_bBold = false;
    m_bItalic = false;
    m_kColor = ColorRGB::WHITE;
    m_iID = ms_iNextID++;
}
//----------------------------------------------------------------------------
RendererFont& RendererFont::operator= (const RendererFont& rkFont)
{
    m_kStrFace = rkFont.m_kStrFace;
    m_iSize = rkFont.m_iSize;
    m_bBold = rkFont.m_bBold;
    m_bItalic = rkFont.m_bItalic;
    m_kColor = rkFont.m_kColor;
    m_iID = rkFont.m_iID;
    return *this;
}
//----------------------------------------------------------------------------
string& RendererFont::Face ()
{
    return m_kStrFace;
}
//----------------------------------------------------------------------------
const string& RendererFont::Face () const
{
    return m_kStrFace;
}
//----------------------------------------------------------------------------
int& RendererFont::Size ()
{
    return m_iSize;
}
//----------------------------------------------------------------------------
int RendererFont::Size () const
{
    return m_iSize;
}
//----------------------------------------------------------------------------
bool& RendererFont::Bold ()
{
    return m_bBold;
}
//----------------------------------------------------------------------------
bool RendererFont::Bold () const
{
    return m_bBold;
}
//----------------------------------------------------------------------------
bool& RendererFont::Italic ()
{
    return m_bItalic;
}
//----------------------------------------------------------------------------
bool RendererFont::Italic () const
{
    return m_bItalic;
}
//----------------------------------------------------------------------------
ColorRGB& RendererFont::Color ()
{
    return m_kColor;
}
//----------------------------------------------------------------------------
const ColorRGB& RendererFont::Color () const
{
    return m_kColor;
}
//----------------------------------------------------------------------------
int RendererFont::GetID () const
{
    return m_iID;
}
//----------------------------------------------------------------------------
