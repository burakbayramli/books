// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

//----------------------------------------------------------------------------
inline bool& PolygonOffsetState::FillEnabled ()
{
    return m_bFillEnabled;
}
//----------------------------------------------------------------------------
inline bool& PolygonOffsetState::LineEnabled ()
{
    return m_bLineEnabled;
}
//----------------------------------------------------------------------------
inline bool& PolygonOffsetState::PointEnabled ()
{
    return m_bPointEnabled;
}
//----------------------------------------------------------------------------
inline float& PolygonOffsetState::Scale ()
{
    return m_fScale;
}
//----------------------------------------------------------------------------
inline float& PolygonOffsetState::Bias ()
{
    return m_fBias;
}
//----------------------------------------------------------------------------
