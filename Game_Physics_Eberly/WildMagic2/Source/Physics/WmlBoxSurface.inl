// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

//----------------------------------------------------------------------------
inline BSplineVolumef*& BoxSurface::Volume ()
{
    return m_pkVolume;
}
//----------------------------------------------------------------------------
inline const BSplineVolumef* BoxSurface::GetVolume () const
{
    return m_pkVolume;
}
//----------------------------------------------------------------------------
inline int BoxSurface::GetUSamples () const
{
    return m_iUSamples;
}
//----------------------------------------------------------------------------
inline int BoxSurface::GetVSamples () const
{
    return m_iVSamples;
}
//----------------------------------------------------------------------------
inline int BoxSurface::GetWSamples () const
{
    return m_iWSamples;
}
//----------------------------------------------------------------------------
