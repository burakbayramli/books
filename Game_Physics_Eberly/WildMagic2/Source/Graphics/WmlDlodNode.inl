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
inline Vector3f& DlodNode::ModelCenter ()
{
    return m_kModelLodCenter;
}
//----------------------------------------------------------------------------
inline const Vector3f& DlodNode::ModelCenter () const
{
    return m_kModelLodCenter;
}
//----------------------------------------------------------------------------
inline const Vector3f& DlodNode::WorldCenter () const
{
    return m_kWorldLodCenter;
}
//----------------------------------------------------------------------------
inline float DlodNode::GetModelMinSqrDistance (int i) const
{
    assert( i < GetQuantity() );
    return m_afModelMinSqrDist[i];
}
//----------------------------------------------------------------------------
inline float DlodNode::GetModelMaxSqrDistance (int i) const
{
    assert( i < GetQuantity() );
    return m_afModelMaxSqrDist[i];
}
//----------------------------------------------------------------------------
inline void DlodNode::GetModelSqrDistance (int i, float& rfMinSqrDist,
    float& rfMaxSqrDist) const
{
    assert( i < GetQuantity() );
    rfMinSqrDist = m_afModelMinSqrDist[i];
    rfMaxSqrDist = m_afModelMaxSqrDist[i];
}
//----------------------------------------------------------------------------
inline float DlodNode::GetWorldMinSqrDistance (int i) const
{
    assert( i < GetQuantity() );
    return m_afWorldMinSqrDist[i];
}
//----------------------------------------------------------------------------
inline float DlodNode::GetWorldMaxSqrDistance (int i) const
{
    assert( i < GetQuantity() );
    return m_afWorldMaxSqrDist[i];
}
//----------------------------------------------------------------------------
inline void DlodNode::GetWorldSqrDistance (int i, float& rfMinSqrDist,
    float& rfMaxSqrDist) const
{
    assert( i < GetQuantity() );
    rfMinSqrDist = m_afWorldMinSqrDist[i];
    rfMaxSqrDist = m_afWorldMaxSqrDist[i];
}
//----------------------------------------------------------------------------
