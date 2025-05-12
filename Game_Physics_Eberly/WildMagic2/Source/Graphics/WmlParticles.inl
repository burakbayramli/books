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
inline void Particles::SetActiveQuantity (int iActiveQuantity)
{
    if ( iActiveQuantity <= m_iVertexQuantity )
        m_iActiveQuantity = iActiveQuantity;
    else
        m_iActiveQuantity = m_iVertexQuantity;
}
//----------------------------------------------------------------------------
inline int Particles::GetActiveQuantity () const
{
    return m_iActiveQuantity;
}
//----------------------------------------------------------------------------
inline float* Particles::Sizes ()
{
    return m_afSize;
}
//----------------------------------------------------------------------------
inline const float* Particles::Sizes () const
{
    return m_afSize;
}
//----------------------------------------------------------------------------
inline float& Particles::SizeAdjust ()
{
    return m_fSizeAdjust;
}
//----------------------------------------------------------------------------
inline const float& Particles::SizeAdjust () const
{
    return m_fSizeAdjust;
}
//----------------------------------------------------------------------------
inline const TriMeshPtr Particles::GetMesh () const
{
    return m_spkMesh;
}
//----------------------------------------------------------------------------
