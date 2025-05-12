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
inline int BezierMesh::GetPatchQuantity () const
{
    return m_iPatchQuantity;
}
//----------------------------------------------------------------------------
inline BezierPatchPtr* BezierMesh::Patches ()
{
    return m_aspkPatch;
}
//----------------------------------------------------------------------------
inline const BezierPatchPtr* BezierMesh::Patches () const
{
    return m_aspkPatch;
}
//----------------------------------------------------------------------------
inline BezierPatchPtr BezierMesh::Patch (int i)
{
    assert( i < m_iPatchQuantity );
    return m_aspkPatch[i];
}
//----------------------------------------------------------------------------
inline const BezierPatchPtr BezierMesh::Patch (int i) const
{
    assert( i < m_iPatchQuantity );
    return m_aspkPatch[i];
}
//----------------------------------------------------------------------------
inline TriMeshPtr BezierMesh::GetMesh ()
{
    return m_spkMesh;
}
//----------------------------------------------------------------------------
inline int BezierMesh::GetTessellationLevel () const
{
    return m_iLevel;
}
//----------------------------------------------------------------------------
