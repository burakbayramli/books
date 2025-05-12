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
inline int VETMesh::GetVertexQuantity () const
{
    return (int)m_kVMap.size();
}
//----------------------------------------------------------------------------
inline int VETMesh::GetEdgeQuantity () const
{
    return (int)m_kEMap.size();
}
//----------------------------------------------------------------------------
inline int VETMesh::GetTriangleQuantity () const
{
    return (int)m_kTMap.size();
}
//----------------------------------------------------------------------------
inline VETMesh* VETMesh::Create () const
{
    return new VETMesh;
}
//----------------------------------------------------------------------------
inline void VETMesh::OnVertexInsert (int,bool,void*&)
{
}
//----------------------------------------------------------------------------
inline void VETMesh::OnVertexRemove (int,bool,void*)
{
}
//----------------------------------------------------------------------------
inline void VETMesh::OnEdgeInsert (const Edge&,bool,void*&)
{
}
//----------------------------------------------------------------------------
inline void VETMesh::OnEdgeRemove (const Edge&,bool,void*)
{
}
//----------------------------------------------------------------------------
inline void VETMesh::OnTriangleInsert (const Triangle&,bool,void*&)
{
}
//----------------------------------------------------------------------------
inline void VETMesh::OnTriangleRemove (const Triangle&,bool,void*)
{
}
//----------------------------------------------------------------------------
inline const VETMesh::VMap& VETMesh::GetVertexMap () const
{
    return m_kVMap;
}
//----------------------------------------------------------------------------
inline const VETMesh::EMap& VETMesh::GetEdgeMap () const
{
    return m_kEMap;
}
//----------------------------------------------------------------------------
inline const VETMesh::TMap& VETMesh::GetTriangleMap () const
{
    return m_kTMap;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// VETMesh::Edge
//----------------------------------------------------------------------------
inline VETMesh::Edge::Edge ()
{
}
//----------------------------------------------------------------------------
inline VETMesh::Edge::Edge (int iV0, int iV1)
{
    if ( iV0 < iV1 )
    {
        // v0 is minimum
        m_aiV[0] = iV0;
        m_aiV[1] = iV1;
    }
    else
    {
        // v1 is minimum
        m_aiV[0] = iV1;
        m_aiV[1] = iV0;
    }
}
//----------------------------------------------------------------------------
inline bool VETMesh::Edge::operator< (const Edge& rkE) const
{
    if ( m_aiV[1] < rkE.m_aiV[1] )
        return true;

    if ( m_aiV[1] == rkE.m_aiV[1] )
        return m_aiV[0] < rkE.m_aiV[0];

    return false;
}
//----------------------------------------------------------------------------
inline bool VETMesh::Edge::operator== (const Edge& rkE) const
{
    return m_aiV[0] == rkE.m_aiV[0] && m_aiV[1] == rkE.m_aiV[1];
}
//----------------------------------------------------------------------------
inline bool VETMesh::Edge::operator!= (const Edge& rkE) const
{
    return !operator==(rkE);
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// VETMesh::Triangle
//----------------------------------------------------------------------------
inline VETMesh::Triangle::Triangle ()
{
}
//----------------------------------------------------------------------------
inline VETMesh::Triangle::Triangle (int iV0, int iV1, int iV2)
{
    if ( iV0 < iV1 )
    {
        if ( iV0 < iV2 )
        {
            // v0 is minimum
            m_aiV[0] = iV0;
            m_aiV[1] = iV1;
            m_aiV[2] = iV2;
        }
        else
        {
            // v2 is minimum
            m_aiV[0] = iV2;
            m_aiV[1] = iV0;
            m_aiV[2] = iV1;
        }
    }
    else
    {
        if ( iV1 < iV2 )
        {
            // v1 is minimum
            m_aiV[0] = iV1;
            m_aiV[1] = iV2;
            m_aiV[2] = iV0;
        }
        else
        {
            // v2 is minimum
            m_aiV[0] = iV2;
            m_aiV[1] = iV0;
            m_aiV[2] = iV1;
        }
    }
}
//----------------------------------------------------------------------------
inline bool VETMesh::Triangle::operator< (const Triangle& rkT) const
{
    if ( m_aiV[2] < rkT.m_aiV[2] )
        return true;

    if ( m_aiV[2] == rkT.m_aiV[2] )
    {
        if ( m_aiV[1] < rkT.m_aiV[1] )
            return true;

        if ( m_aiV[1] == rkT.m_aiV[1] )
            return m_aiV[0] < rkT.m_aiV[0];
    }

    return false;
}
//----------------------------------------------------------------------------
inline bool VETMesh::Triangle::operator== (const Triangle& rkT) const
{
    return (m_aiV[0] == rkT.m_aiV[0]) &&
          ((m_aiV[1] == rkT.m_aiV[1] && m_aiV[2] == rkT.m_aiV[2]) ||
           (m_aiV[1] == rkT.m_aiV[2] && m_aiV[2] == rkT.m_aiV[1]));
}
//----------------------------------------------------------------------------
inline bool VETMesh::Triangle::operator!= (const Triangle& rkT) const
{
    return !operator==(rkT);
}
//----------------------------------------------------------------------------
