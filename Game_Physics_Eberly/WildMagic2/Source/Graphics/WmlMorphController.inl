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
inline int MorphController::GetTargetQuantity () const
{
    return m_iTargetQuantity;
}
//----------------------------------------------------------------------------
inline Vector3f* MorphController::SetVertices (int i, Vector3f* akVertex)
{
    assert( i < m_iTargetQuantity );
    Vector3f* akSave = m_aakVertex[i];
    m_aakVertex[i] = akVertex;
    return akSave;
}
//----------------------------------------------------------------------------
inline Vector3f* MorphController::GetVertices (int i)
{
    assert( i < m_iTargetQuantity );
    return m_aakVertex[i];
}
//----------------------------------------------------------------------------
inline int MorphController::GetKeyQuantity () const
{
    return m_iKeyQuantity;
}
//----------------------------------------------------------------------------
inline float* MorphController::SetTimes (float* afTime)
{
    float* afSave = m_afTime;
    m_afTime = afTime;
    return afSave;
}
//----------------------------------------------------------------------------
inline float* MorphController::GetTimes ()
{
    return m_afTime;
}
//----------------------------------------------------------------------------
inline float* MorphController::SetWeights (int i, float* afWeight)
{
    assert( i < m_iKeyQuantity );
    float* afSave = m_aafWeight[i];
    m_aafWeight[i] = afWeight;
    return afSave;
}
//----------------------------------------------------------------------------
inline float* MorphController::GetWeights (int i)
{
    assert( i < m_iKeyQuantity );
    return m_aafWeight[i];
}
//----------------------------------------------------------------------------
inline bool& MorphController::UpdateNormals ()
{
    return m_bUpdateNormals;
}
//----------------------------------------------------------------------------
