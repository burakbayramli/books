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
inline int Geometry::GetVertexQuantity () const
{
    return m_iVertexQuantity;
}
//----------------------------------------------------------------------------
inline Vector3f& Geometry::Vertex (int i)
{
    return m_akVertex[i];
}
//----------------------------------------------------------------------------
inline const Vector3f& Geometry::Vertex (int i) const
{
    return m_akVertex[i];
}
//----------------------------------------------------------------------------
inline Vector3f* Geometry::Vertices ()
{
    return m_akVertex;
}
//----------------------------------------------------------------------------
inline const Vector3f* Geometry::Vertices () const
{
    return m_akVertex;
}
//----------------------------------------------------------------------------
inline Vector3f& Geometry::Normal (int i)
{
    return m_akNormal[i];
}
//----------------------------------------------------------------------------
inline const Vector3f& Geometry::Normal (int i) const
{
    return m_akNormal[i];
}
//----------------------------------------------------------------------------
inline Vector3f* Geometry::Normals ()
{
    return m_akNormal;
}
//----------------------------------------------------------------------------
inline const Vector3f* Geometry::Normals () const
{
    return m_akNormal;
}
//----------------------------------------------------------------------------
inline ColorRGB& Geometry::Color (int i)
{
    return m_akColor[i];
}
//----------------------------------------------------------------------------
inline const ColorRGB& Geometry::Color (int i) const
{
    return m_akColor[i];
}
//----------------------------------------------------------------------------
inline ColorRGB* Geometry::Colors ()
{
    return m_akColor;
}
//----------------------------------------------------------------------------
inline const ColorRGB* Geometry::Colors () const
{
    return m_akColor;
}
//----------------------------------------------------------------------------
inline Vector2f& Geometry::Texture (int i)
{
    return m_akTexture[i];
}
//----------------------------------------------------------------------------
inline const Vector2f& Geometry::Texture (int i) const
{
    return m_akTexture[i];
}
//----------------------------------------------------------------------------
inline Vector2f* Geometry::Textures ()
{
    return m_akTexture;
}
//----------------------------------------------------------------------------
inline const Vector2f* Geometry::Textures () const
{
    return m_akTexture;
}
//----------------------------------------------------------------------------
inline Vector2f& Geometry::Texture1 (int i)
{
    return m_akTexture1[i];
}
//----------------------------------------------------------------------------
inline const Vector2f& Geometry::Texture1 (int i) const
{
    return m_akTexture1[i];
}
//----------------------------------------------------------------------------
inline Vector2f* Geometry::Textures1 ()
{
    return m_akTexture1;
}
//----------------------------------------------------------------------------
inline const Vector2f* Geometry::Textures1 () const
{
    return m_akTexture1;
}
//----------------------------------------------------------------------------
inline Vector2f& Geometry::Texture2 (int i)
{
    return m_akTexture2[i];
}
//----------------------------------------------------------------------------
inline const Vector2f& Geometry::Texture2 (int i) const
{
    return m_akTexture2[i];
}
//----------------------------------------------------------------------------
inline Vector2f* Geometry::Textures2 ()
{
    return m_akTexture2;
}
//----------------------------------------------------------------------------
inline const Vector2f* Geometry::Textures2 () const
{
    return m_akTexture2;
}
//----------------------------------------------------------------------------
inline Vector2f& Geometry::Texture3 (int i)
{
    return m_akTexture3[i];
}
//----------------------------------------------------------------------------
inline const Vector2f& Geometry::Texture3 (int i) const
{
    return m_akTexture3[i];
}
//----------------------------------------------------------------------------
inline Vector2f* Geometry::Textures3 ()
{
    return m_akTexture3;
}
//----------------------------------------------------------------------------
inline const Vector2f* Geometry::Textures3 () const
{
    return m_akTexture3;
}
//----------------------------------------------------------------------------
inline Vector2f& Geometry::TextureBump (int i)
{
    return m_akTextureBump[i];
}
//----------------------------------------------------------------------------
inline const Vector2f& Geometry::TextureBump (int i) const
{
    return m_akTextureBump[i];
}
//----------------------------------------------------------------------------
inline Vector2f* Geometry::TexturesBump ()
{
    return m_akTextureBump;
}
//----------------------------------------------------------------------------
inline const Vector2f* Geometry::TexturesBump () const
{
    return m_akTextureBump;
}
//----------------------------------------------------------------------------
inline ColorRGB*& Geometry::LightVectors ()
{
    return m_akColor;
}
//----------------------------------------------------------------------------
inline const ColorRGB* Geometry::LightVectors () const
{
    return m_akColor;
}
//----------------------------------------------------------------------------
inline Bound& Geometry::ModelBound ()
{
    return m_kBound;
}
//----------------------------------------------------------------------------
inline const Bound& Geometry::ModelBound () const
{
    return m_kBound;
}
//----------------------------------------------------------------------------
inline const RenderStatePtr* Geometry::GetRenderStateArray () const
{
    return m_aspkState;
}
//----------------------------------------------------------------------------
inline VertexShaderPtr Geometry::GetVertexShader () const
{
    return m_pkVertexShader;
}
//----------------------------------------------------------------------------
inline PixelShaderPtr Geometry::GetPixelShader () const
{
    return m_pkPixelShader;
}
//----------------------------------------------------------------------------
inline ShaderConstants* Geometry::GetVertexShaderConstants () const
{
    return m_pkVertexShaderConsts;
}
//----------------------------------------------------------------------------
inline ShaderConstants* Geometry::GetPixelShaderConstants () const
{
    return m_pkPixelShaderConsts;
}
//----------------------------------------------------------------------------
inline void Geometry::SetVertexShader (VertexShader* pkVertexShader)
{
    m_pkVertexShader = pkVertexShader;
    if ( m_pkVertexShaderConsts )
    {
        delete m_pkVertexShaderConsts;
    }

    if ( pkVertexShader )
    {
        m_pkVertexShaderConsts = pkVertexShader->BuildConstants();
    }
    else
    {
        m_pkVertexShaderConsts = NULL;
    }
}
//----------------------------------------------------------------------------
inline void Geometry::SetPixelShader (PixelShader* pkPixelShader)
{
    m_pkPixelShader = pkPixelShader;
    if ( m_pkPixelShaderConsts )
    {
        delete m_pkPixelShaderConsts;
    }
    if ( pkPixelShader )
    {
        m_pkPixelShaderConsts = pkPixelShader->BuildConstants();
    }
    else
    {
        m_pkPixelShaderConsts = NULL;
    }
}
//----------------------------------------------------------------------------
inline ShaderConst* Geometry::GetVertexConst (const char* acName)
{
    if ( m_pkVertexShaderConsts )
    {
        return m_pkVertexShaderConsts->GetConstant(acName);
    }
    else
    {
        return NULL;
    }
}
//----------------------------------------------------------------------------
inline ShaderConst* Geometry::GetPixelConst (const char* acName)
{
    if ( m_pkPixelShaderConsts )
    {
        return m_pkPixelShaderConsts->GetConstant(acName);
    }
    else
    {
        return NULL;
    }
}
//----------------------------------------------------------------------------
