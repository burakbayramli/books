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
inline int Renderer::GetStringWidth (const char*) const
{
    return 0;
}
//----------------------------------------------------------------------------
inline int Renderer::GetCharacterWidth (const char) const
{
    return 0;
}
//----------------------------------------------------------------------------
inline int Renderer::GetFontHeight () const
{
    return 0;
}
//----------------------------------------------------------------------------
inline int Renderer::GetX () const
{
    return m_iX;
}
//----------------------------------------------------------------------------
inline int Renderer::GetY () const
{
    return m_iY;
}
//----------------------------------------------------------------------------
inline int Renderer::GetWidth () const
{
    return m_iWidth;
}
//----------------------------------------------------------------------------
inline int Renderer::GetHeight () const
{
    return m_iHeight;
}
//----------------------------------------------------------------------------
inline CameraPtr Renderer::GetCamera ()
{
    return m_spkCamera;
}
//----------------------------------------------------------------------------
inline bool Renderer::BeginScene ()
{
    // The DX renderer has such a concept, so it must override this function.
    // The OpenGL renderer does not need this, but applications should call
    // it for portability across renderers.
    return true;
}
//----------------------------------------------------------------------------
inline void Renderer::EndScene ()
{
    // The DX renderer has such a concept, so it must override this function.
    // The OpenGL renderer does not need this, but applications should call
    // it for portability across renderers.
}
//----------------------------------------------------------------------------
inline void Renderer::ShowCursor (bool)
{
    // stub for derived classes
}
//----------------------------------------------------------------------------
inline void Renderer::ToggleFullScreen (int&, int&)
{
    // stub for derived classes
}
//----------------------------------------------------------------------------
inline bool Renderer::LoadFont (const RendererFont&)
{
    // stub for derived classes
    return false;
}
//----------------------------------------------------------------------------
inline void Renderer::UnloadFont (const RendererFont&)
{
    // stub for derived classes
}
//----------------------------------------------------------------------------
inline void Renderer::SetBackgroundColor (const ColorRGB& rkColor)
{
    m_kBackgroundColor = rkColor;
}
//----------------------------------------------------------------------------
inline const ColorRGB& Renderer::GetBackgroundColor () const
{
    return m_kBackgroundColor;
}
//----------------------------------------------------------------------------
inline bool Renderer::SupportsSpecularAfterTexture () const
{
    return m_bCapSpecularAfterTexture;
}
//----------------------------------------------------------------------------
inline bool Renderer::SupportsPlanarReflection () const
{
    return m_bCapPlanarReflection;
}
//----------------------------------------------------------------------------
inline bool Renderer::SupportsPlanarShadow () const
{
    return m_bCapPlanarShadow;
}
//----------------------------------------------------------------------------
inline bool Renderer::SupportsTextureClampToBorder () const
{
    return m_bCapTextureClampToBorder;
}
//----------------------------------------------------------------------------
inline bool Renderer::SupportsTextureApplyAdd () const
{
    return m_bCapTextureApplyAdd;
}
//----------------------------------------------------------------------------
inline bool Renderer::SupportsTextureApplyCombine () const
{
    return m_bCapTextureApplyCombine;
}
//----------------------------------------------------------------------------
inline bool Renderer::SupportsTextureApplyCombineDot3 () const
{
    return m_bCapTextureApplyCombineDot3;
}
//----------------------------------------------------------------------------
inline bool Renderer::SupportsDot3BumpMapping () const
{
    return m_bCapDot3BumpMapping;
}
//----------------------------------------------------------------------------
inline bool Renderer::SupportsVertexShader () const
{
    return (m_iCapVertShaderVersion != Shader::UNSUPPORTED);
}
//----------------------------------------------------------------------------
inline bool Renderer::SupportsPixelShader () const
{
    return (m_iCapPixShaderVersion != Shader::UNSUPPORTED);
}
//----------------------------------------------------------------------------
inline void Renderer::Draw (int, int, const ColorRGB&, const char*)
{
    // stub for derived classes
}
//----------------------------------------------------------------------------
inline void Renderer::Draw (int, int, const RendererFont&, const char*)
{
     // stub for derived classes  
}
//----------------------------------------------------------------------------
inline void Renderer::Draw (const unsigned char*)
{
     // stub for derived classes  
}
//----------------------------------------------------------------------------
inline void Renderer::Draw (const BumpMap&)
{
    // stub for derived classes
}
//----------------------------------------------------------------------------
inline void Renderer::Draw (const GlossMap&)
{
    // stub for derived classes
}
//----------------------------------------------------------------------------
inline void Renderer::Draw (const Particles&)
{
    // stub for derived classes
}
//----------------------------------------------------------------------------
inline void Renderer::Draw (const PlanarReflection&)
{
    // stub for derived classes
}
//----------------------------------------------------------------------------
inline void Renderer::Draw (const PlanarShadow&)
{
    // stub for derived classes
}
//----------------------------------------------------------------------------
inline void Renderer::Draw (const Polyline&)
{
    // stub for derived classes
}
//----------------------------------------------------------------------------
inline void Renderer::Draw (const Polypoint&)
{
    // stub for derived classes
}
//----------------------------------------------------------------------------
inline void Renderer::Draw (const ProjectedTexture&)
{
    // stub for derived classes
}
//----------------------------------------------------------------------------
inline void Renderer::Draw (const ScreenPolygon&)
{
    // stub for derived classes
}
//----------------------------------------------------------------------------
inline void Renderer::Draw (const TriMesh&)
{
    // stub for derived classes
}
//----------------------------------------------------------------------------
inline int Renderer::GetMaxTextureUnits () const
{
    return m_iMaxTextureUnits;
}
//----------------------------------------------------------------------------
inline void Renderer::ReleaseTexture (Texture*)
{
    // stub for derived classes
}
//----------------------------------------------------------------------------
inline bool Renderer::SupportsMultitexture () const
{
    return m_bCapMultitexture;
}
//----------------------------------------------------------------------------
inline void Renderer::SetPixelShaderConst (ShaderConstants*, const Geometry&)
{
    // stub for derived classes
}
//----------------------------------------------------------------------------
inline void Renderer::SetVertexShaderConst (ShaderConstants*, const Geometry&)
{
    // stub for derived classes
}
//----------------------------------------------------------------------------
inline void Renderer::ReleaseShader(Shader*)
{
    // stub for derived classes
}
//----------------------------------------------------------------------------
inline bool Renderer::SetCurrentVertexShader (VertexShader*)
{
    // stub for derived classes
    return false;
}
//----------------------------------------------------------------------------
inline bool Renderer::SetCurrentPixelShader (PixelShader*)
{
    // stub for derived classes
    return false;
}
//----------------------------------------------------------------------------
inline bool Renderer::SupportsShader (const VertexShader*) const
{
    // stub for derived classes
    return false;
}
//----------------------------------------------------------------------------
inline bool Renderer::SupportsShader (const PixelShader*) const
{
    // stub for derived classes
    return false;
}
//----------------------------------------------------------------------------
