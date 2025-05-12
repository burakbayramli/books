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
inline void Texture::SetImage (Image* pkImage)
{
    m_spkImage = pkImage;
}
//----------------------------------------------------------------------------
inline Image* Texture::GetImage ()
{
    return m_spkImage;
}
//----------------------------------------------------------------------------
inline Texture::CorrectionMode& Texture::Correction ()
{
    return m_eCorrection;
}
//----------------------------------------------------------------------------
inline Texture::ApplyMode& Texture::Apply ()
{
    return m_eApply;
}
//----------------------------------------------------------------------------
inline ColorRGB& Texture::BlendColor ()
{
    return m_kBlendColor;
}
//----------------------------------------------------------------------------
inline ColorRGB& Texture::BorderColor ()
{
    return m_kBorderColor;
}
//----------------------------------------------------------------------------
inline Texture::WrapMode& Texture::Wrap ()
{
    return m_eWrap;
}
//----------------------------------------------------------------------------
inline Texture::FilterMode& Texture::Filter ()
{
    return m_eFilter;
}
//----------------------------------------------------------------------------
inline Texture::MipmapMode& Texture::Mipmap ()
{
    return m_eMipmap;
}
//----------------------------------------------------------------------------
inline Texture::EnvmapMode& Texture::Envmap ()
{
    return m_eEnvmap;
}
//----------------------------------------------------------------------------
inline float& Texture::Priority ()
{
    return m_fPriority;
}
//----------------------------------------------------------------------------
inline void Texture::SetUserData (int iSize, const void* pvData)
{
    assert( 1 <= iSize && iSize <= 8 );
    memcpy(m_acUserData,pvData,iSize*sizeof(char));
}
//----------------------------------------------------------------------------
inline void Texture::GetUserData (int iSize, void* pvData)
{
    assert( 1 <= iSize && iSize <= 8 );
    memcpy(pvData,m_acUserData,iSize*sizeof(char));
}
//----------------------------------------------------------------------------
inline Texture::ApplyCombineFunction& Texture::CombineFuncRGB()
{
    return m_eCombineFuncRGB;
}
//----------------------------------------------------------------------------
inline Texture::ApplyCombineFunction& Texture::CombineFuncAlpha()
{
    return m_eCombineFuncAlpha;
}
//----------------------------------------------------------------------------
inline Texture::ApplyCombineSrc& Texture::CombineSrc0RGB()       
{
    return m_eCombineSrc0RGB;
}
//----------------------------------------------------------------------------
inline Texture::ApplyCombineSrc& Texture::CombineSrc1RGB()
{
    return m_eCombineSrc1RGB;
}
//----------------------------------------------------------------------------
inline Texture::ApplyCombineSrc& Texture::CombineSrc2RGB()
{
    return m_eCombineSrc2RGB;
}
//----------------------------------------------------------------------------
inline Texture::ApplyCombineSrc& Texture::CombineSrc0Alpha()
{
    return m_eCombineSrc0Alpha;     
}
//----------------------------------------------------------------------------
inline Texture::ApplyCombineSrc& Texture::CombineSrc1Alpha()
{
    return m_eCombineSrc1Alpha;     
}
//----------------------------------------------------------------------------
inline Texture::ApplyCombineSrc& Texture::CombineSrc2Alpha()
{
    return m_eCombineSrc2Alpha;     
}
//----------------------------------------------------------------------------
inline Texture::ApplyCombineOperand& Texture::CombineOp0RGB()
{
    return m_eCombineOp0RGB;    
}
//----------------------------------------------------------------------------
inline Texture::ApplyCombineOperand& Texture::CombineOp1RGB()
{
    return m_eCombineOp1RGB;    
}
//----------------------------------------------------------------------------
inline Texture::ApplyCombineOperand& Texture::CombineOp2RGB()
{
    return m_eCombineOp2RGB;    
}
//----------------------------------------------------------------------------
inline Texture::ApplyCombineOperand& Texture::CombineOp0Alpha()
{
    return m_eCombineOp0Alpha;  
}
//----------------------------------------------------------------------------
inline Texture::ApplyCombineOperand& Texture::CombineOp1Alpha()
{
    return m_eCombineOp1Alpha;  
}
//----------------------------------------------------------------------------
inline Texture::ApplyCombineOperand& Texture::CombineOp2Alpha()
{
    return m_eCombineOp2Alpha;  
}
//----------------------------------------------------------------------------
inline Texture::ApplyCombineScale& Texture::CombineScaleRGB()
{
    return m_eCombineScaleRGB;    
}
//----------------------------------------------------------------------------
inline Texture::ApplyCombineScale& Texture::CombineScaleAlpha()
{
    return m_eCombineScaleAlpha;  
}
//----------------------------------------------------------------------------
