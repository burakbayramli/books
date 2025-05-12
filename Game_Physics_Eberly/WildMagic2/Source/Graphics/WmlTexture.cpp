// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlTexture.h"
#include "WmlRenderer.h"
using namespace Wml;

WmlImplementRTTI(Texture,Object);
WmlImplementStream(Texture);

//----------------------------------------------------------------------------
Texture::Texture ()
{
    m_eCorrection = CM_PERSPECTIVE;
    m_eApply = AM_REPLACE;
    m_kBlendColor = ColorRGB::BLACK;
    m_eWrap = WM_CLAMP_S_CLAMP_T;
    m_eFilter = FM_NEAREST;
    m_eMipmap = MM_NONE;
    m_eEnvmap = EM_NONE;
    m_fPriority = 1.0f;
    memset(m_acUserData,0,8*sizeof(char));

    // the following is relevant only if the wrap mode is clamp-to-border
    m_kBorderColor = ColorRGB::BLACK;

    // the following are relevant only if the apply mode is AM_COMBINE
    m_eCombineFuncRGB = ACF_REPLACE;
    m_eCombineFuncAlpha = ACF_REPLACE;
    m_eCombineSrc0RGB = ACS_TEXTURE;
    m_eCombineSrc1RGB = ACS_TEXTURE;
    m_eCombineSrc2RGB = ACS_TEXTURE;
    m_eCombineSrc0Alpha = ACS_TEXTURE;
    m_eCombineSrc1Alpha = ACS_TEXTURE;
    m_eCombineSrc2Alpha = ACS_TEXTURE;
    m_eCombineOp0RGB = ACO_SRC_COLOR;
    m_eCombineOp1RGB = ACO_SRC_COLOR;
    m_eCombineOp2RGB = ACO_SRC_COLOR;
    m_eCombineOp0Alpha = ACO_SRC_COLOR;
    m_eCombineOp1Alpha = ACO_SRC_COLOR;
    m_eCombineOp2Alpha = ACO_SRC_COLOR;
    m_eCombineScaleRGB = ACSC_ONE;
    m_eCombineScaleAlpha = ACSC_ONE;
}
//----------------------------------------------------------------------------
Texture::~Texture ()
{
    // Inform all renderers that the texture is being destroyed.  Any
    // renderer using this texture can free up any associated resources.
    Renderer::OnDestroyTexture(this);

    m_spkImage = NULL;
}
//----------------------------------------------------------------------------
Object* Texture::GetObjectByName (const char* acName)
{
    Object* pkFound = Object::GetObjectByName(acName);
    if ( pkFound )
        return pkFound;

    if ( m_spkImage )
    {
        pkFound = m_spkImage->GetObjectByName(acName);
        if ( pkFound )
            return pkFound;
    }

    return 0;
}
//----------------------------------------------------------------------------
void Texture::GetAllObjectsByName (const char* acName,
    std::vector<Object*>& rkObjects)
{
    Object::GetAllObjectsByName(acName,rkObjects);

    if ( m_spkImage )
        m_spkImage->GetAllObjectsByName(acName,rkObjects);
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* Texture::Factory (Stream& rkStream)
{
    Texture* pkObject = new Texture;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void Texture::Load (Stream& rkStream, Stream::Link* pkLink)
{
    Object::Load(rkStream,pkLink);

    // native data
    StreamReadEnum(rkStream,m_eCorrection);
    StreamReadEnum(rkStream,m_eApply);
    StreamRead(rkStream,m_kBlendColor);
    StreamReadEnum(rkStream,m_eWrap);
    StreamReadEnum(rkStream,m_eFilter);
    StreamReadEnum(rkStream,m_eMipmap);
    StreamRead(rkStream,m_fPriority);

    if ( rkStream.GetVersion() >= Version(1,3) )
    {
        StreamReadEnum(rkStream,m_eEnvmap);
        StreamRead(rkStream,m_kBorderColor);
        StreamReadEnum(rkStream,m_eCombineFuncRGB);
        StreamReadEnum(rkStream,m_eCombineFuncAlpha);
        StreamReadEnum(rkStream,m_eCombineSrc0RGB);
        StreamReadEnum(rkStream,m_eCombineSrc1RGB);
        StreamReadEnum(rkStream,m_eCombineSrc2RGB);
        StreamReadEnum(rkStream,m_eCombineSrc0Alpha);
        StreamReadEnum(rkStream,m_eCombineSrc1Alpha);
        StreamReadEnum(rkStream,m_eCombineSrc2Alpha);
        StreamReadEnum(rkStream,m_eCombineOp0RGB);
        StreamReadEnum(rkStream,m_eCombineOp1RGB);
        StreamReadEnum(rkStream,m_eCombineOp2RGB);
        StreamReadEnum(rkStream,m_eCombineOp0Alpha);
        StreamReadEnum(rkStream,m_eCombineOp1Alpha);
        StreamReadEnum(rkStream,m_eCombineOp2Alpha);
        StreamReadEnum(rkStream,m_eCombineScaleRGB);
        StreamReadEnum(rkStream,m_eCombineScaleAlpha);
    }

    // link data
    Image* pkImage;
    StreamRead(rkStream,pkImage);
    pkLink->Add(pkImage);
}
//----------------------------------------------------------------------------
void Texture::Link (Stream& rkStream, Stream::Link* pkLink)
{
    Object::Link(rkStream,pkLink);

    Object* pkLinkID = pkLink->GetLinkID();
    m_spkImage = (Image*)rkStream.GetFromMap(pkLinkID);
}
//----------------------------------------------------------------------------
bool Texture::Register (Stream& rkStream)
{
    if ( !Object::Register(rkStream) )
        return false;

    if ( m_spkImage )
        m_spkImage->Register(rkStream);

    return true;
}
//----------------------------------------------------------------------------
void Texture::Save (Stream& rkStream)
{
    Object::Save(rkStream);

    // native data
    StreamWriteEnum(rkStream,m_eCorrection);
    StreamWriteEnum(rkStream,m_eApply);
    StreamWrite(rkStream,m_kBlendColor);
    StreamWriteEnum(rkStream,m_eWrap);
    StreamWriteEnum(rkStream,m_eFilter);
    StreamWriteEnum(rkStream,m_eMipmap);
    StreamWrite(rkStream,m_fPriority);

    StreamWriteEnum(rkStream,m_eEnvmap);
    StreamWrite(rkStream,m_kBorderColor);
    StreamWriteEnum(rkStream,m_eCombineFuncRGB);
    StreamWriteEnum(rkStream,m_eCombineFuncAlpha);
    StreamWriteEnum(rkStream,m_eCombineSrc0RGB);
    StreamWriteEnum(rkStream,m_eCombineSrc1RGB);
    StreamWriteEnum(rkStream,m_eCombineSrc2RGB);
    StreamWriteEnum(rkStream,m_eCombineSrc0Alpha);
    StreamWriteEnum(rkStream,m_eCombineSrc1Alpha);
    StreamWriteEnum(rkStream,m_eCombineSrc2Alpha);
    StreamWriteEnum(rkStream,m_eCombineOp0RGB);
    StreamWriteEnum(rkStream,m_eCombineOp1RGB);
    StreamWriteEnum(rkStream,m_eCombineOp2RGB);
    StreamWriteEnum(rkStream,m_eCombineOp0Alpha);
    StreamWriteEnum(rkStream,m_eCombineOp1Alpha);
    StreamWriteEnum(rkStream,m_eCombineOp2Alpha);
    StreamWriteEnum(rkStream,m_eCombineScaleRGB);
    StreamWriteEnum(rkStream,m_eCombineScaleAlpha);

    // m_uiUserData is derived, no need to save

    // link data
    StreamWrite(rkStream,m_spkImage);
}
//----------------------------------------------------------------------------
StringTree* Texture::SaveStrings ()
{
    StringTree* pkTree = new StringTree(26,0,2,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));

    switch ( m_eCorrection )
    {
    case CM_AFFINE:
        pkTree->SetString(1,MakeString("correction = AFFINE"));
        break;
    case CM_PERSPECTIVE:
        pkTree->SetString(1,MakeString("correction = PERSPECTIVE"));
        break;
    default:  // CM_QUANTITY
        break;
    };

    switch ( m_eApply )
    {
    case AM_REPLACE:
        pkTree->SetString(2,MakeString("apply = REPLACE"));
        break;
    case AM_DECAL:
        pkTree->SetString(2,MakeString("apply = DECAL"));
        break;
    case AM_MODULATE:
        pkTree->SetString(2,MakeString("apply = MODULATE"));
        break;
    case AM_BLEND:
        pkTree->SetString(2,MakeString("apply = BLEND"));
        break;
    case AM_ADD:
        pkTree->SetString(2,MakeString("apply = ADD"));
        break;
    case AM_COMBINE:
        pkTree->SetString(2,MakeString("apply = COMBINE"));
        break;
    default:  // AM_QUANTITY
        break;
    };

    switch ( m_eCombineFuncRGB )
    {
    case ACF_REPLACE:
        pkTree->SetString(3,MakeString("CombFuncRGB = REPLACE"));
        break;
    case ACF_MODULATE:
        pkTree->SetString(3,MakeString("CombFuncRGB = MODULATE"));
        break;
    case ACF_ADD:
        pkTree->SetString(3,MakeString("CombFuncRGB = ADD"));
        break;
    case ACF_ADD_SIGNED:
        pkTree->SetString(3,MakeString("CombFuncRGB = ADD_SIGNED"));
        break;
    case ACF_SUBTRACT:
        pkTree->SetString(3,MakeString("CombFuncRGB = SUBTRACT"));
        break;
    case ACF_INTERPOLATE:
        pkTree->SetString(3,MakeString("CombFuncRGB = INTERPOLATE"));
        break;
    case ACF_DOT3_RGB:
        pkTree->SetString(3,MakeString("CombFuncRGB = DOT3_RGB"));
        break;
    case ACF_DOT3_RGBA:
        pkTree->SetString(3,MakeString("CombFuncRGB = DOT3_RGBA"));
        break;
    default:  // ACF_QUANTITY
        break;
    }

    switch ( m_eCombineFuncAlpha )
    {
    case ACF_REPLACE:
        pkTree->SetString(4,MakeString("CombineFuncAlpha = REPLACE"));
        break;
    case ACF_MODULATE:
        pkTree->SetString(4,MakeString("CombineFuncAlpha = MODULATE"));
        break;
    case ACF_ADD:
        pkTree->SetString(4,MakeString("CombineFuncAlpha = ADD"));
        break;
    case ACF_ADD_SIGNED:
        pkTree->SetString(4,MakeString("CombineFuncAlpha = ADD_SIGNED"));
        break;
    case ACF_SUBTRACT:
        pkTree->SetString(4,MakeString("CombineFuncAlpha = SUBTRACT"));
        break;
    case ACF_INTERPOLATE:
        pkTree->SetString(4,MakeString("CombineFuncAlpha = INTERPOLATE"));
        break;
    case ACF_DOT3_RGB:
        pkTree->SetString(4,MakeString("CombineFuncAlpha = DOT3_RGB"));
        break;
    case ACF_DOT3_RGBA:
        pkTree->SetString(4,MakeString("CombineFuncAlpha = DOT3_RGBA"));
        break;
    default:  // ACF_QUANTITY
        break;
    }

    switch ( m_eCombineSrc0RGB )
    {
    case ACS_TEXTURE:
        pkTree->SetString(5,MakeString("CombineSrc0RGB = TEXTURE"));
        break;
    case ACS_PRIMARY_COLOR:
        pkTree->SetString(5,MakeString("CombineSrc0RGB = PRIMARY_COLOR"));
        break;
    case ACS_CONSTANT:
        pkTree->SetString(5,MakeString("CombineSrc0RGB = CONSTANT"));
        break;
    case ACS_PREVIOUS:
        pkTree->SetString(5,MakeString("CombineSrc0RGB = PREVIOUS"));
        break;
    default:  // ACS_QUANTITY
        break;
    }

    switch ( m_eCombineSrc1RGB )
    {
    case ACS_TEXTURE:
        pkTree->SetString(6,MakeString("CombineSrc1RGB = TEXTURE"));
        break;
    case ACS_PRIMARY_COLOR:
        pkTree->SetString(6,MakeString("CombineSrc1RGB = PRIMARY_COLOR"));
        break;
    case ACS_CONSTANT:
        pkTree->SetString(6,MakeString("CombineSrc1RGB = CONSTANT"));
        break;
    case ACS_PREVIOUS:
        pkTree->SetString(6,MakeString("CombineSrc1RGB = PREVIOUS"));
        break;
    default:  // ACS_QUANTITY
        break;
    }

    switch ( m_eCombineSrc2RGB )
    {
    case ACS_TEXTURE:
        pkTree->SetString(7,MakeString("CombineSrc1RGB = TEXTURE"));
        break;
    case ACS_PRIMARY_COLOR:
        pkTree->SetString(7,MakeString("CombineSrc1RGB = PRIMARY_COLOR"));
        break;
    case ACS_CONSTANT:
        pkTree->SetString(7,MakeString("CombineSrc1RGB = CONSTANT"));
        break;
    case ACS_PREVIOUS:
        pkTree->SetString(7,MakeString("CombineSrc1RGB = PREVIOUS"));
        break;
    default:  // ACS_QUANTITY
        break;
    }

    switch ( m_eCombineSrc0Alpha )
    {
    case ACS_TEXTURE:
        pkTree->SetString(8,MakeString("CombineSrc0Alpha = TEXTURE"));
        break;
    case ACS_PRIMARY_COLOR:
        pkTree->SetString(8,MakeString("CombineSrc0Alpha = PRIMARY_COLOR"));
        break;
    case ACS_CONSTANT:
        pkTree->SetString(8,MakeString("CombineSrc0Alpha = CONSTANT"));
        break;
    case ACS_PREVIOUS:
        pkTree->SetString(8,MakeString("CombineSrc0Alpha = PREVIOUS"));
        break;
    default:  // ACS_QUANTITY
        break;
    }

    switch ( m_eCombineSrc1Alpha )
    {
    case ACS_TEXTURE:
        pkTree->SetString(9,MakeString("CombineSrc1Alpha = TEXTURE"));
        break;
    case ACS_PRIMARY_COLOR:
        pkTree->SetString(9,MakeString("CombineSrc1Alpha = PRIMARY_COLOR"));
        break;
    case ACS_CONSTANT:
        pkTree->SetString(9,MakeString("CombineSrc1Alpha = CONSTANT"));
        break;
    case ACS_PREVIOUS:
        pkTree->SetString(9,MakeString("CombineSrc1Alpha = PREVIOUS"));
        break;
    default:  // ACS_QUANTITY
        break;
    }

    switch ( m_eCombineSrc2Alpha )
    {
    case ACS_TEXTURE:
        pkTree->SetString(10,MakeString("CombineSrc2Alpha = TEXTURE"));
        break;
    case ACS_PRIMARY_COLOR:
        pkTree->SetString(10,MakeString("CombineSrc2Alpha = PRIMARY_COLOR"));
        break;
    case ACS_CONSTANT:
        pkTree->SetString(10,MakeString("CombineSrc2Alpha = CONSTANT"));
        break;
    case ACS_PREVIOUS:
        pkTree->SetString(10,MakeString("CombineSrc2Alpha = PREVIOUS"));
        break;
    default:  // ACS_QUANTITY
        break;
    }

    switch ( m_eCombineOp0RGB )
    {
    case ACO_SRC_COLOR:
        pkTree->SetString(11,MakeString("CombineOp0RGB = SRC_COLOR"));
        break;
    case ACO_ONE_MINUS_SRC_COLOR:
        pkTree->SetString(11,
            MakeString("CombineOp0RGB = ONE_MINUS_SRC_COLOR"));
        break;
    case ACO_SRC_ALPHA:
        pkTree->SetString(11,MakeString("CombineOp0RGB = SRC_ALPHA"));
        break;
    case ACO_ONE_MINUS_SRC_ALPHA:
        pkTree->SetString(11,
            MakeString("CombineOp0RGB = ONE_MINUS_SRC_ALPHA"));
        break;
    default:  // ACO_QUANTITY
        break;
    }

    switch ( m_eCombineOp1RGB )
    {
    case ACO_SRC_COLOR:
        pkTree->SetString(12,MakeString("CombineOp1RGB = SRC_COLOR"));
        break;
    case ACO_ONE_MINUS_SRC_COLOR:
        pkTree->SetString(12,
            MakeString("CombineOp1RGB = ONE_MINUS_SRC_COLOR"));
        break;
    case ACO_SRC_ALPHA:
        pkTree->SetString(12,MakeString("CombineOp1RGB = SRC_ALPHA"));
        break;
    case ACO_ONE_MINUS_SRC_ALPHA:
        pkTree->SetString(12,
            MakeString("CombineOp1RGB = ONE_MINUS_SRC_ALPHA"));
        break;
    default:  // ACO_QUANTITY
        break;
    }

    switch ( m_eCombineOp2RGB )
    {
    case ACO_SRC_COLOR:
        pkTree->SetString(13,MakeString("CombineOp2RGB = SRC_COLOR"));
        break;
    case ACO_ONE_MINUS_SRC_COLOR:
        pkTree->SetString(13,
            MakeString("CombineOp2RGB = ONE_MINUS_SRC_COLOR"));
        break;
    case ACO_SRC_ALPHA:
        pkTree->SetString(13,MakeString("CombineOp2RGB = SRC_ALPHA"));
        break;
    case ACO_ONE_MINUS_SRC_ALPHA:
        pkTree->SetString(13,
            MakeString("CombineOp2RGB = ONE_MINUS_SRC_ALPHA"));
        break;
    default:  // ACO_QUANTITY
        break;
    }

    switch ( m_eCombineOp0Alpha )
    {
    case ACO_SRC_COLOR:
        pkTree->SetString(14,MakeString("CombineOp0Alpha = SRC_COLOR"));
        break;
    case ACO_ONE_MINUS_SRC_COLOR:
        pkTree->SetString(14,
            MakeString("CombineOp0Alpha = ONE_MINUS_SRC_COLOR"));
        break;
    case ACO_SRC_ALPHA:
        pkTree->SetString(14,MakeString("CombineOp0Alpha = SRC_ALPHA"));
        break;
    case ACO_ONE_MINUS_SRC_ALPHA:
        pkTree->SetString(14,
            MakeString("CombineOp0Alpha = ONE_MINUS_SRC_ALPHA"));
        break;
    default:  // ACO_QUANTITY
        break;
    }

    switch ( m_eCombineOp1Alpha )
    {
    case ACO_SRC_COLOR:
        pkTree->SetString(15,MakeString("CombineOp1Alpha = SRC_COLOR"));
        break;
    case ACO_ONE_MINUS_SRC_COLOR:
        pkTree->SetString(15,
            MakeString("CombineOp1Alpha = ONE_MINUS_SRC_COLOR"));
        break;
    case ACO_SRC_ALPHA:
        pkTree->SetString(15,MakeString("CombineOp1Alpha = SRC_ALPHA"));
        break;
    case ACO_ONE_MINUS_SRC_ALPHA:
        pkTree->SetString(15,
            MakeString("CombineOp1Alpha = ONE_MINUS_SRC_ALPHA"));
        break;
    default:  // ACO_QUANTITY
        break;
    }

    switch ( m_eCombineOp2Alpha )
    {
    case ACO_SRC_COLOR:
        pkTree->SetString(16,MakeString("CombineOp2Alpha = SRC_COLOR"));
        break;
    case ACO_ONE_MINUS_SRC_COLOR:
        pkTree->SetString(16,
            MakeString("CombineOp2Alpha = ONE_MINUS_SRC_COLOR"));
        break;
    case ACO_SRC_ALPHA:
        pkTree->SetString(16,MakeString("CombineOp2Alpha = SRC_ALPHA"));
        break;
    case ACO_ONE_MINUS_SRC_ALPHA:
        pkTree->SetString(16,
            MakeString("CombineOp2Alpha = ONE_MINUS_SRC_ALPHA"));
        break;
    default:  // ACO_QUANTITY
        break;
    }

    switch ( m_eCombineScaleRGB )
    {
    case ACSC_ONE:
        pkTree->SetString(17,MakeString("CombineScaleRGB = ONE"));
        break;
    case ACSC_TWO:
        pkTree->SetString(17,MakeString("CombineScaleRGB = TWO"));
        break;
    case ACSC_FOUR:
        pkTree->SetString(17,MakeString("CombineScaleRGB = FOUR"));
        break;
    default:  // ACSC_QUANTITY
        break;
    }

    switch ( m_eCombineScaleAlpha )
    {
    case ACSC_ONE:
        pkTree->SetString(18,MakeString("CombineScaleAlpha = ONE"));
        break;
    case ACSC_TWO:
        pkTree->SetString(18,MakeString("CombineScaleAlpha = TWO"));
        break;
    case ACSC_FOUR:
        pkTree->SetString(18,MakeString("CombineScaleAlpha = FOUR"));
        break;
    default:  // ACSC_QUANTITY
        break;
    }

    switch ( m_eWrap )
    {
    case WM_CLAMP_S_CLAMP_T:
        pkTree->SetString(19,MakeString("uv mode = CLAMP_S_CLAMP_T"));
        break;
    case WM_CLAMP_S_WRAP_T:
        pkTree->SetString(19,MakeString("uv mode = CLAMP_S_WRAP_T"));
        break;
    case WM_WRAP_S_CLAMP_T:
        pkTree->SetString(19,MakeString("uv mode = WRAP_S_CLAMP_T"));
        break;
    case WM_WRAP_S_WRAP_T:
        pkTree->SetString(19,MakeString("uv mode = WRAP_S_WRAP_T"));
        break;
    case WM_CLAMP_BORDER_S_CLAMP_BORDER_T:
        pkTree->SetString(19,
            MakeString("uv mode = CLAMP_BORDER_S_CLAMP_BORDER_T"));
        break;
    default:  // WM_QUANTITY
        break;
    };

    switch ( m_eFilter )
    {
    case FM_NEAREST:
        pkTree->SetString(20,MakeString("filter = NEAREST"));
        break;
    case FM_LINEAR:
        pkTree->SetString(20,MakeString("filter = LINEAR"));
        break;
    default:  // FM_QUANTITY
        break;
    };

    switch ( m_eMipmap )
    {
    case MM_NONE:
        pkTree->SetString(21,MakeString("mipmap = NONE"));
        break;
    case MM_NEAREST:
        pkTree->SetString(21,MakeString("mipmap = NEAREST"));
        break;
    case MM_LINEAR:
        pkTree->SetString(21,MakeString("mipmap = LINEAR"));
        break;
    case MM_NEAREST_NEAREST:
        pkTree->SetString(21,MakeString("mipmap = NEAREST_NEAREST"));
        break;
    case MM_NEAREST_LINEAR:
        pkTree->SetString(21,MakeString("mipmap = NEAREST_LINEAR"));
        break;
    case MM_LINEAR_NEAREST:
        pkTree->SetString(21,MakeString("mipmap = LINEAR_NEAREST"));
        break;
    case MM_LINEAR_LINEAR:
        pkTree->SetString(21,MakeString("mipmap = LINEAR_LINEAR"));
        break;
    default:  // MM_QUANTITY
        break;
    }

    switch ( m_eEnvmap )
    {
    case EM_NONE:
        pkTree->SetString(22,MakeString("envmap = NONE"));
        break;
    case EM_IGNORE:
        pkTree->SetString(22,MakeString("envmap = IGNORE"));
        break;
    case EM_SPHERE:
        pkTree->SetString(22,MakeString("envmap = SPHERE"));
        break;
    default:  // EM_QUANTITY
        break;
    }

    pkTree->SetString(23,MakeString("blend color =",m_kBlendColor));
    pkTree->SetString(24,MakeString("priority =",m_fPriority));
    pkTree->SetString(25,MakeString("border color =",m_kBorderColor));

    // children
    pkTree->SetChild(0,Object::SaveStrings());
    pkTree->SetChild(1,m_spkImage->SaveStrings());

    return pkTree;
}
//----------------------------------------------------------------------------
int Texture::GetMemoryUsed () const
{
    int iBaseSize = sizeof(Texture) - sizeof(Object);
    int iTotalSize = iBaseSize + Object::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int Texture::GetDiskUsed () const
{
    return Object::GetDiskUsed() +
        sizeof(m_spkImage) +
        StreamBytesEnum(m_eCorrection) +
        StreamBytesEnum(m_eApply) +
        sizeof(m_kBlendColor) +
        StreamBytesEnum(m_eWrap) +
        StreamBytesEnum(m_eFilter) +
        StreamBytesEnum(m_eEnvmap) +
        StreamBytesEnum(m_eMipmap) +
        sizeof(m_fPriority) +
        sizeof(m_kBorderColor) +
        StreamBytesEnum(m_eCombineFuncRGB) +
        StreamBytesEnum(m_eCombineFuncAlpha) +
        StreamBytesEnum(m_eCombineSrc0RGB) +
        StreamBytesEnum(m_eCombineSrc1RGB) +
        StreamBytesEnum(m_eCombineSrc2RGB) +
        StreamBytesEnum(m_eCombineSrc0Alpha) +
        StreamBytesEnum(m_eCombineSrc1Alpha) +
        StreamBytesEnum(m_eCombineSrc2Alpha) +
        StreamBytesEnum(m_eCombineOp0RGB) +
        StreamBytesEnum(m_eCombineOp1RGB) +
        StreamBytesEnum(m_eCombineOp2RGB) +
        StreamBytesEnum(m_eCombineOp0Alpha) +
        StreamBytesEnum(m_eCombineOp1Alpha) +
        StreamBytesEnum(m_eCombineOp2Alpha) +
        StreamBytesEnum(m_eCombineScaleRGB) +
        StreamBytesEnum(m_eCombineScaleAlpha);
}
//----------------------------------------------------------------------------
