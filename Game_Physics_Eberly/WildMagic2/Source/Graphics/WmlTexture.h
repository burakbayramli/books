// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLTEXTURE_H
#define WMLTEXTURE_H

#include "WmlColorRGB.h"
#include "WmlImage.h"
#include "WmlObject.h"

namespace Wml
{

class WML_ITEM Texture : public Object
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    Texture ();
    virtual ~Texture ();

    enum CorrectionMode
    {
        CM_AFFINE,
        CM_PERSPECTIVE,
        CM_QUANTITY
    };

    enum ApplyMode
    {
        AM_REPLACE,
        AM_DECAL,
        AM_MODULATE,
        AM_BLEND,
        AM_ADD,
        AM_COMBINE,
        AM_QUANTITY
    };

    enum ApplyCombineFunction
    {
        ACF_REPLACE,
        ACF_MODULATE,
        ACF_ADD,
        ACF_ADD_SIGNED,
        ACF_SUBTRACT,
        ACF_INTERPOLATE,
        ACF_DOT3_RGB,
        ACF_DOT3_RGBA,
        ACF_QUANTITY
    };

    enum ApplyCombineSrc
    {
        ACS_TEXTURE,
        ACS_PRIMARY_COLOR,
        ACS_CONSTANT,
        ACS_PREVIOUS,
        ACS_QUANTITY
    };

    enum ApplyCombineOperand
    {
        ACO_SRC_COLOR,
        ACO_ONE_MINUS_SRC_COLOR,
        ACO_SRC_ALPHA,
        ACO_ONE_MINUS_SRC_ALPHA,
        ACO_QUANTITY
    };

    enum ApplyCombineScale
    {
        ACSC_ONE,
        ACSC_TWO,
        ACSC_FOUR,
        ACSC_QUANTITY
    };

    enum WrapMode
    {
        WM_CLAMP_S_CLAMP_T,
        WM_CLAMP_S_WRAP_T,
        WM_WRAP_S_CLAMP_T,
        WM_WRAP_S_WRAP_T,
        WM_CLAMP_BORDER_S_CLAMP_BORDER_T,
        WM_QUANTITY
    };

    enum FilterMode
    {
        FM_NEAREST,
        FM_LINEAR,
        FM_QUANTITY
    };

    enum MipmapMode
    {
        MM_NONE,
        MM_NEAREST,
        MM_LINEAR,
        MM_NEAREST_NEAREST,
        MM_NEAREST_LINEAR,
        MM_LINEAR_NEAREST,
        MM_LINEAR_LINEAR,
        MM_QUANTITY
    };

    enum EnvmapMode
    {
        EM_NONE,
        EM_IGNORE,
        EM_SPHERE,
        EM_QUANTITY
    };

    CorrectionMode& Correction ();  // default: CM_PERSPECTIVE
    ApplyMode& Apply ();            // default: AM_REPLACE
    ColorRGB& BlendColor ();        // default: ColorRGB(0,0,0)
    ColorRGB& BorderColor ();       // default: ColorRGB(0,0,0)
    WrapMode& Wrap ();              // default: WM_CLAMP_S_CLAMP_T
    FilterMode& Filter ();          // default: FM_NEAREST
    MipmapMode& Mipmap ();          // default: MM_NONE
    EnvmapMode& Envmap ();          // default: EM_NONE
    float& Priority ();             // default: 1

    // Store and retrieve up to 8 bytes of data.  The value iSize should be
    // between 1 and 8.
    void SetUserData (int iSize, const void* pvData);
    void GetUserData (int iSize, void* pvData);

    // the following are only relevant if the apply mode is AM_COMBINE
    ApplyCombineFunction& CombineFuncRGB ();   // default: ACF_REPLACE
    ApplyCombineFunction& CombineFuncAlpha (); // default: ACF_REPLACE
    ApplyCombineSrc& CombineSrc0RGB ();        // default: ACS_TEXTURE
    ApplyCombineSrc& CombineSrc1RGB ();        // default: ACS_TEXTURE
    ApplyCombineSrc& CombineSrc2RGB ();        // default: ACS_TEXTURE
    ApplyCombineSrc& CombineSrc0Alpha ();      // default: ACS_TEXTURE
    ApplyCombineSrc& CombineSrc1Alpha ();      // default: ACS_TEXTURE
    ApplyCombineSrc& CombineSrc2Alpha ();      // default: ACS_TEXTURE
    ApplyCombineOperand& CombineOp0RGB ();     // default: ACO_SRC_COLOR
    ApplyCombineOperand& CombineOp1RGB ();     // default: ACO_SRC_COLOR
    ApplyCombineOperand& CombineOp2RGB ();     // default: ACO_SRC_COLOR
    ApplyCombineOperand& CombineOp0Alpha ();   // default: ACO_SRC_COLOR
    ApplyCombineOperand& CombineOp1Alpha ();   // default: ACO_SRC_COLOR
    ApplyCombineOperand& CombineOp2Alpha ();   // default: ACO_SRC_COLOR
    ApplyCombineScale& CombineScaleRGB ();     // default: ACSC_ONE
    ApplyCombineScale& CombineScaleAlpha ();   // default: ACSC_ONE

    void SetImage (Image* pkImage);
    Image* GetImage ();

    // support for searching by name
    virtual Object* GetObjectByName (const char* acName);
    virtual void GetAllObjectsByName (const char* acName,
        std::vector<Object*>& rkObjects);

protected:
    ImagePtr m_spkImage;
    CorrectionMode m_eCorrection;
    ApplyMode m_eApply;
    ColorRGB m_kBlendColor;
    WrapMode m_eWrap;
    FilterMode m_eFilter;
    MipmapMode m_eMipmap;
    EnvmapMode m_eEnvmap;
    float m_fPriority;
    char m_acUserData[8];

    // the following is relevant only if the wrap mode is clamp-to-border
    ColorRGB m_kBorderColor;

    // the following are relevant only if the apply mode is AM_COMBINE
    ApplyCombineFunction m_eCombineFuncRGB;
    ApplyCombineFunction m_eCombineFuncAlpha;
    ApplyCombineSrc m_eCombineSrc0RGB;
    ApplyCombineSrc m_eCombineSrc1RGB;
    ApplyCombineSrc m_eCombineSrc2RGB;
    ApplyCombineSrc m_eCombineSrc0Alpha;
    ApplyCombineSrc m_eCombineSrc1Alpha;
    ApplyCombineSrc m_eCombineSrc2Alpha;
    ApplyCombineOperand m_eCombineOp0RGB;
    ApplyCombineOperand m_eCombineOp1RGB;
    ApplyCombineOperand m_eCombineOp2RGB;
    ApplyCombineOperand m_eCombineOp0Alpha;
    ApplyCombineOperand m_eCombineOp1Alpha;
    ApplyCombineOperand m_eCombineOp2Alpha;
    ApplyCombineScale m_eCombineScaleRGB;
    ApplyCombineScale m_eCombineScaleAlpha;
};

WmlSmartPointer(Texture);
WmlRegisterStream(Texture);
#include "WmlTexture.inl"

}

#endif
