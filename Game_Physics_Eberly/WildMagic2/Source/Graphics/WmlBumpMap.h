// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLBUMPMAP_H
#define WMLBUMPMAP_H

#include "WmlAlphaState.h"
#include "WmlLight.h"
#include "WmlNode.h"
#include "WmlTexture.h"
#include "WmlTextureState.h"

namespace Wml
{

class TriMesh;

class WML_ITEM BumpMap : public Node
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    // Construction and destruction.  The bModulate input controls whether the
    // surface lighting is modulated by the surface material and light
    // combination. That is, it controls whether the bump map is decaled onto
    // the surface or modulates the lighted surface.
    BumpMap (Node* pkObjects, Texture* pkNormalMap, Light* pkLight, 
        bool bModulate);

    virtual ~BumpMap ();

    // member accessors
    NodePtr GetObjects () const;
    TexturePtr GetNormalMap () const;
    TextureStatePtr GetTextureState () const;
    LightPtr GetLight () const;
    AlphaStatePtr GetAlphaState () const;
    bool GetModulate () const;

    // texture blend colors
    void SetCurrentAmbientMaterial (const ColorRGB& rkColor);
    void SetCurrentDiffuseMaterial (const ColorRGB& rkColor,
        int iMaxTextureUnits);

    // compute and store the light vectors for interpolation
    void ComputeLightVectors (TriMesh& rkMesh);

    virtual Object* GetObjectByName (const char* acName);
    virtual void GetAllObjectsByName (const char* acName,
        std::vector<Object*>& rkObjects);

protected:
    BumpMap ();

    // geometric updates
    virtual void UpdateWorldBound ();

    // drawing 
    virtual void Draw (Renderer& rkRenderer);
    
    // RE-specific internal data
    NodePtr m_spkObjects;
    TexturePtr m_spkNormalMap;
    TextureStatePtr m_spkTextureState;
    TextureStatePtr m_spkTextureStateModulated;
    LightPtr m_spkLight;
    AlphaStatePtr m_spkAlphaState;
    bool m_bModulate;
    bool m_bForceCullObjects;
};

WmlSmartPointer(BumpMap);
WmlRegisterStream(BumpMap);
#include "WmlBumpMap.inl"

}

#endif
