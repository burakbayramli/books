// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLGLOSSMAP_H
#define WMLGLOSSMAP_H

#include "WmlNode.h"
#include "WmlAlphaState.h"
#include "WmlTextureState.h"

namespace Wml
{

class Texture;

class WML_ITEM GlossMap : public Node
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    // Construction and destruction.  TO DO.  Do not require the application
    // to specify the texture unit.  This should be transparent to the user.
    GlossMap (Node* pkObjects, Texture* pkTexture, int iTexUnit);
    virtual ~GlossMap ();

    // member accessors
    NodePtr GetObjects () const;
    TextureStatePtr GetTextureState () const;
    int GetTextureUnit () const;
    AlphaStatePtr GetAlphaState () const;

    virtual Object* GetObjectByName (const char* acName);
    virtual void GetAllObjectsByName (const char* acName,
        std::vector<Object*>& rkObjects);

protected:
    GlossMap ();

    // geometric updates
    virtual void UpdateWorldBound ();

    // drawing 
    virtual void Draw (Renderer& rkRenderer);
    
    // RE-specific internal data
    NodePtr m_spkObjects;
    TextureStatePtr m_spkTextureState;
    AlphaStatePtr m_spkAlphaState;
    int m_iTexUnit;
    bool m_bForceCullObjects;
};

WmlSmartPointer(GlossMap);
WmlRegisterStream(GlossMap);
#include "WmlGlossMap.inl"

}

#endif
