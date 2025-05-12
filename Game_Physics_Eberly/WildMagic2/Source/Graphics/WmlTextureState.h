// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLTEXTURESTATE_H
#define WMLTEXTURESTATE_H

#include "WmlRenderState.h"
#include "WmlTexture.h"

namespace Wml
{

WmlSmartPointer(TextureState);

class WML_ITEM TextureState : public RenderState
{
    WmlDeclareDefaultState(TextureState);
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    TextureState ();
    virtual ~TextureState ();

    virtual Type GetType () const;

    enum { MAX_TEXTURES = 4 };

    void Set (int i, Texture* pkTexture);
    Texture* Get (int i);
    TexturePtr Remove (int i);
    void RemoveAll ();

    int GetQuantity () const;

    // support for searching by name
    virtual Object* GetObjectByName (const char* acName);
    virtual void GetAllObjectsByName (const char* acName,
        std::vector<Object*>& rkObjects);

protected:
    // Support for the UpdateRS pass.  The input parameters are the stack of
    // TextureState objects visited during the recursive traversal.  When the
    // traversal reaches a Geometry leaf node, the textures are combined into
    // a single TextureState object that represents the texture state at that
    // leaf.  This function returns that object to the Geometry object.
    virtual RenderState* Extract (int iLastIndex, RenderState* apkState[]);

    TexturePtr m_aspkTexture[MAX_TEXTURES];
};

WmlRegisterStream(TextureState);

}

#endif


