// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLPROJECTEDTEXTURE_H
#define WMLPROJECTEDTEXTURE_H

#include "WmlCamera.h"
#include "WmlNode.h"
#include "WmlTexture.h"
#include "WmlTextureState.h"

namespace Wml
{

class WML_ITEM ProjectedTexture : public Node
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    // construction and destruction
    ProjectedTexture (Node* pkObjects, Camera* pkCamera, Texture* pkTexture);
    virtual ~ProjectedTexture ();

    // member accessors
    NodePtr GetObjects () const;
    CameraPtr GetCamera () const;
    TexturePtr GetTexture () const;
    TextureStatePtr GetTextureState () const;

    virtual Object* GetObjectByName (const char* acName);
    virtual void GetAllObjectsByName (const char* acName,
        std::vector<Object*>& rkObjects);

protected:
    ProjectedTexture ();

    // geometric updates
    virtual void UpdateWorldBound ();

    // drawing 
    virtual void Draw (Renderer& rkRenderer);

    // RE-specific internal data
    NodePtr m_spkObjects;
    CameraPtr m_spkCamera;
    TexturePtr m_spkTexture;
    TextureStatePtr m_spkTextureState;
    bool m_bForceCullObjects;
};

WmlSmartPointer(ProjectedTexture);
#include "WmlProjectedTexture.inl"

}

#endif
