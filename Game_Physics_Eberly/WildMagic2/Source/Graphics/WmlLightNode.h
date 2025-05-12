// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLLIGHTNODE_H
#define WMLLIGHTNODE_H

#include "WmlLight.h"
#include "WmlNode.h"

namespace Wml
{

class WML_ITEM LightNode : public Node
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    // construction
    LightNode (Light* pkLight = NULL, int iQuantity = 1, int iGrowBy = 1);

    // member access
    void SetLight (Light* pkLight);
    Light* GetLight ();
    const Light* GetLight () const;

    // Geometric updates.  The world translation of the node is used for the
    // location of a point/spot light.  The last column of the world rotation
    // matrix is used for the direction of a directional/spot light.  Location
    // is irrelevant for a directional light.  Location and direction are
    // irrelevant for an ambient light.
    virtual void UpdateWorldData (float fAppTime);

    // support for searching by name
    virtual Object* GetObjectByName (const char* acName);
    virtual void GetAllObjectsByName (const char* acName,
        std::vector<Object*>& rkObjects);

protected:
    LightPtr m_spkLight;
};

WmlSmartPointer(LightNode);
WmlRegisterStream(LightNode);
#include "WmlLightNode.inl"

}

#endif
