// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLLIGHTSTATE_H
#define WMLLIGHTSTATE_H

#include "WmlLight.h"
#include "WmlRenderState.h"

namespace Wml
{

WmlSmartPointer(LightState);

class WML_ITEM LightState : public RenderState
{
    WmlDeclareDefaultState(LightState);
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    LightState ();
    virtual ~LightState ();

    virtual Type GetType () const;

    enum { MAX_LIGHTS = 8 };

    int Attach (Light* pkLight);
    int Detach (Light* pkLight);
    LightPtr Detach (int i);
    void DetachAll ();

    int GetQuantity () const;
    Light* Get (int i);

    // support for searching by name
    virtual Object* GetObjectByName (const char* acName);
    virtual void GetAllObjectsByName (const char* acName,
        std::vector<Object*>& rkObjects);

protected:
    // Support for the UpdateRS pass.  The input parameters are the stack of
    // LightState objects visited during the recursive traversal.  When the
    // traversal reaches a Geometry leaf node, the accumulated lights are
    // combined into a single LightState object that represents the light
    // state at that leaf.  This function returns that object to the Geometry
    // object.
    virtual RenderState* Extract (int iLastIndex, RenderState* apkState[]);

    LightPtr m_aspkLight[MAX_LIGHTS];
};

WmlRegisterStream(LightState);

}

#endif
