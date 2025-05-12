// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLRENDERSTATE_H
#define WMLRENDERSTATE_H

#include "WmlController.h"
#include "WmlObject.h"
#include "WmlRenderState.mcr"

namespace Wml
{

WmlSmartPointer(RenderState);

class WML_ITEM RenderState : public Object
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    // supported render states
    enum Type
    {
        RS_ALPHA,
        RS_DITHER,
        RS_CULL,
        RS_FOG,
        RS_LIGHT,
        RS_MATERIAL,
        RS_POLYGONOFFSET,
        RS_SHADE,
        RS_TEXTURE,
        RS_VERTEXCOLOR,
        RS_WIREFRAME,
        RS_ZBUFFER,
        RS_MAX_STATE
    };

    virtual Type GetType () const = 0;


    class WML_ITEM Stack
    {
    public:
        Stack ();
        void Push (RenderState* pkState);
        void Pop (RenderState* pkState);
        void CopyTo (RenderStatePtr aspkState[]);

    protected:
        // Maximum allowable depth for scene graph with respect to render
        // state updating.  The scene graph system itself does not limit the
        // depth.  However, the maximum depth specified here is quite large.
        // If a scene is ever encountered that exceeds this, the scene itself
        // needs to be reassessed.
        enum { RS_MAX_STACK = 128 };

        RenderState* m_aapkState[RS_MAX_STATE][RS_MAX_STACK];
        int m_aiTop[RS_MAX_STATE];
    };

    // Each RenderState-derived class is provided the stack of same-class
    // objects that were visited during an UpdateRS pass.  The class decides
    // how to parse this stack to provide the final render state for that
    // class object.
    virtual RenderState* Extract (int iLastIndex, RenderState* apkState[]);


    class WML_ITEM List
    {
    public:
        RenderStatePtr m_spkState;
        List* m_pkNext;
    };


    static RenderState** GetDefaultStates () { return ms_apkDefault; }

protected:
    // abstract base class
    RenderState ();
    virtual ~RenderState ();

    // default states
    static RenderState* ms_apkDefault[RS_MAX_STATE];
};

WmlRegisterStream(RenderState);

}

#endif


