// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLBILLBOARDNODE_H
#define WMLBILLBOARDNODE_H

#include "WmlNode.h"

namespace Wml
{

class Camera;

class WML_ITEM BillboardNode : public Node
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    // The model space of the billboard has an up-vector of (0,1,0) that is
    // chosen to be the billboard's axis of rotation.  The billboard's plane
    // has normal (0,0,1).

    // construction
    BillboardNode (int iQuantity = 1, int iGrowBy = 1);

protected:
    // geometric updates
    void RotateBillboard (const Camera* pkCamera);
    virtual void UpdateWorldData (float fAppTime);

    // drawing
    virtual void Draw (Renderer& rkRenderer);

    // for deferred updates
    float m_fLastUpdateTime;
};

WmlSmartPointer(BillboardNode);
WmlRegisterStream(BillboardNode);

}

#endif
