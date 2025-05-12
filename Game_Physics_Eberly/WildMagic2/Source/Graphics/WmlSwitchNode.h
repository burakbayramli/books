// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLSWITCHNODE_H
#define WMLSWITCHNODE_H

#include "WmlNode.h"

namespace Wml
{

class WML_ITEM SwitchNode : public Node
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    // construction
    SwitchNode (int iQuantity = 1, int iGrowBy = 1);

    enum { SN_INVALID_CHILD = -1 };

    void SetActiveChild (int iActiveChild);
    int GetActiveChild () const;
    void DisableAllChildren ();

    // Picking support.  The origin and direction of the ray must be in world
    // coordinates.  The application is responsible for deleting the pick
    // records in the array.
    //
    // This function only picks on the active child.  If there
    // is a demand to support inactive children, I will need to add a member
    // to this class indicating whether or not the caller wants active-only
    // or all children to be picked.
    virtual void DoPick (const Vector3f& rkOrigin,
        const Vector3f& rkDirection, PickArray& rkResults);

protected:
    // drawing
    virtual void Draw (Renderer& rkRenderer);

    int m_iActiveChild;
};

WmlSmartPointer(SwitchNode);
WmlRegisterStream(SwitchNode);
#include "WmlSwitchNode.inl"

}

#endif
