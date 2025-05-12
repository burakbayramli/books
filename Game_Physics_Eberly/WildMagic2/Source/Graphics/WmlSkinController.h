// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLSKINCONTROLLER_H
#define WMLSKINCONTROLLER_H

#include "WmlController.h"
#include "WmlGeometry.h"
#include "WmlNode.h"

namespace Wml
{

class WML_ITEM SkinController : public Controller
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    SkinController ();
    virtual ~SkinController ();

    // Bone nodes.  The first bone in the array must be the root bone.  The
    // skin controller needs access to its parent (if any) for setting the
    // trimesh object's local transforms.
    int& BoneQuantity ();
    Node**& Bones ();

    // Skin vertices.  The relationship between vertices and bones is
    // contained in this information.
    class WML_ITEM SkinVertex
    {
    public:
        int m_iIndex;
        float m_fWeight;
        Vector3f m_kOffset;
    };

    int*& SkinVertexQuantities ();
    SkinVertex**& SkinVertices ();

    virtual bool Update (float fAppTime);

protected:
    int m_iBoneQuantity;
    Node** m_apkBone;  // Node*[bone_quantity]
    int* m_aiSkinVertexQuantity;  // int[bone_quantity]

    // skinvert[bone_quantity][skinvertquan[i]], 0 <= i < bone_quantity
    SkinVertex** m_aakSkinVertex;
};

WmlSmartPointer(SkinController);
WmlRegisterStream(SkinController);
#include "WmlSkinController.inl"

}

#endif
