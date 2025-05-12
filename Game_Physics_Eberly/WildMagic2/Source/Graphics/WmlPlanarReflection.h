// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLPLANARREFLECTION_H
#define WMLPLANARREFLECTION_H

#include "WmlNode.h"
#include "WmlTriMesh.h"

namespace Wml
{

class WML_ITEM PlanarReflection : public Node
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    // For a perfect mirror in a closed environment, select the reflectance
    // to be zero.
    PlanarReflection (Node* pkCaster, TriMesh* pkPlane, float fReflectance);
    virtual ~PlanarReflection ();

    // member accessors
    NodePtr GetCaster () const;
    TriMeshPtr GetPlane () const;
    const Vector3f& GetPlaneNormal () const;
    const Vector3f& GetPointOnPlane () const;
    const int& GetStencilValue () const;
    const float& GetReflectance () const;

    virtual Object* GetObjectByName (const char* acName);
    virtual void GetAllObjectsByName (const char* acName,
        std::vector<Object*>& rkObjects);
    
protected:
    PlanarReflection ();

    // geometric updates
    virtual void UpdateWorldBound ();

    // drawing 
    virtual void Draw (Renderer& rkRenderer);
    
    // RE-specific internal data
    NodePtr m_spkCaster;
    TriMeshPtr m_spkPlane;
    float m_fReflectance;
    bool m_bForceCullCaster;
    bool m_bForceCullPlane;

    // model space data for the plane of the reflection
    Vector3f m_kNormal;
    Vector3f m_kPointOnPlane;

    // unique value in the stencil buffer for this feature
    int m_iStencilValue;

    // TO DO:  This needs to be managed at a higher level so that different
    // features can safely share the stencil buffer.
    static int ms_iNextFreeStencilValue;
};

WmlSmartPointer(PlanarReflection);
WmlRegisterStream(PlanarReflection);
#include "WmlPlanarReflection.inl"

}

#endif
