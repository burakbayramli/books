// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLCAMERANODE_H
#define WMLCAMERANODE_H

#include "WmlCamera.h"
#include "WmlNode.h"

namespace Wml
{

class WML_ITEM CameraNode : public Node
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    // construction
    CameraNode (Camera* pkCamera = NULL, int iQuantity = 1, int iGrowBy = 1);

    // member access
    void SetCamera (Camera* pkCamera);
    Camera* GetCamera ();
    const Camera* GetCamera () const;

    // Geometric updates.  The world translation of the node is used for the
    // location of the camera.  Column 0 of the world rotation matrix is used
    // for the camera left vector.  Column 1 is used for the camera up vector.
    // Column 2 is used for the camera direction vector.
    virtual void UpdateWorldData (float fAppTime);

    // support for searching by name
    virtual Object* GetObjectByName (const char* acName);
    virtual void GetAllObjectsByName (const char* acName,
        std::vector<Object*>& rkObjects);

protected:
    CameraPtr m_spkCamera;
};

WmlSmartPointer(CameraNode);
WmlRegisterStream(CameraNode);
#include "WmlCameraNode.inl"

}

#endif
